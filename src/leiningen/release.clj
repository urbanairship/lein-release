(ns leiningen.release
  (:require
   [clojure.java.shell :as sh]
   [clojure.string     :as string])
  (:import
   [java.util.regex Pattern]))

(defn raise [fmt & args]
  (throw (RuntimeException. (apply format fmt args))))

(def ^:dynamic config {})

(def *scm-systems*
     {:git {:add    ["git" "add"]
            :tag    ["git" "tag"]
            :commit ["git" "commit"]
            :push   ["git" "push" "origin" "master"]
            :status ["git" "status"]}})

(defn detect-scm []
  (or
   (:scm config)
   (cond
     (.exists (java.io.File. ".git"))
     :git
     :no-scm-detected
     (raise "Erorr: no scm detected! (I know only about git for now)."))))

(defn sh! [& args]
  (let [res (apply sh/sh args)]
    (.println System/out (:out res))
    (.println System/err (:err res))
    (when-not (zero? (:exit res))
      (raise "Error: command failed %s => %s" args res))))

(defn scm! [cmd & args]
  (let [scm   (detect-scm)
        scm-cmd (get-in *scm-systems* [scm cmd])]
    (if-not scm-cmd
      (raise "No such SCM command: %s in %s" cmd scm))
    (apply sh! (concat scm-cmd args))))

(defn compute-next-development-version [current-version]
  (let [parts             (vec (.split current-version "\\."))
        version-parts     (vec (take (dec (count parts)) parts))
        minor-version     (last parts)
        new-minor-version (str (inc (Integer/parseInt minor-version)) "-SNAPSHOT")]
    (string/join "." (conj version-parts new-minor-version))))

(defn replace-project-version [old-vstring new-vstring]
  (let [proj-file     (slurp "project.clj")
        new-proj-file (.replaceAll proj-file (format "\\(defproject .+? %s" old-vstring) new-vstring )
        matcher       (.matcher
                       (Pattern/compile (format "(\\(defproject .+? )\"\\Q%s\\E\"" old-vstring))
                       proj-file)]
    (if-not (.find matcher)
      (raise "Error: unable to find version string %s in project.clj file!" old-vstring))
    (.replaceFirst matcher (format "%s\"%s\"" (.group matcher 1) new-vstring))))

(defn set-project-version! [old-vstring new-vstring]
  (spit "project.clj" (replace-project-version old-vstring new-vstring)))

(defn detect-deployment-strategy [project]
  (cond
    (:deploy-via config)
    (:deploy-via config)

    (:repositories project)
    :lein-deploy

    :no-deploy-strategy
    :lein-install))

(defn perform-deploy! [project project-jar]
  (case (detect-deployment-strategy project)

    :lein-deploy
    (sh! "lein" "deploy")

    :lein-install
    (sh! "lein" "install")

    :clojars
    (sh! "scp" "pom.xml" project-jar "clojars@clojars.org:")

    (raise "Error: unrecognized deploy strategy: %s" (detect-deployment-strategy))))

(defn extract-project-version-from-file
  ([]
     (extract-project-version-from-file "project.clj"))
  ([proj-file]
     (let [s (slurp proj-file)
           m (.matcher (Pattern/compile "\\(defproject .+? \"([^\"]+?)\"") s)]
       (if-not (.find m)
         (raise "Error: unable to find project version in file: %s" proj-file))
       (.group m 1))))

(defn is-snapshot? [vstring]
  (.endsWith vstring "-SNAPSHOT"))

(defn release [project & args]
  (binding [config (or (:lein-release project) config)]
    (let [current-version  (get project :version)
          release-version  (.replaceAll current-version "-SNAPSHOT" "")
          next-dev-version (compute-next-development-version release-version)
          jar-file-name    (format "%s-%s.jar" (:name project) release-version)]
      (when (is-snapshot? current-version)
        (println (format "setting project version %s => %s" current-version release-version))
        (set-project-version! current-version release-version)
        (println "adding, committing and tagging project.clj")
        (scm! :add "project.clj")
        (scm! :commit "-m" (format "lein-release plugin: preparing %s release" release-version))
        (scm! :tag (format "%s-%s" (:name project) release-version)))
      (when-not (.exists (java.io.File. jar-file-name))
        (println "creating jar and pom files...")
        (sh! "lein" "jar")
        (sh! "lein" "pom"))
      (perform-deploy! project jar-file-name)
      (when-not (is-snapshot? (extract-project-version-from-file))
        (println (format "updating version %s => %s for next dev cycle" release-version next-dev-version))
        (set-project-version! release-version next-dev-version)
        (scm! :add "project.clj")
        (scm! :commit "-m" (format "lein-release plugin: bumped version from %s to %s for next developemnt cycle" release-version next-dev-version)))
      (prn (format "VERSION: %s => %s => %s" current-version release-version next-dev-version)))))

