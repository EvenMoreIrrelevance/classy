(ns build
  (:require
   [clojure.tools.build.api :as b]
   [deps-deploy.deps-deploy :as deps-deploy]
   [clojure.string :as str]
   [clojure.edn :as edn]))

(defn re-quote
  (^String [s]
   (java.util.regex.Pattern/quote s)))

(let [deps-edn (edn/read-string (slurp "deps.edn"))]
  (def lib (get-in deps-edn [:io.github.evenmoreirrelevance/libdesc :lib]))
  (def version (get-in deps-edn [:io.github.evenmoreirrelevance/libdesc :mvn/version])))

(def class-dir "target/classes")
(def jar-file (format "target/%s-%s.jar" (name lib) version))

;; delay to defer side effects (artifact downloads)
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn- jar-opts [opts]
  (assoc opts
    :lib lib :version version
    :jar-file jar-file
    :scm {:tag (str "v" version)}
    :basis @basis
    :class-dir class-dir
    :target "target"
    :src-dirs ["src"]))

(defn clean [_]
  (b/delete {:path "target"}))

(defn sync-pom [_]
  (b/write-pom (conj (jar-opts {}) {:target "." :class-dir nil})))

(defn export-kondo [_]
  (let [postfix (str (str/replace (namespace lib) \. \/) "/" (name lib) "/")]
    (b/copy-dir {:src-dirs [(str ".clj-kondo/" postfix)]
                 :target-dir (str "resources/clj-kondo.exports/" postfix)})))

(defn jar [_]
  (sync-pom _)
  (export-kondo _)
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn install [_]
  (b/install (jar-opts {})))

(when-let [creds
           (try (edn/read-string (slurp "../clojars-credentials.edn"))
             (catch java.io.IOException _e nil))]
  (alter-var-root
    (var deps-deploy/default-repo-settings)
    update "clojars" merge (get creds (namespace lib))))

(defn dump-reader
  [src ^java.io.Writer targ]
  (let [buffsrc (java.io.BufferedReader. ^java.io.Reader src)]
    (doseq [^String l (take-while some? (repeatedly #(.readLine buffsrc)))]
      (.append targ (str l "\n")))))

(defn runit
  ([args] (runit {} args))
  ([{:keys [input error]} args]
   (let [^java.util.List args (vec args)
         input (or input *out*)
         error (or error *err*)
         p (-> (ProcessBuilder. args) (.start))
         re (future (dump-reader (java.io.InputStreamReader. (.getErrorStream p)) error))
         ri (future (dump-reader (java.io.InputStreamReader. (.getInputStream p)) input))
         res (try (loop []
                    (or (try (.waitFor p) (catch InterruptedException _ nil))
                      (recur)))
               (finally (.destroy p)))]
     (run! deref [re ri])
     res)))

(defn test-all
  [_]
  (let [test-files (filter #(str/ends-with? % ".clj")
                     (map str
                       (file-seq (java.io.File. "test/"))))]
    (run! #(load-file %) test-files)
    (@(requiring-resolve 'clojure.test/run-all-tests)
     (re-pattern (str (re-quote (str (namespace lib) "." (name lib) ".test.")) ".*")))))

(defn deploy [{:keys [test?] :as _opts}]
  (let [b (str/trim (with-out-str (runit ["git" "rev-parse" "--abbrev-ref" "HEAD"])))]
    (when-not (= "main" b)
      (throw (ex-info "must be on main branch" {:branch b}))))
  (when-not (and
              (= 0 (runit ["git" "diff-index" "--quiet" "--cached" "HEAD" "--"]))
              (= 0 (runit ["git" "diff-files" "--quiet"])))
    (throw (ex-info "worktree or index not clean" {})))
  (when test?
    (let [{:keys [fail error]} (test-all nil)]
      (when (or (< 0 fail) (< 0 error))
        (throw (ex-info "tests failed." {:fail fail :error error})))))
  (let [tag (str "v" version)]
    (when (= tag (str/trim (with-out-str (runit ["git" "tag" "--list" tag]))))
      (throw (ex-info "version already tagged" {:version version})))
    (jar _opts)
    (when-not (and
                (= 0 (runit ["git" "commit" "-am" (str "deploy " tag)]))
                (= 0 (runit ["git" "tag" "--force" tag]))
                (= 0 (runit ["git" "push"]))
                (= 0 (runit ["git" "push" "origin" "tag" "--force" tag])))
      (throw (ex-info "errors while pushing to repo" {})))
    (deps-deploy/deploy
      {:artifact jar-file
       :installer :remote
       :sign-releases? false})))

(comment 
  (clean nil)
  (test-all nil)
  (deploy {:test? true}))
