(ns ^:no-doc io.github.evenmoreirrelevance.classy.util
  (:require
   [clojure.string :as str]
   [io.github.evenmoreirrelevance.classy.util :as util]))

(defn dots2slashes
  [s]
  (str/replace s \. \/))

(defn ^Class load-and-compile
  ([cname bytecode]
   (load-and-compile nil cname bytecode))
  ([^java.util.List *loaders cname bytecode]
   (locking (or *loaders (Object.))
     (let [loader
           (or (first
                 (drop-while
                   #(try (Class/forName % false cname) (catch ClassNotFoundException _ nil))
                   *loaders))
             (doto (clojure.lang.DynamicClassLoader.) (cond->> *loaders (.add *loaders))))
           cls
           (.defineClass ^clojure.lang.DynamicClassLoader loader cname bytecode nil)]
       (when *compile-files* (Compiler/writeClassFile (dots2slashes cname) bytecode))
       cls))))

(defn map-by
  ([kf xs]
   (map-by kf identity xs))
  ([kf vf xs]
   (map-by {} kf vf xs))
  ([empty kf vf xs]
   (let [empty (or empty {})]
     (cond
       (not (map? empty))
       (throw (IllegalArgumentException. "`empty` must be a map or nil"))
       :else
       (into empty (map (juxt kf vf)) (reverse #_"keep first element w/ given key" xs))))))

(defn distinct-by
  ([kf zeq]
   (sequence (distinct-by kf) zeq))
  ([kf]
   (fn [rf]
     (let [!place (volatile! (transient {}))]
       (fn reducer
         ([] (rf))
         ([out]
          (vreset! !place (transient {}))
          (rf out))
         ([acc in]
          (let [place @!place
                k (kf in)]
            (if (get place k)
              acc
              (do (vreset! !place (assoc! place k true))
                (rf acc in))))))))))

(defmacro the
  [tag expr]
  (let [x_ (with-meta (gensym expr) {:tag tag})]
    `(let [~x_ ~expr] ~x_)))

(defmacro type-of%%
  [sym]
  (let [b (the clojure.lang.Compiler$LocalBinding (get &env sym))]
    (or
      (.getPrimitiveType b)
      (.getJavaClass b)
      nil)))

(defmacro type-of%
  [tag]
  `(let [x# (the ~tag nil)]
     (type-of%% x#)))

(defn type-of
  (^Class [tag]
   (eval `(type-of% ~tag))))

(defmacro throw-when [[bind expr] msg map]
  `(when-let [~bind ~expr]
     (throw (ex-info ~msg ~map))))

(defn is?
  {:inline (fn [p x] `(~p ~x))}
  [p x] (p x))

(defmacro condctx
  ([] nil)
  ([x] x)
  ([l r & more]
   (if (keyword? l)
     `(~(symbol l) ~r (condctx ~@more))
     `(if ~l ~r (condctx ~@more)))))

(defn whenp
  {:inline (fn [p x] `(let [x# ~x] (when x# (when (~p x#) x#))))}
  [p x] (when x (when (p x) x)))

(defn updates
  ([m] m)
  ([m k f & more] (apply updates (update m k f) more)))

(defn assoc-some
  ([m] m)
  ([m k v & more] (apply assoc-some (cond-> m (some? v) (assoc k v)) more)))

(defn require-keys!
  [m keys]
  (util/throw-when [missing (seq (remove #(contains? m %) keys))]
    "some keys missing in map" {:map m :missing missing})
  m)