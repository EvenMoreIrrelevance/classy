(ns ^:no-doc io.github.evenmoreirrelevance.classy.util
  (:require
   [clojure.string :as str]))

(defn dots2slashes
  [s]
  (str/replace s \. \/))

(defmacro once
  [[_def & def-frm]]
  (assert (`#{def} _def))
  `(defonce ~@def-frm))

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

(defmacro the [expr tag]
  (let [x_ (with-meta (gensym expr) {:tag tag})]
    `(let [~x_ ~expr] ~x_)))

(defmacro throw-when [[bind expr] msg map]
  `(when-let [~bind ~expr]
     (throw (ex-info ~msg ~map))))
