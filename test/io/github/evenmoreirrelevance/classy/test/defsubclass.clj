(ns io.github.evenmoreirrelevance.classy.test.defsubclass
  (:require
   [io.github.evenmoreirrelevance.classy.core :as classy]
   [io.github.evenmoreirrelevance.classy.util :as util]
   [clojure.test :as test]))

(defonce tomb (Object.))
#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defonce notfound (Object.))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro entry [k v] `(clojure.lang.MapEntry/create ~k ~v))

(classy/defsubclass A (java.util.ArrayList [^java.util.Collection x])
  [_1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22]
  clojure.lang.IDeref
  (deref [_]
    [_1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22]))

(classy/defsubclass Ex1 [Exception [msg]] []
  clojure.lang.IExceptionInfo
  (getData [_] {:foo :bar})
  (getMessage [_] "I am a stegosaurus!"))

(classy/defsubclass Ex2 [Ex1 [msg]] []
  clojure.lang.IExceptionInfo
  (getMessage [_] "Hello there!"))

(test/deftest defsubclass
  (test/testing "long ctor"
    (test/is (instance? A (->A 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 []))))
  (test/testing "chain inheritance"
    (let [e1 (->Ex1 "hi")
          e2 (->Ex2 "hello")]
      (test/is (= (.getMessage e1) "I am a stegosaurus!"))
      (test/is (= (.getMessage e2) "Hello there!"))
      (test/is (= (.getData e1) (.getData e2) {:foo :bar}))))
  (test/testing "big fat e2e test"
    (eval
      '(classy/defsubclass MapStackIterator [Object []]
         [^:unsynchronized-mutable ^boolean didfront
          ^:unsynchronized-mutable nxt
          front
          ^java.util.Iterator f-iter
          ^java.util.Iterator b-iter]
         java.util.Iterator
         (hasNext [_]
           (loop []
             (or (boolean nxt)
               (if-not didfront
                 (if (.hasNext f-iter)
                   (let [nextf (.next f-iter)]
                     (when-not (identical? tomb (val nextf)) (set! nxt nextf))
                     (recur))
                   (do (set! didfront (boolean true)) (recur)))
                 (if (.hasNext b-iter)
                   (let [nextb (.next b-iter)]
                     (when-not (contains? front (key nextb)) (set! nxt nextb))
                     (recur))
                   false)))))
         (next [self]
           (if (not (.hasNext self))
             (throw (java.lang.IllegalStateException. "exhausted iterator"))
             (let [out nxt] (set! nxt nil) out)))))
    #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
    (declare ->MapStackIterator)
    (eval
      '(classy/defsubclass EditedMap (clojure.lang.APersistentMap [])
         [added meta front back]
         (assoc [self k v]
           (->EditedMap
             (cond-> added (not (contains? self k)) inc)
             nil
             (assoc front k v) back))
         (without [self k]
           (if (contains? self k)
             (->EditedMap (dec added) nil (assoc front k tomb) back)
             self))
         (^clojure.lang.IPersistentMap assocEx
          [self ^Object k ^Object v]
          (if-not (contains? self k)
            (assoc self k v)
            (throw (IllegalArgumentException. "key already exists"))))
         (iterator [_]
           (->MapStackIterator false nil front (.iterator ^Iterable front) (.iterator ^Iterable back)))
         (containsKey [_ k]
           (let [att1 (.valAt ^clojure.lang.IPersistentMap front k notfound)]
             (condp identical? att1
               tomb false
               notfound (contains? back k)
               true)))
         (entryAt [_ k]
           (let [possible (or (find front k) (find back k))]
             (when-not (identical? (val possible) tomb)
               possible)))
         (count [_]
           (+ added (count back)))
         (empty [_]
           (empty back))
         (seq [self]
           (iterator-seq (.iterator self)))
         (valAt [self k]
           (.valAt self k nil))
         (valAt [_ k orelse]
           (let [att1 (.valAt ^clojure.lang.IPersistentMap front k notfound)]
             (condp identical? att1
               tomb orelse
               notfound (.valAt ^clojure.lang.IPersistentMap back k orelse)
               att1)))
         clojure.lang.IObj
         (meta [_] meta)
         (withMeta [_ newmeta] (->EditedMap added newmeta front back))))
    (declare ->EditedMap)
    #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var :inline-def]}
    (defn ^clojure.lang.APersistentMap edited-map
      ([back] (->EditedMap 0 nil {} back)))
    (eval
      '(classy/defsubclass WrapperMap (clojure.lang.APersistentMap [])
         [meta_ keys_ k->v]
         (assoc [self k v] (.assoc (edited-map self) k v))
         (without [self k] (.without (edited-map self) k))
         (assocEx [self k v] (.assocEx (edited-map self) k v))
         (iterator [_]
           (let [k-itr (.iterator ^Iterable keys_)]
             (reify java.util.Iterator
               (next [_] (let [k (.next k-itr)] (entry k (k->v k notfound))))
               (hasNext [_] (.hasNext k-itr)))))
         (containsKey [_ k]
           (not (identical? notfound (k->v k notfound))))
         (entryAt [_ k] (entry k (k->v k)))
         (count [_] (count keys_))
         (empty [_] {})
         (seq [self] (iterator-seq (.iterator self)))
         (valAt [_ k] (k->v k nil))
         (valAt [_ k d] (k->v k d))
         clojure.lang.IObj
         (meta [_] meta_)
         (withMeta [_ newmeta] (->WrapperMap newmeta keys_ k->v))))
    (declare ->WrapperMap)
    (let [class->bean-support
          (memoize
            (fn [cls]
              (let [props2meths
                    (util/map-by
                      #(keyword (.getName ^java.beans.PropertyDescriptor %))
                      #(let [m (doto (.getReadMethod ^java.beans.PropertyDescriptor %) (.setAccessible true))
                             ret-ty (.getPropertyType ^java.beans.PropertyDescriptor %)]
                         (fn [x] (clojure.lang.Reflector/prepRet ret-ty (.invoke m x nil))))
                      (filter
                        #(= 0 (some-> (.getReadMethod ^java.beans.PropertyDescriptor %) (.getParameterCount)))
                        (.getPropertyDescriptors (java.beans.Introspector/getBeanInfo ^Class cls))))]
                [(vec (keys props2meths)) props2meths])))]
      #_{:clj-kondo/ignore [:inline-def]}
      (defn beanie [self]
        (let [[keyz props2meths] (class->bean-support (class self))]
          (->WrapperMap nil keyz #(if-let [f (props2meths %)] (f self) %2))))
      (let [beanie_ (beanie :lmao)
            bean_ (bean :lmao)
            normal_ (into {} beanie_)]
        (test/is (= beanie_ bean_) "bean and beanie equal")
        (test/is (= (assoc beanie_ 1 3) (assoc normal_ 1 3)) "beanie equal to normal after assoc")
        (test/is (= (dissoc beanie_ :name) (dissoc bean_ :name)) "dissoc test")))))
