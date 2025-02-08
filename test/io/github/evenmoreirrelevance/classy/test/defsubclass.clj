(ns io.github.evenmoreirrelevance.classy.test.defsubclass
  (:import java.lang.reflect.Field java.lang.Deprecated)
  (:require
   [clojure.string :as str]
   [io.github.evenmoreirrelevance.classy.core :as classy]
   [io.github.evenmoreirrelevance.classy.util :as util]
   [clojure.test :as test]))

(defonce tomb (Object.))
(defonce notfound (Object.))
(defmacro entry [k v] `(clojure.lang.MapEntry/create ~k ~v))

(definterface AmbiguousOverloads
  (lol [^int x])
  (lol [^long x]))

(test/deftest defsubclass
  (test/testing "syntax checks"
    (test/is
      (thrown? Exception
        (util/evals-in-ns
          (classy/defsubclass DoubleDecls (Object) []
            (hashCode [_] 3)
            (hashCode [_] 3)))))
    (test/is
      (thrown? Exception
        (util/evals-in-ns
          (classy/defsubclass BadClass (int) []))))
    (test/is
      (thrown? Exception
        (util/evals-in-ns
          (classy/defsubclass AmbiguousHints (Object) []
            AmbiguousOverloads
            (lol [_ x] x)
            (lol [_ x] x))))))

  (test/testing "annotations"
    (util/evals-in-ns
      (classy/defsubclass ^{Deprecated true} Annotated (Object)
        [^{Deprecated true} f]
        clojure.lang.ILookup
        (^{Deprecated true} valAt [_ ^{Deprecated true} _f]))
      (let [impl-fd (try
                      (doto ^Field (first
                                     (filter #(str/starts-with? (.getName ^Field %) "EMI_impl")
                                       (.getDeclaredFields Annotated)))
                        (.setAccessible true))
                      (catch IllegalAccessError _ nil)
                      (catch IllegalAccessException _ nil))
            impl-cls (class (some-> ^Field impl-fd (.get nil)))]
        (when impl-cls #_"skip test if we can't access the inner class"
          (let [cls-anns (.getAnnotations Annotated)
                impl-anns (.getAnnotations impl-cls)]
            (test/is (instance? Deprecated (first cls-anns)))
            (test/is (empty? impl-anns)))
          (let [cls-meth (.getMethod Annotated "valAt" (into-array [Object]))
                impl-meth (.getMethod impl-cls "EMI_impl_valAt"
                            (into-array [(.getSuperclass Annotated) Object]))]
            (test/is (instance? Deprecated (first (.getAnnotations cls-meth))))
            (test/is (empty? (.getAnnotations impl-meth)))

            (test/is (instance? Deprecated (first (apply concat (.getParameterAnnotations cls-meth)))))
            (test/is (empty? (apply concat (.getParameterAnnotations impl-meth)))))))))

  (test/testing "hinting"
    (test/is
      (not (util/throwing? Exception
             (util/evals-in-ns
               (classy/defsubclass GoodHints (Object) []
                 AmbiguousOverloads
                 (lol [_ ^int x] x)
                 (lol [_ ^long y] y)))))))

  (test/testing "primitives"
    (util/evals-in-ns
      (test/is
        (not (util/throwing? Exception
               (classy/defsubclass WithIntField (Object) [^int x]
                 clojure.lang.IDeref
                 (deref [_] x)))))))

  (test/testing "chain inheritance"
    (util/evals-in-ns
      (classy/defsubclass Ex1 [Exception [^String msg]] []
        ::classy/extensible? true
        clojure.lang.IExceptionInfo
        (getData [_]
          {:foo :bar})
        (getMessage [_] "ex1 message"))
      (classy/defsubclass Ex2 [Ex1 [^String msg]] []
        ::classy/extensible? true
        (getMessage [_]
          (str "ex2 message; "
            (classy/super-call (.getMessage _)))))
      (classy/defsubclass Ex3 [Ex2 [^String msg]] []
        (getMessage [_]
          (classy/super-call (.getMessage _))))
      (let [e1 ^Ex1 (->Ex1 "hi")
            e2 ^Ex2 (->Ex2 "hello")
            e3 ^Ex3 (->Ex3 "bonjour")]
        (test/is (= (.getMessage e1) "ex1 message"))
        (test/is (= (.getData e1) (.getData e2) {:foo :bar}))
        (test/is (= (.getMessage e2) (.getMessage e3) "ex2 message; ex1 message")))))

  (test/testing "extensibie? option"
    (util/evals-in-ns
      (classy/defsubclass Extensible (Object []) [] ::classy/extensible? true)
      (classy/defsubclass Final (Object []) [])
      (test/is (java.lang.reflect.Modifier/isFinal (.getModifiers Final)))
      (test/is (not (java.lang.reflect.Modifier/isFinal (.getModifiers Extensible))))))

  (test/testing "long ctor"
    (util/evals-in-ns
      (classy/defsubclass LongCtor (java.util.ArrayList [^java.util.Collection x])
        [_1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22]
        clojure.lang.IDeref
        (deref [_]
          [_1 _2 _3 _4 _5 _6 _7 _8 _9 _10 _11 _12 _13 _14 _15 _16 _17 _18 _19 _20 _21 _22]))
      (test/is (instance? LongCtor (->LongCtor 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 [])))))

  (test/testing "big fat e2e test"
    (util/evals-in-ns
      (classy/defsubclass MapStackIterator [Object []]
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
            (let [out nxt] (set! nxt nil) out))))
      (classy/defsubclass EditedMap (clojure.lang.APersistentMap [])
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
        (withMeta [_ newmeta] (->EditedMap added newmeta front back)))
      #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var :inline-def]}
      (defn ^clojure.lang.APersistentMap edited-map
        ([back] (->EditedMap 0 nil {} back)))
      (classy/defsubclass WrapperMap (clojure.lang.APersistentMap [])
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
        (withMeta [_ newmeta] (->WrapperMap newmeta keys_ k->v)))
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
          [(test/is (= beanie_ bean_) "bean and beanie equal")
           (test/is (= (assoc beanie_ 1 3) (assoc normal_ 1 3)) "beanie equal to normal after assoc")
           (test/is (= (dissoc beanie_ :name) (dissoc bean_ :name)) "dissoc test")])))))
