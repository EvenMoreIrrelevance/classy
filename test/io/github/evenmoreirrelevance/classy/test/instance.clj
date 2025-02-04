(ns io.github.evenmoreirrelevance.classy.test.instance
  (:require
   [clojure.test :as test]
   [io.github.evenmoreirrelevance.classy.core :as classy]))

(test/deftest instance-tests
  (test/testing "default"
    (test/is
      (= 0
        (.computeIfAbsent (classy/instance (java.util.AbstractMap)
                            (entrySet [_] #{})
                            (put [_ _k _v] true))
          0 (reify java.util.function.Function (apply [_ _k] 0))))))
  (test/testing "super-call"
    (test/is
      (= 0
        @(classy/instance (java.util.ArrayList [3])
           (^objects toArray [self ^objects x] (classy/super-call (.toArray self x)))
           clojure.lang.IDeref
           (deref [_] (dec (classy/super-call (.size _))))))))
  (test/testing "closures"
    (let [a 3]
      (test/is
        (= a
          (.hashCode (classy/instance (Object) (hashCode [_] a))))))))

(comment 
  (test/run-tests)
  )
 