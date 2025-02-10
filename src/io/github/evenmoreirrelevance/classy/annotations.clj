;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns ^:no-doc io.github.evenmoreirrelevance.classy.annotations
  (:import (org.objectweb.asm MethodVisitor)))
#_"adapted from clojure.core code in order to fully cut off dependency from org.objectweb.asm"

(binding [*warn-on-reflection* false]
  (defn is-annotation? [c]
    (and (class? c)
      (.isAssignableFrom java.lang.annotation.Annotation c)))

  (defn is-runtime-annotation? [^Class c]
    (boolean
      (and (is-annotation? c)
        (when-let [^java.lang.annotation.Retention r
                   (.getAnnotation c java.lang.annotation.Retention)]
          (= (.value r) java.lang.annotation.RetentionPolicy/RUNTIME)))))

  (defn descriptor [^Class c] (org.objectweb.asm.Type/getDescriptor c))
  (declare process-annotation)
  (defn add-annotation [^org.objectweb.asm.AnnotationVisitor av name v]
    (cond
      (vector? v) (let [avec (.visitArray av name)]
                    (doseq [vval v]
                      (add-annotation avec "value" vval))
                    (.visitEnd avec))
      (symbol? v) (let [ev (eval v)]
                    (cond
                      (instance? java.lang.Enum ev)
                      (.visitEnum av name (descriptor (class ev)) (str ev))
                      (class? ev) (.visit av name (org.objectweb.asm.Type/getType ^Class ev))
                      :else (throw (IllegalArgumentException.
                                     (str "Unsupported annotation value: " v " of class " (class ev))))))
      (seq? v) (let [[nested nv] v
                     c (resolve nested)
                     nav (.visitAnnotation av name (descriptor c))]
                 (process-annotation nav nv)
                 (.visitEnd nav))
      :else (.visit av name v)))

  (defn process-annotation [av v]
    (if (map? v)
      (doseq [[k v] v]
        (add-annotation av (name k) v))
      (add-annotation av "value" v)))

  (defn add-annotations
    ([visitor m] (add-annotations visitor m nil))
    ([visitor m i]
     (doseq [[k v] m]
       (when (symbol? k)
         (when-let [c (resolve k)]
           (when (is-annotation? c)
             (let [av (if i
                        (.visitParameterAnnotation ^MethodVisitor visitor i (descriptor c)
                          (is-runtime-annotation? c))
                        (.visitAnnotation visitor (descriptor c)
                          (is-runtime-annotation? c)))]
               (process-annotation av v)
               (.visitEnd av)))))))))
