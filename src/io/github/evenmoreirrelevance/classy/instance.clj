(ns ^:no-doc io.github.evenmoreirrelevance.classy.instance
  (:require [io.github.evenmoreirrelevance.classy.util :as util]
            [clojure.string :as str])
  (:import (java.lang.reflect Modifier Executable Field)
           (clojure.asm.commons GeneratorAdapter)
           (clojure.asm Type ClassWriter Opcodes)))

(defn ^Long flags
  {:inline (fn [& args] `(long (@(var flags) ~@args)))}
  ([x] (or x 0))
  ([x y] (bit-or (or x 0) (or y 0)))
  ([x y & more] (apply bit-or (or x 0) (or y 0) (map #(or % 0) more))))

(defn modifiers
  ^long [x]
  (condp instance? x
    Executable (.getModifiers ^Executable x)
    Field (.getModifiers ^Field x)
    Class (.getModifiers ^Class x)))

(def super-prefix "EMI_super_")
(def impl-prefix "EMI_impl_")

(defn uninteresting?
  [^java.lang.reflect.Method x]
  (let [mods (modifiers x)]
    (or
      (not (zero? (bit-and mods (flags Modifier/STATIC Modifier/FINAL Modifier/PRIVATE))))
      (zero? (bit-and mods (flags Opcodes/ACC_PUBLIC Opcodes/ACC_PROTECTED))))))

(defn cls->implicit-interface
  [^Class cls]
  (->> (.getMethods cls) (remove uninteresting?) (util/distinct-by method-sig)))

(comment (cls->implicit-interface Class))

(let [java-8-class-version 52]
  (defn ^ClassWriter ->cw [access name sig super ifaces]
    (doto (ClassWriter. (bit-or ClassWriter/COMPUTE_FRAMES clojure.asm.ClassWriter/COMPUTE_MAXS))
      (.visit java-8-class-version access name sig super ifaces))))

(defn ^GeneratorAdapter ->ga
  [cw acc name desc sig thrown]
  (GeneratorAdapter. (.visitMethod ^ClassWriter cw acc name desc sig thrown) acc name desc))

(defn class->type
  [^Class cls]
  (Type/getType cls))

(defn exe-return-type
  [^Executable m]
  (if (instance? java.lang.reflect.Method m)
    (.getReturnType ^java.lang.reflect.Method m)
    Void/TYPE))

(defn method-desc
  [^Executable meth]
  (Type/getMethodDescriptor
    (class->type (exe-return-type meth))
    (into-array Type (map class->type (.getParameterTypes meth)))))

(defn method-desc-with-prepended-args
  [shims ^Executable meth]
  (Type/getMethodDescriptor
    (class->type (exe-return-type meth))
    (into-array Type (map class->type (concat shims (.getParameterTypes meth))))))

(def this-ns *ns*)

(defn ^String cls->internal-name
  [cls]
  (Type/getInternalName ^Class cls))

(defn supers->name
  [[^Class supcls ifaces]]
  (str/join "$" (map #(str/replace % \. \_) (cons (.getName supcls) (sort (map #(.getName ^Class %) ifaces))))))

(util/once
  (def ^Class supers->subcls-base
    (memoize
      (fn [[^Class supcls ifaces :as supers]]
        (let [out-name (str (namespace-munge this-ns)
                         "._subclsbase$" (supers->name supers)
                         (str/join "$"))
              cw (->cw (+ Opcodes/ACC_PUBLIC)
                   (util/dots2slashes out-name)
                   nil
                   (cls->internal-name supcls) (into-array String (map cls->internal-name ifaces)))]
          (doseq [^Executable m (cls->implicit-interface supcls)]
            (doto (->ga
                    cw
                    (flags Opcodes/ACC_PUBLIC)
                    (str super-prefix (.getName m))
                    (method-desc m) nil nil)
              (.visitCode)
              (.loadThis)
              (.loadArgs)
              (.visitMethodInsn
                Opcodes/INVOKESPECIAL
                (cls->internal-name supcls) (.getName m) (method-desc m))
              (.returnValue)
              (.endMethod)))
          (if-not (seq (.getConstructors supcls)) ; if no constructor exists, call the default one
            (let [desc (Type/getMethodDescriptor Type/VOID_TYPE (make-array Type 0))]
              (doto (->ga
                      cw
                      (flags Opcodes/ACC_PUBLIC)
                      "<init>"
                      desc nil nil)
                (.visitCode)
                (.loadThis)
                (.visitMethodInsn
                  Opcodes/INVOKESPECIAL
                  (cls->internal-name supcls) "<init>" desc)
                (.returnValue)
                (.endMethod)))
            (doseq [^Executable ctor (.getConstructors supcls)]
              (doto (->ga
                      cw
                      (flags Opcodes/ACC_PUBLIC)
                      "<init>"
                      (method-desc ctor) nil nil)
                (.visitCode)
                (.loadThis)
                (.loadArgs)
                (.visitMethodInsn
                  Opcodes/INVOKESPECIAL
                  (cls->internal-name supcls) "<init>" (method-desc ctor))
                (.returnValue)
                (.endMethod))))
          (util/load-and-compile out-name (.toByteArray (doto cw (.visitEnd)))))))))

(util/once
  (def ^Class supers->impl
    (memoize
      (fn [[^Class supcls ifaces :as args]]
        (let [out-name (str (namespace-munge this-ns) "._impl$" (supers->name args))
              subcls-base (supers->subcls-base args)
              cw (->cw (flags Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE Opcodes/ACC_PUBLIC)
                   (util/dots2slashes out-name) nil
                   (cls->internal-name Object) nil)
              meths (util/distinct-by
                      method-sig
                      (into
                        (vec (cls->implicit-interface supcls))
                        (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
                        ifaces))]
          (doseq [^Executable m meths
                  :let [mname (.getName m)
                        super-impl? (.isAssignableFrom (.getDeclaringClass m) subcls-base)]]
                ; default impl calls the super-method as-is
            (doto (->ga
                    cw
                    (flags Opcodes/ACC_PUBLIC (when-not super-impl? Opcodes/ACC_ABSTRACT))
                    (str impl-prefix mname)
                    (method-desc-with-prepended-args [subcls-base] m) nil nil)
              (.visitCode)
              (as-> mw
                (when super-impl?
                  (doto mw
                    (.loadArgs)
                    (.visitMethodInsn
                      Opcodes/INVOKEVIRTUAL
                      (cls->internal-name subcls-base) (str super-prefix mname) (method-desc m))
                    (.returnValue)
                    (.endMethod))))
              (.visitEnd)))
          (util/load-and-compile out-name (.toByteArray (doto cw (.visitEnd)))))))))

(defn emit-forwarders
  [cw outname ^Class base ^Class impl meths]
  (doseq [^java.lang.reflect.Method m meths
          :let [name (.getName m)]]
    (doto (->ga cw
            (flags (if (Modifier/isPublic (modifiers m)) Opcodes/ACC_PUBLIC Opcodes/ACC_PROTECTED))
            name
            (method-desc m) nil nil)
      (.visitCode)
      (.loadThis)
      (.visitFieldInsn
        Opcodes/GETFIELD
        (util/dots2slashes outname) impl-prefix (Type/getDescriptor impl))
      (.loadThis)
      (.loadArgs)
      (.visitMethodInsn
        (if (.isInterface impl) Opcodes/INVOKEINTERFACE Opcodes/INVOKEVIRTUAL)
        (cls->internal-name impl) (str impl-prefix name)
        (method-desc-with-prepended-args [base] m))
      (.returnValue)
      (.endMethod)
      (.visitEnd))))

(defn ^Class emit-defsubtype-class
  [[cls ifaces outname] ^Class real-impl]
  (let [base
        (supers->subcls-base [cls ifaces])
        ^Executable impl-ctor
        (let [[f & m :as all] (.getConstructors real-impl)]
          (when (seq m) (throw (ex-info "expected `impl` to have one ctor only" {:found all})))
          f)
        cw
        (->cw (+ Opcodes/ACC_PUBLIC)
          (util/dots2slashes outname) nil
          (cls->internal-name base)
          (into-array String (map #(cls->internal-name ^Class %) ifaces)))]
    (doto (.visitField
            cw (+ Opcodes/ACC_PRIVATE) impl-prefix (Type/getDescriptor real-impl)  nil nil)
      (.visitEnd))
    (doseq [^java.lang.reflect.Constructor se-ctor (.getConstructors base)]
                  ; emit wrapper ctor which sets shim and forwards
      (doto (->ga
              cw
              (flags Opcodes/ACC_PUBLIC)
              "<init>" (method-desc-with-prepended-args (.getParameterTypes impl-ctor) se-ctor)
              nil nil)
        (.visitCode)
        (.loadThis)
        (.visitTypeInsn Opcodes/NEW (cls->internal-name real-impl))
        (.dup)
        (.loadArgs 0 (.getParameterCount impl-ctor))
        (.visitMethodInsn
          Opcodes/INVOKESPECIAL
          (cls->internal-name real-impl) "<init>" (method-desc impl-ctor))
        (.visitFieldInsn
          Opcodes/PUTFIELD
          (util/dots2slashes outname) impl-prefix (Type/getDescriptor real-impl))
        (.loadThis)
        (.loadArgs (.getParameterCount impl-ctor) (.getParameterCount se-ctor))
        (.visitMethodInsn
          Opcodes/INVOKESPECIAL
          (cls->internal-name base) "<init>" (method-desc se-ctor))
        (.returnValue)
        (.endMethod)
        (.visitEnd)))

    (emit-forwarders
      cw outname base real-impl
      (util/distinct-by
        method-sig
        (into
          (vec (cls->implicit-interface cls))
          (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
          ifaces)))
    (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))

(util/once
  (def ^Class supers->shell
    (memoize
      (fn [[^Class cls ifaces :as supers]]
        (let [outname (str (namespace-munge this-ns) "._shell$" (supers->name [cls ifaces]))
              impl (supers->impl supers)
              base (supers->subcls-base supers)
              cw (->cw (+ Opcodes/ACC_PUBLIC)
                   (util/dots2slashes outname) nil
                   (cls->internal-name base)
                   (into-array String (map #(cls->internal-name ^Class %) ifaces)))]
          (doto (.visitField
                  cw (+ Opcodes/ACC_PRIVATE) impl-prefix
                  (Type/getDescriptor impl)  nil nil)
            (.visitEnd))
          (doseq [^java.lang.reflect.Constructor ctor (.getConstructors base)]
          ; emit wrapper ctor which sets shim and forwards
            (doto (->ga
                    cw
                    (flags Opcodes/ACC_PUBLIC)
                    "<init>" (method-desc-with-prepended-args [impl] ctor) nil nil)
              (.visitCode)
              (.loadThis)
              (.dup)
              (.loadArgs 1 (.getParameterCount ctor))
              (.visitMethodInsn
                Opcodes/INVOKESPECIAL
                (cls->internal-name base) "<init>" (method-desc ctor))
              (.loadArg 0)
              (.visitFieldInsn
                Opcodes/PUTFIELD
                (util/dots2slashes outname) impl-prefix (Type/getDescriptor impl))
              (.returnValue)
              (.endMethod)
              (.visitEnd)))
          (emit-forwarders
            cw outname base impl
            (util/distinct-by
              method-sig
              (into
                (vec (cls->implicit-interface cls))
                (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
                ifaces)))
          (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd)))))))))
