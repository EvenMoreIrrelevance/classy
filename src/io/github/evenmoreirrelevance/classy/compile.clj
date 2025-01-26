(ns ^:no-doc io.github.evenmoreirrelevance.classy.compile
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

(defn overridable-methods
  [^Class cls]
  (->> (.getMethods cls) (remove uninteresting?) (util/distinct-by method-sig)))

(comment (overridable-methods Class))

(let [java-8-class-version 52]
  (defn ^ClassWriter ->cw [access name sig super ifaces]
    (doto (ClassWriter. (bit-or ClassWriter/COMPUTE_FRAMES clojure.asm.ClassWriter/COMPUTE_MAXS))
      (.visit java-8-class-version access name sig super ifaces))))

(defn ^GeneratorAdapter ->ga
  [cw acc name desc sig thrown]
  (GeneratorAdapter. (.visitMethod ^ClassWriter cw acc name desc sig thrown) acc name desc))

(defn asm-type
  [classname-or-cls]
  (if (string? classname-or-cls)
    (Type/getType (str "L" (util/dots2slashes classname-or-cls) ";"))
    (Type/getType ^Class classname-or-cls)))

(defn exe-return-type
  [^Executable m]
  (if (instance? java.lang.reflect.Method m)
    (.getReturnType ^java.lang.reflect.Method m)
    Void/TYPE))

(defn method-desc
  [^Executable meth]
  (Type/getMethodDescriptor
    (asm-type (exe-return-type meth))
    (into-array Type (map asm-type (.getParameterTypes meth)))))

(defn method-desc-with-prepended-args
  [argtypes ^Executable meth]
  (Type/getMethodDescriptor
    (asm-type (exe-return-type meth))
    (into-array Type (map asm-type (concat argtypes (.getParameterTypes meth))))))

(def this-ns *ns*)

(defn ^String internal-name
  [cls-or-name]
  (if (string? cls-or-name)
    (util/dots2slashes cls-or-name)
    (Type/getInternalName ^Class cls-or-name)))

; to limit the scenario described in the roadmap.
(defonce salt 
  (str/replace (.toString (java.util.UUID/randomUUID)) "-" ""))

(defn supers->name
  [[^Class supcls ifaces]]
  (str/join "$" 
    (concat 
      (cons (.getName supcls) (sort (map #(.getName ^Class %) ifaces)))
      [salt])))

(def ^Class supers->subcls-base
  (memoize
    (fn [[^Class supcls ifaces :as supers]]
      (let [out-name (str (namespace-munge this-ns) "._subclsbase$" (supers->name supers))
            cw (->cw (+ Opcodes/ACC_PUBLIC)
                 (util/dots2slashes out-name)
                 nil
                 (internal-name supcls) (into-array String (map internal-name ifaces)))]
        (doseq [^Executable m (overridable-methods supcls)]
            ;; we do something akin to:
            ;; private void _super_foo(int x) { super.foo(x); }
            ;; static void super_foo(FooBase self, int x) { return self._super_foo(x); }
            ;; in order to get a publicly-callable and non-virtual accessor to the super method 
          (doto (->ga cw
                  (flags Opcodes/ACC_PRIVATE)
                  (str "_" super-prefix (.getName m))
                  (method-desc m) nil nil)
            (.visitCode) (.loadThis) (.loadArgs)
            (.visitMethodInsn
              Opcodes/INVOKESPECIAL
              (internal-name supcls) (.getName m) (method-desc m))
            (.returnValue) (.endMethod))
          (doto (->ga cw
                  (flags Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                  (str super-prefix (.getName m))
                  (method-desc-with-prepended-args [out-name] m) nil nil)
            (.visitCode) (.loadArgs)
            (.visitMethodInsn
              Opcodes/INVOKEVIRTUAL
              (internal-name out-name) (str "_" super-prefix (.getName m)) (method-desc m))
            (.returnValue) (.endMethod)))
        (if-not (seq (.getConstructors supcls))
            ; if no constructor exists, call the default one
          (let [desc (Type/getMethodDescriptor Type/VOID_TYPE (make-array Type 0))]
            (doto (->ga
                    cw
                    (flags Opcodes/ACC_PUBLIC)
                    "<init>"
                    desc nil nil)
              (.visitCode) (.loadThis)
              (.visitMethodInsn
                Opcodes/INVOKESPECIAL
                (internal-name supcls) "<init>" desc)
              (.returnValue) (.endMethod)))
            ; else just wrap
          (doseq [^Executable ctor (.getConstructors supcls)]
            (doto (->ga
                    cw
                    (flags Opcodes/ACC_PUBLIC)
                    "<init>"
                    (method-desc ctor) nil nil)
              (.visitCode) (.loadThis) (.loadArgs)
              (.visitMethodInsn
                Opcodes/INVOKESPECIAL
                (internal-name supcls) "<init>" (method-desc ctor))
              (.returnValue) (.endMethod))))
        (util/load-and-compile out-name (.toByteArray (doto cw (.visitEnd))))))))

(comment
  (.getMethods (supers->subcls-base [java.util.ArrayList #{}]))
  )

(def ^Class supers->impl
  (memoize
    (fn [[^Class supcls ifaces :as args]]
      (let [out-name (str (namespace-munge this-ns) "._impl$" (supers->name args))
            subcls-base (supers->subcls-base args)
            cw (->cw (flags Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE Opcodes/ACC_PUBLIC)
                 (util/dots2slashes out-name) nil
                 (internal-name Object) nil)
            meths (util/distinct-by
                    method-sig
                    (into
                      (vec (overridable-methods supcls))
                      (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
                      ifaces))]
        (doseq [^Executable m meths]
          (let [mname (.getName m)
                super-impl?
                (.isAssignableFrom (.getDeclaringClass m) subcls-base)
                emit-visit
                (fn [mname-prefix]
                  (doto (->ga cw
                          (flags Opcodes/ACC_PUBLIC (when-not super-impl? Opcodes/ACC_ABSTRACT))
                          (str mname-prefix mname)
                          (method-desc-with-prepended-args [subcls-base] m) nil nil)
                    (.visitCode)
                    (as-> mw
                      (when super-impl?
                        (doto mw
                          (.loadArgs)
                          (.visitMethodInsn
                            Opcodes/INVOKESTATIC
                            (internal-name subcls-base) (str super-prefix mname)
                            (method-desc-with-prepended-args [subcls-base] m))
                          (.returnValue)
                          (.endMethod))))
                    (.visitEnd)))]
            (emit-visit super-prefix)
            (emit-visit impl-prefix)))
        (util/load-and-compile out-name (.toByteArray (doto cw (.visitEnd))))))))

(defn emit-accept
  [cw outname ^Class base ^Class impl ^java.lang.reflect.Method m]
  (doto (->ga cw
          (flags (if (Modifier/isPublic (modifiers m)) Opcodes/ACC_PUBLIC Opcodes/ACC_PROTECTED))
          (.getName m)
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
      (internal-name impl) (str impl-prefix (.getName m))
      (method-desc-with-prepended-args [base] m))
    (.returnValue)
    (.endMethod)
    (.visitEnd)))

(defn ^Class emit-defsubtype-class
  [[cls ifaces outname] ^Class real-impl extensible?]
  (let [base
        (supers->subcls-base [cls ifaces])
        ^Executable impl-ctor
        (let [[f & m :as all] (.getConstructors real-impl)]
          (when (seq m) (throw (ex-info "expected `impl` to have one ctor only" {:found all})))
          f)
        cw
        (->cw (flags Opcodes/ACC_PUBLIC (when-not extensible? Opcodes/ACC_FINAL))
          (util/dots2slashes outname) nil
          (internal-name base)
          (into-array String (map #(internal-name ^Class %) ifaces)))]
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
        (.visitTypeInsn Opcodes/NEW (internal-name real-impl))
        (.dup)
        (.loadArgs 0 (.getParameterCount impl-ctor))
        (.visitMethodInsn
          Opcodes/INVOKESPECIAL
          (internal-name real-impl) "<init>" (method-desc impl-ctor))
        (.visitFieldInsn
          Opcodes/PUTFIELD
          (util/dots2slashes outname) impl-prefix (Type/getDescriptor real-impl))
        (.loadThis)
        (.loadArgs (.getParameterCount impl-ctor) (.getParameterCount se-ctor))
        (.visitMethodInsn
          Opcodes/INVOKESPECIAL
          (internal-name base) "<init>" (method-desc se-ctor))
        (.returnValue)
        (.endMethod)
        (.visitEnd)))

    (doseq [m (util/distinct-by method-sig
                (into (vec (overridable-methods cls))
                  (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
                  ifaces))]
      (emit-accept cw outname base real-impl m))
    (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))

(def ^Class supers->shell
  (memoize
    (fn [[^Class cls ifaces :as supers]]
      (let [outname (str (namespace-munge this-ns) "._shell$" (supers->name [cls ifaces]))
            impl (supers->impl supers)
            base (supers->subcls-base supers)
            cw (->cw (+ Opcodes/ACC_PUBLIC)
                 (util/dots2slashes outname) nil
                 (internal-name base)
                 (into-array String (map #(internal-name ^Class %) ifaces)))]
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
              (internal-name base) "<init>" (method-desc ctor))
            (.loadArg 0)
            (.visitFieldInsn
              Opcodes/PUTFIELD
              (util/dots2slashes outname) impl-prefix (Type/getDescriptor impl))
            (.returnValue)
            (.endMethod)
            (.visitEnd)))
        (doseq [m (util/distinct-by
                    method-sig
                    (into (vec (overridable-methods cls))
                      (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
                      ifaces))]
          (emit-accept cw outname base impl m))
        (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))))
