(ns ^:no-doc io.github.evenmoreirrelevance.classy.compile
  (:require
   [clojure.string :as str]
   [io.github.evenmoreirrelevance.classy.util :as util])
  (:import
   (clojure.asm
     ClassWriter
     FieldVisitor
     Opcodes
     Type)
   (clojure.asm.commons GeneratorAdapter)
   (java.lang.reflect Executable Field Modifier)))

(defn descriptor
  (^String [classname-or-class]
   (condp util/is? classname-or-class
     ; NOTE: assumes that if the input is a string, it names an object class.
     string? (if (and 
                   (str/starts-with? classname-or-class "L")
                   (str/ends-with? classname-or-class ";"))
               classname-or-class
               (str "L" (util/dots2slashes classname-or-class) ";"))
     class? (Type/getDescriptor ^Class classname-or-class)
     #(instance? Type %) (.getDescriptor ^Type classname-or-class)
     (throw (ex-info "not a class or string:" {:classname-or-class classname-or-class})))))

(defn internal-name
  [classname-or-class]
  (condp util/is? classname-or-class
    ; NOTE: assumes that if the class is a string, it names an object class.
    string? (util/dots2slashes classname-or-class)
    class? (Type/getInternalName ^Class classname-or-class)
    #(instance? Type %) (.getInternalName ^Type classname-or-class)
    (throw (ex-info "not a class or string:" {:classname-or-class classname-or-class}))))

(defn field
  [{:keys [^Field reflected owner name type] :as args}]
  (util/require-keys!
    (util/assoc-some args
      :owner-iname (some-> (or owner (some-> reflected .getDeclaringClass)) (internal-name))
      :name (or name (some-> reflected .getName))
      :type-desc (some-> (or type (some-> reflected .getType)) (descriptor)))
    [:owner-iname :name :type-desc]))

(defn method
  [{:keys [^Executable reflected owner name params return] :as args}]
  (util/require-keys!
    (util/assoc-some args
      :owner-iname
      (some-> (or owner (some-> reflected .getDeclaringClass)) (internal-name))
      :name
      (or name
        (condp instance? reflected
          java.lang.reflect.Constructor "<init>"
          java.lang.reflect.Method (.getName ^java.lang.reflect.Method reflected)
          nil))
      :param-types
      (some->> (or params (some-> reflected .getParameterTypes))
        (map #(Type/getType (descriptor %)))
        (into-array Type))
      :return-type
      (some-> (or return
                (condp instance? reflected
                  java.lang.reflect.Constructor Void/TYPE
                  java.lang.reflect.Method (.getReturnType ^java.lang.reflect.Method reflected)
                  nil))
        (descriptor) (Type/getType)))
    [:owner-iname :name :param-types :return-type]))

(defn prepend-args [args extant]
  (into-array Type
    (concat
      (map #(Type/getType (descriptor %)) args)
      extant)))

(defn method-insn [^GeneratorAdapter ga opcode {:keys [owner-iname name param-types return-type]}]
  (.visitMethodInsn ga opcode owner-iname name (Type/getMethodDescriptor return-type param-types)))
(defn field-insn [^GeneratorAdapter ga opcode {:keys [owner-iname name type-desc]}]
  (.visitFieldInsn ga opcode owner-iname name type-desc))

(defn visit-field ^FieldVisitor [^ClassWriter cw access {:keys [init name type-desc signature]}]
  (.visitField cw access name type-desc signature init))
(defn visit-method ^GeneratorAdapter
  [^ClassWriter cw access {:keys [name param-types return-type signature thrown]}]
  (let [desc (Type/getMethodDescriptor return-type param-types)]
    (GeneratorAdapter. (.visitMethod ^ClassWriter cw access name desc signature thrown) access name desc)))
(defn emit-wrapping-ctor* [cw acc ctor_ args body*]
  (doto (visit-method cw acc (update ctor_ :param-types #(prepend-args args %)))
    (body*)
    (.loadThis) (.loadArgs (count args) (count (:param-types ctor_)))
    (method-insn Opcodes/INVOKESPECIAL ctor_)
    (.returnValue) (.endMethod)))
(defmacro emit-wrapping-ctor [cw acc ctor_ args & body]
  `(emit-wrapping-ctor* 
     ~cw ~acc ~ctor_ ~args (fn [ga#] (doto (util/the GeneratorAdapter ga#) ~@body))))

(defn ^Long flags
  {:inline (fn expand-flags
             ([a] `(long (or ~a 0)))
             ([a1 & args]
              `(long (bit-or ~@(for [a (cons a1 args)] `(or ~a 0))))))}
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

(let [java-8-class-version 52]
  (defn ^ClassWriter ->cw [access name sig super ifaces]
    (doto (ClassWriter. (bit-or ClassWriter/COMPUTE_FRAMES clojure.asm.ClassWriter/COMPUTE_MAXS))
      (.visit java-8-class-version access name sig super ifaces))))

(def this-ns *ns*)

; to limit the scenario described in the roadmap.
(defonce salt (str/replace (.toString (java.util.UUID/randomUUID)) "-" ""))

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
            cw (->cw 
                 Opcodes/ACC_PUBLIC
                 (util/dots2slashes out-name) nil
                 (internal-name supcls) (into-array String (map internal-name ifaces)))]
        ; we do something akin to:
        ; private void _super_foo(int x) { super.foo(x); }
        ; static void super_foo(FooBase self, int x) { return self._super_foo(x); }
        ; in order to get a publicly-callable and non-virtual accessor to the super method 
        (doseq [^Executable m (overridable-methods supcls)
                :let [m_ (method {:reflected m :owner supcls})
                      sup_
                      (update (method {:reflected m :owner out-name})
                        :name #(str "_" super-prefix %))
                      stat_ (util/updates m_
                              :name #(str super-prefix %)
                              :param-types #(prepend-args [out-name] %))]]
          (doto (visit-method cw Opcodes/ACC_PRIVATE sup_) (.visitCode)
            (.loadThis) (.loadArgs)
            (method-insn Opcodes/INVOKESPECIAL m_)
            (.returnValue) (.endMethod))
          (doto (visit-method cw (flags Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC) stat_) (.visitCode)
            (.loadArgs)
            (method-insn Opcodes/INVOKEVIRTUAL sup_)
            (.returnValue) (.endMethod)))
        (doseq [ctor (or
                       (seq (map #(do {:reflected %}) (.getConstructors supcls)))
                       [{:name "<init>" :params [] :return Void/TYPE :owner supcls}])]
          (emit-wrapping-ctor cw Opcodes/ACC_PUBLIC (method ctor) []))
        (util/load-and-compile out-name (.toByteArray (doto cw (.visitEnd))))))))

(comment
  (.getMethods (supers->subcls-base [java.util.ArrayList #{}])))

(def ^Class supers->impl
  (memoize
    (fn [[^Class supcls ifaces :as args]]
      (let [out-name (str (namespace-munge this-ns) "._impl$" (supers->name args))
            subcls-base (supers->subcls-base args)
            cw (->cw
                 (flags Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE Opcodes/ACC_PUBLIC)
                 (util/dots2slashes out-name) nil
                 (internal-name Object) nil)
            meths (util/distinct-by
                    method-sig
                    (into
                      (vec (overridable-methods supcls))
                      (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
                      ifaces))]
        (doseq [^Executable m meths
                :let [m_ (method {:reflected m})
                      stat_ (util/updates (method {:reflected m :owner subcls-base})
                              :name #(str super-prefix %)
                              :param-types #(prepend-args [subcls-base] %))]]
          (let [super-impl?
                (.isAssignableFrom (.getDeclaringClass m) subcls-base)
                emit-visit
                (fn [mname-prefix]
                  (doto (visit-method
                          cw
                          (flags Opcodes/ACC_PUBLIC (when-not super-impl? Opcodes/ACC_ABSTRACT))
                          (util/updates m_
                            :name #(str mname-prefix %)
                            :param-types #(prepend-args [subcls-base] %))) (.visitCode)
                    (as-> mw
                      (when super-impl?
                        (doto mw (.loadArgs)
                          (method-insn Opcodes/INVOKESTATIC stat_)
                          (.returnValue)
                          (.endMethod))))
                    (.visitEnd)))]
            (emit-visit super-prefix)
            (emit-visit impl-prefix)))
        (util/load-and-compile out-name (.toByteArray (doto cw (.visitEnd))))))))

(defn emit-accept
  [cw base ^Class impl impl-fd m]
  (let [m_ (method {:reflected m})
        impl-m_ (util/updates (method {:reflected m :owner impl})
                  :name #(str impl-prefix %)
                  :param-types #(prepend-args [base] %))]
    (doto (visit-method cw
            (flags (if (Modifier/isPublic (modifiers m)) Opcodes/ACC_PUBLIC Opcodes/ACC_PROTECTED))
            m_) (.visitCode)
      (.loadThis)
      (field-insn Opcodes/GETFIELD impl-fd)

      (.loadThis) (.loadArgs)
      (method-insn (if (.isInterface impl) Opcodes/INVOKEINTERFACE Opcodes/INVOKEVIRTUAL)
        impl-m_)
      (.returnValue) (.endMethod))))

(defn ^Class emit-defsubtype-class
  [[cls ifaces outname] ^Class real-impl extensible?]
  (let [base
        (supers->subcls-base [cls ifaces])
        impl-ctor_
        (let [[f & m :as all] (.getConstructors real-impl)]
          (util/throw-when [_ (seq m)] 
            "impl must have exactly one ctor" {:impl real-impl :found (count all)})
          (method {:reflected f}))
        cw
        (->cw
          (flags Opcodes/ACC_PUBLIC (when-not extensible? Opcodes/ACC_FINAL)) 
          (util/dots2slashes outname) nil 
          (internal-name base) (into-array String (map #(internal-name %) ifaces)))
        impl-fd (field {:owner outname :name (name (gensym impl-prefix)) :type real-impl})]
    (doto (visit-field cw Opcodes/ACC_PRIVATE impl-fd)
      (.visitEnd))
    (doseq [se-ctor (.getConstructors base)
            :let [se-ctor_ (method {:reflected se-ctor})]]
      (emit-wrapping-ctor cw Opcodes/ACC_PUBLIC se-ctor_ (:param-types impl-ctor_)
        (.loadThis) (.visitTypeInsn Opcodes/NEW (internal-name real-impl))
        (.dup) (.loadArgs 0 (count (:param-types impl-ctor_)))
        (method-insn Opcodes/INVOKESPECIAL impl-ctor_)
        (field-insn Opcodes/PUTFIELD impl-fd)))
    (doseq [m (util/distinct-by method-sig
                (into (vec (overridable-methods cls))
                  (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
                  ifaces))]
      (emit-accept cw base real-impl impl-fd m))
    (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))

(def ^Class supers->shell
  (memoize
    (fn [[^Class cls ifaces :as supers]]
      (let [outname (str (namespace-munge this-ns) "._shell$" (supers->name [cls ifaces]))
            impl (supers->impl supers)
            base (supers->subcls-base supers)
            cw (->cw
                 Opcodes/ACC_PUBLIC 
                 (util/dots2slashes outname) nil 
                 (internal-name base) (into-array String (map #(internal-name ^Class %) ifaces)))
            impl-fd (field {:owner outname :name (name (gensym impl-prefix)) :type impl})]
        (doto (visit-field cw Opcodes/ACC_PRIVATE impl-fd) 
          (.visitEnd))
        (doseq [ctor (.getConstructors base)
                :let [ctor_ (method {:reflected ctor})]]
          (emit-wrapping-ctor cw Opcodes/ACC_PUBLIC ctor_ [impl]
            (.loadThis) (.loadArg 0)
            (field-insn Opcodes/PUTFIELD impl-fd)))
        (doseq [m (util/distinct-by
                    method-sig
                    (into (vec (overridable-methods cls))
                      (comp (mapcat #(.getMethods ^Class %)) (remove uninteresting?))
                      ifaces))]
          (emit-accept cw base impl impl-fd m))
        (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))))
