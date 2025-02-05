(ns ^:no-doc io.github.evenmoreirrelevance.classy.compile
  (:require
   [clojure.string :as str]
   [io.github.evenmoreirrelevance.classy.util :as util]
   [io.github.evenmoreirrelevance.classy.parse :as parse])
  (:import
   (clojure.asm
     ClassWriter
     FieldVisitor
     ClassVisitor
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

(defn emit-field 
  (^FieldVisitor [^ClassVisitor cw access {:keys [init name type-desc signature]}]
   (.visitField cw access name type-desc signature init)))
(defn emit-method
  (^GeneratorAdapter [^ClassVisitor cw access {:keys [name param-types return-type signature thrown]}]
   (let [desc (Type/getMethodDescriptor return-type param-types)]
     (GeneratorAdapter.
       (.visitMethod ^ClassVisitor cw access name desc signature thrown) access name desc))))

(defn emit-wrapping-ctor* [cw acc ctor_ args body*]
  (doto (emit-method cw acc (update ctor_ :param-types #(prepend-args args %)))
    (body*)
    (.loadThis) (.loadArgs (count args) (count (:param-types ctor_)))
    (method-insn Opcodes/INVOKESPECIAL ctor_)
    (.returnValue) (.endMethod)))
(defmacro emit-wrapping-ctor [cw acc ctor_ args & body]
  `(emit-wrapping-ctor* 
     ~cw ~acc ~ctor_ ~args (fn [ga#] (doto (util/the GeneratorAdapter ga#) ~@body))))

(def super-prefix "EMI_super_")
(def impl-prefix "EMI_impl_")

(let [java-8-class-version 52]
  (defn ^ClassWriter ->cw [access name sig super ifaces]
    (doto (ClassWriter. (bit-or ClassWriter/COMPUTE_FRAMES clojure.asm.ClassWriter/COMPUTE_MAXS))
      (.visit java-8-class-version access name sig super ifaces))))

(def this-ns *ns*)

; to limit the scenario described in the roadmap.

(let [salt (delay (str/replace (.toString (java.util.UUID/randomUUID)) "-" ""))]
  (defonce unique-suffix (fn [] (str @salt (gensym "$")))))

(defn stub-key
  [{:keys [base ifaces field-specs]}]
  (let [pubs (remove :private? field-specs)]
    [base ifaces
     (mapv :name pubs)
     (mapv #(meta (:form %)) pubs)]))

(defn super-call-entry-point
  [cls m]
  (util/updates (method {:reflected m :owner cls})
    :name #(str super-prefix %)
    :param-types #(prepend-args [cls] %)))

(def ^Class subclass-stub
  (util/locking-memo
    stub-key 
    (fn [{:keys [^Class base ifaces field-specs] :as _stub-desc}]
      (let [outname
            (str (namespace-munge this-ns) "._subcls_stub$" (unique-suffix))
            pub-fields
            (mapv #(field (assoc % :owner outname)) (remove :private? field-specs))
            cw 
            (->cw
              Opcodes/ACC_PUBLIC
              (util/dots2slashes outname) nil
              (internal-name base) (into-array String (map internal-name ifaces)))]
        ; emit public fields
        (doseq [fd pub-fields]
          (doto (emit-field cw (util/flags Opcodes/ACC_PUBLIC Opcodes/ACC_FINAL) fd)
            (@Compiler/ADD_ANNOTATIONS (meta (:form fd)))
            (.visitEnd)))
        ; we do something akin to:
        ; private void _super_foo(int x) { super.foo(x); }
        ; static void super_foo(FooBase self, int x) { return self._super_foo(x); }
        ; in order to get a publicly-callable and non-virtual accessor to the super method 
        (doseq [m (util/distinct-by method-sig
                    (filter parse/overrideable?
                      (.getMethods base)))
                :let [m_ (method {:reflected m :owner base})
                      sup_ (update (method {:reflected m :owner outname})
                             :name #(str "_" super-prefix %))]]
          (doto (emit-method cw Opcodes/ACC_PRIVATE sup_) (.visitCode)
            (.loadThis) (.loadArgs)
            (method-insn Opcodes/INVOKESPECIAL m_)
            (.returnValue) (.endMethod))
          (doto (emit-method cw 
                  (util/flags Opcodes/ACC_PUBLIC Opcodes/ACC_STATIC)
                  (super-call-entry-point outname m)) 
            (.visitCode)
            (.loadArgs)
            (method-insn Opcodes/INVOKEVIRTUAL sup_)
            (.returnValue) (.endMethod)))
        (doseq [ctor (or
                       (seq (map #(do {:reflected %}) (.getConstructors base)))
                       [{:name "<init>" :params [] :return Void/TYPE :owner base}])]
          (emit-wrapping-ctor cw Opcodes/ACC_PUBLIC (method ctor) (map :type pub-fields)
            (as-> ga
              (doseq [[i fd] (map vector (range) pub-fields)]
                (.loadThis ga) (.loadArg ga i)
                (field-insn ga Opcodes/PUTFIELD fd)))))
        (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))))

(comment
  (let [parsed (parse/parse-extension-form
                 {:base-sym 'java.util.ArrayList
                  :body '[clojure.lang.IDeref java.util.List
                          (deref [_] 3)]
                  :fields '[^int dio]})
        stub (subclass-stub parsed)]
    [(.getDeclaredMethods stub)
     (.getConstructors stub)
     (.getFields stub)])
  )

(def ^Class overrides-impl
  (util/locking-memo
    stub-key
    (fn [{:keys [^Class base sig->meths] :as stub-desc}]
      (let [outname (str (namespace-munge this-ns) "._impl$" (unique-suffix))
            stub (subclass-stub stub-desc)
            cw (->cw
                 (util/flags Opcodes/ACC_ABSTRACT Opcodes/ACC_INTERFACE Opcodes/ACC_PUBLIC)
                 (util/dots2slashes outname) nil
                 (internal-name Object) nil)]
        (doseq [[_ ms] sig->meths
                ^Executable m ms
                :let [m_ (method {:reflected m})]]
          (let [base-meth?
                (.isAssignableFrom (.getDeclaringClass m) base)
                emit-visit
                (fn [mname-prefix super-call-if-base-meth?]
                  (doto (emit-method
                          cw
                          (util/flags 
                            Opcodes/ACC_PUBLIC 
                            (when-not (and base-meth? super-call-if-base-meth?) Opcodes/ACC_ABSTRACT))
                          (util/updates m_
                            :name #(str mname-prefix %)
                            :param-types #(prepend-args [stub] %)))
                    (cond-> (and base-meth? super-call-if-base-meth?)
                      (doto 
                        (.visitCode) 
                        (.loadArgs)
                        (method-insn Opcodes/INVOKESTATIC (super-call-entry-point stub m))
                        (.returnValue) (.endMethod)))
                    (.visitEnd)))]
            (emit-visit super-prefix true)
            (emit-visit impl-prefix false)))
        (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))))

(defn ^Class defsubtype-cls
  [{cls-anns :meta :keys [outname ^Class real-impl extensible? stub-desc]}]
  (let [{:keys [ifaces field-specs sig->impl-spec sig->meths]}
        stub-desc
        priv-fd-specs (vec (filter :private? field-specs))
        stub
        (subclass-stub stub-desc)
        impl-stateless?
        (= 0 (count priv-fd-specs))
        impl-ctor_
        (let [[f & m :as all] (.getConstructors real-impl)]
          (util/throw-when [_ (seq m)]
            "impl must have exactly one ctor" {:impl real-impl :found (count all)})
          (method {:reflected f}))
        cw
        (->cw
          (util/flags Opcodes/ACC_PUBLIC (when-not extensible? Opcodes/ACC_FINAL))
          (util/dots2slashes outname) nil
          (internal-name stub) (into-array String (map #(internal-name %) ifaces)))
        impl-fd
        (field {:owner outname :name (name (gensym impl-prefix)) :type real-impl})
        get-impl-fd
        (if impl-stateless?
          #(field-insn % Opcodes/GETSTATIC impl-fd)
          #(doto ^GeneratorAdapter %
             (.loadThis)
             (field-insn Opcodes/GETFIELD impl-fd)))]
    (@Compiler/ADD_ANNOTATIONS cw cls-anns)
    #_"emit field and <clinit> if stateless" 
    (.visitEnd
      (emit-field cw
        (util/flags Opcodes/ACC_PRIVATE (when impl-stateless? Opcodes/ACC_STATIC))
        impl-fd))
    (when impl-stateless?
      (doto (emit-method cw (util/flags Opcodes/ACC_PRIVATE Opcodes/ACC_STATIC)
              (method {:name "<clinit>" :params [] :return Void/TYPE :owner outname}))
        (.visitTypeInsn Opcodes/NEW (internal-name real-impl))
        (.dup) (method-insn Opcodes/INVOKESPECIAL impl-ctor_)
        (field-insn Opcodes/PUTSTATIC impl-fd)
        (.returnValue) (.endMethod))) 
    #_"wrap all ctors"
    (doseq [se-ctor (.getConstructors stub)
            :let [se-ctor_ (method {:reflected se-ctor})]]
      (if impl-stateless?
        (emit-wrapping-ctor cw Opcodes/ACC_PUBLIC se-ctor_ [])
        (emit-wrapping-ctor cw Opcodes/ACC_PUBLIC se-ctor_ (:param-types impl-ctor_)
          (.loadThis) (.visitTypeInsn Opcodes/NEW (internal-name real-impl))
          (.dup) (.loadArgs 0 (count (:param-types impl-ctor_)))
          (method-insn Opcodes/INVOKESPECIAL impl-ctor_)
          (field-insn Opcodes/PUTFIELD impl-fd))))
    #_"emit overrides"
    (doseq [[sig impl-spec] sig->impl-spec
            m (sig->meths sig)]
      (let [annots (let [[_name [_self & args]] (:form impl-spec)]
                     (mapv meta args))
            m_ (method {:reflected m})
            impl-m_ (util/updates (method {:reflected m :owner real-impl})
                      :name #(str impl-prefix %)
                      :param-types #(prepend-args [stub] %))
            mw (emit-method cw
                 (if (Modifier/isProtected (util/modifiers m)) Opcodes/ACC_PROTECTED Opcodes/ACC_PUBLIC)
                 m_)]
        (doseq [[i ann] (map vector (range) annots)] 
          (@Compiler/ADD_ANNOTATIONS mw ann i))
        (doto mw
          (.visitCode)
          (get-impl-fd) (.loadThis) (.loadArgs) 
          (method-insn Opcodes/INVOKEVIRTUAL impl-m_)
          (.returnValue) (.endMethod))))
    (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))

(defn ^Class instance-cls
  [{:keys [sig->meths sig->impl-spec ifaces] :as parsed}]
  (let [outname (str (namespace-munge this-ns) "._shell$" (unique-suffix))
        impl (overrides-impl parsed)
        stub (subclass-stub parsed)
        cw (->cw
             Opcodes/ACC_PUBLIC
             (util/dots2slashes outname) nil
             (internal-name stub)
             (into-array String (map #(internal-name ^Class %) ifaces)))
        impl-fd (field {:owner outname :name (name (gensym impl-prefix)) :type impl})]
    #_"emit impl field and ctor"
    (.visitEnd (emit-field cw Opcodes/ACC_PRIVATE impl-fd))
    (doseq [ctor (.getConstructors stub)
            :let [ctor_ (method {:reflected ctor})]]
      (emit-wrapping-ctor cw Opcodes/ACC_PUBLIC ctor_ [impl]
        (.loadThis) (.loadArg 0)
        (field-insn Opcodes/PUTFIELD impl-fd)))
    #_"Emit overrides, but only for the methods that are actually implemented
       As it turns out, methods that differ only by return type are actually different
       on the JVM."
    (doseq [[sig impl-spec] sig->impl-spec
            m (sig->meths sig)]
      (let [[m-anns par-anns]
            (let [[name_ [_self & args]] (:form impl-spec)]
              [(meta name_) (mapv meta args)])
            m_
            (method {:reflected m})
            impl-m_
            (util/updates (method {:reflected m :owner impl})
              :name #(str impl-prefix %)
              :param-types #(prepend-args [stub] %))
            mw
            (emit-method cw
              (if (Modifier/isProtected (util/modifiers m)) Opcodes/ACC_PROTECTED Opcodes/ACC_PUBLIC)
              m_)] 
        (doseq [[i ann] (map vector (range) par-anns)]
          (@Compiler/ADD_ANNOTATIONS mw ann i))
        (doto mw
          (@Compiler/ADD_ANNOTATIONS m-anns)
          (.visitCode)
          (.loadThis) (field-insn Opcodes/GETFIELD impl-fd) (.loadThis) (.loadArgs)
          (method-insn Opcodes/INVOKEINTERFACE impl-m_)
          (.returnValue) (.endMethod))))
    (util/load-and-compile outname (.toByteArray (doto cw (.visitEnd))))))