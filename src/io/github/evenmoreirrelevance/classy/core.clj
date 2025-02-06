(ns io.github.evenmoreirrelevance.classy.core
  (:require
   [clojure.string :as str]
   [io.github.evenmoreirrelevance.classy.util :as util]
   [io.github.evenmoreirrelevance.classy.parse :as parse]
   [io.github.evenmoreirrelevance.classy.compile :as compile]))

(defmacro super-call "
Akin to calling a `super` method in Java. 
Behavior for calls outside of `instance` and `defsubclass` impl bodies is unspecified."
  [[m targ & args :as wrapped]]
  (util/throw-when [_ (not (str/starts-with? (name m) "."))]
    "expected syntax: (super-call (.<method> targ ...args))" {:found wrapped})
  (util/throw-when [_ (not (get &env 'EMI_in_impl_body))]
    "super-call disallowed outside of impl bodies" {})
  `(. ~'EMI_in_impl_body (~(symbol (str compile/super-prefix (subs (name m) 1))) ~targ ~@args)))

(let [dehint-prim 
      #(vary-meta % update :tag
         (fn [t]
           (when-not (some-> (util/type-of t) (.isPrimitive))
             t)))]
  (defn ^:private impl-body
    [^Class base fd-specs [name_ [self & args] & body]]
    (let [self_ (gensym "self_")
          hinted? (some #(:tag (meta %)) args)
          sig-args (map #(with-meta %1 (meta %2)) (repeatedly #(gensym "arg_")) args)]
      `(~(vary-meta (symbol (str compile/impl-prefix name_)) assoc :tag (:tag (meta name_)))
        [~'EMI_in_impl_body ~(cond-> self_ hinted? (vary-meta assoc :tag (.getName base))) ~@sig-args]
        (let [~@(apply concat
                  (for [fd fd-specs]
                    `[~(dehint-prim fd) (. ~self_ ~(symbol (str "-" (munge (name fd)))))]))
              ~self ~self_]
          (loop [~@(interleave (map dehint-prim args) sig-args)]
            ~@body))))))

(defmacro instance "
Evaluates to an instance of `supcls` initialized with `ctor-args`, 
which can override its own methods like `reify` does and implement more interfaces.
Super calls to public or protected methods are available via the `super-call` macro.
Note that unlike in `reify`, the output instance isn't an `IObj` by default.

Note that the class of the output is left unspecified, so it mustn't be relied upon."
  [[supcls & ctor-args] & reify-syntax]
  (let [[raw-opts raw-body]
        (->> reify-syntax
          (partition-all 2)
          (map vec)
          (split-with #(keyword? (% 0))))
        opts
        (into {} raw-opts)
        body
        (apply concat raw-body)
        parsed
        (parse/parse-extension-form {:base-sym supcls :body body})
        base (compile/subclass-stub parsed)
        impl (compile/overrides-impl parsed)
        shell (compile/instance-cls parsed)
        output `(new
                  ~(symbol (.getName shell))
                  (reify
                    ~@(apply concat opts)
                    ~(symbol (.getName impl))
                    ~@(sequence
                        (comp (filter seq?) (map (partial impl-body base [])))
                        body))
                  ~@ctor-args)]
    output))

(defmacro defsubclass "
Defines a subclass of `supcls` with the given added fields and overridden methods.
all usual options apply and the mutable fields can still be set in the bodies as per usual.
The class will implement all the constructors of the base, with the extra fields
added as head parameters; if any `ctor-fn-targets` are specified, a ->relname fn
will be defined as well which forwards to the appropriate ctor; it is recommended that
if none are listed, a custom ctor fn is still defined for the sake of repl-friendliness.

Unlike -say- a Proxy output, the output class can be inherited from with no friction if the 
::extensible? option is specified to be truthy; however, it's still discouraged."
  {:clj-kondo/ignore [:redefined-var :inline-def]}
  [relname [base & ctor-fn-targets] fields & opts+specs]
  (let [[raw-opts raw-specs] (->> opts+specs
                               (partition-all 2)
                               (map vec)
                               (split-with #(keyword? (% 0))))
        {privates true publics false} (group-by parse/fd-spec-private? fields)
        {:keys [::extensible?] :as opts} (into {} raw-opts)
        body (apply concat raw-specs)
        absname (str (namespace-munge *ns*) "." relname) 
        stub-desc (parse/parse-extension-form {:base-sym base :body body :fields fields})
        implname (str "_EMI_real_impl$" relname)
        stub (compile/subclass-stub stub-desc)
        impl (compile/overrides-impl stub-desc) 
        ctor-spec-overlong? #(< 20 (+ (count %) (count fields)))
        {short-specs false long-specs true} (group-by ctor-spec-overlong? ctor-fn-targets)]
    (util/throw-when [_ (not= (count ctor-fn-targets) (count (util/distinct-by count ctor-fn-targets)))]
      "ctor fn targets must be distinguishable by count alone" {:specs ctor-fn-targets})
    (util/throw-when [colliding (seq (filter
                                       #(not= (+ (count %) (count fields))
                                          (count (-> #{} (into fields) (into %))))
                                       ctor-fn-targets))]
      "ctor fn target arg names mustn't collide with field names" {:colliding colliding})
    (intern *ns* (symbol (str "->" relname)))
    (let [real-impl
          (eval
            `(deftype ~(vary-meta (symbol implname) assoc :private true :no-doc true)
               [~@privates]
               ~@(apply concat (dissoc opts ::extensible?))
               ~(symbol (.getName impl))
               ~@(sequence
                   (comp (filter seq?) (map (partial impl-body stub publics)))
                   body)))]
      
      (compile/defsubtype-cls
        {:stub-desc stub-desc
         :outname absname
         :real-impl real-impl
         :extensible? extensible?
         :meta (meta relname)})
      `(do
         (import '~(symbol absname))
         ~@(when (seq ctor-fn-targets)
             `[(defn ~(with-meta (symbol (str "->" relname)) (meta relname))
                 ~@(for [s short-specs]
                     `([~@(map #(vary-meta % dissoc :tag) (concat fields s))]
                       (new ~(symbol absname)
                         #_"Privates are passed in before publics."
                         ~@(concat privates publics)
                         ~@s)))
                 ~@(when (seq long-specs)
                     (let [head-args (repeatedly 19 #(gensym "arg"))
                           rest-arg (gensym "rest-arg")]
                       `([~@head-args & ~rest-arg]
                         (case (+ 19 (count ~rest-arg))
                           ~@(apply concat
                               (for [s long-specs]
                                 [(+ (count fields) (count s))
                                  `(let [~@(interleave (concat fields s) head-args)
                                         [~@(drop (count head-args) (concat fields s))] ~rest-arg]
                                     (new ~(symbol absname)
                                       ~@(concat privates publics)
                                       ~@s))]))
                           (throw (java.lang.IllegalArgumentException.
                                    (str "bad arity:" (+ 19 (count ~rest-arg))))))))))])
         ~(symbol absname)))))
