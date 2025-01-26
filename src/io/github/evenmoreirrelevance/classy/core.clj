(ns io.github.evenmoreirrelevance.classy.core
  (:require
   [clojure.string :as str]
   [io.github.evenmoreirrelevance.classy.util :as util]
   [io.github.evenmoreirrelevance.classy.instance :as instance]))

(defmacro super-call "
Akin to calling a `super` method in Java. 
Behavior for calls outside of `instance` and `defsubclass` impl bodies is unspecified."
  [[m targ & args]]
  (when-not (str/starts-with? (name m) ".")
    (ex-info "expected syntax: `(super-call (.<method> targ ...args))" {}))
  (when-not (get &env 'EMI_in_impl_body)
    (throw (ex-info "super-call disallowed outside of impl bodies" {}))) 
  `(. ~'EMI_in_impl_body (~(symbol (str instance/super-prefix (subs (name m) 1))) ~targ ~@args)))

(defn ^:private resolve-iface
  [sym]
  (let [r (resolve sym)]
    (cond
      (var? r)
      (cast Class (or (:on-interface @r) (throw (ex-info "not a protocol:" {:sym sym}))))
      (not (class? r))
      (throw (ex-info "not a class" {:sym sym :resolved-type (class r)}))
      (not (.isInterface ^Class r))
      (throw (ex-info "not an interface" {:sym sym :resolved-val r}))
      :else r)))

(defn ^:private impl-body
  [^Class base [name_ [self & args] & body]]
  (let [sig-args (map #(with-meta %1 (meta %2))
                   (repeatedly #(gensym "arg_"))
                   args)
        hinted? (some #(:tag (meta %)) args)]
    `(~(vary-meta (symbol (str instance/impl-prefix name_)) assoc :tag (:tag (meta name_)))
      [~'EMI_in_impl_body ~(cond-> self hinted? (vary-meta assoc :tag (.getName base))) ~@sig-args]
      (loop [~@(interleave args sig-args)]
        ~@body))))

(defmacro instance "
Evaluates to an instance of `supcls` initialized with `ctor-args`, 
which can override its own methods like `reify` does and implement more interfaces.
Super calls to public or protected methods are available via the `super-call` macro.
Note that unlike in `reify`, the output instance isn't an `IObj` by default.

Note that the class of the output is left unspecified, so it mustn't be relied upon."
  [[supcls & ctor-args] & reify-syntax]
  (let [[raw-opts raw-body] (->> reify-syntax
                              (partition-all 2)
                              (map vec)
                              (split-with #(keyword? (% 0))))
        opts (into {} raw-opts)
        body (apply concat raw-body)
        supcls_ (resolve supcls)
        ifaces (into #{} (comp (filter symbol?) (map resolve-iface)) body)
        supers [supcls_ ifaces]
        base (instance/supers->subcls-base supers)
        impl (instance/supers->impl supers)
        shell (instance/supers->shell supers)
        output `(new
                  ~(symbol (.getName shell))
                  (reify
                    ~@(apply concat opts)
                    ~(symbol (.getName impl))
                    ~@(sequence
                        (comp (filter seq?) (map (partial impl-body base)))
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

Unlike -say- a Proxy output, the output class can be inherited from with no friction;
however, it's still discouraged.
Note that unlike in `deftype` the fields are not accessible from the instance,
effectively being private to it."
  [relname [supcls & ctor-fn-targets] fields & deftype-syntax]
  (let [[raw-opts raw-body] (->> deftype-syntax
                              (partition-all 2)
                              (map vec)
                              (split-with #(keyword? (% 0))))
        opts (into {} raw-opts)
        body (apply concat raw-body)
        absname (str (namespace-munge *ns*) "." relname)
        implname (str "_EMI_real_impl$" relname)
        supcls_ (resolve supcls)
        ifaces (into #{} (comp (filter symbol?) (map resolve-iface)) body)
        supers [supcls_ ifaces]
        base (instance/supers->subcls-base supers)
        impl (instance/supers->impl supers)
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
          (eval ;;we need the real impl at compile-time
            `(deftype ~(vary-meta (symbol implname) assoc :private true :no-doc true)
               ~fields
               ~@(apply concat opts)
               ~(symbol (.getName impl))
               ~@(sequence
                   (comp (filter seq?) (map (partial impl-body base)))
                   body)))]
      (instance/emit-defsubtype-class [supcls_ ifaces absname] real-impl)
      `(do
         (import '~(symbol absname))
         ~@(when (seq ctor-fn-targets)
             `[(defn ~(with-meta (symbol (str "->" relname)) (meta relname))
                 ~@(for [s short-specs]
                     `([~@(map #(vary-meta % dissoc :tag) (concat fields s))]
                       (new ~(symbol absname)
                         ~@fields
                         ~@s)))
                 ~@(when (seq long-specs)
                     (let [head-args (repeatedly 19 #(gensym "arg"))
                           rest-arg (gensym "rest-arg")]
                       `([~@head-args & ~rest-arg]
                         (case (+ 19 (count ~rest-arg))
                           ~@(apply
                               concat
                               (for [s long-specs]
                                 [(+ (count fields) (count s))
                                  `(let [~@(interleave (concat fields s) head-args)
                                         [~@(drop (count head-args) (concat fields s))] ~rest-arg]
                                     (new ~(symbol absname)
                                       ~@fields
                                       ~@s))]))
                           (throw (java.lang.IllegalArgumentException.
                                    (str "bad arity:" (+ 19 (count ~rest-arg))))))))))])
         ~(symbol absname)))))
