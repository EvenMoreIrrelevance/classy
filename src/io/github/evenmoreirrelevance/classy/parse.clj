(ns ^:no-doc io.github.evenmoreirrelevance.classy.parse
  (:require
   [io.github.evenmoreirrelevance.classy.util :as util])
  (:import
   (java.lang.reflect Method Modifier)
   (org.objectweb.asm Opcodes)))

(defn fd-spec-private?
  #_"keep this around; parsing the fields and then proceeeding to pull the form right back out
     feels like it isn't the right choice"
  [fd-spec]
  (or
    (contains? (meta fd-spec) :unsynchronized-mutable)
    (contains? (meta fd-spec) :volatile-mutable)))

(defn parse-field
  [fd-sym]
  {:form fd-sym
   :name (munge (name fd-sym))
   :type (or (util/whenp .isPrimitive (util/type-of (:tag (meta fd-sym)))) Object)
   :private? (fd-spec-private? fd-sym)})

(defn parse-impl
  [sigs [name_ [_self & args] & _body :as impl]]
  (let
    [name+arity->sigs
     (group-by (fn [[name_ args]] [name_ (count args)]) sigs)
     hinted?
     (some #(:tag (meta %)) args)
     munged-name
     (munge (name name_))
     resolved-sig
     (if-not hinted?
       (let [[m & more] (get name+arity->sigs [munged-name (count args)])]
         (util/throw-when [_ (or (nil? m) (seq more))]
           "couldn't resolve single method for non-hinted impl"
           {:first-candidate m
            :more-candidates more
            :impl impl})
         m)
       (let [impl-sig [munged-name (map #(or (util/type-of (:tag (meta %))) Object) args)]]
         (util/throw-when [_ (not (contains? sigs impl-sig))]
           "couldn't resolve sig for hinted impl" {:impl-sig impl-sig :impl impl})
         impl-sig))]
    {:form impl
     :name munged-name
     :hinted? (some #(:tag (meta %)) args)
     :resolved-sig resolved-sig}))

(defn parse-iface
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

(defn parse-class
  [sym]
  (let [r (resolve sym)]
    (util/throw-when [_ (not (class? r))]
      "Couldn't parse class" {:sym sym :resolved-val r})
    r))

(defn overrideable?
  [^Method x]
  (let [mods (util/modifiers x)]
    (and
      (not (zero? (bit-and mods (util/flags Opcodes/ACC_PUBLIC Opcodes/ACC_PROTECTED))))
      (zero? (bit-and mods (util/flags Modifier/STATIC Modifier/FINAL Modifier/PRIVATE))))))

(defn base-method?
  [class ^Method meth]
  (let [owner (.getDeclaringClass meth)]
    (.isAssignableFrom owner class)))

(defn parse-extension-form
  [{:keys [base-sym body fields]}]
  (let [base (parse-class base-sym)
        ifaces (into #{} (comp (filter symbol?) (map parse-iface)) body)
        sigs->meths (group-by #(pop (method-sig %))
                      (util/distinct-by method-sig
                        (sort-by #(if (base-method? base %) 1 0)
                          (filter overrideable?
                            (mapcat #(.getMethods ^Class %)
                              (cons base ifaces))))))
        sig->impl-specs (group-by :resolved-sig
                          (vec (map (partial parse-impl (into #{} (keys sigs->meths)))
                                 (filter seq? body))))]
    (util/throw-when [dupes (seq (keep #(when (< 1 (count (val %))) (key %)) sig->impl-specs))]
      "duplicate impl for some methods." {:dupes dupes})
    {:base base
     :ifaces ifaces
     :sig->meths sigs->meths
     :sig->impl-spec (update-vals sig->impl-specs first)
     :field-specs (mapv parse-field fields)}))

(comment
  (parse-extension-form
    {:base-sym 'clojure.lang.APersistentMap
     :fields '[meta_ keys_ k->v]
     :body
     '[(assoc [self k v] (.assoc (edited-map self) k v))
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
       (withMeta [_ newmeta] (->WrapperMap newmeta keys_ k->v))]}))
