(ns bridle.constraint
  (:require
   [clojure.core.match :as match]
   [clojure.set :as set]))

(defmulti constrain
  "Interpret a constraint definition."
  first)

(def constrain-map)

(defn key-not-found? [x] (= x :clojure.core.match/not-found))
(defn key-found? [x] (not= x :clojure.core.match/not-found))

;; constrain a map. You may specify :has and :allow keys.  any constraints on
;; component types are specified in a map from key to constraint, passed to
;; :types
(defmethod constrain 'constrain-map
  [form]
  (let [{:keys [has allow types]} (apply hash-map (rest form))
        allow-specified? allow  ;; allow specification of empty sequence
        allow (set allow)
        inferred (keys types)
        has (set has)
        map-keys (fn []
                   (into {}
                         (map
                          (fn possibly-guarded [k]
                            (let [t (get types k)]
                              (vector
                               k
                               (list
                                '_
                                :when
                                (let [t (or t [])
                                      t (if (sequential? t) t [t])]
                                  (vec
                                   (concat
                                    (if (has k) [`key-found?])
                                    (map
                                     (fn [t1]
                                       (if (has k)
                                         t1
                                         `(fn [x#]
                                            (or (key-not-found? x#)
                                                (~t1 x#)))))
                                     t))))
                                t))))
                          (distinct (concat has allow (keys types))))))]
    `(fn [data#]
       (and
        (instance? java.util.Map data#)
        (match/match
         [data#]
         [~(if allow-specified?
             (list (map-keys) :only (vec (concat has allow)))
             (map-keys))] true
         [{}] false)))))

(defmacro constrain-pred-fn
  "Generate a constraint checking function.
   The `form` is a constrain form."
  [form]
  (constrain form))

(defn assert-on-constraint-failed
  "A constraint-failed-handler that asserts."
  [constraint-name value]
    (assert false (format "%s failed: %s" constraint-name value)))

(def ^{:dynamic true :doc "Constraint failure handler"}
  constraint-failed-handler assert-on-constraint-failed)

(defmacro defconstraint
  "Define a named constraint."
  [constraint-name form]
  `(defn ~constraint-name [value#]
     (let [f# ~(constrain form)]
       (or (f# value#)
        (constraint-failed-handler ~constraint-name value#)))))
