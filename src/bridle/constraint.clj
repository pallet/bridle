(ns bridle.constraint
  (:require
   [clojure.core.match :as match]
   [clojure.set :as set]))

(defmulti constrain
  "Interpret a constraint definition."
  first)

(def constrain-map)

(defn match-not-found [x] (= x :clojure.core.match/not-found))

;; constrain a map. You may specify :mandatory and :optional keys.
;; You may also specify :allow-other-keys (defaults true)
(defmethod constrain 'constrain-map
  [form]
  (let [{:keys [mandatory optional types allow-other-keys]
         :or {allow-other-keys true}} (apply hash-map (rest form))
         optional (set optional)
         inferred (keys types)
         mandatory (set/union mandatory
                              (set/difference (set inferred) optional))
         map-keys (fn []
                    (into {}
                          (map
                           (fn possibly-guarded [k]
                             (if-let [t (get types k)]
                               (vector
                                k
                                (list
                                 '_ :when
                                 ;; We would like to add nil? as an alternative
                                 ;; guard for optional keys. But since the
                                 ;; functions are passed to :when as symbols
                                 ;; they can not be comp'd
                                 (if (optional k)
                                   (let [t (if (sequential? t) t [t])]
                                     (vec (map
                                           (fn [t1]
                                             `(fn [x#]
                                                (or (match-not-found x#)
                                                    (~t1 x#))))
                                           t)))
                                   t)))
                               (vector k '_)))
                           (concat mandatory optional))))]
    `(fn [data#]
       (match/match
        [data#]
        [~(if allow-other-keys
            (map-keys)
            (list (map-keys) :only (vec (concat mandatory optional))))] true
        [{}] false))))

(defmacro constrain-pred-fn
  "Generate a constraint checking function.
   The `form` is a constrain form."
  [form]
  (constrain form))

(defmacro defconstraint
  "Define a named constraint."
  [constraint-name]
  `(defn ~constraint-name [form#]
     (when-not (constraint-pred-fn form#)
       (assert false (format "%s failed: %s" ~constraint-name form#)))))
