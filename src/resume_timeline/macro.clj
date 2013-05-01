(ns art-work.macro
  (:require [clojure.pprint :as pp]))

(defmacro defnr [tname args body]
  (let [[named# _ therest#] (partition-by #(= :& %) args)
        theargs# (vec (concat named# therest#))
        mainarg# ['a]]
    #_(println (str "the args:" theargs#))
    (println (str "the let: " (first (map vector theargs# mainarg#))))
    `(defn ~tname ~mainarg#
       (let #_(~['x `(first ~'a)])
            [ (first ~theargs#)  (first ~mainarg#)]
         ~body))))

(macroexpand '(defnr work [x y :& z]  (+ x 1)))
(defnr work [x y :& z]  (+ x 1))
(work [3 4 5]) ;; 4



#_(filter  #(= 'rest %) [1 'rest])


(defnr test-func [x y & z]
  (+ x y))

(test-func 5 6 7 8)

(defmacro splitter [name args & body]
  (let [func (first body)
        lister (second body)])
  `(map func lister))

(splitter
 []
 ((+ x 1) [1 2]))

(defn defunits-chaining [u units prev]
  (if (some #(= u %) prev)
    (throw (Throwable. (str u " depends on " prev))))
  #_(println units)
  #_(println "-----")
  (let [spec (first (filter #(= u (first %)) units))]
    (if (nil? spec)
      (throw (Throwable. (str "unknown unit " u)))
      (let [chain (second spec)]
        (if (list? chain)
          (* (first chain)
             (defunits-chaining
               (second chain)
               units
               (cons u prev)))
          chain)))))

(defmacro defunits [quantity base-unit & units]
  `(defmacro ~(symbol (str "unit-of-" quantity))
     [valu# un#]
     `(* ~valu#
         ~(if (= un# ~base-unit) 1
            ~@(map (fn [x]
                       `(~(first x)
                         ~(defunits-chaining
                             (first x)
                             (cons `(~base-unit 1)
                                   (partition 2 units))
                             nil)))
                     (partition 2 units))
            #_(map (fn [x]
                     `(~(first x)
                       ~(defunits-chaining
                          (first x)
                          (cons `(~base-unit 1)
                                (partition 2 units))
                          nil)))
                   (partition 2 units))))))

(defmacro test []
  `(~@(t t)))
(clojure.pprint/pprint  (macroexpand '(test)))


(clojure.pprint/pprint  (macroexpand '(defunits time s
                   m 60
                   h 3600)))
;; transform ->
;; (if (clojure.core/= un__2631__auto__ s) 1 (m 60) (h 3600)))))))
;; (if (clojure.core/= un__2631__auto__ s) 1 m 60 h 3600))))))
(defunits time s
     m 60
     h 3600)

(unit-of-time 4 m)



