(ns art-work.macro
  (:require [clojure.pprint :as pp]))

(defmacro defnr [tname args body]
  (let [[named# _ therest#] (partition-by #(= :& %) args)
        theargs# (vec (concat named# therest#))
        mainarg# ['a]]
    (print (str "the args:" theargs#))
    (print (str "main arg" mainarg#))
    `(defn ~tname ~mainarg#
       (let (vec (map vector ~theargs# mainarg))
         ~body))))

(macroexpand '(defnr work [x y :& z]  (+ x 1)))
(defnr work [x y :& z]  (+ x 1))
(work [3 4 5]) ;; 4



#_(filter  #(= 'rest %) [1 'rest])


(defnr test-func [x y & z]
  (+ x y))

(test-func 5 6 7 8)


