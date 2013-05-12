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
  (let [macro-name (symbol (str "unit-of-" quantity))
        valsym (gensym "val__")
        unitsym (gensym "unit__")]
    (list `defmacro
          macro-name
          [valsym unitsym]
       `(* ~valsym
           (case ~unitsym
              ~base-unit 1
              ~@(mapcat (fn [x]
                          `(~(first x)
                            ~(defunits-chaining
                               (first x)
                               (cons `(~base-unit 1)
                                     (partition 2 units))
                               nil)))
                        (partition 2 units)))))))


(clojure.pprint/pprint  (macroexpand '(defunits time s
                   m 60
                   h 3600)))

(defunits time s
     m 60
     h (60 m))

(unit-of-time 4 h)

(ns http)

(defn post [url]
  {:body "hellp world"})

(ns app
  (:require [clojure.test :refer [deftest is run-tests]]))

(deftest is-a-macro
  (with-redefs [http/post (fn [url] {:body "goodbye world"})]
    (is (= {:body "goodbye world"} (http/post "http://service.com/greet")))))

(run-tests)





