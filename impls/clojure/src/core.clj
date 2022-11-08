(ns core
  (:require [printer]))

(defn equals [x y]
  (and (= (type x) (type y))
       (cond
         (list? x) (and (= (count x) (count y))
                        (every? #(apply equals %) (partition 2 (interleave x y))))
         :else
         (= x y))))

(def namespace
  {'+ +
   '- -
   '* *
   '/ /
   'do (fn [& exprs] (last exprs))
   'list list
   'list? list?
   'empty? empty?
   'count count
   '= equals
   '< <
   '<= <=
   '> >
   '>= >=
   'prn (fn [x]
          (println (printer/ast->string x))
          (flush))})

(comment (equals '() nil))
