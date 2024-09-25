(ns filipencopav.short-cirquit
  (:refer-clojure :exclude [let])
  (:require [filipencopav.short-circuit :as sc]))

(defmacro fn-transducer
  "`short-circuit` internal."
  [func]
  `(sc/fn-transducer ~func))

(defmacro let [bindings & body]
  `(sc/let ~bindings
     ~@body))

(defmacro exit [x]
  `(sc/exit ~x))
