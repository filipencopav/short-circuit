(ns filipencopav.short-circuit
  (:refer-clojure :exclude [let])
  (:require [clojure.spec.alpha :as spec]))

(defmacro let [bindings & body]
  (if (<= 2 (count bindings))
    `(clojure.core/let [~@(take 2 bindings)]
       (if (reduced? ~(first bindings))
         (deref ~(first bindings))
         (let [~@(drop 2 bindings)]
           ~@body)))
    `(do ~@body)))

(spec/fdef filipencopav.short-circuit/let
  :args (spec/cat :bindings :clojure.core.specs.alpha/bindings
               :body (spec/* any?)))

(defn exit [x]
  (reduced x))
