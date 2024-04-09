(ns filipencopav.short-cirquit
  (:refer-clojure :exclude [let]))

(defn fn-transducer
  "`short-cirquit` internal."
  [func]
  (fn [xf]
    (fn
      ([] (xf))
      ([result] (xf result))
      ([result input]
       (if (reduced? result)
         result
         (xf (func result) input))))))

(defn- make-context-rebinder
  "Creates the /syntax/ for an anonymous function, which:
  1. Binds the variables from `context`
  2. Runs its own init logic, and binds it to `result`
  3. Checks if `result` is reduced:
     1. If yes -> `result`.
     2. If no -> new context with itself in it"
  [prev-varnames new-varname expr]
  `(fn [context#]
     (clojure.core/let [{:syms ~(into [] prev-varnames)} context#
           result# ~expr]
       (if (reduced? result#)
         result#
         (-> context#
           (assoc '~new-varname result#))))))

(defn- add-body-context-rebinder
  "Appends a body context rebinding fn to env"
  [body env]
  (update env :fns conj
    `(fn [context#]
       (clojure.core/let [{:syms ~(into [] (:vars env))} context#]
         ~@body))))

(defn- update-env [previous-env new-binding]
  (-> previous-env
    (update :vars conj (first new-binding))
    (update :fns conj
      (make-context-rebinder
        (:vars previous-env)
        (first new-binding)
        (second new-binding)))))

(defmacro let [bindings & body]
  (when-not (even? (count bindings))
    (throw (ex-info "Uneven number of bindings in `short-cirquit/let`"
             {:bindings bindings})))
  `(transduce
     (comp
       ~@(->> (partition 2 bindings)
           (reduce update-env {:vars [] :fns []})
           (add-body-context-rebinder body)
           (:fns)
           (map (fn [func-syntax]
                  `(fn-transducer ~func-syntax)))))
     (fn
       ([] {})
       ([r#] r#)
       ([r# _#] r#))
     ;; transducer won't start if it's empty
     {'anything# nil}))

(defn exit [x]
  (reduced x))
