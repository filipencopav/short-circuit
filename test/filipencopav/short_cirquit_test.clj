(ns filipencopav.short-cirquit-test
  (:require [filipencopav.short-cirquit :as sc]
            [clojure.test :as t :refer [deftest is]]))

(deftest basic-let-behavior
  (is
    (= (sc/let [a 1
                b 2
                c (+ a b)
                b 5]
         [a b c])
      [1 5 3])))

(deftest early-exit
  (is (= (sc/let
             [a 1
              b 2
              c (reduced 3)
              d (throw (ex-info "unreachable" {}))]
           (throw (ex-info "also unreachable" {})))
        3)))

(deftest no-early-exit
  (is (= (sc/let [a 1
                  b (- (inc a))
                  c (if false
                      (sc/exit {:error "msg"})
                      b)]
           [a b c])
        [1 -2 -2])))

(defn- reduce-helper [vec]
  (reduce (fn [acc next]
            (if (= 2 next)
              (reduced acc)
              (+ acc next)))
    vec))

(t/deftest- reduce-helper-t1
  (is (= (reduce-helper [3 2 1])
        3)))

(t/deftest- reduce-helper-t2
  (is (= (reduce-helper [3 1 1 1])
        6)))

(deftest with-inner-reduce
  (is (= (sc/let [a [3 2 1]
                  b (reduce-helper a)
                  c '(this value will be reached)]
           [a b c])

        [[3 2 1] ;; a
         3       ;; b
         '(this  ;; c
            value will be reached)])))

(deftest rebinding-1
  (is (= (sc/let [a 1
                  b 2
                  b (inc b) ;; (inc 2)
                  b (inc b) ;; (inc b) aka (inc 3) aka (inc (inc 2))
                  c b]
           [a b c])
        [1 4 4])))

(deftest rebinding-2
  (is (= (sc/let [a 1
                  b 2
                  b (inc b)
                  b (inc b)
                  c b]
           [a b c])
        [1 4 4])))

(defn- inner-call-helper [val]
  (sc/exit val))

(deftest exit-from-inner-call
  (is (= (sc/let [a 1
                  b (inner-call-helper a)
                  c (throw (ex-info "unreachable" {}))]
           (throw (ex-info "unreachable" {})))
        1)))
