(ns filipencopav.short-circuit-test
  (:require [filipencopav.short-circuit :as sc]
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

(defn- get-evaluation-exception [form]
  (try (eval form)
    (catch Exception e e)))

(deftest validates-parameters
  (t/are [x]
    (= clojure.lang.Compiler$CompilerException
       (type (get-evaluation-exception x)))
    `(sc/let [~'a])
    `(sc/let ())
    `(sc/let [nil 1])))

(comment
  (require '[criterium.core :as c])

  ;;;; `sc/let` benchmark
  ;;
  ;;   Evaluation count : 1560780 in 60 samples of 26013 calls.
  ;;       Execution time sample mean : 38.599516 µs
  ;;              Execution time mean : 38.601035 µs
  ;; Execution time sample std-deviation : 219.078972 ns
  ;;     Execution time std-deviation : 223.751631 ns
  ;;    Execution time lower quantile : 38.287076 µs ( 2.5%)
  ;;    Execution time upper quantile : 39.042642 µs (97.5%)
  ;;                    Overhead used : 4.062608 ns
  (let [pairs (flatten (repeatedly 200 (juxt gensym #(rand-int 1000))))]
    (c/bench
     `(sc/let [~@pairs]
        [~@(reverse pairs)])
     :verbose))

  ;;;; `let` benchmark
  ;; Evaluation count : 1555980 in 60 samples of 25933 calls.
  ;;       Execution time sample mean : 38.554304 µs
  ;;              Execution time mean : 38.555960 µs
  ;; Execution time sample std-deviation : 197.808731 ns
  ;;     Execution time std-deviation : 202.462309 ns
  ;;    Execution time lower quantile : 38.205704 µs ( 2.5%)
  ;;    Execution time upper quantile : 38.919466 µs (97.5%)
  ;;                    Overhead used : 4.062608 ns
  (let [pairs (flatten (repeatedly 200 (juxt gensym #(rand-int 1000))))]
    (c/bench
     `(let [~@pairs]
        [~@(reverse pairs)])
     :verbose))




)
