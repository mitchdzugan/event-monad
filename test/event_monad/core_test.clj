(ns event-monad.core-test
  (:require [clojure.test :refer :all]
            [event-monad.core :as em]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 9
           (:a (em/run-sync {:init (fn [state _]
                                     {:state (merge state {:a 1})
                                      :dispatch [:inca 1]})
                             :side (fn [state by]
                                     {:efct (fn [e] {:state (merge state {:a (((:getf e)) by (:a state))})})})
                             :inca (fn [state by]
                                     {:state (merge state {:a (+ by (:a state))})})}
                            {:efct {:getf (fn [] #(+ %1 %2))}}
                            {}
                            [[:init] [:inca 3] [:side 4]]))))))

