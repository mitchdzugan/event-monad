(ns event-monad.core-test
  (:require [clojure.test :refer :all]
            [event-monad.core :refer :all]))

(let-events [[:init ([state _ ] {:state (merge state {:a 1})
                                :dispatch [[:inca 1]]})]
             [:inca ([state by] {:state (merge state {:a (+ by (:a state))})})]
             [:side ([state by] {:efct [(f <- (:getf))
                                        {:state (merge state {:a (f by (:a state))})}]})]])

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 10
           (:a (run-sync {:init (fn [state _] {:state (merge state {:a 1})
                                               :dispatch (free-mdo [[:inca 1]])
                                               })
                          :inca (fn [state by]
                                  {:state (merge state
                                                 {:a (+ by (:a state))})})
                          :side (fn [state by]
                                  {:efct (free-mdo [(f <- (:getf))
                                                    {:state (merge state {:a (f (:a state) by)})}])})}
                         {:efct {:getf (fn [] #(+ %1 %2))}}
                         {:efct (fn [{{a :a} :state}] (free-mdo [{:state {:a (+ 1 a)}}]))}
                         [[:init nil] [:inca 3] [:side 4]]))))))
