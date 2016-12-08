(ns event-monad.core (:require [clojure.core.async :as async]))

(defmacro mdo
  [env forms]
  (letfn [(use-env [form]
            (clojure.walk/postwalk (fn [f]
                                     (if (keyword? f)
                                       `(or (get ~env ~f) ~f)
                                       f))
                                   form))]
    (reduce (fn [res form]
              (if (and (seq? form) (= '<- (second form)))
                `((fn [~(first form)] ~res) ~(use-env (first (reverse form))))
                `((fn [~'_] ~res) ~(use-env form))))
            (-> forms reverse first use-env)
            (-> forms reverse rest))))

(defmacro free-mdo [forms] `(fn [~'env] (mdo ~'env ~forms)))

(defn handle-res
  [res state]
  (if (map? res)
    (->> res
         (filter (fn [[tag _]] (not= tag :state)))
         (reduce (fn [state [tags computation]]
                   (let [env (->> [tags]
                                  flatten
                                  (reduce (fn [env tag]
                                            (merge env
                                                   (get-in state [:_meta
                                                                  :transformers
                                                                  tag])))))
                         (get-in state [:_meta :interpreters tag])
                         res (computation env)]
                     (handle-res (((or (get-in state [:_meta :transformers tag])
                                       (fn [x] (fn [_] x))) res) env) state)))
                 (merge state (:state res))))
    state))

(defn manage-events
  [handlers interpreters transformers]
  (let [event-chan (async/chan)
        dispatch-chan (async/chan)
        dispatch-queue (atom [])
        dispatch (fn [event-name args]
                   (reset! dispatch-queue (conj @dispatch-queue [event-name args]))
                   (async/go
                     (async/<! dispatch-chan)
                     (let [[[event-name args] & rest-queue] @dispatch-queue]
                       (async/>! event-chan [event-name args])
                       (reset! dispatch-queue rest-queue)
                       (async/>! dispatch-chan true))))]
    (async/go (async/>! dispatch-chan true))
    (async/go-loop [state {:_meta {:handlers handlers
                                   :interpreters (merge {:dispatch {:dispatch (fn [[event-name args]] (dispatch event-name args))}} interpreters)
                                   :transformers (merge {:dispatch (fn [d] (free-mdo [(:dispatch d)]))} transformers)}}]
      (let [[event-name args] (async/<! event-chan)
            event-res ((get-in state [:_meta :handlers event-name]) state args)]
        (recur (handle-res event-res state))))
    dispatch))

(defn run-sync
  [handlers interpreters transformers events]
  (let [res (atom {:done false})
        dispatch (manage-events (merge handlers {:_end (fn [state _]
                                                         (reset! res {:done true
                                                                      :state state})
                                                         {})})
                                interpreters
                                transformers)]
    (doseq [[event-name args] (concat events [[:_end {}]])]
      (dispatch event-name args)
      (Thread/sleep 500))
    (loop []
      (Thread/sleep 3000)
      (if (:done @res)
        (:state @res)
        (recur)))))

