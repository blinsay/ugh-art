(ns ugh-art.ell.core
  "Core functions for L-systems."
  (:require [quil.core :as q]))

(declare system->points)

(defn branch
  [state branch-seq]
  (let [branch-points (system->points branch-seq
                                      (assoc state
                                             :segments []
                                             :current-segment [(select-keys state [:x :y])]))]
    (assoc state
           :segments (concat (:segments state)
                             branch-points))))

(defn move
  [state _]
  (let [{:keys [x y heading d]} state
        next-x (+ x (* d (q/cos heading)))
        next-y (+ y (* d (q/sin heading)))]
    (assoc state
           :x next-x
           :y next-y
           :current-segment (conj (:current-segment state) {:x next-x :y next-y}))))

(defn move-to
  [state _]
  (let [{:keys [x y heading d]} state
        next-x (+ x (* d (q/cos heading)))
        next-y (+ y (* d (q/sin heading)))]
    (assoc state
           :x next-x
           :y next-y
           :segments (conj (:segments state) (:current-segment state))
           :current-segment [{:x next-x :y next-y}])))

(defn rotate
  [state op]
  (let [{:keys [heading theta]} state]
    (case op
      :+ (assoc state :heading (+ heading theta))
      :- (assoc state :heading (- heading theta)))))

(defn process-item
  [state item]
  (if (vector? item)
    (branch state item)
    (case item
      :f      (move    state item)
      :F      (move-to state item)
      (:+ :-) (rotate  state item)
      state)))

(defn system->points
  ([symbols d theta]
   (system->points symbols {:x 0 :y 0} 0 d theta))
  ([symbols position heading d theta]
   (system->points symbols (merge position
                                  {:d d
                                   :theta theta
                                   :heading heading
                                   :segments []
                                   :current-segment [position]})))
  ([symbols state]
   (let [final-state (reduce process-item state symbols)]
     (conj (:segments final-state) (:current-segment final-state)))))
