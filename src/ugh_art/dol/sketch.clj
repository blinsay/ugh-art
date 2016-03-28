(ns ugh-art.dol.sketch
  (:require [quil.core :as q]
            [quil.middleware]
            [ugh-art.dol.rules :as rules]
            [ugh-art.dol.turtle :as turtle]
            [ugh-art.utils :refer :all]))

(defn rule-cycle
  [n rule]
  (let [generations (rules/generations n rule)
        turtles (map (fn [idx] {:d (/ 1 (Math/pow (:d rule) idx))
                                :theta (:theta rule)})
                     (range n))
        build-state (fn [idx state turtle]
                      {:idx idx
                       :state state
                       :turtle turtle})]
    (atom
     (cycle
      (map build-state (range n) generations turtles)))))

(def states
  (rule-cycle 5 rules/daisy-chain))

(defn draw
  []
  (let [{:keys [idx state turtle]} (first @states)]
    (q/background 0)
    (q/stroke 255)
    (q/text (str (+ 1 idx)) 40 40)
    (q/translate 400 600)
    (turtle/draw-simple state turtle 250 250)
    (swap! states (fn [s] (rest s)))))

(defn start []
  (q/defsketch sketch
    :name "repl-l-system"
    :size [800 800]
    :setup (fn []
             (q/smooth)
             (q/frame-rate 1)
             (black-background))
    :draw (fn [] (draw))
    :middleware [quil.middleware/pause-on-error]))
