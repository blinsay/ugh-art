(ns ugh-art.ell.sketch
  (:require [quil.core :as q]
            [quil.middleware]
            [ugh-art.ell.core :as ell]))

(def p1
  (ell/rulefn
   :F [:F [:+ :F] :F [:- :F] [:F]]))

(def p2
  (ell/rulefn
   :X [:F [:+ :X] :F [:- :X] :+ :X]
   :F [:F :F]))

(def plant (ell/generation p1 [:F] 3))

(def points
  (ell/tree->points plant 5 (q/radians 22.25)))

(def breeze
  (let [starting-angles (range 20 25 0.25)
        angles (concat starting-angles
                       (repeat 150 25)
                       (reverse starting-angles))]
    (atom
     (cycle
      (map #(ell/tree->points plant 5 (q/radians %1)) angles)))))

(defn draw-points
  [points]
  (doseq [[{x1 :x y1 :y} {x2 :x y2 :y}] (partition 2 1 points)]
    (q/line x1 y1 x2 y2)))

(defn draw
  []
  (q/background 187 234 250)
  (q/stroke 161 217 155)
  (q/scale 2)
  (q/translate 250 250)
  (q/rotate (q/radians -90))
  (let [ps (first @breeze)]
    (doseq [segment ps]
      (draw-points segment)))
  (swap! breeze rest))

(defn start
  []
  (q/defsketch sketch
    :name "repl-l-system-too"
    :size [800 800]
    :setup (fn []
             (q/smooth)
             (q/frame-rate 20)
             (q/background 0))
    :draw draw
    :middleware [quil.middleware/pause-on-error]))
