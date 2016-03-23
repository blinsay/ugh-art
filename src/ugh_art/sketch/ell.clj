(ns ugh-art.sketch.ell
  (:require [quil.core :as q]
            [quil.middleware]
            [ugh-art.ell.core :as ell]))

(def plant
  (let [generator (ell/node-rewriter
                   (ell/rulefn
                    :F [:F :F :- [:- :F :+ :F :+ :F] :+ [:+ :F :- :F :- :F]]))
        generations (iterate generator [:F])]
    (nth generations 6)))

(def points
  (ell/system->points plant 4 (q/radians 22.25)))

(defn draw-points
  [points]
  (doseq [[{x1 :x y1 :y} {x2 :x y2 :y}] (partition 2 1 points)]
    (q/line x1 y1 x2 y2)))

(defn draw
  []
  (q/background 229 245 224)
  (q/stroke 161 217 155)

  (q/translate 400 750)
  (q/rotate (q/radians -90))
  (doseq [segment points]
    (draw-points segment)))

(defn start
  []
  (q/defsketch sketch
    :name "repl-l-system-too"
    :size [800 800]
    :setup (fn []
             (q/smooth)
             (q/frame-rate 1)
             (q/background 0))
    :draw draw
    :middleware [quil.middleware/pause-on-error]))
