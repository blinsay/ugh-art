(ns ugh-art.ell.growth
  (:require [quil.core :as q]
            [quil.middleware]
            [ugh-art.ell.core :as ell]))

(def p1
  (ell/rulefn
   :F [:F [:+ :F] :F [:- :F] [:F]]))

(def plant (ell/generation p1 [:F] 3))

(def growing
  (let [lengths (mapcat (fn [x] (repeat 5 x))
                        (range 0 (+ 1 (ell/max-segment-length plant))))
        prefix->points (fn [n]
                         (ell/tree->points (ell/take-segments plant n)
                                           5
                                           (q/radians 22.5)))]
    (atom (cycle (map prefix->points lengths)))))

(defn draw-points
  ([points] (draw-points points 0 0))
  ([points xoff yoff]
   (doseq [[{x1 :x y1 :y} {x2 :x y2 :y}] (partition 2 1 points)]
     (q/line (+ xoff x1) (+ yoff y1) (+ xoff x2) (+ yoff y2)))))

(defn draw
  []
  (q/background 0)
  (q/stroke 255)
  (q/scale 3)
  (q/translate 50 260)
  (q/rotate (q/radians -90))
  (let [ps (first @growing)]
    (doseq [xoffset (range 0 200 50)
            yoffset (flatten (repeat 5 [0 85 180]))]
      (doseq [segment ps]
        (draw-points segment yoffset xoffset)))
    (swap! growing rest)))

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
