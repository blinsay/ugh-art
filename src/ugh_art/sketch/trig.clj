(ns ugh-art.sketch.trig
  (:require [quil.core :as q]))

(defn- draw-lines [points]
  (doseq [pair (partition 2 1 points)]
    (apply q/line pair)))

(defn draw-func
  "Draw a function as if the origin was in the middle of the screen. Draws a
  point for every value in the range."
  [f from to step]
  (q/with-translation [(/ (q/width) 2) (/ (q/height) 2)]
    (draw-lines (map f (range from to step)))))

(defn rose
  "A trigonometric rose. k controls the number of leaves the rose has and scale
  increases the radius of the petals. offset adds a constant offset to the
  radius of each petal and spreads flowers apart."
  ([k scale] (rose k scale 0))
  ([k scale offset]
   (fn [t]
     (let [x (+ (* (q/cos (* k t)) (q/cos t)) (* offset (q/cos t)))
           y (+ (* (q/cos (* k t)) (q/sin t)) (* offset (q/sin t)))]
       [(* scale x) (* scale y)]))))

(def scales
  (atom (cycle (range 1 10))))

(defn draw []
  (let [scale (first @scales)
        f (rose scale 100)]
    (q/background 0)
    (q/stroke 255)
    (q/text (str scale) 20 20)
    (draw-func f 0 (* 2 Math/PI) 0.01)
    (swap! scales (fn [s] (rest s)))))

(defn run []
  (q/defsketch trig-fuckery
    :name "repl-trig"
    :size [800 800]
    :settings #(q/smooth 2)
    :setup (fn []
             (q/frame-rate 1)
             (q/background 255))
    :draw  (fn [] (draw))
    :middleware [quil.middleware/pause-on-error]))
