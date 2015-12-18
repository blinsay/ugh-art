(ns ugh-art.sketch.gary
  (:require [quil.core :as q]
            [quil.middleware]
            [ugh-art.utils :refer :all]))

(defn regular
  "Draw gary."
  []
  (let [{:keys [gary img-width img-height]} (q/state)
        scale (min (x-scale gary) (y-scale gary))]
    (black-background)
    (q/image gary 0 0 (* scale img-width) (* scale img-height))))

(defn dots []
  "Draw gary as floaty dots."
  (let [{:keys [gary pixels img-width img-height]} (q/state)
        xsc (x-scale gary)
        ysc (y-scale gary)
        sample (fn [] (let [x (rand-int img-width)
                            y (rand-int img-height)]
                        [x y (q/get-pixel gary x y)]))
        samples (repeatedly 25000 sample)]
    (doseq [[x y color] samples]
      (let [draw-x (q/ceil (* x xsc))
            draw-y (q/ceil (* y ysc))]
        (q/no-stroke)
        (q/fill color)
        (q/ellipse draw-x draw-y 8 12)))))

(defn dots-pixels []
  "Draw gary as pixely dots."
  (let [{:keys [gary pixels img-width img-height]} (q/state)
        xsc (x-scale gary)
        ysc (y-scale gary)
        sample (fn [] (let [x (rand-int img-width)
                            y (rand-int img-height)
                            loc (+ x (* y img-width))]
                        [x y (pixel->rgba (aget pixels loc))]))
        samples (repeatedly 25000 sample)]
    (doseq [[x y [r g b _]] samples]
      (let [draw-x (q/ceil (* x xsc))
            draw-y (q/ceil (* y ysc))]
        (q/no-stroke)
        (q/fill r g b 100)
        (q/ellipse draw-x draw-y 8 12)))))

(defn shuffle-pixels
  "DESTRUCTIVELY SHUFFLES GARY"
  []
  (let [{:keys [gary pixels img-width img-height]} (q/state)
        scale (min (x-scale gary) (y-scale gary))]
    (do
      (doseq [[i p] (map-indexed vector pixels)]
        (aset pixels i (shuffle-color p))))
    (q/update-pixels gary)
    (black-background)
    (q/image gary 0 0 (* scale img-width) (* scale img-height))))

(defn draw [] (dots))

(defn run []
  (q/defsketch trig-fuckery
    :name "repl-trig"
    :size [800 800]
    :settings #(q/smooth 2)
    :setup (fn []
             (q/smooth)
             (q/frame-rate 1)
             (black-background)
             (let [img (q/load-image "./images/gary.png")]
               (q/set-state! :gary img
                             :pixels (q/pixels img)
                             :img-width (.width img)
                             :img-height (.height img))))
    :draw  (fn [] (draw))
    :middleware [quil.middleware/pause-on-error]))
