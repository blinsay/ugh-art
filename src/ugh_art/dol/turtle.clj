(ns ugh-art.dol.turtle
  "Drawing functions for DOL system."
  (:require [quil.core :as q]
            [ugh-art.utils :refer [uniq black-background]]))

(defn turtle-step
  [turtle sym]
  (let [{:keys [x y heading d theta]} turtle]
    (case sym
      :f (assoc turtle
                :x (+ x (* d (q/cos heading)))
                :y (+ y (* d (q/sin heading))))
      :+ (assoc turtle :heading (+ heading theta))
      :- (assoc turtle :heading (- heading theta)))))

(defn- turtle-point
  [turtle]
  (select-keys turtle [:x :y]))

(defn turtle-points
  [turtle states]
  (let [states (reductions turtle-step turtle states)]
    (uniq (map turtle-point states))))

(defn scale
  [points x-scale y-scale]
  (map (fn [{:keys [x y]}]
         {:x (* x-scale x) :y (* y-scale y)})
       points))

(defn translate
  [points x-offset y-offset]
  (map (fn [{:keys [x y]}]
         {:x (+ x-offset x) :y (+ y-offset y)})
       points))

(defn draw-points
  [points]
  (doseq [[{x1 :x y1 :y} {x2 :x y2 :y}] (partition 2 1 points)]
    (do
      (q/line x1 y1 x2 y2))))

(defn bounding-box [points]
  (reduce (fn [{:keys [xmin ymin xmax ymax]} {:keys [x y]}]
            {:xmin (apply min (remove nil? [xmin x]))
             :ymin (apply min (remove nil? [ymin y]))
             :xmax (apply max (remove nil? [xmax x]))
             :ymax (apply max (remove nil? [ymax y]))})
          {}
          points))

(def default-turtle
  {:x 0 :y 0 :heading 0 :d 1})

(defn draw-simple
  [state turtle xscale yscale]
  (draw-points
   (scale (turtle-points (into default-turtle turtle) state) xscale yscale)))

(defn draw
  "Returns a function that draws the given DOL system.

  The point-set is generated once, and then redrawn every time the returned
  function is called, centered in the Processing window."
  [state turtle]
  (let [points (turtle-points (into default-turtle turtle) state)
        bbox (bounding-box points)
        width  (- (:xmax bbox) (:xmin bbox))
        height (- (:ymax bbox) (:ymin bbox))]
    (fn []
      (let [padding (* 0.15 (min (q/width) (q/height)))
            margin (/ padding 2)
            xscale (/ (- (q/width) padding) width)
            yscale (/ (- (q/height) padding) height)
            scaled (scale points xscale yscale)
            psbox (bounding-box scaled)
            xoffset (- margin (:xmin psbox))
            yoffset (- margin (:ymin psbox))
            ps (translate scaled xoffset yoffset)]
        (q/with-stroke [255]
          (black-background)
          (draw-points ps))))))
