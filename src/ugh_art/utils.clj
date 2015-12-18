(ns ugh-art.utils
  (:require [quil.core :as q]))

(def black-background (fn [] (q/background 0)))
(def white-background (fn [] (q/background 255)))

(defn uniq [l]
  ((fn take-if [xs skip-item]
     (when (seq xs)
       (let [f (first xs)
             r (lazy-seq (take-if (rest xs) f))]
         (if (= f skip-item)
           r
           (cons f r)))))
   l nil))

(defn x-scale [img]
  (/ (q/width) (.width img)))

(defn y-scale [img]
  (/ (q/height) (.height img)))

(defmacro get-byte [x n]
  `(bit-and (unsigned-bit-shift-right ~x ~n) 0xFF))

(defn pixel->rgba [^long pixel]
  [(get-byte pixel 16)
   (get-byte pixel 8)
   (get-byte pixel 0)
   (get-byte pixel 24)])

(defn rgba->pixel [[r g b a]]
  (bit-or (bit-shift-left a 24)
          (bit-shift-left r 16)
          (bit-shift-left g 8)
          (bit-shift-left b 0)))

(defn int->hex [n]
  (format "%x" (bit-and 0xFFFFFFFF n)))

(defn shuffle-color [color]
  (-> color
      pixel->rgba
      shuffle
      rgba->pixel))
