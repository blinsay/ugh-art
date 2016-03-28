(ns ugh-art.ell.core
  "Core functions for L-systems."
  (:require [quil.core :as q]))

;; trees
;;
;; Instead of representing L-system alphabets as turtle-string, it's easier to
;; think of them as trees with no value at non-leaf nodes, and an arbitrary
;; number of children. In the strongly typed word, they'd be defined as
;;
;;     Tree<T> = Array<Either<T, Tree<T>>>

;; For some T that represents the L-system alphabet.
;;
;; Here, a tree is a vector and each leaf is a keyword, which has the upside of
;; letting an L-system tree look just like an L-system string with some extra
;; colons added. For example, the L-system 'FF[+F]-F' would be represented
;; in tree-form as [:F :F [:+ :F] :- :F].

(defn tree-mapcat
  ([xs f] (tree-mapcat xs f []))
  ([xs f already-mapped]
   (if (seq xs)
     (let [x      (first xs)
           result (if (vector? x)
                    (concat already-mapped [(tree-mapcat x f [])])
                    (concat already-mapped (f x)))]
       (tree-mapcat (rest xs) f result))
     (vec already-mapped))))

(defn take-segments
  ([xs n]
   (if (and (> n 0) (seq xs))
     (let [x (first xs)
           m (if (= x :F)
               (- n 1)
               n)]
       (if (vector? x)
         (cons (vec (take-segments x m)) (take-segments (rest xs) m))
         (cons x (take-segments (rest xs) m))))
     [])))

;; L-systems

(defmacro rulefn
  "returns a fn that replaces the given keyword with a replacement vector"
  [& rules]
  `(fn [x#]
     (condp = x# ~@rules [x#])))

(defn rewriter
  "return a fn that rewrites terms in an L-system using the given rulefn"
  [rulefn]
  (fn [system]
    (tree-mapcat system rulefn)))

(defn rewrite
  "rewrite terms in an L-system using the given rulefn. equivalent to ((node-rewriter rulefn) system)"
  [system rulefn]
  ((rewriter rulefn) system))

(defn generation
  [rulefn x n]
  (nth (iterate (rewriter rulefn) x) n))

(defn generations
  [rulefn x n]
  (take n (iterate (rewriter rulefn) x)))

;; Convert an L-System tree to points

(declare tree->points)

(defn branch
  [state branch-seq]
  (let [branch-points (tree->points branch-seq
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
      :F      (move    state item)
      :f      (move-to state item)
      (:+ :-) (rotate  state item)
      state)))

(defn tree->points
  ([symbols d theta]
   (tree->points symbols {:x 0 :y 0} 0 d theta))
  ([symbols position heading d theta]
   (tree->points symbols (merge position
                                {:d d
                                 :theta theta
                                 :heading heading
                                 :segments []
                                 :current-segment [position]})))
  ([symbols state]
   (let [final-state (reduce process-item state symbols)]
     (conj (:segments final-state) (:current-segment final-state)))))
