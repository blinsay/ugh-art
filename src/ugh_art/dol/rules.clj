(ns ugh-art.dol.rules
  "Deterministic, context-free L-systems.

  DOL systems use :f :+ and :- as their alphabet.

    :f - move forward a fixed distance
    :+ - rotate in the \"positive\" direction
    :- - rotate in the \"negative\" direction

  Based on:
    http://algorithmicbotany.org/papers/abop/abop-ch1.pdf
    https://en.wikipedia.org/wiki/L-system"
  (:require [quil.core :as q]
            [ugh-art.utils :refer :all]))

(defmacro rulefn
  "Define a production rule for an l-system where each 'word' is represented as
   a seq of 'letters'. The rule is defined as a function that takes a seq and
   returns the succesor word as a seq.

   Takes the name of the rulefn and a set of production rules. Each production
   rule takes the form of

     letter next-letters

   where next-letters is a seq of letters. Any letter not represented in the
   list of production rules will be left as-is when generating a successor."
  [& rules]
  `(fn [state#]
     (mapcat (fn [x#]
               (condp = x# ~@rules [x#]))
             state#)))

(defmacro defsystem
  "Define a DOL system with the given initial state, theta, and production
   rules. See rulefn for more info on specifying production rules."
  [rname theta d initial-state & rules]
  `(def ~rname {:initial-state ~initial-state
                :d ~d
                :theta ~theta
                :f (rulefn ~@rules)}))

(defn generations
  "Shorthand for getting a seq of the first n generations of a rule."
  [n rule]
  (map vec (take n (iterate (:f rule) (:initial-state rule)))))

(defn generation
  "Shorthand for getting the nth generation of a rule."
  [n rule]
  (last (generations n)))

;; ----------------------------------------------------------------------------

(def algae
  (rulefn
   :a [:a :b]
   :b [:a]))

(def anabena-catenula
  (rulefn
   :ar [:al :br]
   :al [:bl :ar]
   :br [:ar]
   :bl [:al]))

(defsystem koch-island
  (q/radians 90) 4
  [:f :+ :f :+ :f :+ :f]
  :f [:f :- :f :+ :f :+ :f :f :- :f :- :f :+ :f])

(defsystem koch-snowflake
  (q/radians 60) 3
  [:f :- :- :f :- :- :f]
  :f [:f :+ :f :- :- :f :+ :f])

(defsystem koch-anti-snowflake
  (q/radians 60) 3
  [:f :- :- :f :- :- :f]
  :f [:f :- :f :+ :+ :f :- :f])

(defsystem an-maze-ing
  (q/radians 90) 1
  [:f :+ :f :+ :f :+ :f]
  :f [:f :f :+ :f :- :f :+ :f :+ :f :f])

(defsystem squarepinski
  (q/radians 90) 3
  [:f :- :f :- :f :- :f]
  :f [:f :f :- :f :- :- :f :- :f])

(defsystem daisy-chain
  (q/radians 90) 3
  [:f :- :f :- :f :- :f]
  :f [:f :f :- :f :- :f :- :f :- :f :- :f :+ :f])
