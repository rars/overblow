(ns
  ^{:doc "Sound patterns."
    :author "Richard Russell"}
  overblow.rhythm.riff
  (:use [overtone.core]))

(defn relative-to-absolute-beats [xs start]
  (map (fn [x] (concat (list (+ start (first x))) (rest x))) xs))

(defn linear-beat-separator [separation]
  (fn [beat] (+ separation beat)))

(defn sine-beat-separator [separation]
  (fn [beat] (+ beat (* 3.0 (Math/sin (/ beat 10.0))))))

(defn arpeggiator
  ([harmony-atom inst metro beat] (arpeggiator harmony-atom inst metro beat (linear-beat-separator 1)))
  ([harmony-atom inst metro beat separatorfn]
   (when (> beat (count @harmony-atom))
     (let [length (count @harmony-atom)
           index (mod beat length)
           n1 (midi->hz (nth @harmony-atom index))
           n2 (midi->hz (nth @harmony-atom (mod (- index 1) length)))
           n3 (midi->hz (nth @harmony-atom (mod (- index 2) length)))
           n4 (midi->hz (nth @harmony-atom (mod (- index 3) length)))]

       (at (metro beat) (inst n1 0.5))
       (at (metro beat) (inst n2 0.25))
       (at (metro beat) (inst n3 0.125))
       (at (metro beat) (inst n4 0.0625))
       (apply-at (metro (separatorfn beat))
                 arpeggiator
                 [harmony-atom inst metro (separatorfn beat) separatorfn])))))
