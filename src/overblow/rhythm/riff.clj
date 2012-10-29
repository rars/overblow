(ns overblow.rhythm.riff
  (:use [overtone.live]))

(def metro (metronome 400))

(def chord-seq
  [[0 :c4 :major]
   [8 :f4 :major]
   [12 :g4 :major]])

(defn relative-to-absolute-beats [xs start]
  (map (fn [x] (concat (list (+ start (first x))) (rest x))) xs))


(def harmony (atom (chord :g4 :minor)))

(defn arpeggiator [harmony-atom inst metro beISat]
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
      (apply-at (metro (+ 1 beat))
                arpeggiator
                [harmony-atom inst metro (+ beat 1)]))))

(def my-notes (cycle (chord :Bb4 :major7)))

(arpeggiator harmony super-string metro (metro))

(reset! harmony (chord :a4 :dim7))

(stop)
