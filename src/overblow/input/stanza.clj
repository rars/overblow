(ns
  ^{:doc "For describing the structure of a composition"
    :author "Richard Russell"}
  overblow.input.stanza
  (:use [overtone.core]))

; Usage: (chord-sequence (:c4 :major) (:d4 :minor))
(defmacro chord-sequence
  [& chords]
    `(map (fn [xarg#] (apply chord xarg#)) (quote ~chords)))
