(ns
  ^{:doc "Collection of bass sounds."
    :author "Richard Russell"}
  overblow.inst.bass
  (:require [overtone.core]))


; Note: use (adsr attack decay sustain release) to get an ADSR envelope

(definst bass1
  " Bass sound 1"
  [freq 440
   amp 0.5
   att 0.75
   decay 0.5
   sus 0.8
   rel 1.0]
  (let [env (env-gen (adsr att decay sus rel) gate :action FREE)])
  (mix [(square 120) (saw 123)])

  (sin-osc freq))


(definst pluck-bass
  [freq 440]
  (pluck (* (square freq) (env-gen (perc 0.001 2) :action FREE)) 1 3 (/ 1 freq)))
