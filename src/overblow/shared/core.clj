(ns overblow.shared.core
  (:use [overtone.live]))

(def metro (metronome 120))

(defn next-bar-start [barlen]
  (let [current-beat (metro)]
    (+ current-beat (- barlen (mod current-beat barlen)))))
