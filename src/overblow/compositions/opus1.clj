(ns
  overblow.compositions.opus1
  (:use [overtone.core])
  (:require [overblow.inst.strings :as strings]
            [overblow.rhythm.riff :as riff]))

(defn- get-chord [beat]
  (let [bar (quot (mod beat 12) 4)]
    (case bar
      0 (chord :c4 :minor)
      1 (chord :f4 :minor)
      2 (chord :g4 :minor))))

(def metro (metronome 220))

(defn start []
    (riff/arpeggiator-fn get-chord strings/super-string metro (metro)))
