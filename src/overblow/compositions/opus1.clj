(ns
  overblow.compositions.opus1
  (:use [overtone.core])
  (:require [overblow.inst.strings :as strings]
            [overblow.rhythm.riff :as riff]
            [overblow.rhythm.perc :as perc]
            [overblow.inst.perc :as percsounds]))

(defn- get-chord [beat]
  (let [bar (quot (mod beat 16) 4)]
    (case bar
      0 (chord :e3 :minor7)
      1 (chord :g3 :major)
      2 (chord :d3 :sus4)
      3 (chord :a3 :7sus4))))

(defn- get-bridge-chords [beat]
  (let [bar (quot (mod beat 16) 4)]
    (case bar
      0 (chord :c3 :9)
      1 (chord :d3 :sus4)
      2 (chord :e3 :minor7))))

(def metro (metronome 144))

(def instruments
  {:bass perc/ssnare1
   :snare perc/ssnare2
   :hat perc/shat1})

(defn start []
    ;(perc/play-pattern #(perc/funky %1 metro instruments) (metro) metro 4)
    (riff/arpeggiator-fn get-chord percsounds/kick metro (metro)))
