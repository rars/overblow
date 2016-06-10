(ns
  ^{:doc "Collection of bass sounds."
    :author "Richard Russell"}
  overblow.inst.bass
  (:use [overtone.core]))

; Note: use (adsr attack decay sustain release) to get an ADSR envelope

(definst bass1
  " Bass sound 1"
  [freq 440
   amp 0.5
   att 0.75
   decay 0.5
   sus 0.8
   rel 1.0]
;;  (let [env (env-gen (adsr att decay sus rel) gate :action FREE)])
  (mix [(square 120) (saw 123)])
  (sin-osc freq))

(definst pluck-bass
  [freq 440]
  (pluck (* (square freq) (env-gen (perc 0.001 2) :action FREE)) 1 3 (/ 1 freq)))

(defsynth our-bass [f1 50 f2 100 f3 200 amp 3]
  (let [env     (env-gen (perc 0.05 0.7 1 -5))
        sig     (* amp env (+ (lpf:ar (saw f1) (* 2 f1))
                              (lpf:ar (saw f2) (* 2 f2))
                              (lpf:ar (saw f3) (* 1 f3))))]
    (out 0 (pan2 sig))))

(defn play-bass [n]
  (our-bass (midi->hz (- n 12)) (midi->hz n) (midi->hz (+ n 7))))

(defn play-bass-riff [metro start-beat [next-note & riff]]
  (if (coll? next-note)
    (doseq [[i n] (map-indexed vector next-note)]
      (when n
        (at (metro (+ start-beat (* i (/ 1 (count next-note))))) (play-bass n))))
    (when next-note
      (at (metro start-beat) (play-bass next-note))))
  (when riff
    (apply-at (metro (+ 1 start-beat)) play-bass-riff [metro (+ 1 start-beat) riff])))

;(play-bass-riff metro (next-bar-start (count bass-riff)) (apply concat (repeat bass-riff)))
