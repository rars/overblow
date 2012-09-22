(ns overblow.shared.ian
  (:use [overblow.shared.core]
        [overtone.core]))

(defsynth slider [freq1 220 freq2 880]
  (let [alpha (in:kr 80 1)
        f (+ (* alpha freq1) (* (- 1 alpha) freq2))
        sig (sin-osc f)]
    (out 0 (pan2 sig))))

(defsynth our-bass [f-from 50 f-to 100]
  (let [alpha (in:kr 80 1)
        f     (+ (* alpha f-to) (* (- 1 alpha) f-from))
        f1    (/ f 2)
        f2    f
        f3    (* 1.5 f)
        env   (env-gen (perc 0.05 1.7 1 -5))
        sig   (* 3 env (+ (lpf:ar (saw f1) (* 2 f1))
                          (lpf:ar (saw f2) (* 2 f2))
                          (lpf:ar (saw f3) (* 1 f3))))]
    (out 0 (pan2 sig))))

(defsynth linear-slide [start-val 0 end-val 1 duration 1]
  (out:kr 80 (line:kr start-val end-val duration FREE)))

(defsynth attack-slide [start-freq 220 target-freq 880 slide-duration 0.2]
  (let [env (env-gen (perc 0.2 1))
        f (line:kr start-freq target-freq slide-duration)
        sig (* env (sin-osc f))]
    (out 0 (pan2 sig))))
