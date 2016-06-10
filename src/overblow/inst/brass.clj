(ns
  ^{:doc "Collection of brass-type sounds."
    :author "Richard Russell"}
  overblow.inst.brass
  (:use [overtone.core]))

; TODO: a lot
(defsynth trumpet [f1 50 amp 1 att 0.01 rel 1 level 1]
  (let [growl-env (env-gen (envelope [0 1 0.5 0] [(/ att 10) 0.5 1.0]))
        growl-mod (* growl-env  (lf-tri:ar 80))
        env (env-gen (adsr att 0.1 rel level) :action FREE)
        noise (* 0.1  (rlpf:ar (white-noise) f1))
        res-freq (* (/ amp 10) f1)
        Q (* 0.5 (+ 1 amp))
        sig (* amp env
               (+ noise  (normalizer (rlpf:ar (saw f1)
                                              (* (+ 1.0 (* 0.5 growl-mod)) res-freq) Q))))]
    (out 0 (pan2 sig))))
