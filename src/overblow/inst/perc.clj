(ns
  ^{:doc "Collection of percussion sounds."
    :author "Richard Russell"}
  overblow.inst.perc
  (:use [overtone.core]))

(defsynth kick [f 50 amp 1]
  (let [noise-env (env-gen (envelope [0 0.1 0] [0.02 0.01]))
        noise (* noise-env (normalizer (pink-noise)))
        pitch-env (env-gen (envelope [0 1 0] [0.02 0.5]))
        pitch (+ noise-env pitch-env)
        env (env-gen (perc 0.05 0.1 amp -5) :action FREE)
        sig (* env (mix [noise (normalizer (lpf:ar (saw f)
                                                   (* f (+ 1 (* 0.12 pitch)))))]))]
    (out 0 (pan2 sig))))

; Passes a signal through a series of band reject filters
(defmacro sig-bandreject-freq [sig freqs]
  (let [brfilter (fn [x y] (list 'brf:ar x y 5))]
    (reduce brfilter (concat [sig] freqs))))

; Generates a snare type synthdef
(defmacro gen-snare [name f seed]
  (let [rnd-gen (java.util.Random. seed)
        rnd-nums (take 1 (repeatedly #(.nextDouble rnd-gen)))
        notch-freq (vec (for [i rnd-nums] (* i f)))]
    `(defsynth ~name []
       (let [env# (env-gen (perc 0.01 0.15 1) :action FREE)
             freq# (lpf (pink-noise) ~f)
             filtered# (sig-bandreject-freq freq# ~notch-freq)
             sig# ('* env# (normalizer filtered#))]
         (out 0 (pan2 sig#))))))

(defsynth snare-additive []
  (let [tosc (lf-tri 111)
        sosc (mix [(sin-osc 180)  (sin-osc 330)])
        env (env-gen (perc 0.1 0.3) :action FREE)
        sig1 (* env sosc)
        sig2 (* env (mix [(freq-shift tosc 175) (freq-shift tosc 224)]))]
    (out 0 (pan2 (mix [sig1 sig2])))))

(defsynth snare-additive2 []
  (let [tosc (lf-tri 111)
        env (env-gen (perc 0.1 0.4) :action FREE)
        sig (mix [(sin-osc 180) (sin-osc 330)
                  (freq-shift tosc 175) (freq-shift tosc 224)])]
    (out 0 (pan2 (* env sig)))))

(defsynth cymbal []
  (let [env (env-gen (perc 0.01 0.75 1) :action FREE)
        initial-env (env-gen (adsr 0.0 0.2 0.0 0.0))
        higherpass-env (env-gen (adsr 0.2 3.6 0.0 0.0))
        carrier-freq 2500
        modulator-freq 1000
        pulse-carrier (pulse 2500)
        pulse-modulator (pulse 1000)
        attack-sig (* initial-env (bpf pulse-carrier
                                       (+ carrier-freq (* 20000 initial-env))))
        higherpass-sig (hpf pulse-carrier (+ carrier-freq (* 20000 higherpass-env)))
        sig (* env (mix [attack-sig higherpass-sig]))]
    (out 0 (pan2 sig))))
