(ns
  ^{:doc "Collection of percussion sounds."
    :author "Richard Russell"}
  overblow.inst.perc
  (:use [overtone.core]))

(defsynth kick [f 50]
  (let [noise-env (env-gen (envelope [0 0.3 0] [0.02 0.01]))
        noise (* noise-env (normalizer (pink-noise)))
        pitch-env (env-gen (envelope [0 1 0] [0.02 0.5]))
        pitch (+ noise-env pitch-env)
        env (env-gen (perc 0.05 0.1 1 -5) :action FREE)
        sig (* env (+ noise (normalizer (lpf:ar (saw f)
                                                (* f (+ 1 (* 0.12 pitch)))))))]
    (out 0 (pan2 sig))))

(demo (kick 52))

; Passes a signal through a series of band reject filters
(defmacro sig-bandreject-freq [sig freqs]
  (let [brfilter (fn [x y] (list 'brf:ar x y 5))]
    (reduce brfilter (concat [sig] freqs))))

; Generates a snare type synthdef
(defmacro gen-snare [name f seed]
  (let [rnd-gen (java.util.Random. seed)
        rnd-nums (take 7 (repeatedly #(.nextDouble rnd-gen)))
        notch-freq (vec (for [i rnd-nums] (* i f)))]
    `(defsynth ~name []
       (let [env# (env-gen (perc 0.01 0.15 1 -5) :action FREE)
             freq# (lpf (pink-noise) ~f)
             filtered# (sig-bandreject-freq freq# ~notch-freq)
             sig# ('* env# (normalizer filtered#))]
         (out 0 (pan2 sig#))))))

(gen-snare snare1 60 200)
(gen-snare snare1 90 200)
(gen-snare snare1 120 200)
(demo (snare1))

(stop)

(defsynth cymbal []
  (let [env (env-gen (perc 0.01 0.2 1) :action FREE)
        initial-env (env-gen (adsr 0.0 0.2 0.0 0.0))
        carrier-freq 2500
        modulator-freq 1000
        pulse-carrier (pulse 2500)
        pulse-modulator (pulse 1000)
        attack-sig (* initial-env (bpf pulse-carrier
                                       (+ carrier-freq (* 20000 initial-env))))
        sig (* env attack-sig)]
    (out 0 (pan2 sig))))
