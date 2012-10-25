(ns
  ^{:doc "Collection of string sounds."
    :author "Richard Russell"}
  overblow.inst.strings
  (:use [overtone.core]))

(defsynth string1 [f 62]
  (let [env1 (env-gen (perc 0.2 1.0) :action FREE)
        vco1 (saw (vibrato f 2 0.02))
        env2 (env-gen (envelope [0 0.5 0.3 0.0] [0.2 0.4 0.4]))
        vco2 (saw (* f 1.01))
        sig (mix [(* env1 vco1) (* env2 vco2)])]
    (out 0 (pan2 sig))))

(demo (string1 790))

(defsynth string2 [f 62]
  (let [
        vco1 (saw (vibrato f 5 0.02))
        vco2 (saw (* f 1.01))
        env (env-gen (perc 0.2 1.0) :action FREE)
        sig (mix [vco1 vco2])]
    (out 0 (pan2 (* env sig)))))

; Suitable for high frequency notes
(defsynth string3 [f 62]
  (let [vco1 (pulse f (vibrato 0.225 2 1.1))
        env (env-gen (perc 0.2 1.0) :action FREE)
        sig (* env vco1)]

    (out 0 (pan2 sig))))

(demo (string3 1100))

(defsynth super-string [f 62]
  (let [mod1 (lf-tri 5)
        vco1 (pulse f (+ 0.275 (* mod1 0.225)))
        env1 (env-gen (perc 0.2 1.0) :action FREE)
        mod2 (lf-tri 4)
        vco2 (pulse (* f 1.01) (+ 0.275 (* mod2 0.225)))
        env2 (env-gen (perc 0.1 0.6 0.5))
        vco3 (saw (vibrato (* f 0.99) 5 0.02))
        sig (mix [(* env1 vco1) (* env2 vco2) (* env2 vco3)])]
    (out 0 (pan2 sig))))

(demo (super-string 820))
