(ns overblow.shared.ian
  (:use [overblow.shared.core]
        [overtone.core]))

(demo (saw))



(defsynth wobble-slide [duration 0.2 repeats 1 out-bus 80]
  (let [freq (/ repeats duration)
        mod-freq (/ 0.5 duration)
        env (line:kr 1 1 duration FREE)]
    (out:kr out-bus (* (sin-osc:kr mod-freq)
                     ; (line:kr 0.9 1 duration)
                     (/ (+ 1 (sin-osc:kr freq (/ (Math/PI) 2))) 2)
                     ))))
