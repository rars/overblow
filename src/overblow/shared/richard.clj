(ns overblow.shared.richard
  (:use [overblow.shared.core]
        [overtone.core]))

(definst siren [freq 440 freqchange 100 pulserate 2] (sin-osc (+ freq (* freqchange (sin-osc pulserate)))))

(definst strange-sound
  [freq]
  (sin-osc freq (* 35 (sin-osc 2))))

(defsynth dtmf-synth [f1 440 f2 220 amp 0.2]
  (let [env (envelope [1 1 0] [0.2 0])
        e   (env-gen env :action FREE)]
    (out 0 (pan2 (* amp (* e (+ (sin-osc f1) (sin-osc f2))))))))

(def dtmf-freqs [697 770 852 941 1209 1336 1477])

(defn freq-grid [freqs]
  (cons [(nth freqs 3) (nth freqs 5)]
        (for [x (take 3 freqs)
              y (drop 4 freqs)]
          [x y])))

;; DTMF frequencies for each digit
(def dtmf (freq-grid dtmf-freqs))


(defn play-dial-tone
  [metro beat separation inst sequence]
  (when (not (= '() sequence))
    (let [a (first (first sequence))
          b (second (first sequence))
          next-beat (+ beat separation)]
      (at (metro beat) (inst a b))
      (apply-at (metro next-beat) #(play-dial-tone metro next-beat separation inst (rest sequence))))))

(defn play-dial-tone
  [metro beat seps inst sequence]
  (when (not (= '() sequence))
    (let [a (first (first sequence))
          b (second (first sequence))
          separation (first seps)
          next-beat (+ beat separation)]
      (at (metro beat) (inst a b))
      (apply-at (metro next-beat) #(play-dial-tone metro next-beat (rest seps) inst (rest sequence))))))



(defn gen-dial-random-seq
  "Generates a random sequence of n DTMF frequency pairs."
  [n]
  (map (fn [x] (nth dtmf x)) (take n (repeatedly (fn [] (rand-int 10))))))

(defn gen-dial-seq
  "Generates a sequence of DTMF frequency pairs corresponding to the specified
   sequence of phone number digits."
  [sequence]
  (map (fn [x] (nth dtmf x)) sequence))

;; DTMF tuned frequencies.
(def dtmf-tuned (cons [523 1661] (for [x [698.5 830 1047] y [1397 1661 2093]] [x y])))

(defn lin-interpolate
  "Linearly interpolates between x and y by the ratio alpha."
  [alpha x y]
  (+ (* alpha y) (* (- 1 alpha) x)))

(defn dialtone-lookup-smoothed-freq
  [alpha n tuned-freq-grid]
  (map (fn [x y] (lin-interpolate alpha x y)) (nth dtmf n) (nth tuned-freq-grid n)))

(defn rising-edge
  "If n < wait returns 0, if n > (wait + transition) returns 1,
   otherwise returns a linear interpolation between 0 and 1 according to its
   position inbetween wait and transition. i.e. returns ((n-wait)/transition)."
  [wait transition n]
  (if (< n wait)
    0
    (if (< n (+ wait transition))
      (/ (- n wait) transition)
      1)))

(defn exp-edge
  "If n < wait returns 0, if n > (wait + transition) returns 1,
   otherwise returns a linear interpolation between 0 and 1 according to its
   position inbetween wait and transition. i.e. returns ((n-wait)/transition)."
  [wait k n]
  (if (< n wait)
    1
    (Math/exp (- 0 (* k (- n wait))))))

(defn gen-smooth-dial-seq
  " Generates a random dial sequence of length n that linearly interpolates
    from atonal to tonal in n steps."
  [n]
  (map (fn [x alpha]
         (dialtone-lookup-smoothed-freq alpha x))
       (take n (repeatedly (fn [] (rand-int 10))))
       (map (fn [x] (/ x n)) (range n))))

(defn gen-smooth-riseedge-dial-seq
  " Generates an infinite dial sequence that interpolates to tonal from atonal
    using a rising edge function specified by the wait and transition parameters."
  [wait transition]
  (map (fn [x alpha]
         (dialtone-lookup-smoothed-freq alpha x))
       (repeatedly (fn [] (rand-int 10)))
       (map (fn [x] (rising-edge wait transition x)) (range))))

;; Insert your phone number into the sequence to dial it!
(play-dial-tone metro (metro) (repeat 1.0) dtmf-synth (gen-dial-seq '(0 1 2 3 4)))

(defn play-accelerating-seq
  [min-sep max-sep time-change-wait k freq-change-wait freq-change-duration]
  (play-dial-tone metro (metro)
                  (map (fn [x] (+ min-sep (* max-sep (exp-edge time-change-wait k x))))
                       (range))
                  dtmf-synth (gen-smooth-riseedge-dial-seq freq-change-wait freq-change-duration)))

(play-accelerating-seq 0.25 1 5 0.1 10 10)


(stop)

;; Auto freq lookup

(defn note-to-all-octaves
  [note min-note max-note]
  (clojure.set/union
   (loop [x note notes []]
     (if (< x min-note)
       (set notes)
       (recur (- x 12) (conj notes x))))
   (loop [x note notes []]
     (if (> x max-note)
       (set notes)
       (recur (+ x 12) (conj notes x))))))

(defn frequencies-in-chord
  [chord min-note max-note]
  (let [notes (apply clojure.set/union
                   (map (fn [x] (note-to-all-octaves x min-note max-note)) chord))
        freqs (map midi->hz notes)]
    freqs))

(defn find-nearest
  [freq freq-set]
  (let [groups (group-by (fn [x] (> x freq)) freq-set)
        large-freq (reduce min (groups true))
        small-freq (reduce max (groups false))]
    (if (< (- large-freq freq) (- freq small-freq))
      large-freq
      small-freq)))

(println (sort (frequencies-in-chord (chord :c4 :major) 30 100)))

(find-nearest 1230 (frequencies-in-chord (chord :c4 :major) 30 100))

(defn dtmf-tuned-list
  [freq-sequence chord min-note max-note]
  (let [freq-set (frequencies-in-chord chord min-note max-note)]
    (n-from-sequence (count freq-sequence) freq-set)))

(defn dtmf-tuned
  [pairs chord min-note max-note]
  (let [freq-set (frequencies-in-chord chord min-note max-note)
        freq-mapping (fn [x] (find-nearest x freq-set))]
    (map (fn [[x y]] [(freq-mapping x) (freq-mapping y)]) pairs)))


;; New stuff using atoms

(def tuning (atom 0))
(def separation (atom 1))
(def freq-grid (atom (dtmf-tuned dtmf (chord :f4 :major) 10 100)))

(defn gen-smooth-dial-seq [alpha-atom freq-grid]
  (map (fn [x]
         (dialtone-lookup-smoothed-freq @alpha-atom x @freq-grid))
       (repeatedly (fn [] (rand-int 10)))))

(defn play-dial-tone
  [metro beat sep-atom inst sequence]
  (when (not (= '() sequence))
    (let [a (first (first sequence))
          b (second (first sequence))
          next-beat (+ beat @sep-atom)]
      (at (metro beat) (inst a b))
      (apply-at (metro next-beat) #(play-dial-tone metro next-beat sep-atom inst (rest sequence))))))

(play-dial-tone metro (metro) separation dtmf-synth (gen-smooth-dial-seq tuning))

(reset! tuning 1)
(reset! tuning 0)

(defn lin-ramp-atom [a duration target]
  (future
    (when (not= (float @a) (float target))
      (let [start-val  @a
            start-time (System/currentTimeMillis)
            end-time   (+ start-time duration)]
        (while (< (System/currentTimeMillis) end-time)
          (let [alpha (/ (- (System/currentTimeMillis) start-time) duration)]
            (reset! a (+ (* (- 1 alpha) start-val) (* alpha target))))
          (Thread/sleep 10))
        (reset! a target)))))

(lin-ramp-atom tuning 3000 1)

(defn exp-ramp-atom [a duration target]
  (future
    (when (not= (float @a) (float target))
      (let [start-val  @a
            start-time (System/currentTimeMillis)
            end-time   (+ start-time duration)]
        (while (< (System/currentTimeMillis) end-time)
          (let [k 5 ;; Get to 0.006 by 1
                alpha (Math/exp (- (* k (/ (- (System/currentTimeMillis) start-time) duration))))]
            (reset! a (+ (* alpha start-val) (* (- 1 alpha) target))))
          (Thread/sleep 10))
        (reset! a target)))))


(reset! tuning 0)
(reset! separation 1)
(play-dial-tone metro (metro) separation dtmf-synth (gen-smooth-dial-seq tuning freq-grid))

(lin-ramp-atom tuning 10000 1)
(exp-ramp-atom separation 10000 0.25)

(reset! freq-grid (dtmf-tuned dtmf (chord :c4 :major) 10 100))


  ;; 697 -> 698.5 F5
  ;; 770 -> 830 G#5
  ;; 852 -> 1047 C6
  ;; 941 -> 523 C5
  ;; 1209 -> 1397 F6
  ;; 1336 -> 1661 G#6
 ;; 1477 -> 2093 C7


(defn n-from-sequence
  [n sequence]
  (let [length (count sequence)
        indices (for [x (range n)] (Math/floor (* (- length 1) (/ x (- n 1)))))]
    ; (clojure.pprint/pprint indices)
    (for [x indices] (nth sequence x))))
