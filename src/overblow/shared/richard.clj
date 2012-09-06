(ns overblow.shared.richard
  (:use [overblow.shared.core]))

;; DTMF frequencies for each digit
(def dtmf (cons [941 1336] (for [x [697 770 852] y [1209 1336 1477]] [x y])))

(defn play-dial-tone
  [metro beat separation inst sequence]
  (when (not (= '() sequence))
    (let [a (first (first sequence))
          b (second (first sequence))
          next-beat (+ beat separation)]
      (at (metro beat) (inst a b))
      (apply-at (metro next-beat) #(play-dial-tone metro next-beat separation inst (rest sequence))))))

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
  [alpha n]
  (map (fn [x y] (lin-interpolate alpha x y)) (nth dtmf n) (nth dtmf-tuned n)))

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
(play-dial-tone metro (metro) 1.0 dtmf-synth (gen-dial-seq '(0 1 2 3 4)))


  ;; 697 -> 698.5 F5
  ;; 770 -> 830 G#5
  ;; 852 -> 1047 C6
  ;; 941 -> 523 C5
  ;; 1209 -> 1397 F6
  ;; 1336 -> 1661 G#6
;; 1477 -> 2093 C7
