(ns overblow.rhythm.perc
  (:use [overtone.live]
        [overtone.inst.drum]))

(defn pattern-to-times
  [pattern beat elements-per-beat metro]
  (let [beats (map (fn [x y] (if (= x 1) (+ beat (/ y elements-per-beat)) 0)) pattern (range))]
    (map (fn [x] (metro x)) (filter (fn [y] (> y 0)) beats))))

(defn triplet-pattern-to-times
  [pattern beat metro]
  (let [beats (flatten  (map (fn [x y]
                               (map (fn [a b] (if (= a 1)
                                               (+ beat (* 2 y) (/ (* 2 b) 3))
                                               0)) x (range))
                               ) pattern (range)))]
    (map (fn [x] (metro x)) (filter (fn [y] (> y 0)) beats))))

(defn sequence-pattern
  [inst pattern note-len beat metro]
  (let [times (pattern-to-times pattern beat note-len metro)]
    (doseq [t times] (at t (inst)))))

(defn sequence-triplet-pattern
  [inst pattern note-len beat metro]
  (let [times (triplet-pattern-to-times pattern beat metro)]
    (doseq [t times] (at t (inst)))))

(defn bossa-nova
  " A bossa-nova percussion pattern. Plays for 8 beats starting at beat at the
    tempo dedicated to by metro. bass, hat and block are the three instruments
    involved in the pattern. "
  [bass hat block beat metro]
  (let [bass-pattern (flatten (repeat 4 [1 0 0 1]))
        block-pattern (take 16 (flatten (repeat 6 [0 0 1])))]
    (sequence-pattern bass bass-pattern 2 beat metro)
    (sequence-pattern hat (repeat 16 1) 2 beat metro)
    (sequence-pattern block block-pattern 2 beat metro)))

(defn samba
  " A samba pattern in common time. Plays for 4 beats starting at beat at the
    tempo described by metro. bass, hat and block are the three instruments used
    in the pattern. "
  [bass hat block beat metro]
  (let [bass-pattern (cons 1 (take 14 (flatten (repeat 4 [0 0 1 1]))))
        hat-pattern (take 16 (rest  (flatten (repeat 5 [0 0 0 1]))))
        block-pattern (take 16 (flatten (repeat 3 [1 0 1 0 1 1 0])))]
    (sequence-pattern bass bass-pattern 4 beat metro)
    (sequence-pattern hat hat-pattern 4 beat metro)
    (sequence-pattern block block-pattern 4 beat metro)))

(defn reggae
  [bass hat block beat metro]
  (let [hat-pattern (flatten (repeat 4 [1 0 0 1]))
        block-pattern [0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
        bass-pattern (flatten (repeat 4 [1 0 0 0]))]
    (sequence-pattern bass bass-pattern 4 beat metro)
    (sequence-pattern hat hat-pattern 4 beat metro)
    (sequence-pattern block block-pattern 4 beat metro)))

(defn sixteenth-off-beat
  [bass hat snare beat metro]
  (let [bass-pattern (flatten (repeat 2 [1 0 1 0]))
        snare-pattern (flatten (repeat 2 [0 1 0 1]))
        hat-pattern (take 16 (flatten (repeat 2 [1 1 1 1 1 1 1 1])))]
    (sequence-pattern bass bass-pattern 1 beat metro)
    (sequence-pattern hat hat-pattern 2 beat metro)
    (sequence-pattern snare snare-pattern 1 beat metro)))

(defn funky
  [bass hat snare beat metro]
  (let [bass-pattern [0 0 1 1 0 1 0 0]
        snare-pattern [1 0 0 1]
        hat-pattern (repeat 8 1)]
    (sequence-pattern bass bass-pattern 2 beat metro)
    (sequence-pattern snare snare-pattern 1 beat metro)
    (sequence-pattern hat hat-pattern 2 beat metro)))

(defn blues
  [bass snare hat beat metro]
  (let [bass-pattern (flatten (repeat 2 [1 1 1 0 0 0]))
        snare-pattern (flatten (repeat 2 [0 0 0 1 0 0]))
        hat-pattern (repeat 16 1)]
    (sequence-pattern bass bass-pattern 4 beat metro)
    (sequence-pattern snare snare-pattern 4 beat metro)
    (sequence-pattern hat hat-pattern 4 beat metro)))

(defn blues-shuffle
  [bass hat snare beat metro]
  (let [bass-pattern (repeat 2 [1 0 0])
        snare-pattern [[0 0 0] [1 0 0]]
        hat-pattern (repeat 2 [1 0 1])]
    (sequence-triplet-pattern bass bass-pattern 4 beat metro)
    (sequence-triplet-pattern snare snare-pattern 4 beat metro)
    (sequence-triplet-pattern hat hat-pattern 4 beat metro)))

(defn blues-shuffle2
  [bass hat snare beat metro]
  (let [bass-pattern [[1 0 0] [0 0 1] [0 0 0] [0 0 1] [1 0 0] [0 1 1] [0 0 0] [0 0 1]]
        snare-pattern [[0 0 0] [0 0 0] [1 0 0] [0 0 0] [0 0 0] [0 0 0] [1 0 0] [ 0 0 0]]
        hat-pattern (repeat 8 [1 0 1])]
    (sequence-triplet-pattern bass bass-pattern 4 beat metro)
    (sequence-triplet-pattern snare snare-pattern 4 beat metro)
    (sequence-triplet-pattern hat hat-pattern 4 beat metro)))

(defn rap
  [bass snare hat beat metro]
  (let [bass-pattern [1 0 0 0 1 1 0 0]
        snare-pattern [0 0 1 0 0 0 1 1]
        hat-pattern (repeat 8 1)]
    (sequence-pattern bass bass-pattern 2 beat metro)
    (sequence-pattern snare snare-pattern 2 beat metro)
    (sequence-pattern hat hat-pattern 2 beat metro)))

(defn play-pattern
  " Repeats a pattern indefinitely. Starts immediately once called.
    pattern-length is the length of the pattern in beats. "
  [pattern-fn beat metro pattern-length]
  (let [next-bar (+ beat pattern-length)]
    (at (metro beat) (pattern-fn beat)) ; pattern-fn needs beat argument since apply-at will call it ~300ms before actual beat.
    (apply-at (metro next-bar) #'play-pattern [pattern-fn next-bar metro pattern-length])))

(defn start-pattern
  [pattern length metro]
  (let [start-beat (metro)]
    (play-pattern #(pattern kick closed-hat2 noise-snare %1 metro) start-beat metro length)))

(start-pattern samba 4 metro)

(start-pattern bossa-nova 8 metro)

(start-pattern sixteenth-off-beat 7.75 metro)

(start-pattern reggae 4 metro)

(play-pattern #(samba quick-kick closed-hat open-hat %1 metro) (metro) metro 4)

(play-pattern #(bossa-nova quick-kick closed-hat open-hat %1 metro) (metro) metro 8)

(play-pattern #(reggae kick3 closed-hat noise-snare %1 metro) (metro) metro 4)
