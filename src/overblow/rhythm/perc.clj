(ns overblow.rhythm.perc)

(defn pattern-to-times
  [pattern beat elements-per-beat metro]
  (let [beats (map (fn [x y] (if (= x 1) (+ beat (/ y elements-per-beat)) 0)) pattern (range))]
    (map (fn [x] (metro x)) (filter (fn [y] (> y 0)) beats))))

(defn bossa-nova
  " A bossa-nova percussion pattern. Plays for 8 beats starting at beat at the
    tempo dedicated to by metro. bass, hat and block are the three instruments
    involved in the pattern. "
  [bass hat block beat metro]
  (let [bass-pattern (flatten (repeat 4 [1 0 0 1]))
        bass-times (pattern-to-times bass-pattern beat 2 metro)
        hat-times (map (fn [x] (metro (+ beat (/ x 2)))) (range 16))
        block-pattern (take 16 (flatten (repeat 6 [0 0 1])))
        block-times (pattern-to-times block-pattern beat 2 metro)]
    (doseq [time hat-times] (at time (hat)))
    (doseq [time bass-times] (at time (bass)))
    (doseq [time block-times] (at time (block)))))

(defn samba
  " A samba pattern in common time. Plays for 4 beats starting at beat at the
    tempo described by metro. bass, hat and block are the three instruments used
    in the pattern. "
  [bass hat block beat metro]
  (let [bass-pattern (cons 1 (take 14 (flatten (repeat 4 [0 0 1 1]))))
        bass-times (pattern-to-times bass-pattern beat 4 metro)
        hat-pattern (take 16 (rest  (flatten (repeat 5 [0 0 0 1]))))
        hat-times (pattern-to-times hat-pattern beat 4 metro)
        block-pattern (take 16 (flatten (repeat 3 [1 0 1 0 1 1 0])))
        block-times (pattern-to-times block-pattern beat 4 metro)]
    (doseq [time bass-times] (at time (bass)))
    (doseq [time hat-times] (at time (hat)))
    (doseq [time block-times] (at time (block)))))

(defn reggae
  [bass hat block beat metro]
  (let [hat-pattern (flatten (repeat 4 [1 0 0 1]))
        hat-times (pattern-to-times hat-pattern beat 4 metro)
        block-pattern [0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0]
        block-times (pattern-to-times block-pattern beat 4 metro)
        bass-pattern (flatten (repeat 4 [1 0 0 0]))
        bass-times (pattern-to-times bass-pattern beat 4 metro)]
    (doseq [time bass-times] (at time (bass)))
    (doseq [time hat-times] (at time (hat)))
    (doseq [time block-times] (at time (block)))))

(defn sixteenth-off-beat
  [bass hat snare beat metro]
  (let [bass-pattern (flatten (repeat 2 [1 0 1 0]))
        bass-times (pattern-to-times bass-pattern beat 1 metro)
        snare-pattern (flatten (repeat 2 [0 1 0 1]))
        snare-times (pattern-to-times snare-pattern beat 1 metro)
        hat-pattern (take 16 (flatten (repeat 2 [1 1 1 1 1 1 1 1])))
        hat-times (pattern-to-times hat-pattern beat 2 metro)]
    (doseq [time bass-times] (at time (bass)))
    (doseq [time snare-times] (at time (snare)))
    (doseq [time hat-times] (at time (hat)))))

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
    (play-pattern #(pattern kick3 closed-hat noise-snare %1 metro) start-beat metro length)))

(start-pattern samba 4 metro)

(start-pattern bossa-nova 8 metro)

(start-pattern sixteenth-off-beat 7.75 metro)

(start-pattern reggae 4 metro)

(play-pattern #(samba quick-kick closed-hat open-hat %1 metro) (metro) metro 4)

(play-pattern #(bossa-nova quick-kick closed-hat open-hat %1 metro) (metro) metro 8)

(play-pattern #(reggae kick3 closed-hat noise-snare %1 metro) (metro) metro 4)
