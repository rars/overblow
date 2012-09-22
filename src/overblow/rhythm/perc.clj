(ns overblow.rhythm.perc
  (:use [overtone.live]
        [overtone.inst.drum]))

(def metro (metronome 128))

(defn shat1 [] (sample-player (sample (freesound-path 3721))))

(defn ssnare1 [] (sample-player (sample (freesound-path 26903))))
(defn ssnare2 [] (sample-player (sample (freesound-path 38927))))
(defn ssnare3 [] (sample-player (sample (freesound-path 774))))
(defn ssnare4 [] (sample-player (sample (freesound-path 441))))
(defn ssnare5 [] (sample-player (sample (freesound-path 442))))

(defn bongo1 [] (sample-player (sample (freesound-path 29801))))
(defn bongo2 [] (sample-player (sample (freesound-path 69053))))
(defn bongo3 [] (sample-player (sample (freesound-path 69052))))
(defn bongo4 [] (sample-player (sample (freesound-path 99754))))
(defn bongo5 [] (sample-player (sample (freesound-path 57136))))
(defn bongo6 [] (sample-player (sample (freesound-path 69051))))
(defn bongo7 [] (sample-player (sample (freesound-path 57138))))

(def bongo-instruments
  {:bongo-small bongo1
   :bongo-large bongo2
   :scrape [bongo1 bongo3 bongo4 bongo5 bongo6 bongo7]})

(def instruments {:bass kick :block noise-snare :hat closed-hat :snare noise-snare})

(def vary-instruments
  {:bass kick
   :snares [ssnare1 ssnare2 ssnare4 ssnare5]
   :hats [shat1]})

(defn pattern-to-times
  " Converts a pattern expressed as a sequence of 0s and 1s to a
    sequence of timestamps representing when an event should take place.

    beat -- when the sequence will begin
    elements-per-beat -- how many elements of pattern are contained in a beat
    metro -- a metronome for converting beats to timestamps.

    E.g. (pattern-to-times [1 0 1 0] (metro) 1 metro) will return times
    for the first and third beat."
  [pattern beat elements-per-beat metro]
  (let [conversion-fn (fn [x y] (if (= x 1)
                                 (+ beat (/ y elements-per-beat))
                                 0))
        beats (map conversion-fn pattern (range))]
    (map metro (filter (fn [y] (> y 0)) beats))))

(defn triplet-pattern-to-times
  " Converts a pattern expressed as a sequence of sequences of 0s and 1s to a        sequence of timestamps representing when an event should take place.

    Example input pattern: [[1 0 1] [0 0 1]].

    beat -- when the sequence will begin
    metro -- a metronome for converting beats to timestamps.

    While triplets can be expressed using pattern-to-times with three elements
    per beat and placing 1s on either the 1st, 4th or 6th positions, they are
    sufficiently common to deserve their own convenience syntax."
  [pattern beat metro]
  (let [conversion-fn (fn [x y]
                        (map (fn [a b] (if (= a 1)
                                        (+ beat (* 2 y) (/ (* 2 b) 3))
                                        0))
                             x (range)))
        beats (flatten (map conversion-fn pattern (range)))]
    (map metro (filter (fn [y] (> y 0)) beats))))

(defn sequence-pattern
  [inst pattern note-len beat metro]
  (let [times (pattern-to-times pattern beat note-len metro)]
    (doseq [t times] (at t (inst)))))

(defn sample-from
  [n collection]
  (let [indexes (repeatedly  #(rand-int (count collection)))]
    (take n (map (fn [x] (nth collection x)) indexes))))

(defn sequence-vary-pattern
  [inst-list pattern note-len beat metro]
  (let [times (pattern-to-times pattern beat note-len metro)
        insts (sample-from (count times) inst-list)
        time-inst-pairs (map vector times insts)]
    (doseq [[time inst] time-inst-pairs] (at time (inst)))))

(defn sequence-triplet-pattern
  [inst pattern note-len beat metro]
  (let [times (triplet-pattern-to-times pattern beat metro)]
    (doseq [t times] (at t (inst)))))

(defn play-pattern
  " Repeats a pattern indefinitely. Starts immediately once called.
    pattern-length is the length of the pattern in beats. "
  [pattern-fn beat metro pattern-length]
  (let [next-bar (+ beat pattern-length)]
    (at (metro beat) (pattern-fn beat)) ; pattern-fn needs beat argument since apply-at will call it ~300ms before actual beat.
    (apply-at (metro next-bar) #'play-pattern [pattern-fn next-bar metro pattern-length])))

(defn start-pattern
  [pattern length metro instruments]
  (let [start-beat (metro)]
    (play-pattern #(pattern %1 metro instruments) start-beat metro length)))

(defmacro def-drum-pattern
  "Defines a drum pattern."
  [name doc pattern-list]
  `(defn ~name
     ~doc
     [beat# metro# instruments#]
     (doseq [pattern-desc# ~pattern-list]
       (let [inst-name# (nth pattern-desc# 0)
             pattern# (nth pattern-desc# 1)
             elems-per-beat# (nth pattern-desc# 2)
             vary# (if (> (count pattern-desc#) 3)
                     (nth pattern-desc# 3)
                     false)]
         (if vary#
           (sequence-vary-pattern (instruments# inst-name#)
                                  pattern#
                                  elems-per-beat#
                                  beat# metro#)
           (sequence-pattern (instruments# inst-name#)
                             pattern#
                             elems-per-beat#
                             beat# metro#))))))

(def-drum-pattern
  bossa-nova
  " A bossa-nova percussion pattern. Plays for 8 beats starting at beat at the
    tempo dedicated to by metro. bass, hat and block are the three instruments
    involved in the pattern.

    instruments requires values for the keys :bass :hat and :block
    "
  [[:bass (flatten (repeat 4 [1 0 0 1])) 2]
   [:hat (repeat 16 1) 2]
   [:block (take 16 (flatten (repeat 6 [0 0 1])))]])

(def-drum-pattern
  samba
  " A samba pattern in common time. Plays for 4 beats starting at beat at the
    tempo described by metro. bass, hat and block are the three instruments used
    in the pattern.

    instruments requires values for the keys :bass :block and :hat"
  [[:bass (cons 1 (take 14 (flatten (repeat 4 [0 0 1 1])))) 4]
   [:hat (take 16 (rest  (flatten (repeat 5 [0 0 0 1])))) 4]
   [:block (take 16 (flatten (repeat 3 [1 0 1 0 1 1 0]))) 4]])

(def-drum-pattern
  reggae
  " Reggae drum pattern. "
  [[:bass (flatten (repeat 4 [1 0 0 0])) 4]
   [:hat (flatten (repeat 4 [1 0 0 1])) 4]
   [:block [0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0] 4]])

(def-drum-pattern
  sixteenth-off-beat
  " Sixteenth off-beat drum pattern exercise. "
  [[:bass (flatten (repeat 2 [1 0 1 0])) 1]
   [:snare (flatten (repeat 2 [0 1 0 1])) 1]
   [:hat (take 16 (flatten (repeat 2 [1 1 1 1 1 1 1 1]))) 2]])

(def-drum-pattern
  funky
  "Funky drum pattern."
  [[:bass [0 0 1 1 0 1 0 0] 2]
   [:snare [1 0 0 1] 1]
   [:hat (repeat 8 1) 2]])

(def-drum-pattern
  blues
  "Basic blues pattern."
  [[:bass (flatten (repeat 2 [1 1 1 0 0 0])) 4]
   [:snare (flatten (repeat 2 [1 1 1 0 0 0])) 4]
   [:hat (repeat 16 1) 4]])

(def-drum-pattern
  rap
  "Basic rap beat."
  [[:bass [1 0 0 0 1 1 0 0] 2]
   [:snare [0 0 1 0 0 0 1 1] 2]
   [:hat (repeat 8 1) 2]])

(def-drum-pattern
  bongo
  "Bongo pattern"
  [[:bongo-small [1 1 1 0] 1]
   [:bongo-large [0 0 0 1] 1]
   [:scrape [0 1 0 0 1 1 0 1 0 1 1 1 1 0 0 0] 4 true]])

(def-drum-pattern
  samba-vary
  " A samba pattern in common time. Plays for 4 beats starting at beat at the
    tempo described by metro. bass, hat and block are the three instruments used
    in the pattern.

    instruments requires values for the keys :bass :hats :snares
    :hats -- a sequence of hat instruments
    :snares -- a sequence of snare instruments "
  [[:bass (cons 1 (take 14 (flatten (repeat 4 [0 0 1 1])))) 4]
   [:hats (take 16 (rest  (flatten (repeat 5 [0 0 0 1])))) 4 true]
   [:snares (take 16 (flatten (repeat 3 [1 0 1 0 1 1 0]))) 4 true]])

(defn triplet-pat
  [triplet-list]
  ((fn [triplets result]
     (if (> (count triplets) 0)
       (let [x (first triplets)]
         (recur (rest triplets) (conj result [(nth x 0) 0 (nth x 1) 0 (nth x 2) 0])))
       (flatten result))) triplet-list []))

(def-drum-pattern
  blues-shuffle
  " Blues shuffle "
  [[:bass (triplet-pat (repeat 2 [1 0 0])) 3]
   [:snare (triplet-pat [[0 0 0] [1 0 0]]) 3]
   [:hat (triplet-pat (repeat 2 [1 0 1])) 3]])

(def-drum-pattern
  blues-shuffle2
  " Blues shuffle 2"
  [[:bass (triplet-pat [[1 0 0] [0 0 1] [0 0 0] [0 0 1] [1 0 0] [0 1 1] [0 0 0] [0 0 1]]) 3]
   [:snare (triplet-pat [[0 0 0] [0 0 0] [1 0 0] [0 0 0] [0 0 0] [0 0 0] [1 0 0] [ 0 0 0]]) 3]
   [:hat (triplet-pat (repeat 8 [1 0 1])) 3]])


(start-pattern samba
               4 overblow.shared.core/metro instruments)

(start-pattern bossa-nova 4 metro instruments)

(start-pattern sixteenth-off-beat 7.75 metro)

(start-pattern blues-shuffle 4 metro instruments)

(start-pattern reggae 4 metro)

(play-pattern #(samba quick-kick closed-hat open-hat %1 metro) (metro) metro 4)

(play-pattern #(bossa-nova quick-kick closed-hat open-hat %1 metro) (metro) metro 8)

(play-pattern #(funky kick3 closed-hat ssnare3 %1 metro) (metro) metro 4)
