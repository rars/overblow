(ns
    ^{:doc "Collection of methods for making it convenient to express
            musical forms."
      :author "Ian Davies"}
  overblow.input.transcribe)

(defmacro note-transcribe
  " Transcribes a sequence of notes to a vector of note names.
    E.g.
    (note-transcribe 3 c d# 2 gb a 4 a)
    -> (:c3 :d#3 :gb2 :a2 :a4)"
  [& args]
  `(second
    (reduce (fn [[oct# lst#] head#]
              (if (number? head#)
                [head# lst#]
                [oct# (conj lst# (keyword (str head# oct#)))]))
            [3 []]
            '~args)))
