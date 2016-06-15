(ns
  ^{:doc "Helps to define the structure of a musical piece."
    :author "Richard Russell"}
  overblow.rhythm.structure)

(defn bar [beat]
  (quot beat 4))
