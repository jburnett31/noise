(ns noise.samples
  (:use overtone.live
        overtone.sc.machinery.synthdef
        overtone.inst.drum))

(use 'overtone.sc.machinery.synthdef)

(kick :amp 1)
(kick2)
(kick3 :amp 1)
(kick4)
(dub-kick)

(def metro (metronome 128))

(defn snares [beat note]
  (at (metro beat) (snare :freq note :amp 1))
  (apply-at (metro (inc beat)) #'snares (inc beat) (+ 20 note) []))

(snares (metro) 100)
(stop)

(open-hat)
(closed-hat)
(hat-demo)
(closed-hat2)
(hat3)
(soft-hat)

(snare)
(snare2)
(noise-snare)
(tone-snare)

(tom)
(clap)

(bing)

(defn snare-loop [beat notes]
  (let [note (first notes)]
    (at (metro beat) (tone-snare :freq (midi->hz note)))
    (apply-at (metro (+ 0.5 beat)) #'snare-loop (+ 0.5 beat) (next (cycle notes)) [])))

(defn play-these []
  (map note [:c3 :d3 :e3 :f3 :g3]))
;(def play-these [60 65 70 75 80])

(snare-loop (metro) (play-these))
(stop)

(defn drum-loop [beat]
  (at (metro beat) (kick :amp 1))
  (at (metro (inc beat)) (snare :amp 1))
  (at (metro (+ 2 beat)) (kick :amp 1))
  (at (metro (+ 2.5 beat)) (kick :amp 1))
  (at (metro (+ 3 beat)) (snare :amp 1))
  (apply-at (metro (+ 4 beat)) #'drum-loop (+ 4 beat) []))

(drum-loop (metro))
(stop)

(defn add-more [beat]
  (at (metro beat)) (closed-hat)
  (at (metro (+ 0.15 beat)) (open-hat))
  (apply-at (metro (+ 3 beat)) #'add-more (+ 3 beat) []))

(add-more (metro))
(stop)


(def server (osc-server 44100 "osc-clj"))
(zero-conf-on)
(osc-listen server (fn [msg] (println msg)) :debug)
(osc-rm-listener server :debug)

;touch-osc beatmachine layout page 1

(osc-handle server "/1/push1" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (kick :amp 1))))
(osc-handle server "/1/push2" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (kick2 :amp 1))))
(osc-handle server "/1/push3" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (kick3 :amp 1))))
(osc-handle server "/1/push4" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (kick4 :amp 1))))
(osc-handle server "/1/push5" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (snare :amp 1))))
(osc-handle server "/1/push6" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (tone-snare :amp 0.5))))
(osc-handle server "/1/push7" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (open-hat :amp 0.5))))
(osc-handle server "/1/push8" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (closed-hat :amp 0.5))))
(osc-handle server "/1/push9" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (hat-demo :amp 0.5))))
(osc-handle server "/1/push10" (fn [msg] (if (= 1.0 (first (:args msg)))
                                          )))
(osc-handle server "/1/push11" (fn [msg] (if (= 1.0 (first (:args msg)))
                                          )))
(osc-handle server "/1/push12" (fn [msg] (if (= 1.0 (first (:args msg)))
                                          )))
