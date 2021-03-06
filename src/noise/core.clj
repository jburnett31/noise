(ns noise.core
  (:use overtone.live
        overtone.sc.machinery.synthdef))

(definst foo [] (saw 220))
(foo)
(stop)

(definst kick [amp 0.5 decay 0.6 freq 65]
  (* (sin-osc freq (* Math/PI 0.5) amp)
     (env-gen (perc 0 decay) 1 1 0 1 FREE)))
(kick)

(use '(overtone.inst synth drum))

(definst beep [note 60 vol 0.2]
  (let [freq (midicps note)
        src (sin-osc freq)
        env (env-gen (perc 0.3 2) :action FREE)]
    (* vol src env)))

(def ps (atom []))

(defn play-blues [instr pitch-classes]
  (doseq [pitch pitch-classes]
    (swap! ps conj pitch)
    (instr pitch)))

(defn play-seq [count instr notes durs time odds]
  (when (and notes durs)
    (let [dur   (- (/ (first durs) 1.2) 10 (rand-int 20))
          pitch (first notes)
          n-time (+ time dur)]
      (at time
        (when (> (rand) (- 1 odds))
          (tom))

        (when (zero? count)
          (kick)
          (bass (midi->hz (first pitch)) (* 4 (/ dur 1000.0))))

        (when (#{1 3} count)
          (if (> (rand) (- 1 odds))
            (bass (midi->hz (first pitch)) (* 4 (/ dur 1000.0 2)) 0.1))
          (snare))

        (when (= 2 count)
          (kick))

        (play-blues instr pitch))
      (at (+ time (* 0.5 dur))
        (closed-hat 0.1))
      (apply-at n-time #'play-seq
                [(mod (inc count) 4) instr (next notes) (next durs) n-time odds]))))

(def blues-chords
  [:i  :major
   :iv :major
   :i  :major7
   :i  :7
   :iv :major
   :iv :7
   :i  :major
   :i  :major
   :v  :major
   :v  :7
   :i  :major
   :v  :7])

(def bass-line (map first (partition 4 blues-chords)))

(defn progression [chord-seq key-note octave scale]
  (for [[roman-numeral chord-type] (partition 2 chord-seq)]
    (chord (+ (note (str (name key-note) octave))
              (degree->interval roman-numeral scale))
           chord-type)))

(defn blue-beep []
  (play-seq 0 beep
            (cycle (mapcat #(repeat 4 %) (map sort (progression blues-chords :a 3 :ionian))))
            (cycle [1200 1204 1195 1206])
            (now)
            0.2))
(blue-beep)
(stop)

(defn blue-ks1 []
  (play-seq 0 ks1
            (cycle (map sort (progression blues-chords :a 2 :ionian)))
            (take 80 (map #(* 1.5 %) (cycle [530 524 532 528])))
            (now)
            0.5))

(defn blue-ks1-demo []
  (play-seq 0 ks1-demo
            (cycle (map sort (progression blues-chords :a 2 :ionian)))
            (take 80 (map #(* 1.5 %) (cycle [530 524 532 528])))
            (now)
            0.5))
(blue-ks1)
(blue-ks1-demo)
(stop)

(demo 60
      (let [bpm 120
            notes [40 41 28 28 28 27 25 35 78]
            trig (impulse:kr (/ bpm 120))
            freq (midicps (lag (demand trig 0 (dxrand notes INF)) 0.25))
            swr (demand trig 0 (dseq [1 6 6 2 1 2 4 8 3 3] INF))
            sweep (lin-exp (lf-tri swr) -1 1 40 3000)
            wob (apply + (saw (* freq [0.99 1.01])))
            wob (lpf wob sweep)
            wob (* 0.8 (normalizer wob))
            wob (+ wob (bpf wob 1500 2))
            wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

            kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
            kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
            kick (clip2 kick 1)

            snare (* 3 (pink-noise [1 1]) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
            snare (+ snare (bpf (* 4 snare) 2000))
            snare (clip2 snare 1)]

        (clip2 (+ wob kick snare) 1)))
(stop)

(definst snare [note 60 ]
  (let [note (midi->hz note)
        sig (pink-noise [1 1])
        imp ()
        ;sn (* 3 sig imp)
        ;sn (+ sn (bpf (* 4 sn) 2000))
        ;sn (clip2 sn 1)
        ]
    (* imp sig)
    ))

(defsynth dubstep [bpm 120 wobble 1 note 50 snare-vol 1 kick-vol 1 v 1]
  (let [trig (impulse:kr (/ bpm 120))
        freq (midicps note)
        swr (demand trig 0 (dseq [wobble] INF))
        sweep (lin-exp (lf-tri swr) -1 1 40 3000)
        wob (apply + (saw (* freq [0.99 1.01])))
        wob (lpf wob sweep)
        wob (* 0.8 (normalizer wob))
        wob (+ wob (bpf wob 1500 2))
        wob (+ wob (* 0.2 (g-verb wob 9 0.7 0.7)))

        kickenv (decay (t2a (demand (impulse:kr (/ bpm 30)) 0 (dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] INF))) 0.7)
        kick (* (* kickenv 7) (sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
        kick (clip2 kick 1)

        snare (* 3 (pink-noise [1 1]) (apply + (* (decay (impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
        snare (+ snare (bpf (* 4 snare) 2000))
        snare (clip2 snare 1)]

    (out 0    (* v (clip2 (+ wob (* kick-vol kick) (* snare-vol snare)) 1)))))

(dubstep)
(ctl 5 :wobble 8)
(ctl 5 :note 40)
(ctl 5 :bpm 250)
(stop)

(def metro (metronome 128))

(definst c-hat [amp 0.8 t 0.04]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(definst o-hat [amp 0.8 t 0.5]
  (let [env (env-gen (perc 0.001 t) 1 1 0 1 FREE)
        noise (white-noise)
        sqr (* (env-gen (perc 0.01 0.04)) (pulse 880 0.2))
        filt (bpf (+ sqr noise) 9000 0.5)]
    (* amp env filt)))

(defn swinger [beat]
  (at (metro beat) (o-hat 10.0))
  (at (metro (inc beat)) (c-hat 10.0))
  (at (metro (+ 1.65 beat)) (c-hat 10.0))
  (apply-at (metro (+ 2 beat)) #'swinger (+ 2 beat) []))


(swinger (metro))
(stop)

(defn beaty [beat]
  (at (metro beat) (o-hat 10.0))
  (at (metro (inc beat)) (c-hat 10.0))
  (at (metro (+ 1.65 beat)) (c-hat 10.0))
  (at (metro (+ 2 beat)) (o-hat 10.0))
  (at (metro (+ 3 beat)) (kick 10.0))
  (apply-at (metro (+ 4 beat)) #'beaty (+ 4 beat) []))
(beaty (metro))
(stop)

(defn bassy [beat]
  (at (metro beat) (kick 10.0))
  (at (metro (+ 0.15 beat)) (kick 8.0))
  (apply-at (metro (+ 0.3 beat)) #'bassy (+ 0.3 beat) []))
(bassy (metro))
(stop)

(definst saw-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 1.0]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (saw freq)
     vol))
(saw-wave)

(definst square-wave [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.2]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-pulse freq)
     vol))
(square-wave)

(definst noisey [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 1.0]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (white-noise) ; also have (white-noise) and others...
     vol))
(noisey 440 0.05 0.9 0.1 1.0)

(definst triangle-wave [freq 440 attack 0.01 sustain 0.1 release 0.4 vol 1.0]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (lf-tri freq)
     vol))
(triangle-wave 200)

(definst spooky-house [freq 440 width 0.2
                       attack 0.3 sustain 4 release 0.3
                       vol 1.0]
  (* (env-gen (lin-env attack sustain release) 1 1 0 1 FREE)
     (sin-osc (+ freq (* 20 (lf-pulse:kr 0.5 0 width))))
     vol))
(spooky-house)

(definst kick2 []
  (let [src (sin-osc 80)
        env (env-gen (perc 0.001 0.3) :action FREE)]
    (* 0.7 src env)))

(definst overpad [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq (midicps note)
        env (env-gen (perc attack release) :action FREE)
        f-env (+ freq (* 3 freq (env-gen (perc 0.012 (- release 0.1)))))
        bfreq (/ freq 2)
        sig (apply +
                   (concat (* 0.7 (sin-osc [bfreq (* 0.99 bfreq)]))
                           (lpf (saw [freq (* freq 1.01)]) f-env)))
        audio (* amp env sig)]
    audio))
(overpad)

(def stuff [60 65 80 50 70])
(defn prog [beat notes]
  (at (metro beat)
    (overpad (first notes)))
  (apply-at (metro (inc beat)) #'prog (inc beat) (next notes) []))
(prog (metro) stuff)


(defn play-stuff [beat notes attacks]
  (when notes
    (let [note (first notes)
          attack (first attacks)
          amp 0.6
          release 0.5]
      (at beat (overpad note amp attack release))
      (apply-at (metro (inc beat)) #'play-stuff (inc beat) (next notes) (next attacks) []))))
(play-stuff (metro) (cycle stuff) (cycle '(0.1 0.01 0.001)))
(stop)


(definst noise1 [note 60 amp 0.7 attack 0.001 release 2]
  (let [freq (midi->hz note)
        env (env-gen (perc attack release) :action FREE)
        sig (sin-osc freq 0.0)
        audio (* amp env sig)]
    audio))

(defn player [beat notes]
  (let [notes (if (empty? notes)
                [50 55 53 50]
                notes)]
    (at (metro beat)
      (kick2))
    (at (metro beat)
      (if (zero? (mod beat 5))
        (overpad (+ 24 (choose notes)) 0.2 0.75 0.005)))
    (at (metro (+ 0.5 beat))
      (if (zero? (mod beat 6))
        (overpad (+ 12 (choose notes)) 0.5 0.15 0.1)
        (overpad (choose notes) 0.5 0.15 0.1)))
    (apply-at (metro (inc beat)) #'player (inc beat) (next notes) [])))

(player (metro) [])
(stop)

(definst trancy-waves []
  (* 0.2
     (+ (sin-osc 200) (saw 200) (saw 203) (sin-osc 400))))
(trancy-waves)
(kill trancy-waves)





(def server (osc-server 44100 "osc-clj"))
(zero-conf-on)
(osc-listen server (fn [msg] (println msg)) :debug)
(osc-rm-listener server :debug)
(osc-handle server "/1/toggle1" (fn [msg] (if (= 1.0 (first (:args msg)))
                                           (def ds (dubstep 120 1 50 1 1 3))
                                           (kill ds))))
(osc-handle server "/1/fader1" (fn [msg] (let [bpm (first (:args msg))]
                                          (ctl ds :bpm (+ 50 (* bpm 200))))))
(osc-handle server "/1/fader2" (fn [msg] (let [wob (first (:args msg))]
                                          (ctl ds :wobble (* wob 8)))))
(osc-handle server "/1/fader3" (fn [msg] (let [note (first (:args msg))]
                                          (ctl ds :note (+ 20 (* 100 note))))))
(osc-handle server "/1/toggle2" (fn [msg] (if (= 1.0 (first (:args msg)))
                                           (def sw (swinger (metro)))
                                           (stop))))
(osc-handle server "/1/push1" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (spooky-house 440 0.2 0.3 4 0.3 3.0))))
(osc-handle server "/1/push2" (fn [msg] (if (= 1.0 (first (:args msg)))
                                         (kick))))
(ctl ds :v 3)
(stop)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))
