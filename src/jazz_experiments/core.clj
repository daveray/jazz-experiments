(ns jazz-experiments.core
  (:use overtone.core
        [overtone.inst synth drum]
        clojure.repl))

; ok, here are some quick demos inspired by the current thread on the Overtone list...
(def diatonic-trichords
  {:i   :major
   :ii  :minor
   :iii :minor
   :iv  :major
   :v   :major
   :vi  :minor
   :vii :diminished})

(def diatonic-tetrachords
  {:i   :major7
   :ii  :minor7
   :iii :minor7
   :iv  :major7
   :v   :dom7
   :vi  :m7-5
   :vii :dim7 })

; this is a hack...
(def six-tetrachords
  {:i   :6
   :ii  :6
   :iii :6
   :iv  :6
   :v   :6
   :vi  :6
   :vii :6})

(def jazz-progressions
  [[:ii :v]
   [:ii :v :i]
   [:i :vi :ii :v]
   [:i :vi :ii :v :iii :vi :ii :v]])

(def pop-progressions
  [[:i :ii :v]
   [:i :iv :v :v]
   [:i :i :iv :v]
   [:i :iv :i :v]
   [:i :v :vi :iv]
   [:i :iv :v :iv]])


(def blues-progressions
  [[:i :i :i :i :iv :iv :i :i :v :iv :i :i]])

(defn chord-progression
  "Map a chord progression represnted by a seq of intervals
  into a seq of maps, using a specific chord family.

  (chord-progression diatonic-trichords [:i :iv :v]) ;=> [{:degree :i, :chord-type :major}, ...]
  "
  [in-key scale chord-family progression]
  (map (fn [degree]
         (let [chord-type (get chord-family degree)
               chord-root (+ (note in-key) (degree->interval degree scale))]
           {:degree degree
            :chord-type chord-type
            :notes (chord chord-root chord-type)}))
       progression))

(defn expand-by
  "Expand elements of a seq.

  (expand-by 2 [:a :b]) ;=> (:a :a :b :b)
  "
  [n elems]
  (mapcat #(repeat n %) elems))

(defn play-notes
  "Play a set of notes using the given instrument function.

    (play-notes piano #{40 44 47})
  "
  ([inst-fn notes vel]
   (play-notes (now) inst-fn notes))
  ([t inst-fn notes vel]
   (at t (doall (map #(inst-fn % vel) notes)))))

;(alias play play-notes)

(defn progression-player
  "Play a progression using block chords (all notes at same time)."
  [style-fn t len inst chords vel speed]
  (when chords
    (let [next-t (+ t len)]
      (doseq [[note-t note note-vel] (style-fn (:notes (first chords)) len vel speed)]
        (at (+ t note-t) (inst note note-vel)))
      (apply-at next-t #'progression-player [style-fn next-t len inst (next chords) vel speed]))))

(defn offset 
  "Given a schedule (notes at certain times) adjust their times by off."
  [off]
  (fn [[t & more]]
    (cons (+ t off) more)))

(defn comp-style 
  "Compose several styles together into a single style."
  [& styles]
  (fn [notes len vel speed]
    (apply concat ((apply juxt styles) notes len vel speed))))

(defn block-style 
  "Play all notes in each chord simultaneously"
  [notes len vel speed]
  (for [n (cons (- (first notes) 12) notes)]
    [0 n vel]))

; Define a player that uses block style
(def block-chord-player (partial progression-player block-style))

(defn bass-note-style 
  "Play the first note in each chord adjusted by the given number
  of octaves down."
  [octaves]
  (fn [notes len vel speed]
    [[0 (- (first notes) (* octaves 12)) (- vel 15)]]))

(defn arpeggiated-style 
  "Arpeggiate each chord"
  [notes len vel speed]
  (for [[i n] (map-indexed vector (sort notes))]
    [(* i speed) n vel]))

; Define a player that plays a bass note and arpeggiates the chord
(def arp-player 
  (partial progression-player 
           (comp-style (bass-note-style 2) arpeggiated-style)))


(defn broken-style
  "Jeff's 'broken' style. Doesn't include base notes."
  [notes len vel speed]
  (let [broken [(nth notes 2) (first notes) (second notes) (first notes)]
        broken-notes (concat broken broken)
        synco (* 3.5 speed)]
    ; broken or rocked
    (if (zero? (rand-int 2))
      (map (offset synco) 
            (arpeggiated-style (choose [notes broken-notes]) len vel speed))
      (concat 
        (block-style (next notes) len (- vel 20) speed)
        [[(+ synco (* 1 speed)) (first notes) vel]]
        (map (offset (+ synco (* 2 speed))) 
              (block-style (next notes) len (- vel 20) speed ))
        [[(+ synco (* 3 speed)) (first notes) vel]]))))

; Jeff's broken player. Compose broken style with bass notes.
(def broken-chord-player 
  (partial progression-player 
           (comp-style (bass-note-style 1) broken-style)))

; left hand arpeggiates the chord [:i :iii :v :vi] 2 octaves below middle C
; from low to hi while the right hand plays straight chord on the up beats.
(defn shuffle-player
  "Play a left and right hand chord progressions using a blues shuffle style.
  (bass notes arpeggiated, and right hand as block chords)"
  [t len inst treble-chords bass-chords vel]

  ; TODO This isn't very satisfying

  ; One player plays the arpeggiated bass notes  
  (progression-player 
    (fn [notes len vel speed]
      (arpeggiated-style notes len vel (* 0.25 len)))
      t len inst bass-chords vel 0)
  
  ; Another plays the right hand on off beats. 
  (progression-player 
    (fn [notes len vel speed]
      (apply concat (for [i (range 4)]
        (map (offset (+ (* i 0.25 len) (* 0.125 len)))
            (block-style notes len vel speed)))))
      t len inst treble-chords (- vel 10) 0))

(definst z
  [note 60 amp 0.8 len 0.4
   attack 0.05 decay 0.2 sustain 0.7 release 0.2
   fattack 0.12 fdecay 0.1 fsustain 0.9 frelease 0.02]
  (let [gate (env-gen (perc 0 len) 1 :action FREE)
        freq (midicps note)
        osc1 (saw [freq (* 1.1 freq)])
        osc2 (bpf (white-noise) freq 0.1)
        oscs (+ osc1 [osc2 osc2])
        amp-env (env-gen (adsr attack decay sustain release) gate :action FREE)
        amped (* 5 oscs)
        dist (distort amped)
        enved (* dist amp-env)
        f-env (env-gen (adsr fattack fdecay fsustain frelease) gate :action FREE)
        verb (free-verb (first enved) (second enved) :mix 0.5 :room 0.8 :damp 0.9)
        filt (lpf verb (* 20 freq f-env))]
    (* amp filt)))

(definst ding [note 60 amp 0.4 a 0.2 b 0.2]
  (let [snd (sin-osc (midicps note))
        env (env-gen (perc a b) :action FREE)]
    (* env snd amp)))

; experiment with evaling different p functions to switch between instruments
(defn p
  [note vel]
  (ding note (/ vel 120.0) 0.05 0.4))

; Be sure to move the mouse around when this one is playing...
(defn p
  [note vel]
  (ks1-demo note (* 1.2 (/ vel 120.0))))

(defn p
  [note vel]
  (z note (/ vel 120.0)))

; uncomment and try this one if you have the mdapiano plugin setup
(comment
(use 'overtone.inst.piano)
(defn p
  [note vel]
  (piano note 1 (+ vel (rand-int 10)) 0.3 0.1 0.5))
)

; modify bar-ms and re-eval these two to change the tempo in these examples
(def bar-ms 1200)
(def strum-ms (/ bar-ms 4))

(def bass-chords
  (-> (chord-progression :g1 :diatonic six-tetrachords (nth blues-progressions 0))
    (update-all :chord-type (fn [_] :6))))

(def treble-chords
       (-> (chord-progression :g3 :diatonic diatonic-trichords (nth blues-progressions 0))
         (update-all :notes #(invert-chord % 2))))

(block-chord-player (+ 100 (now)) bar-ms #'p
              (chord-progression :Bb3 :diatonic diatonic-trichords
                                 (expand-by 2 (nth jazz-progressions 0)))
              60 0)
(arp-player (+ 100 (now)) bar-ms #'p
            (chord-progression :e3 :diatonic diatonic-trichords
                               (expand-by 2 (nth jazz-progressions 2)))
            60 strum-ms)
(broken-chord-player (+ 100 (now)) bar-ms #'p
              (chord-progression :Bb3 :diatonic diatonic-trichords
                                 (expand-by 2 (nth jazz-progressions 0)))
              60 strum-ms)
(shuffle-player (+ 100 (now)) 2200 #'p
                treble-chords bass-chords
                60)
(comment
; best played with the piano (otherwise it sounds super cheesy)
(shuffle-player (+ 100 (now)) 2200 #'p
                treble-chords bass-chords
                60)

; Eval each of these to hear various progressions played in different ways

(block-chord-player (+ 100 (now)) bar-ms #'p
              (chord-progression :Bb3 :diatonic diatonic-trichords
                                 (expand-by 2 (nth jazz-progressions 0)))
              60 0)

(broken-chord-player (+ 100 (now)) bar-ms #'p
              (chord-progression :Bb3 :diatonic diatonic-trichords
                                 (expand-by 2 (nth jazz-progressions 0)))
              60 strum-ms)

(block-chord-player (+ 100 (now)) bar-ms #'p
              (chord-progression :a4 :diatonic diatonic-tetrachords
                                 (nth blues-progressions 0))
              60 0)

(arp-player (+ 100 (now)) bar-ms #'p
            (chord-progression :e3 :diatonic diatonic-trichords
                               (expand-by 2 (nth jazz-progressions 2)))
            60 strum-ms)

(broken-chord-player (+ 100 (now)) (* 1.5 bar-ms) #'p
              (chord-progression :e3 :diatonic diatonic-trichords
                                 (expand-by 2 (nth jazz-progressions 2)))
              60 (* 1.5 strum-ms))

; and with some random inversions
(broken-chord-player (+ 100 (now)) (* 1.5 bar-ms) #'p
              (-> (chord-progression :e3 :diatonic diatonic-trichords
                                 (expand-by 2 (nth jazz-progressions 2)))
                (update-all :notes #(invert-chord % (- (rand-int 2) 1))))
              60 (* 1.5 strum-ms))

; with jazz chords and wider range of inversions
(broken-chord-player (+ 100 (now)) (* 1.5 bar-ms) #'p
              (-> (chord-progression :e3 :diatonic diatonic-tetrachords
                                 (expand-by 2 (nth jazz-progressions 2)))
                (update-all :notes #(invert-chord % (- (rand-int 4) 2))))
              60 (* 1.5 strum-ms))

(arp-player (+ 100 (now)) bar-ms #'p
            (-> (chord-progression :e4 :diatonic diatonic-tetrachords
                                   (expand-by 2 (nth jazz-progressions 2)))
              (update-every-n 2 1 :notes #(invert-chord % 1)))
            60 strum-ms)


(arp-player (+ 100 (now)) bar-ms #'p
            (-> (chord-progression :e3 :diatonic diatonic-tetrachords
                                   (expand-by 2 (nth jazz-progressions 3)))
              (update-every-n 2 1 :notes #(invert-chord % 1)))
            60 strum-ms)

; randomized chord inversions
(arp-player (+ 100 (now)) bar-ms #'p
            (-> (chord-progression :f4 :diatonic diatonic-tetrachords
                                   (expand-by 2 (nth jazz-progressions 3)))
              (update-all :notes #(invert-chord % (- (rand-int 4) 2))))
            60 strum-ms)

; starts with the base chord and moves up through the inversions
(arp-player (+ 100 (now)) bar-ms #'p
            (-> (chord-progression :e3 :diatonic diatonic-tetrachords
                                   (expand-by 4 (nth jazz-progressions 3)))
              (update-every-n 4 1 :notes #(invert-chord % 1))
              (update-every-n 4 2 :notes #(invert-chord % 2))
              (update-every-n 4 3 :notes #(invert-chord % 3)))
            60 strum-ms)

; define left and right hand chords, with left hand playing 6th chords and right
; hand playing the second inversion of 7th chords


)
