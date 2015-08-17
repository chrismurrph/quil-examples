(ns quil-site.flow-gases
  (:require [quil.middleware :as m]
            [quil.core :as q]
            [quil-site.maths-utils :as u]))

(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(def fps 30)

(defn setup []
  ;(println "In setup")
  (q/rect-mode :radius)
  (q/frame-rate fps)
  {:molecule-particles []
   :elapsed 0})

(defn render-molecule-ball [molecule-ball]
  (let [size 10
        [r g b] (:col molecule-ball)]
    (q/fill r g b 200)
    (q/ellipse 0 0 size size)))

(def width 500)
(def height 500)
(def centre [(/ width 2) (/ height 2)])

(defn x-val [vec] (first vec))
(defn y-val [vec] (second vec))

(defn random-colour
  []
  [(u/random-float 150 255) (u/random-float 100 200) (u/random-float 0 100)])

;; Future enhancement is for right at start there to be much bigger hatchery area and many more created
;; This way user won't be distracted by seeing them spread to the outside
(def hatchery-size 15)

;; If one every fps is too much we can be random
;; If need more then we won't use conj but concat(?), and return a vector here
(defn create-molecule-ball []
  (let [x-random (u/random-float (- hatchery-size) hatchery-size)
        y-random (u/random-float (- hatchery-size) hatchery-size)
        dir (u/calc-direction [x-random y-random])
        pos [(+ (x-val centre) x-random)
             (+ (y-val centre) y-random)]]
    {:pos pos
     :dir dir
     :z 1.0
     :col (random-colour)
     :speed 0.1
     :render-fn render-molecule-ball}))

;; Named 'particles' even although currently one-only created
(defn emit-molecule-particles [state]
  (update-in state [:molecule-particles] conj (create-molecule-ball))
  ;(println (str "S/have added a ball, count is " (count (:molecule-particles state))))
  )

(defn move-molecule-ball [molecule-ball]
  (let [[x y] (:pos molecule-ball)
        dir (:dir molecule-ball)
        [delta-x delta-y] (u/radians->vector dir 0.33)
        [new-x new-y] (u/translate-v2 [x y] [delta-x delta-y])]
    ;(do println (new-x) println (new-y))
    (assoc-in molecule-ball [:pos] [new-x new-y])))

(defn update-state [state]
  (-> state
      (update-in [:elapsed] #(inc %))
      emit-molecule-particles
      (update-in [:molecule-particles] (fn [molecule-particles] (map move-molecule-ball molecule-particles)))))

(defn on-screen? [x y]
  (let [margin -10
        res (and (<= (- margin) x (+ margin (q/width)))
                 (<= (- margin) y (+ margin (q/height))))]
    ;(when-not res (println "Going off"))
    res))

(defn draw-entity [entity]
  (let [[x y] (:pos entity)
        dir (:dir entity)
        ;z (:z entity)
        render-fn (:render-fn entity)
        screen-x x
        screen-y y]
    (when (on-screen? screen-x screen-y)
      (q/push-matrix)
      (q/translate screen-x screen-y)
      (q/rotate dir)
      (render-fn entity)
      (q/pop-matrix)
      )))

(defn draw-state [state]
  ;(println "In draw-state")
  (q/background (pulse 20 40 15.0)
                (pulse 40 60 40.0)
                (pulse 50 70  5.0))
  (q/no-stroke)
  ;(println (str "molecule-particles count " (count (:molecule-particles state))))
  (doseq [molecule (:molecule-particles state)]
    (draw-entity molecule)))

(q/defsketch flow-gases
             :host "canvas"
             :size [500 500]
             :setup setup
             :update update-state
             :draw draw-state
             :middleware [m/fun-mode])
