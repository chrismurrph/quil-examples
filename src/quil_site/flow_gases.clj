(ns quil-site.flow-gases
  (:require [quil.middleware :as m]
            [quil.core :as q]))

(defn rand-between [low high]
  (let [diff (- high low)]
    (+ low (rand diff))))

(defn pulse [low high rate]
  (let [diff (- high low)
        half (/ diff 2)
        mid (+ low half)
        s (/ (q/millis) 1000.0)
        x (q/sin (* s (/ 1.0 rate)))]
    (+ mid (* x half))))

(def fps 30)

(defn setup []
  (println "In setup")
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

;; If one every fps is too much we can be random
;; If need more then we won't use conj but concat(?), and return a vector here
(defn create-molecule-ball []
  {:pos [(+ (/ width 2) (rand-between -3 3))
         (+ (/ height 2) (rand-between -3 3))]
   :dir (rand q/TWO-PI)
   :z 1.0
   :col [(rand-between 150 255)
         (rand-between 100 200)
         (rand-between 0 100)]
   :speed 0.1
   :render-fn render-molecule-ball})

;; Calling particles even although max is one
(defn emit-molecule-particles [state]
  (update-in state [:molecule-particles] conj (create-molecule-ball))
  ;(println (str "S/have added a ball, count is " (count (:molecule-particles state))))
  )

(defn move-molecule-ball [molecule-ball]
  (let [[x y] (:pos molecule-ball)
        new-x (+ x 0.033)
        new-y (+ y 0.033)]
    ;(do println (new-x) println (new-y))
    (assoc-in molecule-ball [:pos] [new-x new-y])))

(defn update-state [state]
  (-> state
      (update-in [:elapsed] #(inc %))
      emit-molecule-particles
      (update-in [:molecule-particles] (fn [molecule-particles] (map move-molecule-ball molecule-particles)))))

(defn on-screen? [x y]
  true)

(defn draw-entity [entity]
  (let [[x y] (:pos entity)
        dir (:dir entity)
        ;z (:z entity)
        render-fn (:render-fn entity)
        screen-x x
        screen-y y]
    (when (on-screen? screen-x screen-y)
      (println (str "In draw-entity at " screen-x screen-y) )
      (q/push-matrix)
      (q/translate screen-x screen-y)
      (q/rotate dir)
      (render-fn entity)
      (q/pop-matrix)
      )))

(defn draw-state [state]
  (println "In draw-state")
  (q/background (pulse 20 40 15.0)
                (pulse 40 60 40.0)
                (pulse 50 70  5.0))
  (q/no-stroke)
  (println (str "molecule-particles count " (count (:molecule-particles state))))
  (doseq [molecule (:molecule-particles state)]
    (draw-entity molecule)))

(q/defsketch flow-gases
             :host "canvas"
             :size [500 500]
             :setup setup
             :update update-state
             :draw draw-state
             :middleware [m/fun-mode])
