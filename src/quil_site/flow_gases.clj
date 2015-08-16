(ns quil-site.flow-gases
  (:require [quil.middleware :as m]
            [quil.core :as q]))

(defn rand-between [low high]
  (let [diff (- high low)]
    (+ low (rand diff))))

(def fps 30)

(defn setup []
  (q/rect-mode :radius)
  (q/frame-rate fps)
  {:molecules []
   :elapsed 0})

(defn render-molecule [molecule]
  (let [size 10
        [r g b] (:col molecule)]
    (q/fill r g b 200)
    (q/ellipse 0 0 size size)))

(def height 500)
(def width 500)

;; If one every fps is too much we can be random
;; If need more then we won't use conj but concat(?), and return a vector here
(defn create-molecule []
  {:pos [(+ (/ height 2) (rand-between -3 3))
         (+ (/ width 2) (rand-between -3 3))]
   :dir (rand q/TWO-PI)
   :z 1.0
   :col [(rand-between 150 255)
         (rand-between 100 200)
         (rand-between 0 100)]
   :speed 0.1
   :render-fn render-molecule})

(defn move-molecule [molecule]
  (update-in molecule [:pos] #(+ % 0.033)))

(defn update-state [state]
  (-> state
      (update-in [:elapsed] #(inc %))
      (update-in [:molecules] conj (create-molecule))
      (update-in [:molecules] (fn [molecules] (map move-molecule molecules)))))

(q/defsketch flow-gases
             :host "canvas"
             :size [500 500]
             :setup setup
             :update update-state
             :draw draw-state
             :middleware [m/fun-mode])
