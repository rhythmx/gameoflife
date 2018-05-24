(ns gameoflife.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(defn rand-row [nrows]
  (vec (take nrows (repeatedly #(rand-int 2)))))

(defn rand-board [ncols nrows]
  (vec (take ncols (repeatedly #(rand-row nrows)))))

(defn setup []
  (let [cols 32
        rows 32]
    
    ;; Set frame rate to 30 frames per second.
    (q/frame-rate 30)

    ;; Set color mode to HSB (HSV) instead of default RGB.
    (q/color-mode :hsb)
    
    ;; setup function returns initial state object
    {:cols cols
     :rows rows
     :board (rand-board cols rows)}))

(defn x-overflow? [state x]
  (or (>= x (:cols state)) (< x 0)))

(defn y-overflow? [state y]
  (or (>= y (:rows state)) (< y 0)))
  
(defn board-pos-val [state x y]
  (if (not (or (x-overflow? state x) (y-overflow? state y)))
    (get (get (:board state) x) y)
    0))

(defn neighbor-count [state x y]
  (+
   (board-pos-val state (- x 1) (- y 1))
   (board-pos-val state x       (- y 1))
   (board-pos-val state (+ x 1) (- y 1))
   (board-pos-val state (- x 1) y)
   (board-pos-val state (+ x 1) y)
   (board-pos-val state (- x 1) (+ y 1))
   (board-pos-val state x       (+ y 1))
   (board-pos-val state (+ x 1) (+ y 1))))

(defn update-cell [state x y]
  (let [n (neighbor-count state x y)]
    (if (> (board-pos-val state x y) 0.5)
      ;; live cell
      (if (or (< n 2) (> n 3)) 0 1)
      ;; dead cell
      (if (= n 3) 1 0))))

(defn update-board [state]
  (vec (map-indexed
        (fn [x col]
          (vec
           (map-indexed (fn [y val] (update-cell state x y)) col)
           ))
        (:board state))))


(defn update-state [state]
  ;; Update sketch state by changing circle color and position.
  {:cols (:cols state)
   :rows (:rows state)
   :board (if false (rand-board (:cols state) (:rows state)) (update-board state))
   })


(defn draw-cell [state x y]
  (when (> (board-pos-val state x y) 0.5)
    (let [xspacing (/ (q/width) (:cols state))
          x1 (* xspacing x)
          yspacing (/ (q/height) (:rows state))
          y1 (* yspacing y)]
      (q/rect x1 y1 xspacing yspacing))))

(defn draw-board [state]
  (doseq [x (range (:cols state))]
    (doseq [y (range (:rows state))]
      (draw-cell state x y))))

(defn draw-grid [rows cols]
  ;; Print column dividers
  (let [nlines (- cols 1)
        spacing (/ (q/width) cols)]
    (doseq [x (range nlines)]
      (let [xpos (* spacing (+ x 1))] 
        (q/line xpos 0 xpos (q/height)))))
  ;; print row dividers
  (let [nlines (- rows 1)
        spacing (/ (q/height) rows)]
    (doseq [y (range nlines)]
      (let [ypos (* spacing (+ y 1))] 
        (q/line 0 ypos (q/width) ypos)))))

(defn draw-border []
  (q/line 0 0 (- (q/width) 1) 0)
  (q/line 0 (- (q/height) 1) (- (q/width) 1) (- (q/height) 1))
  (q/line 0 0 0 (- (q/height) 1))
  (q/line (q/width) 0 (- (q/width) 1) (- (q/height) 1)))  

(defn draw-state [state]  
  ;; Set colors
  (q/background 255)
  (q/stroke 90 255 0)
  (q/fill 255 255 0)
  
  (draw-border)
  (draw-grid (:cols state) (:rows state))
  (draw-board state)
  )

(defn -main [& args]
  (q/defsketch gameoflife
    :title "You spin my circle right round"
    :size [500 500]
    ; setup function called only once, during sketch initialization.
    :setup setup
    ; update-state is called on each iteration before draw-state.
    :update update-state
    :draw draw-state
    :features [:keep-on-top]
    ; This sketch uses functional-mode middleware.
    ; Check quil wiki for more info about middlewares and particularly
    ; fun-mode.
    :middleware [m/fun-mode]))
