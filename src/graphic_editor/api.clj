(ns graphic-editor.api
  (:require [clojure.string :as s]
            [graphic-editor.matrix :as m]
            [graphic-editor.utils :as u]
            [graphic-editor.validation :as v]))


(defn I
  "Given a width and a height, creates a new image with the given sizes"
  [w h]
  (swap! image-data assoc :cols w :rows h)
  (swap! image-data assoc :items (into #{} (for [col (range w)
                                                 row (range h)]
                                             {:x (inc col) :y (inc row) :color "O"}))))


(defn C
  "Clears the table, setting all pixels color to O"
  []
  (let [items (into #{} (for [pixel (:items @image-data)]
                          (assoc pixel :color "O")))]
    (swap! image-data assoc :items items)))


(defn L
  "Given the coordinate of a pixel and a color, color the given pixel with the given color"
  [x y c]
  (if-let [pixel (u/?pixel x y)]
    (do (swap! image-data update :items disj pixel)
        (swap! image-data update :items conj (assoc pixel :color c)))
    (u/err-handler :invalid-pixel {:x x :y y})))


(defn V
  "Given the x coordinate and the y coordinates of the first and last pixel, draws a vertical segment of the given color"
  [x ys ye c]
  (if-let [_ (v/validate-coords "V" [x ys ye])]
    (doseq [row (range ys (inc ye))]
      (L x row c))
    (u/err-handler :invalid-args "V")))


(defn H
  "Given the y coordinate and the x coordinates of the first and last pixel, draws an horizontal segment of the given color"
  [xs xe y c]
  (if-let [_ (v/validate-coords "H" [xs xe y])]
    (doseq [col (range xs (inc xe))]
      (L col y c))
    (u/err-handler :invalid-args "H")))


(defn S
  "Display the current image state"
  []
  (let [sorted-items (u/sort-items)]
    (println (->> sorted-items
                  (mapv #(u/get-row %))
                  (clojure.string/join "\n")))))


(defn F
  "Fill a region, starting from the pixel at the given coords colors the pixel and all the neighbour pixels of the same color. Repeats for all the pixels in the region"
  [x y c]
  (let [M  (atom (m/create-matrix (:cols @image-data) (:rows @image-data)))
        c0 (:color (u/?pixel x y))]
    (letfn [(f-recur [x y c]
              (reset! M (m/change @M x y true))
              (L x y c)
              (let [dx (dec x)
                    ix (inc x)
                    dy (dec y)
                    iy (inc y)]
                (when (and (pos? dx) (not (m/get-value @M dx y)) (= (u/?pixel-color dx y) c0))
                  (f-recur dx y c))
                (when (and (<= ix (:cols @image-data)) (not (m/get-value @M ix y)) (= (u/?pixel-color ix y) c0))
                  (f-recur ix y c))
                (when (and (pos? dy) (not (m/get-value @M x dy)) (= (u/?pixel-color x dy) c0))
                  (f-recur x dy c))
                (when (and (<= iy (:rows @image-data)) (not (m/get-value @M x iy)) (= (u/?pixel-color x iy) c0))
                  (f-recur x iy c))))]
      (f-recur x y c))))


(defn input->function
  "Takes the user input and calls the related function"
  [s]
  (when-not (empty? s)
    (let [args-str    (s/split s #"\s")
          f-name      (first args-str)
          args        (mapv read-string (rest args-str))
          valid-args? (v/validate-args f-name (when (< 1 (count (s/trim s))) (subs s 2)))
          apply-fn    (fn [f] (if valid-args?
                                (apply f args)
                                (u/err-handler :invalid-args f-name)))]
      (case f-name
        "I" (apply-fn I)
        "C" (apply-fn C)
        "L" (apply-fn L)
        "V" (apply-fn V)
        "H" (apply-fn H)
        "F" (apply-fn F)
        "S" (apply-fn S)
        (u/err-handler :invalid-function f-name)))))





