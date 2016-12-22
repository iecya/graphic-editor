(ns graphic-editor.api
  (:require [clojure.string :as s]
            [graphic-editor.data :refer [image-data]]
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
  []
  (let [sorted-items (u/sort-items)]
    (->> sorted-items
         (mapv #(u/get-row %))
         (clojure.string/join "\n"))))


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





