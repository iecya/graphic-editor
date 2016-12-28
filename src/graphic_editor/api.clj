(ns graphic-editor.api
  (:require [clojure.string :as s]
            [graphic-editor.utils :as u]
            [graphic-editor.validation :as v]))


(defn I
  "Given a width and a height, creates a new image with the given sizes"
  [w h img]
  (assoc img :cols w
             :rows h
             :items (into #{} (for [col (range w)
                                    row (range h)]
                                {:x (inc col) :y (inc row) :color "O"}))))


(defn C
  "Clears the table, setting all pixels color to O"
  [img]
  (assoc img :items (into #{} (for [pixel (:items img)]
                                (assoc pixel :color "O")))))


(defn L
  "Given the coordinate of a pixel and a color, color the given pixel with the given color"
  [x y c img]
  (if-let [pixel (u/?pixel x y img)]
    (update img :items #(-> %
                            (disj pixel)
                            (conj (assoc pixel :color (str c)))))
    (u/err-handler :invalid-pixel {:x x :y y} img)))


(defn V
  "Given the x coordinate and the y coordinates of the first and last pixel, draws a vertical segment of the given color"
  [x' ys ye c img]
  (if-let [_ (v/validate-coords "V" [x' ys ye] img)]
    (update img :items (u/v-segment x' ys ye c))
    (u/err-handler :invalid-args "V" img)))


(defn H
  "Given the y coordinate and the x coordinates of the first and last pixel, draws an horizontal segment of the given color"
  [xs xe y' c img]
  (if-let [_ (v/validate-coords "H" [xs xe y'] img)]
    (update img :items (u/h-segment xs xe y' c))
    (u/err-handler :invalid-args "H" img)))


(defn S
  "Display the current image state"
  [img]
  (let [sorted-items (u/sort-items img)]
    (println (->> sorted-items
                  (mapv #(u/get-row %))
                  (clojure.string/join "\n")))
    img))


(defn F
  [x y c img]
  (u/fill (list (u/?pixel x y img)) c img))


(defn input->function
  "Takes the user input and calls the related function"
  [s img]
  (when-not (empty? s)
    (let [args-str    (s/split s #"\s")
          f-name      (first args-str)
          args        (mapv read-string (rest args-str))
          valid-args? (v/validate-args f-name args)
          apply-fn    (fn [f] (if valid-args?
                                (apply f (conj args img))
                                (u/err-handler :invalid-args f-name img)))]
      (case f-name
        "I" (apply-fn I)
        "C" (apply-fn C)
        "L" (apply-fn L)
        "V" (apply-fn V)
        "H" (apply-fn H)
        "F" (apply-fn F)
        "S" (apply-fn S)
        (u/err-handler :invalid-function f-name img)))))





