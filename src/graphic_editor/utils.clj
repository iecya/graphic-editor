(ns graphic-editor.utils
  (:require [clojure.set :as set]))

(defn get-pixel
  [x y img]
  (get img [x y]))

(defn color-pixels
  [pixels color image]
  (into image
        (for [pixel pixels]
          [pixel color])))

(defn get-pixel-siblings
  [[x y :as _pixel] img]
  (for [x' (range (dec x) (+ 2 x))
        y' (range (dec y) (+ 2 y))
        ;; only include siblings that exists in the image and are the same color as the current pixel
        :when (= (get img [x' y'])
                 (get img [x y]))]
    [x' y']))

(defn extend-list
  [pixels img]
  (let [siblings (mapcat #(get-pixel-siblings % img) pixels)]
    (set/union (set pixels) (set siblings))))

(defn fill-area
  "Given a list of pixel and a color, extend the list with the siblings of the pixels. If the extended list is equal to the original list, color all the pixels in the list with the given color, otherwise extend the new list"
  [pixels c img]
  (let [extended-list (extend-list pixels img)]
    (if (= pixels extended-list)
      (color-pixels pixels c img)
      (fill-area extended-list c img))))

(defn err-handler
  "Given an error type, print the related error message and return the image"
  [err-type arg img]
  (println (case err-type
             :invalid-function (str "Sorry, function " arg " does not exist")
             :invalid-args (str "Sorry, function " arg " received a wrong number or type of arguments")
             :invalid-pixel (str "Pixel at coordinates(" (:x arg) ", " (:y arg) ") is not in the image")
             :invalid-coords (str "Sorry, function " arg " received invalid coordinates")))
  img)

