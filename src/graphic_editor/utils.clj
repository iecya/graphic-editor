(ns graphic-editor.utils
  (:require [graphic-editor.data :refer [image-data]]))

(defn ?pixel
  "Checks if a pixel exists at the given coordinates"
  [x y]
  (some #(when (and (= x (:x %)) (= y (:y %))) %) (:items @image-data)))

(defn sort-items
  []
  (->> (:items @image-data)
       (group-by :y)
       sort
       vals
       (mapv #(sort-by :x %))))

(defn err-handler
  [err-type & [arg]]
  (case err-type
    :invalid-function (str "Sorry, function " arg " does not exist")
    :invalid-args (str "Sorry, function " arg " received a wrong number or type of arguments")
    :invalid-pixel (str "Pixel at coordinates(" (:x arg) ", " (:y arg) ") is not in the image")))

(defn get-row
  [items]
  (apply str (map :color items)))