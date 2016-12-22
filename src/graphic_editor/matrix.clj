(ns graphic-editor.matrix
  (:require [graphic-editor.data :refer [image-data]]
            [graphic-editor.utils :as u]))


(defn create-matrix [w h]
  (vec (repeat w (vec (repeat h false)))))


(defn change [M x y v]
  (let [row     (nth M (dec x))
        new-row (assoc row (dec y) v)]
    (assoc M (dec x) new-row)))


(defn get-value [M x y]
  (let [row (nth M (dec x))]
    (nth row (dec y))))
