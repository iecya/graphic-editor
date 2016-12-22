(ns graphic-editor.validation
  (:require [schema.core :as sc]
            [graphic-editor.data :refer [image-data]]))

(def max-size 250)
(def min-size 1)


(def segment-regex #"\d\s\d\s\d\s[A-Z]")

(defn validate-f
  [f]
  (#{"I" "C" "L" "V" "H" "S"} f))


(defn validate-args
  [f input]
  (prn "f =" f)
  (prn "args =" input)
  (case f
    "I" (re-find #"[1-9]\s[1-9]" input)
    "C" (nil? input)
    "L" (re-find #"\d\s\d\s[A-Z]" input)
    "V" (re-find segment-regex input)
    "H" (re-find segment-regex input)
    "S" (nil? input)
    nil))


(defn validate-coords
  [f & [args]]
  (case f
    "V" (and (pos? (first args))
             (< (first args) (:cols @image-data))
             (pos? (second args))
             (< (second args) (:rows @image-data))
             (< (second args) (last args) (:rows @image-data)))
    "H" (and (pos? (first args))
             (< (first args) (:cols @image-data))
             (pos? (second args))
             (< (second args) (:rows @image-data))
             (< (second args) (last args) (:rows @image-data)))
    nil))






(def pixel-schema
  "Schema for a single pixel of the image"
  {:x     (sc/both sc/Num
                   (sc/pred #(<= % (:cols @image-data))))
   :y     (sc/both sc/Num
                   (sc/pred #(<= % (:rows @image-data))))
   :color sc/Str})


(def image-data-schema
  "Schema for the image data"
  {:cols  (sc/both sc/Num
                   (sc/pred #(pos? %)))
   :rows  (sc/both sc/Num
                   (sc/pred #(<= min-size % max-size)))
   :items #{pixel-schema}})
