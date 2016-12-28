(ns graphic-editor.validation
  (:require [clojure.string :as s]))


(defn validate-v-segment
  [args img]
  (let [coords (butlast args)
        y-coords (rest coords)
        x-coords (first coords)]
      (and (every? integer? coords)
           (re-find #"[A-Z]" (str (last args)))
           (every? #(<= 1 % (:rows img)) y-coords)
           (<= 1 x-coords (:cols img)))))


(defn validate-h-segment
  [args img]
  (let [coords   (butlast args)
        y-coords (last coords)
        x-coords (butlast coords)]
    (and (every? integer? coords)
         (re-find #"[A-Z]" (str (last args)))
         (every? #(<= 1 % (:cols img)) x-coords)
         (<= 1 y-coords (:rows img)))))


(defn single-px-coords
  [args img]
  (let [coords (butlast args)]
    (and (every? integer? coords)
         (re-find #"[A-Z]" (str (last args)))
         (<= 1 (first coords) (:cols img))
         (<= 1 (last coords) (:rows img)))))


(defn validate-args
  [f args img]
  (case f
    "I" (every? #(and (integer? %)
                      (<= 1 % 250)) args)
    "C" (empty? args)
    "L" (single-px-coords args img)
    "V" (validate-v-segment args img)
    "H" (validate-h-segment args img)
    "F" (single-px-coords args img)
    "S" (empty? args)
    nil))


(defn validate-coords
  [f coords img]
  (when (every? integer? coords)
    (case f
      "V" (and (pos? (first coords))
               (<= (first coords) (:cols img))
               (pos? (second coords))
               (apply < (rest coords))
               (<= (last coords) (:rows img)))
      "H" (and (pos? (first coords))
               (< (first coords) (second coords))
               (<= (second coords) (:cols img))
               (pos? (last coords))
               (<= (last coords) (:rows img)))
      nil)))
