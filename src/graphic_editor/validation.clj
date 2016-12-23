(ns graphic-editor.validation
  (:require [clojure.string :as s]))


(defn validate-v-segment
  [input img]
  (let [args     (s/split input #"\s")
        coords   (butlast args)
        y-coords (rest coords)
        x-coords (first coords)]
    (when (and (every? #(re-find #"\d+" %) coords)
               (re-find #"[A-Z]" (last args)))
      (and (->> y-coords
                (mapv #(Integer. %))
                (every? #(<= 1 % (:rows img))))
           (<= 1 (Integer. x-coords) (:cols img))))))


(defn validate-h-segment
  [input img]
  (let [args     (s/split input #"\s")
        coords   (butlast args)
        y-coords (last coords)
        x-coords (butlast coords)]
    (when (and (every? #(re-find #"\d+" %) coords)
               (re-find #"[A-Z]" (last args)))
      (and (->> x-coords
                (mapv #(Integer. %))
                (every? #(<= 1 % (:cols img))))
           (<= 1 (Integer. y-coords) (:rows img))))))


(defn single-px-coords
  [input img]
  (let [args     (s/split input #"\s")
        coords   (butlast args)
        n-coords (mapv #(Integer. %) coords)]
    (when (and (every? #(re-find #"\d+" %) coords)
               (re-find #"[A-Z]" (last args)))
      (and (<= 1 (first n-coords) (:cols img))
           (<= 1 (last n-coords) (:rows img))))))


(defn validate-args
  [f input]
  (case f
    "I" (let [foo (s/split input #"\s")]
          (when (every? #(re-find #"\d+" %) foo)
            (->> foo
                 (mapv #(Integer. %))
                 (every? #(<= 1 % 250)))))
    "C" (nil? input)
    "L" (re-find #"\d\s\d\s[A-Z]" input)
    "V" (re-find segment-regex input)
    "H" (re-find segment-regex input)
    "F" (re-find #"\d\s\d\s[A-Z]" input)
    "S" (nil? input)
    nil))


(defn validate-coords
  [f & [args]]
  (case f
    "V" (and (pos? (first args))
             (<= (first args) (:cols @image-data))
             (pos? (second args))
             (apply < (rest args))
             (<= (last args) (:rows @image-data)))
    "H" (and (pos? (first args))
             (< (first args) (second args))
             (<= (second args) (:cols @image-data))
             (pos? (last args))
             (<= (last args) (:rows @image-data)))
    nil))
