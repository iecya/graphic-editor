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
  [f input img]
  (case f
    "I" (let [args (s/split input #"\s")]
          (when (every? #(re-find #"\d+" %) args)
            (->> args
                 (mapv #(Integer. %))
                 (every? #(<= 1 % 250)))))
    "C" (nil? input)
    "L" (single-px-coords input img)
    "V" (validate-v-segment input img)
    "H" (validate-h-segment input img)
    "F" (single-px-coords input img)
    "S" (nil? input)
    nil))


(defn validate-coords
  [f coords img]
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
    nil))
