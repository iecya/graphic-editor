(ns graphic-editor.api
  (:require [clojure.string :as s]
            [graphic-editor.utils :as u]
            [graphic-editor.validation :as v]
            [clojure.string :as string]))

;; THIS IS NOT AN API NAMESPACE (I think)


;; THE FUNCTION NAMES ARE HORRIBLE!


;; The image object could be a map of coords as keys and color as value
;; i.e. {[x y] c}
;; This way I could have a single function that reduces through the key-value pairs
;; and applies the reducing function (passed in as argument)

(defn new-image
  [width height _img]
  (into {}
        (for [x (range width)
              y (range height)]
          [[x y] 0])))

(defn clear-image
  [image]
  ;; my first approach here was to reduce to the existing key-value pairs of the image
  ;; and assoc every key in a new map (accumulator) assigning the default color 0
  ;; I then thought that maybe this is a better and cleaner approach (not sure about performance)
  (let [[width height] (-> image (keys) (sort) last)]
    (new-image (inc width) (inc height) nil)))

(defn color-pixel
  [x y c img]
  {:pre [(and (every? integer? [x y c])
              (map? img))]}
  (assoc img [x y] c))

(defn vertical-segment
  [x ys ye c img]
  {:pre [(true? (v/validate-coords :horizontal [x ys ye]))]}
  (u/color-pixels  (v/get-segment-pixels :vertical x ys ye) c img))

(defn horizontal-segment
  [xs y xe c img]
  {:pre [(true? (v/validate-coords :horizontal [xs y xe]))]}
  (u/color-pixels (v/get-segment-pixels :horizontal xs y xe) c img))

(defn get-rows
  [rows]
  (->> (vals rows)
       (map (fn [pixels]
              (map val pixels)))))

(defn display-image
  [img]
  (println (->> img
                (sort-by key)
                (group-by (comp last first))
                get-rows
                (map #(string/join " " %))
                (string/join "\n")))
  img)

(defn fill-region
  [x y c img]
  {:pre [(not (nil? (u/get-pixel x y img)))]}
  (u/fill-area (list [x y]) c img))


(defn input->function
  "Takes the user input and calls the related function"
  [s img]
  (when-not (empty? s)
    (let [args-str    (s/split s #"\s")
          f-name      (first args-str)

          ;; read-string is not the safest way to read numeric values from strings
          ;; by changing the color to be an Integer as well, the safest way to convert
          ;; a string to a number is to coerce using java Int.
          args        (mapv read-string (rest args-str))

          ;; Maybe a better use of Schema/Spec to validate args? Probably using {:pre} inside each function (as they might have different requirements)
          ;; I can't use {:pre} inside each function if I want to display a user friendly message
          valid-args? (v/validate-args f-name args)
          apply-fn    (fn [f] (if valid-args?
                                (apply f (conj args img))
                                (u/err-handler :invalid-args f-name img)))]
      (case f-name
        "I" (apply-fn new-image)
        "C" (apply-fn clear-image)
        "L" (apply-fn color-pixel)
        "V" (apply-fn vertical-segment)
        "H" (apply-fn horizontal-segment)
        "F" (apply-fn fill-region)
        "S" (apply-fn display-image)
        (u/err-handler :invalid-function f-name img)))))





