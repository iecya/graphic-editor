(ns graphic-editor.utils)


(defn ?pixel
  "Checks if a pixel exists at the given coordinates"
  [x' y' img]
  (some (fn [{:keys [x y] :as px}]
          (when (and (= x x') (= y y'))
            px)) (:items img)))


(defn ?pixel-color
  "If pixel exist, returns its color"
  [x y img]
  (when-let [px (?pixel x y img)]
    (:color px)))

(defn sort-items
  [img]
  (->> (:items img)
       (group-by :y)
       sort
       vals
       (mapv #(sort-by :x %))))

(defn err-handler
  [err-type arg img]
  (println (case err-type
             :invalid-function (str "Sorry, function " arg " does not exist")
             :invalid-args (str "Sorry, function " arg " received a wrong number or type of arguments")
             :invalid-pixel (str "Pixel at coordinates(" (:x arg) ", " (:y arg) ") is not in the image")))
  img)


(defn get-row
  [items]
  (apply str (map :color items)))