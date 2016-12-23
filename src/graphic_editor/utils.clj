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


(defn same-color?
  [px0 px]
  (= (:color px0) (:color px)))


(defn sort-items
  [img]
  (->> (:items img)
       (group-by :y)
       sort
       vals
       (mapv #(sort-by :x %))))


(defn v-segment
  [x' ys ye c]
  (fn [items]
    (into #{} (for [{:keys [x y] :as px} items]
                (if (and (= x x') (<= ys y ye))
                  (do
                    (disj items px)
                    (assoc px :color (str c)))
                  px)))))


(defn h-segment
  [xs xe y' c]
  (fn [items]
    (into #{} (for [{:keys [x y] :as px} items]
                (if (and (= y y') (<= xs x xe))
                  (do
                    (disj items px)
                    (assoc px :color (str c)))
                  px)))))


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