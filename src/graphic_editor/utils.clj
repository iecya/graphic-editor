(ns graphic-editor.utils)


(defn ?pixel
  "Checks if a pixel exists at the given coordinates"
  [x' y' img]
  (some (fn [{:keys [x y] :as px}]
          (when (and (= x x') (= y y'))
            px)) (:items img)))


(defn same-color?
  "Returns true if the given pixels have the same color, false otherwise"
  [px0 px]
  (= (:color px0) (:color px)))


(defn sort-items
  "Returns the list of pixel sorted first by :y and then by :x"
  [img]
  (->> (:items img)
       (group-by :y)
       sort
       vals
       (mapv #(sort-by :x %))))


(defn v-segment
  "Given the coords for a vertical segment, returns a new set of pixels after changing color to the pixels included in the segment area"
  [x' ys ye c]
  (fn [items]
    (into #{} (for [{:keys [x y] :as px} items]
                (if (and (= x x') (<= ys y ye))
                  (do
                    (disj items px)
                    (assoc px :color (str c)))
                  px)))))


(defn h-segment
  "Given the coords for a horizontal segment, returns a new set of pixels after changing color to the pixels included in the segment area"
  [xs xe y' c]
  (fn [items]
    (into #{} (for [{:keys [x y] :as px} items]
                (if (and (= y y') (<= xs x xe))
                  (do
                    (disj items px)
                    (assoc px :color (str c)))
                  px)))))


(defn color-region
  "Given the coords for region of neighbour pixels of the same color, returns a new set of pixels after changing color to the pixels included in the region"
  [region clr]
  (fn [items]
    (into #{} (for [px items]
                (if (some #(= px %) region)
                  (do
                    (disj items px)
                    (assoc px :color clr))
                  px)))))


(defn get-siblings
  "Returns a function that, for the given pixel, returns a list of its valid siblings of the same color"
  [img]
  (fn [{:keys [x y] :as px}]
    (let [siblings (list (?pixel x (inc y) img)
                         (?pixel x (dec y) img)
                         (?pixel (dec x) y img)
                         (?pixel (inc x) y img))]
      (filter (fn [n] (and (not (nil? n))
                           (same-color? px n))) siblings))))


(defn extend-list
  "Returns a list formed by the given list and the siblings of each pixels that are not already included in the original list"
  [l img]
  (let [siblings         (mapcat (get-siblings img) l)
        missing-siblings (filter (fn [s] (when (every? #(not= s %) l) s)) siblings)]
    (concat l (distinct missing-siblings))))


(defn fill
  "Given a list of pixel and a color, extend the list with the siblings of the pixels. If the extended list is equal to the original list, color all the pixels in the list with the given color, otherwise extend the new list"
  [l c img]
  (let [extended-list (extend-list l img)]
    (if-not (= l extended-list)
      (fill extended-list c img)
      (update img :items (color-region extended-list c)))))


(defn err-handler
  "Given an error type, print the related error message and return the image"
  [err-type arg img]
  (println (case err-type
             :invalid-function (str "Sorry, function " arg " does not exist")
             :invalid-args (str "Sorry, function " arg " received a wrong number or type of arguments")
             :invalid-pixel (str "Pixel at coordinates(" (:x arg) ", " (:y arg) ") is not in the image")
             :invalid-coords (str "Sorry, function " arg " received invalid coordinates")))
  img)


(defn get-row
  "Given a list of sorted pixels, returns a string made of the color of each pixel"
  [items]
  (apply str (map :color items)))

