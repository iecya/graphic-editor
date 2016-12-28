(ns graphic-editor.utils)


(defn ?pixel
  "Checks if a pixel exists at the given coordinates"
  [x' y' img]
  (some (fn [{:keys [x y] :as px}]
          (when (and (= x x') (= y y'))
            px)) (:items img)))


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


(defn color-region
  [region clr]
  (fn [items]
    (into #{} (for [px items]
                (if (some #(= px %) region)
                  (do
                    (disj items px)
                    (assoc px :color clr))
                  px)))))


(defn get-siblings
  [img]
  "Returns a function that, for the given pixel, returns a list of its valid siblings of the same color"
  (fn [{:keys [x y] :as px}]
    (let [siblings (list (?pixel x (inc y) img)
                         (?pixel x (dec y) img)
                         (?pixel (dec x) y img)
                         (?pixel (inc x) y img))]
      (filter (fn [n] (and (not (nil? n))
                           (same-color? px n))) siblings))))


(defn extend-list
  [l img]
  (let [all-siblings     (mapcat (get-siblings img) l)
        missing-siblings (filter (fn [s] (when (every? #(not= s %) l) s)) all-siblings)
        cleared-siblings (distinct missing-siblings)]
    (concat l cleared-siblings)))


(defn fill
  [l c img]
  (let [extended-list (extend-list l img)]
    (if-not (= l extended-list)
      (fill extended-list c img)
      (update img :items (color-region extended-list c)))))


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

