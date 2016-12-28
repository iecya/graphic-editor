(ns graphic-editor.validation)


(defn args-validation
  [args]
  (let [coords   (butlast args)]
    (and (every? integer? coords)
         (re-find #"[A-Z]" (str (last args))))))


(defn validate-args
  [f args]
  (case f
    "I" (every? #(and (integer? %)
                      (<= 1 % 250)) args)
    "C" (empty? args)
    "L" (args-validation args)
    "V" (args-validation args)
    "H" (args-validation args)
    "F" (args-validation args)
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
