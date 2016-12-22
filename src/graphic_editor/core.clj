(ns graphic-editor.core
  (:gen-class))

(defn -main
  []
  (loop [s (read-line)]
    (when-not (= s "quit")
      (prn s)
      (recur (read-line)))))
