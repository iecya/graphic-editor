(ns graphic-editor.core
  (:gen-class)
  (:require [graphic-editor.api :as api]))

(defn -main
  []
  (loop [s (read-line)]
    (when-not (or (= s "q") (= s "Q") (= s "quit") (= s "QUIT"))
      (api/input->function s)
      (recur (read-line)))))
