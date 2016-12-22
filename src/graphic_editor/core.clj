(ns graphic-editor.core
  (:gen-class)
  (:require [graphic-editor.api :as api]))


(def welcome-msg (str "\n\n\n=================================================\n\n\n"
                      (clojure.string/upper-case "***** Welcome to Graphic Editor *****")
                      "\nTo quit the session, please type 'q', 'Q', 'quit' or 'QUIT'"
                      "\n\n\n================================================="
                      "\n\n\n"))

(defn -main
  []
  (println welcome-msg)
  (loop [s (read-line)]
    (if (or (= s "q") (= s "Q") (= s "quit") (= s "QUIT"))
      "Bye Bye!"
      (do (api/input->function s)
          (recur (read-line))))))
