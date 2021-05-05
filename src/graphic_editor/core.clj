(ns graphic-editor.core
  (:gen-class)
  (:require [graphic-editor.api :as api]))


(def welcome-msg (str "\n\n\n=================================================\n\n\n"
                      (clojure.string/upper-case "***** Welcome to Graphic Editor *****")
                      "\nTo quit the session, please type 'X'"
                      "\n\n\n================================================="
                      "\n\n\n"))

;; The main function should receive the (optional) args and then keep listening to the read-line
;; Another way to implement this application is to start it with an input to create the image on startup
;; (it wouldn't make sense to have an image editor without an image). If no input are passed to create the new image
;; some default values would be used
;; the image than can be modified buy sending edit requests via API
(defn -main
  []
  (println welcome-msg)
  (let [image {}]
    (loop [img image
           s (read-line)]
    (if (= s "X")
      "Bye Bye!"
      (recur (api/input->function s img) (read-line))))))
