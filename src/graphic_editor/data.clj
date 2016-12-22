(ns graphic-editor.data)

(def init-image-data
  {:cols  1
   :rows  1
   :items #{{:x 1 :y 1 :color "O"}}})


(def image-data (atom init-image-data))
