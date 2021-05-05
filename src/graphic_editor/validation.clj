(ns graphic-editor.validation)

(defn args-validation
	[args]
	(every? integer? args))

(defn validate-args
	[f args]
	(case f
		"I" (args-validation args)
		"C" (empty? args)
		"L" (args-validation args)
		"V" (args-validation args)
		"H" (args-validation args)
		"F" (args-validation args)
		"S" (empty? args)
		nil))

(defn get-segment-pixels
	[dir start-x start-y end]
	(let [horizontal? (= :horizontal dir)
				start (if horizontal? start-x start-y)]
		(for [coord (range start (inc end))]
			(if horizontal? [coord start-y] [start-x coord]))))


(defn validate-coords
	[dir [start-x start-y end :as coords]]
	(when (every? integer? coords)
		(every? (comp not nil?)
						(get-segment-pixels dir start-x start-y end))))
