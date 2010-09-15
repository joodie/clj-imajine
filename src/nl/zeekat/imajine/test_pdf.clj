(ns nl.zeekat.imajine.test-pdf
  (:use nl.zeekat.imajine.core
        nl.zeekat.imajine.pdf-magick
        clojure.java.io))

(defn convert-pdf-to-jpg
  [filename]
  (with-pdf pdf (as-file filename)
    (loop [p (pages pdf) c 1]
      (let [img (page-to-image (first p))]
        (copy (image-stream img) (as-file (str filename "-" c ".jpg"))))
      (if (next p)
        (recur (next p) (inc c))))))
