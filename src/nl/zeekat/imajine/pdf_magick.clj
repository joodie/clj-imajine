(ns nl.zeekat.imajine.pdf-magick
  (:use nl.zeekat.imajine.core
        clojure.contrib.shell-out
        [clojure.contrib.string :only [split trim]]
        clojure.java.io)
  (:import java.util.UUID
           java.io.File
           java.io.InputStream
           java.io.FileInputStream))

(def #^{:doc "path where `convert' and `identify' are located"}
  *image-magick-path* "")

(defn run
  [& args]
  (let [rv (apply sh :return-map true (str *image-magick-path* (first args)) (rest args))]
    (if (= (:exit rv) 0)
      (:out rv)
      (throw (Exception. (str "Error running " args " exit value:" (:exit rv) "stderr: " (:err rv)))))))

(defmacro with-pdf
  "Load a pdf file or stream for processing. Closes the document after body is done"
  [name file & body]
  `(let [file# ~file
         tmp-pdf# (File/createTempFile "pdf-magick" ".pdf")
         tmp-png# (File/createTempFile "pdf-magick" ".png")]
     
     (try
       (with-open [stream# (if (instance? InputStream file#)
                             file#
                             (FileInputStream. file#))]
         (copy stream# tmp-pdf#)
         (let [~name {:pdf (.getAbsolutePath tmp-pdf#)
                      :png (.getAbsolutePath tmp-png#)
                      :pages (Integer. (last (split #" +" (trim (run "identify" "-density" "2" "-format" "%p " (.getAbsolutePath tmp-pdf#))))))}]
           ~@body))
       (finally
        (.delete tmp-pdf#)
        (.delete tmp-png#)))))

(defn- get-page
  [doc num]
  [["convert" "-define" "pdf:use-cropbox=true" "-depth" "8" "-colorspace" "sRGB" "-antialias" (str (:pdf doc) "[" (dec num) "]") (:png doc)] (:png doc)])

(defn pages
  "returns a lazy seq of all pages of a pdf document"
  ([doc]
     (pages doc 1))
  ([doc start-page-number]
     (lazy-seq
      (if (> start-page-number (:pages doc))
        nil
        (cons (get-page doc start-page-number) (pages doc (inc start-page-number)))))))

(defn page-to-image
  "convert a page to a BufferedImage"
  [[cmd file]]
  (apply run cmd)
  (read-image (as-file file)))


