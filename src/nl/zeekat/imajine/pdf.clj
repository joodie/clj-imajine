(ns nl.zeekat.imajine.pdf
  (:use nl.zeekat.imajine.core)
  (:import com.sun.pdfview.PDFFile
           com.sun.pdfview.PDFPage
           java.nio.ByteBuffer
           java.io.InputStream
           java.io.FileInputStream
           java.nio.channels.Channels
           java.nio.channels.FileChannel
           java.nio.channels.FileChannel$MapMode
           java.io.File
           java.awt.image.BufferedImage
           java.awt.Rectangle))

(defmacro with-pdf
  "Load a pdf file or stream as a com.sun.pdfview.PDFFile. Closes the document after body is done"
  [name file & body]
  `(let [file# ~file]
     (with-open [stream# (if (instance? InputStream file#)
                           file#
                           (FileInputStream. file#))
                 channel# (Channels/newChannel stream#)]
       (let [buffer# (.map channel# FileChannel$MapMode/READ_ONLY 0 (.size channel#))
             ~name (PDFFile. buffer#)]
         ~@body))))

(defn pages
  "returns a lazy seq of all pages of a PDFFile"
  ([#^PDFFile doc]
     (pages doc 1))
  ([#^PDFFile doc start-page-number]
     (lazy-seq
      (if (> start-page-number (.getNumPages doc))
        nil
        (cons (.getPage doc start-page-number true) (pages doc (inc start-page-number)))))))


(defn page-to-image
  "convert a page to a BufferedImage"
  [#^PDFPage page]
  (let [rect (Rectangle. 0 0 (int (-> page .getBBox .getWidth)) (int (-> page .getBBox .getHeight)))
        img (.getImage page (.width rect) (.height rect) rect nil true true)
        buffimg (BufferedImage. (.width rect) (.height rect) BufferedImage/TYPE_INT_RGB)
        g2d (.createGraphics buffimg)]
    (.drawImage g2d img 0 0 nil)
    buffimg))

