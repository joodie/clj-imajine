(ns nl.zeekat.imajine.pdf
  (:use nl.zeekat.imajine.core)
  (:import org.pdfbox.pdmodel.PDDocument
           org.pdfbox.pdmodel.PDPage
           java.io.File))

(defmacro with-pdf
  "Load a pdf file as a PDDocument. Closes the document after body is done"
  [name file & body]
  `(with-open [~name (PDDocument/load ~file)]
     ~@body))

(defn pages-as-images
  "returns a seq of all pages of a PDDocument as a BufferedImage"
  [#^PDDocument doc]
  (seq (-> doc .getDocumentCatalog .getAllPages)))

(defn page-to-image
  "convert a page to a BufferedImage"
  [page]
  (.convertToImage page))


