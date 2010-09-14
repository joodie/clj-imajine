(ns nl.zeekat.imajine
  (:use clojure.contrib.math))

(import [javax.imageio ImageIO ImageWriter ImageWriteParam IIOImage]
        [javax.imageio.stream MemoryCacheImageOutputStream]
        [java.awt.image BufferedImage]
        [java.awt RenderingHints]
        [java.awt.geom AffineTransform]
        [java.io File IOException ByteArrayOutputStream ByteArrayInputStream InputStream])

(defn read-image
  "Read a BufferedImage from an InputStream or File"
  [f]
  (ImageIO/read f))

(defn image-dimensions
  "returns [width height] of the given image"
  [#^BufferedImage image]
  [(.getWidth image) (.getHeight image)])

(defn bounding-box
  "return maximum [width height scale] given a bounding box and a set of dimensions.
width and heights are integer, scale is a double"
  [bbox-width bbox-height orig-w orig-h]
  (let [scale (double (if (> (/ bbox-height orig-h) (/ bbox-width orig-w))
                 (/ bbox-width orig-w)
                 (/ bbox-height orig-h)))]
    [(int (ceil (* scale orig-w))) (int (ceil (* scale orig-h))) scale]))

(defn resize 
  "Resize a BufferedImage. Returns a BufferedImage"
  [#^BufferedImage input w h]
  (let [[width height scale] (apply bounding-box w h (image-dimensions input))
        output (BufferedImage. width height (.getType input))]
    (let [g2d (.createGraphics output)] 
      (try 
       (.setRenderingHint g2d RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BICUBIC)
       (.drawRenderedImage g2d input (AffineTransform/getScaleInstance scale scale))
       (finally 
        (.dispose g2d))))
    output))

(defn image-stream
  "convert the image to a byte stream. format-name defaults to \"JPG\"
Available formats are dependent on whatever javax.imageio.ImageIO provides on your system"
  ([#^BufferedImage image format-name]
  (let [writer (first (iterator-seq (ImageIO/getImageWritersByFormatName format-name)))]
    (let [stream (ByteArrayOutputStream.)
          output (MemoryCacheImageOutputStream. stream)] 
      (doto (.getDefaultWriteParam writer)
        (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
        (.setCompressionQuality 0.9))
      (doto writer
        (.setOutput output)
        (.write (IIOImage. image nil nil)))
      (ByteArrayInputStream. (.toByteArray stream)))))
  ([#^BufferedImage image]
      (image-stream image "JPG")))




