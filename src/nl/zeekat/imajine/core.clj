(ns nl.zeekat.imajine.core
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
    [(int (round (* scale orig-w))) (int (round (* scale orig-h))) scale]))

(defn- resize*
  "Resize a BufferedImage. Returns a BufferedImage"
  [#^BufferedImage input w h]
  (let [output (BufferedImage. w h (.getType input))
        g2d (.createGraphics output)]
    (try 
      (.setRenderingHint g2d RenderingHints/KEY_INTERPOLATION RenderingHints/VALUE_INTERPOLATION_BILINEAR)
      (.drawImage g2d input 0 0 w h nil)
      (finally 
       (.dispose g2d)))
    output))

(defn resize
  "Resize a BufferedImage. Returns a BufferedImage"
  [#^BufferedImage input w h]
  (let [[in-w in-h] (image-dimensions input)
        [width height] (bounding-box w h in-w in-h)
        halve-w (/ in-w 2)
        halve-h (/ in-h 2)]
    (if (or (<= in-w width)
            (<= in-h height)
            (<= halve-w width)
            (<= halve-h height))
      (resize* input width height)
      (recur (resize* input halve-w halve-h) w h))))


(defn image-stream
  "convert the image to a byte stream. format-name defaults to \"JPG\"
Available formats are dependent on whatever javax.imageio.ImageIO provides on your system"
  ([#^BufferedImage image format-name]
  (let [writer (first (iterator-seq (ImageIO/getImageWritersByFormatName format-name)))]
    (let [stream (ByteArrayOutputStream.)
          output (MemoryCacheImageOutputStream. stream)
          params (.getDefaultWriteParam writer)] 
      (doto params
        (.setCompressionMode ImageWriteParam/MODE_EXPLICIT)
        (.setCompressionQuality 0.95))
      (doto writer
        (.setOutput output)
        (.write nil (IIOImage. image nil nil) params))
      (ByteArrayInputStream. (.toByteArray stream)))))
  ([#^BufferedImage image]
      (image-stream image "JPG")))
