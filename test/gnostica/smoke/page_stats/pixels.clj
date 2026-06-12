(ns gnostica.smoke.page-stats.pixels
  (:require [gnostica.smoke.browser :as browser])
  (:import [java.io ByteArrayInputStream]
           [java.util Base64]
           [javax.imageio ImageIO]))

(def expected-table-surface-color "#1c0715")
(def expected-table-clear-color "#0a0308")
(def min-velvet-pixels 120)
(def min-dark-table-pixels 120)
(defn velvet-pixel? [argb]
  (let [r (bit-and (bit-shift-right argb 16) 0xff)
        g (bit-and (bit-shift-right argb 8) 0xff)
        b (bit-and argb 0xff)]
    (and (>= r 24)
         (>= b 18)
         (<= g 80)
         (<= b 130)
         (> r g)
         (> b g)
         (>= (- r g) 12)
         (>= (- b g) 4))))
(defn dark-table-pixel? [argb]
  (let [r (bit-and (bit-shift-right argb 16) 0xff)
        g (bit-and (bit-shift-right argb 8) 0xff)
        b (bit-and argb 0xff)]
    (and (<= r 18)
         (<= g 14)
         (<= b 20))))
(defn screenshot-pixel-stats! [client {:strs [x y width height]}]
  (let [result (browser/cdp-command! client
                                     "Page.captureScreenshot"
                                     {"format" "png"
                                      "clip" {"x" x
                                              "y" y
                                              "width" width
                                              "height" height
                                              "scale" 1}})
        bytes (.decode (Base64/getDecoder) ^String (get result "data"))
        image (ImageIO/read (ByteArrayInputStream. bytes))]
    (if-not image
      {"ok" false
       "error" "Chrome returned screenshot bytes that ImageIO could not decode."}
      (let [image-width (.getWidth image)
            image-height (.getHeight image)
            step-x (max 1 (quot image-width 80))
            step-y (max 1 (quot image-height 80))
            colors (volatile! (transient #{}))
            velvet-pixels (volatile! 0)
            dark-table-pixels (volatile! 0)
            sampled (volatile! 0)]
        (doseq [sample-x (range 0 image-width step-x)
                sample-y (range 0 image-height step-y)]
          (vswap! sampled inc)
          (let [argb (.getRGB image sample-x sample-y)]
            (when (velvet-pixel? argb)
              (vswap! velvet-pixels inc))
            (when (dark-table-pixel? argb)
              (vswap! dark-table-pixels inc))
            (vswap! colors conj! argb)))
        {"ok" true
         "width" image-width
         "height" image-height
         "sampledPixels" @sampled
         "velvetPixels" @velvet-pixels
         "darkTablePixels" @dark-table-pixels
         "distinctColors" (count (persistent! @colors))}))))
(defn pixel-ok? [stats]
  (and (true? (get stats "ok"))
       (>= (long (or (get stats "sampledPixels") 0)) 100)
       (>= (long (or (get stats "distinctColors") 0)) 16)
       (>= (long (or (get stats "velvetPixels") 0)) min-velvet-pixels)
       (>= (long (or (get stats "darkTablePixels") 0)) min-dark-table-pixels)))
