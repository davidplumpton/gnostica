(ns gnostica.icon-layout-test
  (:require [clojure.test :refer [deftest is]]
            [gnostica.icon-layout :as icon-layout]))

(deftest special-move-icons-use-shared-two-times-scale
  (is (= 2 icon-layout/card-icon-scale))
  (is (= 36 icon-layout/dom-card-icon-width-percent))
  (is (= 6 icon-layout/dom-card-icon-gap-px))
  (is (= 264 icon-layout/texture-card-icon-size))
  (is (= 36 icon-layout/texture-card-icon-gap)))

(deftest largest-icon-stack-fits-card-textures
  (is (= 864
         (icon-layout/texture-icon-stack-span icon-layout/max-card-icon-count)))
  (is (icon-layout/texture-icon-stack-fits? icon-layout/max-card-icon-count)))

(deftest dom-style-exposes-card-icon-layout-variables
  (is (= {"--gnostica-icon-stack-top" "4%"
          "--gnostica-icon-stack-left" "5%"
          "--gnostica-icon-stack-width" "36%"
          "--gnostica-icon-stack-gap" "6px"
          "--gnostica-icon-scale" "2"}
         (icon-layout/dom-icon-stack-style))))
