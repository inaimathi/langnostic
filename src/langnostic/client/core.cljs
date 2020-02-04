(ns langnostic.client.core
  (:require [crate.core :refer [html]]))

(defn query-selector
  ([selector] (query-selector js/document selector))
  ([elem selector]
   (-> elem (.querySelector selector))))

(defn query-selector-all
  ([selector] (query-selector-all js/document selector))
  ([elem selector]
   (.call (.-slice (.-prototype js/Array))
          (-> elem (.querySelectorAll selector)))))

(.log js/console "hello world!")
