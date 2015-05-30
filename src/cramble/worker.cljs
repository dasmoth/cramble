(ns cramble.worker
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cramble.core :as cram]
            [cljs.core.async :refer [put! close! chan]]
            [clojure.walk :refer [keywordize-keys]]))

;;
;; Temporary copy from cljs.core, remove once next Clojurescript release
;; appears...
;;

(defn random-uuid []
  (letfn [(hex [] (.toString (rand-int 15) 16))]
    (let [rhex (.toString (bit-or 0x8 (bit-and 0x3 (rand-int 14))) 16)]
      (UUID.
        (str (hex) (hex) (hex) (hex)
             (hex) (hex) (hex) (hex) "-"
             (hex) (hex) (hex) (hex) "-"
             "4"   (hex) (hex) (hex) "-"
             rhex  (hex) (hex) (hex) "-"
             (hex) (hex) (hex) (hex)
             (hex) (hex) (hex) (hex)
             (hex) (hex) (hex) (hex))))))

;; Table of CRAM objects we're managing.

(def crams (atom {}))

(defmulti handle-command :command)

(defmethod handle-command "connect"
  [{:keys [uri index_uri]}]
  (go
    (let [cram (<! (cram/read-cram uri))
          crai (<! (cram/read-crai index_uri))
          id (str (random-uuid))]
      (swap! crams assoc id {:cram cram
                             :crai crai})
      (clj->js {:handle id}))))

(defmethod handle-command "fetch"
  [{:keys [handle seq min max]}]
  (go
    (if-let [{:keys [cram crai]} (@crams handle)]
      {:reads (<! (cram/read-region cram crai seq min max))}
      {:error (str "Bad handle " handle)})))

(defmethod handle-command :default
  [{:keys [command]}]
  (go
    #js {:error (str "Unknown command " command)}))
          
      

(defn handle-message [ev]
  (go
    (let [data    (keywordize-keys (js->clj (.-data ev)))
          resp    (<! (handle-command data))]
      (.postMessage js/self (clj->js {:tag    (:tag data)
                                      :result resp})))))
        

(set! (.-onmessage js/self) handle-message)
