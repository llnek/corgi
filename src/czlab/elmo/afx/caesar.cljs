;; Copyright (c) 2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.caesar

  (:require-macros [czlab.elmo.afx.core :as ec])
  (:require [clojure.string :as cs]
            [czlab.elmo.afx.core :as ec :refer [xmod]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def VISCHS (str " @N/\\Ri2}aP`(xeT4F3mt;8~%r0v:L5$+Z{'V)\"CKIc>z.*"
                 "fJEwSU7juYg<klO&1?[h9=n,yoQGsW]BMHpXb6A|D#q^_d!-"))
(def VISCHS-LEN  (count VISCHS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- chat "" [pos & [s]] (.charAt (or s VISCHS) pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getch "" [ch]
  (loop [pos 0]
    (if-not
      (< pos VISCHS-LEN) -1 (if (= ch (chat pos)) pos (recur (inc pos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- slideForward "" [delta cpos]
  (let [pos (+ cpos delta)]
    (chat (if (>= pos VISCHS-LEN) (- pos VISCHS-LEN) pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- slideBack "" [delta cpos]
  (let [pos (- cpos delta)] (chat (if (< pos 0) (+ VISCHS-LEN pos) pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- shiftEnc "" [shift delta cpos]
  (if (< shift 0) (slideForward delta cpos) (slideBack delta cpos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- shiftDec "" [shift delta cpos]
  (if (< shift 0) (slideBack delta cpos) (slideForward delta cpos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn encrypt "" [src shift]
  (->>
    (if (and (string? src) (not-empty src) (not (zero? shift)))
      (let [len (count src)
            delta (-> (js/Math.abs shift) (xmod VISCHS-LEN))]
        (loop [pos 0 out (transient [])]
          (if-not (< pos len)
            (persistent! out)
            (let [ch (chat pos src) p (getch ch)]
              (recur (inc pos)
                     (conj! out (if-not (< p 0)
                                  (shiftEnc shift delta p) ch))))))) [])
    (cs/join "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn decrypt "" [cipher shift]
  (->>
    (if (and (string? cipher) (not-empty cipher) (not (zero? shift)))
      (let [len (count cipher)
            delta (-> (js/Math.abs shift) (xmod VISCHS-LEN))]
        (loop [pos 0 out (transient [])]
          (if-not (< pos len)
            (persistent! out)
            (let [ch (chat pos cipher) p (getch ch)]
              (recur (inc pos)
                     (conj! out (if-not (< p 0)
                                  (shiftDec shift delta p) ch))))))) [])
    (cs/join "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



