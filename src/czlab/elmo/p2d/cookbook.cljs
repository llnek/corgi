;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.cookbook

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 do->true assoc!!]])

  (:require [czlab.elmo.afx.core :as ec :refer [abs* sqr* n# num?? invert]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private FLT_EPSILON js/Number.EPSILON)
(def ^:private NEG-DEG-2PI (- 360.0))
(def DEG-2PI 360.0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- CMP "" [x y]
  (<= (abs* (- x y))
      (* FLT_EPSILON (max 1.0 (max (abs* x) (abs* y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec3 "" [& [x y z]] {:x (num?? x 0) :y (num?? y 0) :z (num?? z 0)})
(defn vec2 "" [& [x y]] {:x (num?? x 0) :y (num?? y 0)})

(defn v3-array "" [v] (let [{:keys [x y z] v}] [x y z]))
(defn v2-array "" [v] (let [{:keys [x y] v}] [x y]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn modDeg "" [deg]
  (let [d (if (> deg DEG-2PI)
            (modDeg (- deg DEG-2PI)) deg)]
    (if (< d NEG-DEG-2PI) (modDeg (+ deg DEG-2PI)) d)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rad->deg
  "" [r] (modDeg (* DEG-2PI (/ r TWO-PI))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deg->rad
  "" [d] (* TWO-PI (/ (modDeg d) DEG-2PI)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-eq? "" [v1 v2]
  (let [{x2 :x y2 :y} v2
        {x1 :x y1 :y} v1]
    (and (CMP x1 x2) (CMP y1 y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-eq? "" [v1 v2]
  (let [{x2 :x y2 :y z2 :z} v2
        {x1 :x y1 :y z1 :z} v1]
    (and (CMP x1 x2) (CMP y1 y2) (CMP z1 z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-neq? "" [v1 v2] (not (v2-eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-neq? "" [v1 v2] (not (v3-eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-add "" [v1 v2]
  (let [{x2 :x y2 :y} v2
        {x1 :x y1 :y} v1] (vec2 (+ x1 x2) (+ y1 y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-add "" [v1 v2]
  (let [{x2 :x y2 :y z2 :z} v2
        {x1 :x y1 :y z1 :z} v1] (vec3 (+ x1 x2) (+ y1 y2) (+ z1 z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-sub "" [v1 v2]
  (let [{x2 :x y2 :y} v2
        {x1 :x y1 :y} v1] (vec2 (- x1 x2) (- y1 y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-sub "" [v1 v2]
  (let [{x2 :x y2 :y z2 :z} v2
        {x1 :x y1 :y z1 :z} v1] (vec3 (- x1 x2) (- y1 y2) (- z1 z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-mult "" [v1 v2]
  (let [{x2 :x y2 :y} v2
        {x1 :x y1 :y} v1] (vec2 (* x1 x2) (* y1 y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-mult "" [v1 v2]
  (let [{x2 :x y2 :y z2 :z} v2
        {x1 :x y1 :y z1 :z} v1] (vec3 (* x1 x2) (* y1 y2) (* z1 z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-div "" [v1 v2]
  (let [{x2 :x y2 :y} v2
        {x1 :x y1 :y} v1] (vec2 (/ x1 x2) (/ y1 y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-div "" [v1 v2]
  (let [{x2 :x y2 :y z2 :z} v2
        {x1 :x y1 :y z1 :z} v1] (vec3 (/ x1 x2) (/ y1 y2) (/ z1 z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-scale "" [v n]
  (let [{:keys [x y]} v] (vec2 (* x n) (* y n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-scale "" [v n]
  (let [{:keys [x y z]} v] (vec3 (* x n) (* y n) (* z n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dot "" [v1 v2]
  (let [{x2 :x y2 :y} v2
        {x1 :x y1 :y} v1] (+ (* x1 x2) (* y1 y2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-dot "" [v1 v2]
  (let [{x2 :x y2 :y z2 :z} v2
        {x1 :x y1 :y z1 :z} v1] (+ (* x1 x2) (* y1 y2) (* z1 z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-len "" [v] (sqrt* (v2-dot v v)))
(defn v3-len "" [v] (sqrt* (v3-dot v v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-lensq "" [v] (v2-dot v v))
(defn v3-lensq "" [v] (v3-dot v v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-dist "" [v1 v2] (v2-len v1 v2))
(defn v3-dist "" [v1 v2] (v3-len v1 v2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-distsq "" [v1 v2] (v2-lensq v1 v2))
(defn v3-distsq "" [v1 v2] (v3-lensq v1 v2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-rot "" [v rad]
  (let [{:keys [x y]} v
        s (js/Math.sin rad)
        c (js/Math.cos rad)]
    (vec2 (- (* x c) (* y s))
          (+ (* x s) (* y c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-unit "" [v] (v2-scale v (invert (v2-len v))))
(defn v3-unit "" [v] (v3-scale v (invert (v3-len v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-xss "" [v1 v2]
  (let [{x2 :x y2 :y z2 :z} v2
        {x1 :x y1 :y z1 :z} v1]
    (vec3 (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-angle "" [v1 v2]
  (js/Math.acos (/ (v2-dot v1 v2)
                   (sqrt* (* (v2-lensq v1) (v2-lensq v2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-angle "" [v1 v2]
  (js/Math.acos (/ (v3-dot v1 v2)
                   (sqrt* (* (v3-lensq v1) (v3-lensq v2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-proj "" [length dir]
  (v2-scale dir (/ (v2-dot length dir) (v2-lensq dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-proj "" [length dir]
  (v3-scale dir (/ (v3-dot length dir) (v3-lensq dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-perp "" [length dir]
  (v2-sub length (v2-proj length dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-perp "" [length dir]
  (v3-sub length (v3-proj length dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-reflect "" [src normal]
  (v2-sub src (v2-scale normal (* (v2-dot src normal) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-reflect "" [src normal]
  (v3-sub src (v3-scale normal (* (v3-dot src normal) 2))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


