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

  (:require [czlab.elmo.afx.core :as ec :refer [sqrt* abs* sqr* n# num?? invert]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private FLT_EPSILON js/Number.EPSILON)
(def ^:private NEG-DEG-2PI (- 360.0))
(def ^:private TWO-PI (* 2 js/Math.PI))
(def ^:private PI js/Math.PI)
(def DEG-2PI 360.0)
(def COS js/Math.cos)
(def SIN js/Math.sin)
(def TAN js/Math.tan)
(def ATAN2 js/Math.atan2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- CMP "" [x y]
  (<= (abs* (- x y))
      (* FLT_EPSILON (max 1.0 (max (abs* x) (abs* y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec3 "" [& [x y z]] {:x (num?? x 0) :y (num?? y 0) :z (num?? z 0)})
(defn vec2 "" [& [x y]] {:x (num?? x 0) :y (num?? y 0)})

(defn v3-array "" [v] (let [{:keys [x y z]} v] [x y z]))
(defn v2-array "" [v] (let [{:keys [x y]} v] [x y]))

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
(defn v2-dist "" [v1 v2] (v2-len (v2-sub v1 v2)))
(defn v3-dist "" [v1 v2] (v3-len (v3-sub v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-distsq "" [v1 v2] (v2-lensq (v2-sub v1 v2)))
(defn v3-distsq "" [v1 v2] (v3-lensq (v2-sub v1 v2)))

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
(comment
"Row Major!
These matrices are row major, with a linear memory layout.
This means, a 4x4 transform matrix breaks down into the
following components:

Xx, Xy, Xz, 0
Yx, Yy, Yz, 0
Zx, Zy, Zz, 0
Tx, Ty, Tz, 1

Where the rotation about the X axis is defined by the vector:
(Xx, Xy, Xy). The scale of the matrix is found in it's main
diagonal: (Xx, Yy, Zz).

The matrix is laid out by rows in memory as well. This gives
us a nice linear memory map. This is what the above matrix
looks like in memory:

[Xx, Xy, Xz, 0, Yx, Yy, Yz, 0, Zx, Zy, Zz, 0, Tx, Ty, Tz, 1]")

(comment
"Pre Multiplication!
Matrices are concatenated left to right. This means a MVP
matrix could be built like so
mat4 MVP = model * view * projection
Or the world transform: SRT
scale first, rotate second, translate last
world = scale * rotate * translate
This makes reading matrix multiplication quiet intuitive.
The parent child relation-ship looks like this
child.absolute = child.relative * parent.absolute
world = local * parent.world")

(comment
"Row vectors
Vectors are treated as 1x3 (or 1x4) row vectors. This means in the
multiplication chain they MUST be put in front of matrices! Like so
vec3 final = pointVECTOR * model * view * projection
This is because we can only multiply matrices if their inner
dimensions match. This makes our multiplication be 1x4 * 4x4.")

(comment
"Left Handed!
The Projection and Orthographic projection functions product left
handed matrices. That is, +Z goes INTO the screen.")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;MATRIX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mx-pos "" [rows cols r c] (+ (- c 1) (* (- r 1) cols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mx-new*
  "" [rows cols cells] {:rows rows :cols cols :cells cells})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mx-new "" [rows cols]
  {:rows rows :cols cols :cells (make-array js/Number (* rows cols))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat2 "" [& [_11 _12 _21 _22 :as args]]
  {:rows 2 :cols 2
   :cells (if (empty? args)
            #js [1 0 0 1]
            #js [_11 _12 _21 _22])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat3 "" [& [_11 _12 _13 _21 _22 _23 _31 _32 _33 :as args]]
  {:rows 3 :cols 3
   :cells (if (empty? args)
            #js [1 0 0
                 0 1 0
                 0 0 1]
            #js [_11 _12 _13 _21 _22 _23 _31 _32 _33])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4 "" [& [_11 _12 _13 _14
                  _21 _22 _23 _24
                  _31 _32 _33 _34
                  _41 _42 _43 _44 :as args]]
  {:rows 4 :cols 4
   :cells (if (empty? args)
            #js [1 0 0 0
                 0 1 0 0
                 0 0 1 0
                 0 0 0 1]
            #js [_11 _12 _13 _14
                 _21 _22 _23 _24
                 _31 _32 _33 _34
                 _41 _42 _43 _44])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mx-eq? "" [a b]
  (let [{ra :rows ca :cols va :cells} a
        {rb :rows cb :cols vb :cells} b]
    (loop [i 0
           SZ (* ra ca) ok? (and (= ra rb)(= ca cb))]
      (if (or (>= i SZ)
              (not ok?))
        ok?
        (recur (+ 1 i) SZ (not (CMP (nth va i) (nth vb i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mx-neq? "" [a b] (not (mx-eq? a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mx-xpose "" [m]
  (let [{:keys [rows
                cols
                cells]} m
        tmp (transient [])
        out (mx-new cols rows)]
    (dotimes [c cols]
      (dotimes [r rows]
        (conj! tmp (nth cells
                        (+ c (* rows
                                (mod r rows)))))))
    (assoc out :cells (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-fastInverse "" [m] (mx-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- CELL "" [rows cols r c] (+ (- c 1) (* rows (- r 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-fastInverse "" [m]
  (let [{:keys [cells] :as out} (mx-xpose m)
        right (vec3 (CELL 4 4 1 1) (CELL 4 4 1 2) (CELL 4 4 1 3))
        up (vec3 (CELL 4 4 2 1) (CELL 4 4 2 2) (CELL 4 4 2 3))
        forward (vec3 (CELL 4 4 3 1) (CELL 4 4 3 2) (CELL 4 4 3 3))
        position (vec3 (CELL 4 4 4 1) (CELL 4 4 4 2) (CELL 4 4 4 3))]
    (aset cells (CELL 4 4 4 1) 0)
    (aset cells (CELL 4 4 4 2) 0)
    (aset cells (CELL 4 4 4 3) 0)
    (aset cells (CELL 4 4 1 4) 0)
    (aset cells (CELL 4 4 2 4) 0)
    (aset cells (CELL 4 4 3 4) 0)
    (aset cells (CELL 4 4 4 1) (- (v3-dot right position)))
    (aset cells (CELL 4 4 4 2) (- (v3-dot up position)))
    (aset cells (CELL 4 4 4 3) (- (v3-dot forward position))) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mx-scale "" [m n]
  (let [{:keys [rows cols cells]} m
        tmp (transient [])
        out (mx-new rows cols)]
    (dotimes [i (* rows cols)]
      (conj! tmp (* n (nth cells i))))
    (assoc out :cells (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mx-multAB "" [a b]
  (let [{aRows :rows aCols :cols aCells :cells} a
        {bRows :rows bCols :cols bCells :cells} b
        out (make-array js/Number (* aRows bCols))]
    (assert (= aCols bRows) "mismatch matrices")
    (dotimes [i aRows]
      (dotimes [j bCols]
        (aset out
              (+ j (* i bCols))
              (reduce (fn [sum k]
                        (+ sum
                           (* (nth aCells (+ k (* i aCols)))
                              (nth bCells (+ j (* k bCols)))))) 0 (range bRows)))))
    (mx-new* aRows bCols out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-determ "" [m]
  (let [{:keys [cells]} m]
    (- (* (nth cells 0) (nth cells 3))
       (* (nth cells 1) (nth cells 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mx-cut "" [m row col]
  (let [{:keys [rows cols cells]} m
        ;change to zero indexed
        row' (- 1 row)
        col' (- 1 col)
        tmp (transient [])]
    (dotimes [i rows]
      (dotimes [j cols]
        (when-not (or (= i row')
                      (= j col'))
          (conj! tmp (nth cells (+ j (* i cols)))))))
    (mx-new* (- 1 rows) (- 1 cols) (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-minor "" [m]
  (let [{:keys [rows cols cells]} m
        tmp (transient [])]
    (dotimes [i rows]
      (dotimes [j cols]
        (conj! tmp (m2-determ (mx-cut m i j)))))
    (mx-new* rows cols (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-minor "" [m]
  (let [{:keys [cells]} m]
    (mat2 (nth cells 3) (nth cells 2)
          (nth cells 1) (nth cells 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mx-cofactor "" [minor]
  (let [{:keys [rows cols cells]} minor
        tmp (make-array js/Number (* rows cols))]
    (dotimes [i rows]
      (dotimes [j cols]
        (let [k (+ i (* j cols))]
          (aset tmp
                k
                (* (nth cells k) (js/Math.pow -1.0 (+ i j)))))))
    (mx-new* rows cols tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-cofactor "" [m] (mx-cofactor (m2-minor m)))
(defn m3-cofactor "" [m] (mx-cofactor (m3-minor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-determ "" [m]
  (let [{mCells :cells size :rows} m
        c (m3-cofactor m)
        {cCells :cells} c]
    (reduce (fn [sum j]
              (+ sum
                 (nth mCells (+ j (* size 0)))
                 (nth cCells (+ j 0)))) 0 size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-minor "" [m]
  (let [{:keys [cells rows cols]} m
        tmp (transient [])]
    (dotimes [i rows]
      (dotimes [j cols]
        (conj! tmp (m3-determ (mx-cut m i j)))))
    (mx-new* rows cols (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-cofactor "" [m] (mx-cofactor (m4-minor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-determ "" [m]
  (let [{:keys [rows cols cells] size :rows} m
        c (m4-cofactor m)
        {cCells :cells} c]
    (reduce (fn [sum j]
              (+ sum
                 (nth cells (+ j (* rows 0)))
                 (nth cCells (+ j 0)))) 0 size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-adjugate "" [m]
  (mx-xpose (m2-cofactor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-adjugate "" [m]
  (mx-xpose (m3-cofactor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-adjugate "" [m]
  (mx-xpose (m4-cofactor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-inv "" [m]
  (let [d (m2-determ m)]
    (if (CMP d 0) (mat2) (mx-scale (m2-adjugate m) (/ 1 d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-inv "" [m]
  (let [d (m3-determ m)]
    (if (CMP d 0) (mat3) (mx-scale (m3-adjugate m) (/ 1 d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-inv "" [m]
  (let [d (m4-determ m)]
    (if (CMP d 0) (mat4) (mx-scale (m4-adjugate m) (/ 1 d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-toColMajor "" [m] (mx-xpose m))
(defn m3-toColMajor "" [m] (mx-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-fromColMajor "" [m] (mx-xpose m))
(defn m3-fromColMajor "" [m] (mx-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-txlate*
  "" [x y z] (mat4 1 0 0 0
                   0 1 0 0
                   0 0 1 0
                   x y z 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-txlate "" [v3]
  (let [{:keys [x y z]} v3] (m4-txlate* x y z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-fromM3 "" [m]
  (let [{:keys [rows cols cells]} m
        [r1 r2 r3] (partition rows cells)]
    (mx-new* (inc rows)
             (inc cols)
             (clj->js (concat r1 [0] r2 [0] r3 [0] [0 0 0 1])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getTranslationM4 "" [m]
  (let [{:keys [rows cols cells]} m
        [r1 r2 r3 r4] (partition rows cells)]
    (vec3 (nth r4 0) (nth r4 1) (nth r4 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-scale* "" [x y z]
  (mat4 x 0 0 0
        0 y 0 0
        0 0 z 0
        0 0 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-scale
  "" [v3] (let [{:keys [x y z]} v3] (m4-scale* x y z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getScaleM4 "" [m]
  (let [{:keys [rows cols cells]} m
        [r1 r2 r3 r4] (partition rows cells)]
    (vec3 (nth r1 0) (nth r2 1) (nth r3 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotation2x2 "" [angle]
  (mat2 (COS angle) (SIN angle)
        (- (SIN angle)) (COS angle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-yawPitchRoll "" [yaw pitch roll]
  (mx-new* 4 4
           #js [(+ (* (COS roll) (COS yaw))
                   (* (SIN roll) (SIN pitch) (SIN yaw)))
                (* (SIN roll) (COS pitch))
                (+ (* (COS roll) (- (SIN yaw)))
                   (* (SIN roll) (SIN pitch) (COS yaw)))
                0
                (+ (* (- (SIN roll)) (COS yaw))
                   (* (COS roll) (SIN pitch) (SIN yaw)))
                (* (COS roll) (COS pitch))
                (+ (* (SIN roll) (SIN yaw))
                   (* (COS roll) (SIN pitch) (COS yaw)))
                0
                (* (COS pitch) (SIN yaw))
                (- (SIN pitch))
                (* (COS pitch) (COS yaw))
                0
                0 0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XRotation "" [rad]
  (mx-new* 4 4
           #js [1 0 0 0
                0 (COS rad) (SIN rad) 0
                0 (- (SIN rad)) (COS rad) 0
                0 0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XRotation3x3 "" [rad]
  (mx-new* 3 3
           #js [1 0 0
                0 (COS rad) (SIN rad)
                0 (- (SIN rad)) (COS rad)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YRotation "" [rad]
  (mx-new* 4 4
           #js [(COS rad) 0 (- (SIN rad)) 0
                0 1 0 0
                (SIN rad) 0 (COS rad) 0
                0 0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YRotation3x3 "" [rad]
  (mx-new* 3 3
           #js [(COS rad) 0 (- (SIN rad))
                0 1 0
                (SIN rad) 0 (COS rad)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ZRotation "" [rad]
  (mx-new* 4 4
           #js [(COS rad) (SIN rad) 0 0
                (- (SIN rad)) (COS rad) 0 0
                0 0 1 0
                0 0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ZRotation3x3 "" [rad]
  (mx-new* 3 3
           #js [(COS rad) (SIN rad) 0
                (- (SIN rad)) (COS rad) 0
                0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-rotation "" [pitch yaw roll]
  (mx-multAB (mx-multAB (ZRotation roll)
                        (XRotation pitch)) (YRotation yaw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotation3x3 "" [pitch yaw roll]
  (mx-multAB (mx-multAB (ZRotation3x3 roll)
                        (XRotation3x3 pitch)) (YRotation3x3 yaw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-orthogonalize "" [m]
  (let [{:keys [rows cols cells]} m
        [r1 r2 r3 r4] (partition rows cells)
        xAxis (vec3 (nth r1 0) (nth r1 1) (nth r1 2))
        yAxis (vec3 (nth r2 0) (nth r2 1) (nth r2 2))
        zAxis (v3-xss xAxis yAxis)
        {xx :x xy :y xz :z} (v3-xss yAxis zAxis)
        {yx :x yy :y yz :z} (v3-xss zAxis xAxis)
        {zx :x zy :y zz :z} (v3-xss xAxis yAxis)]
    (mx-new* 4 4
             #js [xx xy xz (nth r1 3)
                  yx yy yz (nth r2 3)
                  zx zy zz (nth r3 3)
                  (nth r4 0) (nth r4 1) (nth r4 2) (nth r4 3)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-orthogonalize "" [m]
  (let [{:keys [rows cols cells]} m
        [r1 r2 r3] (partition rows cells)
        xAxis (vec3 (nth r1 0) (nth r1 1) (nth r1 2))
        yAxis (vec3 (nth r2 0) (nth r2 1) (nth r2 2))
        zAxis (v3-xss xAxis yAxis)
        {xx :x xy :y xz :z} (v3-xss yAxis zAxis)
        {yx :x yy :y yz :z} (v3-xss zAxis xAxis)
        {zx :x zy :y zz :z} (v3-xss xAxis yAxis)]
    (mx-new* 3 3
             #js [xx xy xz
                  yx yy yz
                  zx zy zz])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-axisAngle "" [axis rad]
  (let [{x' :x y' :y z' :z} axis
        c (COS rad)
        s (SIN rad)
        t (- 1 c)
        [x y z]
        (if-not (CMP (v3-lensq axis) 1)
          (let [ilen (/ 1 (v3-len axis))]
            [(* x' ilen) (* y' ilen) (* z' ilen)])
          [x' y' z'])]
  (mx-new* 4 4
           #js [(+ c (* t x x))
                (+ (* t x y) (* s z))
                (- (* t x z) (* s y))
                0
                (- (* t x y) (* s z))
                (+ c (* t y y))
                (+ (* t y z) (* s x))
                0
                (+ (* t x z)(* s y))
                (- (* t y z)(* s x))
                (+ c (* t z z))
                0
                0 0 0 1])))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn axisAngle3x3 "" [axis rad]
  (let [{x' :x y' :y z' :z} axis
        c (COS rad)
        s (SIN rad)
        t (- 1 c)
        [x y z]
        (if-not (CMP (v3-lensq axis) 1)
          (let [ilen (/ 1 (v3-len axis))]
            [(* x' ilen)(* y' ilen)(* z' ilen)])
          [x' y' z'])]
    (mx-new* 3 3
             #js [(+ c (* t x x))
                  (+ (* t x y)(* s z))
                  (- (* t x z)(* s y))
                  (- (* t x y)(* s z))
                  (+ c (* t y y))
                  (+ (* t y z)(* s x))
                  (+ (* t x z)(* s y))
                  (- (* t y z)(* s x))
                  (+ c (* t z z))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-multPoint "" [v3 m4]
  (let [{:keys [x y z]} v3
        [r1 r2 r3 r4] (partition 4 (:cells m4))]
    (vec3 (+ (* x (nth r1 0))
             (* y (nth r2 0))
             (* z (nth r3 0))
             (* 1 (nth r4 0)))
          (+ (* x (nth r1 1))
             (* y (nth r2 1))
             (* z (nth r3 1))
             (* 1 (nth r4 1)))
          (+ (* x (nth r1 2))
             (* y (nth r2 2))
             (* z (nth r3 2))
             (* 1 (nth r4 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-multVec4 "" [v3 m4]
  (let [{:keys [x y z]} v3
        [r1 r2 r3 r4] (partition 4 (:cells m4))]
    (vec3 (+ (* x (nth r1 0))
             (* y (nth r2 0))
             (* z (nth r3 0))
             (* 0 (nth r4 0)))
          (+ (* x (nth r1 1))
             (* y (nth r2 1))
             (* z (nth r3 1))
             (* 0 (nth r4 1)))
          (+ (* x (nth r1 2))
             (* y (nth r2 2))
             (* z (nth r3 2))
             (* 0 (nth r4 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-multVec3 "" [v3 m3]
  (let [{:keys [x y z]} v3
        [r1 r2 r3] (partition 3 (:cells m3))]
    (vec3 (v3-dot v3 (vec3 (nth r1 0)(nth r2 0)(nth r3 0)))
          (v3-dot v3 (vec3 (nth r1 1)(nth r2 1)(nth r3 1)))
          (v3-dot v3 (vec3 (nth r1 2)(nth r2 2)(nth r3 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-txform* "" [scale eulerRotation translate]
  (let [{:keys [x y z]} eulerRotation]
    (mx-multAB (mx-multAB (m4-scale scale)
                          (m4-rotation x y z)) (m4-txlate translate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-txform "" [scale rotationAxis rotationAngle translate]
  (mx-multAB (mx-multAB (m4-scale scale)
                        (m4-axisAngle rotationAxis rotationAngle))
             (m4-txlate translate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-lookAt "" [pos target up]
  (let [{fx :x fy :y fz :z :as forward} (v3-unit (v3-sub target pos))
        {rx :x ry :y rz :z :as right} (v3-unit (v3-xss up forward))
        {nx :x ny :y nz :z :as newUp} (v3-xss forward right)]
    (mx-new* 4 4
             #js [rx nx fx 0
                  ry ny fy 0
                  rz nz fz 0
                  (- (v3-dot right pos))
                  (- (v3-dot newUp pos))
                  (- (v3-dot forward pos)) 1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;https://msdn.microsoft.com/en-us/library/windows/desktop/bb147302(v=vs.85).aspx
;;
(defn m4-proj "" [fov aspect zNear zFar]
  (let [tanHalfFov (TAN (* fov 0.5))
        fovY (/ 1 tanHalfFov) ;;cot(fov/2)
        fovX (/ fovY aspect) ;;cot(fov/2) / aspect
        r33 (/ zFar (- zFar zNear)) ;;far/range
        {:keys [cells] :as ret} (mat4)]

    (aset cells (mx-pos 4 4 1 1) fovX)
    (aset cells (mx-pos 4 4 2 2) fovY)
    (aset cells (mx-pos 4 4 3 3) r33)
    (aset cells (mx-pos 4 4 3 4) 1)
    (aset cells (mx-pos 4 4 4 3) (* (- zNear) r33)) ;-near * (far / range)
    (aset cells (mx-pos 4 4 4 4) 0)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Derived following: http://www.songho.ca/opengl/gl_projectionmatrix.html
;;Above was wrong, it was OpenGL style, our matrices are DX style
;;Correct impl:
;;https://msdn.microsoft.com/en-us/library/windows/desktop/bb205347(v=vs.85).aspx

(defn m4-ortho "" [left right bottom top zNear zFar]
  (let [_11 (/ 2 (- right left))
        _22 (/ 2 (- top bottom))
        _33 (/ 1 (- zFar zNear))
        _41 (/ (+ left right) (- left right))
        _42 (/ (+ top bottom) (- bottom top))
        _43 (/ zNear (- zNear zFar))]
    (mx-new* 4 4
             #js [_11 0 0 0
                  0 _22 0 0
                  0 0 _33 0
                  _41 _42 _43 1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-decompose "" [rot1]
  (let [rot (mx-xpose rot1)
        [r1 r2 r3]
        (partition 3 rot)
        sy (sqrt* (+ (sqr* (nth r1 0))
                     (sqr* (nth r2 0))))
        singular? (< sy 1e-6)
        [x y z]
        (if-not singular?
          [(ATAN2 (nth r3 1)(nth r3 2))
           (ATAN2 (- (nth r3 0)) sy)
           (ATAN2 (nth r2 0) (nth r1 0))]
          [(ATAN2 (- (nth r2 2))
                  (nth r2 1))
           (ATAN2 (- (nth r3 0)) sy) 0])] (vec3 x y z)))

;;kenl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Size2D "" [w h] {:width w :height h})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line2D "" [a b] {:start a :end b})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Point2D "" [x y] (vec2 x y))

(defn- half* "" [v]
  (loop [[k & more] (keys v)
         out {}]
    (if-not (some? k)
      out
      (recur more (assoc out k (/ (get v k) 2))))))

(defn- p2d "" [& [x y]]
  (if (some? x)
    (if (number? y) (Point2D x y) x) (Point2D 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle
  "" [radius & [x y]] {:radius radius :pos (p2d x y)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn RectangleAA
  "" [width height & [x y]] {:origin (p2d x y) :size (Size2D width height)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Rectangle
  "" [width height & [x y]] {:origin (p2d x y)
                             :angle 0
                             :size (Size2D width height)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn BoundingShape "" []
  (atom {:circles [] :rectangles []}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Interval "" [& [minv maxv]]
  {:min (num?? minv 0) :max (num?? maxv 0)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(defn Point3D "" [& [x y z]] (vec3 (num?? x 0)
                                   (num?? y 0)(num?? z 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line3D "" [start end] {:start start :end end})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Ray
  "" [& [origin dir]]
  {:origin (or origin (Point3D 0 0 0))
   :dir (if (some? dir) (v3-unit dir) (vec3 0 0 1))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Sphere
  "" [& [pos radius]]
  {:radius (if (some? radius) radius 1)
   :pos (if (some? pos) pos (Point3D 0 0 0))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn AABB
  ;;Rectangle3D
  "half size" [& [pos size]]
  {:size (if (some? size) size (vec3 1 1 1))
   :pos (if (some? pos) pos (Point3D 0 0 0))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn OBB
  "half size" [& [pos size orient]]
  {:size (if (some? size) size (vec3 1 1 1))
   :orient (if (some? orient) orient (mat3))
   :pos (if (some? pos) pos (Point3D 0 0 0))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Plane
  "" [& [normal dist]]
  {:dist (if (number? dist) dist 0)
   :normal (if (some? normal) normal (vec3 1 0 0))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Triangle "" [& [a b c :as args]]
  {:size 3 :cells (if (empty? args)
                    #js [0 0 0 0 0 0 0 0 0]
                    #js [(:x a)(:y a)(:z a) (:x b)(:y b)(:z b) (:x c)(:y c)(:z c)])})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn BVHNode "" []
  {:bounds (AABB)
   :numTriangles 0
   :triangles nil
   :children []})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Mesh "" []
  {:numTriangles 0
   :accelerator nil ;BVHNOde
   ;union { Triangle* triangles; Point* vertices; float* values; };
   })

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Model "" []
  {:bounds (AABB)
   :parent nil :content nil :flag false :pos (Point3D) :rotation (vec3)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Frustum
  ;;top bottom left right _near _far
  {:planes #js [(Plane)(Plane)(Plane)(Plane)(Plane)(Plane)]})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn RaycastResult
  "" []
  {:point (vec3) :normal (vec3) :t 0 :hit? false})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn CollisionManifold "" []
  {:colliding? false :normal (vec3) :depth 0 :contacts []})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn l3-len "" [line]
  (let [{:keys [start end]} line] (v3-len (v3-sub start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn l3-lensq "" [line]
  (let [{:keys [start end]} line] (v3-lensq (v3-sub start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rayFromPoints
  "" [from to] (Ray from (v3-unit (v3-sub to from))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getMin3D "" [aabb]
  (let [{:keys [pos size]} aabb
        {x1 :x y1 :y z1 :z} (v3-add pos size)
        {x2 :x y2 :y z2 :z} (v3-sub pos size)]
    (vec3 (min x1 x2) (min y1 y2) (min z1 z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getMax3D "" [aabb]
  (let [{:keys [pos size]} aabb
        {x1 :x y1 :y z1 :z} (v3-add pos size)
        {x2 :x y2 :y z2 :z} (v3-sub pos size)]
    (vec3 (max x1 x2) (max y1 y2) (max z1 z2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aabbFromMinMax "" [min3 max3]
  (AABB (v3-scale (v3-add min3 max3) 0.5)
        (v3-scale (v3-sub max3 min3) 0.5)))


float PlaneEquation(const Point& point, const Plane& plane) {
  return Dot(point, plane.normal) - plane.distance;
}

#ifndef NO_EXTRAS
float PlaneEquation(const Plane& plane, const Point& point) {
  return Dot(point, plane.normal) - plane.distance;
}
#endif

#ifndef NO_EXTRAS
std::ostream& operator<<(std::ostream& os, const Line& shape) {
  os << "start: (" << shape.start.x << ", " << shape.start.y << ", " << shape.start.z << "), end: (";
  os << shape.end.x << ", " << shape.end.y << ", " << shape.end.z << ")";
  return os;
}

std::ostream& operator<<(std::ostream& os, const Ray& shape) {
  os << "origin: (" << shape.origin.x << ", " << shape.origin.y << ", " << shape.origin.z << "), ";
  os << "direction: (" << shape.direction.x << ", " << shape.direction.y << ", " << shape.direction.z << ")";
  return os;
}

std::ostream& operator<<(std::ostream& os, const Sphere& shape) {
  os << "position:" << shape.position.x << ", " << shape.position.y << ", " << shape.position.z << "), ";
  os << "radius: " << shape.radius;
  return os;
}

std::ostream& operator<<(std::ostream& os, const AABB& shape) {
  vec3 min = GetMin(shape);
  vec3 max = GetMax(shape);
  os << "min: (" << min.x << ", " << min.y << ", " << min.z << "), ";
  os << "max: (" << max.x << ", " << max.y << ", " << max.z << ")";
  return os;
}

std::ostream& operator<<(std::ostream& os, const Plane& shape) {
  os << "normal: (" << shape.normal.x << ", " << shape.normal.y << ", " << shape.normal.z << "), ";
  os << "distance: " << shape.distance;
  return os;
}

std::ostream& operator<<(std::ostream& os, const Triangle& shape) {
  os << "a: (" << shape.a.x << ", " << shape.a.y << ", " << shape.a.z << "), ";
  os << "b: (" << shape.b.x << ", " << shape.b.y << ", " << shape.b.z << "), ";
  os << "c: (" << shape.c.x << ", " << shape.c.y << ", " << shape.c.z << ")";
  return os;
}

std::ostream& operator<<(std::ostream& os, const OBB& shape) {
  os << "position:" << shape.position.x << ", " << shape.position.y << ", " << shape.position.z << "), ";
  os << "size:" << shape.size.x << ", " << shape.size.y << ", " << shape.size.z << "), ";
  os << "x basis:" << shape.orientation._11 << ", " << shape.orientation._21 << ", " << shape.orientation._31 << "), ";
  os << "y basis:" << shape.orientation._12 << ", " << shape.orientation._22 << ", " << shape.orientation._32 << "), ";
  os << "z basis:" << shape.orientation._13 << ", " << shape.orientation._23 << ", " << shape.orientation._33 << ")";
  return os;
}
#endif

bool PointInSphere(const Point& point, const Sphere& sphere) {
  return MagnitudeSq(point - sphere.position) < sphere.radius * sphere.radius;
}

bool PointOnPlane(const Point& point, const Plane& plane) {
  // This should probably use an epsilon!
  //return Dot(point, plane.normal) - plane.distance == 0.0f;

  return CMP(Dot(point, plane.normal) - plane.distance, 0.0f);
}

bool PointInAABB(const Point& point, const AABB& aabb) {
  Point min = GetMin(aabb);
  Point max = GetMax(aabb);

  if (point.x < min.x || point.y < min.y || point.z < min.z) {
    return false;
  }
  if (point.x > max.x || point.y > max.y || point.z > max.z) {
    return false;
  }

  return true;
}

bool PointInOBB(const Point& point, const OBB& obb) {
  vec3 dir = point - obb.position;

  for (int i = 0; i < 3; ++i) {
    const float* orientation = &obb.orientation.asArray[i * 3];
    vec3 axis(orientation[0], orientation[1], orientation[2]);

    float distance = Dot(dir, axis);

    if (distance > obb.size.asArray[i]) {
      return false;
    }
    if (distance < -obb.size.asArray[i]) {
      return false;
    }
  }

  return true;
}

Point ClosestPoint(const Sphere& sphere, const Point& point) {
  vec3 sphereToPoint = point - sphere.position;
  Normalize(sphereToPoint);
  sphereToPoint = sphereToPoint * sphere.radius;
  return sphereToPoint + sphere.position;
}

Point ClosestPoint(const AABB& aabb, const Point& point) {
  Point result = point;
  Point min = GetMin(aabb);
  Point max = GetMax(aabb);

  result.x = (result.x < min.x) ? min.x : result.x;
  result.y = (result.y < min.x) ? min.y : result.y;
  result.z = (result.z < min.x) ? min.z : result.z;

  result.x = (result.x > max.x) ? max.x : result.x;
  result.y = (result.y > max.x) ? max.y : result.y;
  result.z = (result.z > max.x) ? max.z : result.z;

  return result;
}

Point ClosestPoint(const OBB& obb, const Point& point) {
  Point result = obb.position;
  vec3 dir = point - obb.position;

  for (int i = 0; i < 3; ++i) {
    const float* orientation = &obb.orientation.asArray[i * 3];
    vec3 axis(orientation[0], orientation[1], orientation[2]);

    float distance = Dot(dir, axis);

    if (distance > obb.size.asArray[i]) {
      distance = obb.size.asArray[i];
    }
    if (distance < -obb.size.asArray[i]) {
      distance = -obb.size.asArray[i];
    }

    result = result + (axis * distance);
  }

  return result;
}

Point ClosestPoint(const Plane& plane, const Point& point) {
  // This works assuming plane.Normal is normalized, which it should be
  float distance = Dot(plane.normal, point) - plane.distance;
  // If the plane normal wasn't normalized, we'd need this:
  // distance = distance / DOT(plane.Normal, plane.Normal);

  return point - plane.normal * distance;
}

bool PointOnLine(const Point& point, const Line& line) {
  Point closest = ClosestPoint(line, point);
  float distanceSq = MagnitudeSq(closest - point);
  return CMP(distanceSq, 0.0f);
}

Point ClosestPoint(const Line& line, const Point& point) {
  vec3 lVec = line.end - line.start; // Line Vector
  // Project "point" onto the "Line Vector", computing:
  // closest(t) = start + t * (end - start)
  // T is how far along the line the projected point is
  float t = Dot(point - line.start, lVec) / Dot(lVec, lVec);
  // Clamp t to the 0 to 1 range
  t = fmaxf(t, 0.0f);
  t = fminf(t, 1.0f);
  // Return projected position of t
  return line.start + lVec * t;
}

bool PointOnRay(const Point& point, const Ray& ray) {
  if (point == ray.origin) {
    return true;
  }

  vec3 norm = point - ray.origin;
  Normalize(norm);
  float diff = Dot(norm, ray.direction); // Direction is normalized
  // If BOTH vectors point in the same direction, diff should be 1
  return CMP(diff, 1.0f);
}

Point ClosestPoint(const Ray& ray, const Point& point) {
  // Project point onto ray,
  float t = Dot(point - ray.origin, ray.direction);
  // Not needed if direction is normalized!
  // t /= Dot(ray.direction, ray.direction);

  // We only want to clamp t in the positive direction.
  // The ray extends infinatley in this direction!
  t = fmaxf(t, 0.0f);

  // Compute the projected position from the clamped t
  // Notice we multiply r.Normal by t, not AB.
  // This is becuase we want the ray in the direction
  // of the normal, which technically the line segment is
  // but this is much more explicit and easy to read.
  return Point(ray.origin + ray.direction * t);
}

#ifndef NO_EXTRAS
bool PointInPlane(const Point& point, const Plane& plane) {
  return PointOnPlane(point, plane);
}
bool PointInLine(const Point& point, const Line& line) {
  return PointOnLine(point, line);
}
bool PointInRay(const Point& point, const Ray& ray) {
  return PointOnRay(point, ray);
}
bool ContainsPoint(const Sphere& sphere, const Point& point) {
  return PointInSphere(point, sphere);
}
bool ContainsPoint(const Point& point, const Sphere& sphere) {
  return PointInSphere(point, sphere);
}
bool ContainsPoint(const AABB& aabb, const Point& point) {
  return PointInAABB(point, aabb);
}
bool ContainsPoint(const Point& point, const AABB& aabb) {
  return PointInAABB(point, aabb);
}
bool ContainsPoint(const Point& point, const OBB& obb) {
  return PointInOBB(point, obb);
}
bool ContainsPoint(const OBB& obb, const Point& point) {
  return PointInOBB(point, obb);
}
bool ContainsPoint(const Point& point, const Plane& plane) {
  return PointOnPlane(point, plane);
}
bool ContainsPoint(const Plane& plane, const Point& point) {
  return PointOnPlane(point, plane);
}
bool ContainsPoint(const Point& point, const Line& line) {
  return PointOnLine(point, line);
}
bool ContainsPoint(const Line& line, const Point& point) {
  return PointOnLine(point, line);
}
bool ContainsPoint(const Point& point, const Ray& ray) {
  return PointOnRay(point, ray);
}
bool ContainsPoint(const Ray& ray, const Point& point) {
  return PointOnRay(point, ray);
}
Point ClosestPoint(const Point& point, const Sphere& sphere) {
  return ClosestPoint(sphere, point);
}
Point ClosestPoint(const Point& point, const AABB& aabb) {
  return ClosestPoint(aabb, point);
}
Point ClosestPoint(const Point& point, const OBB& obb) {
  return ClosestPoint(obb, point);
}
Point ClosestPoint(const Point& point, const Plane& plane) {
  return ClosestPoint(plane, point);
}
Point ClosestPoint(const Point& point, const Line& line) {
  return ClosestPoint(line, point);
}
Point ClosestPoint(const Point& point, const Ray& ray) {
  return ClosestPoint(ray, point);
}
Point ClosestPoint(const Point& p, const Triangle& t) {
  return ClosestPoint(t, p);
}
#endif

bool SphereSphere(const Sphere& s1, const Sphere& s2) {
  float radiiSum = s1.radius + s2.radius;
  float sqDistance = MagnitudeSq(s1.position - s2.position);
  return sqDistance < radiiSum * radiiSum;
}

bool SphereAABB(const Sphere& sphere, const AABB& aabb) {
  Point closestPoint = ClosestPoint(aabb, sphere.position);
  float distSq = MagnitudeSq(sphere.position - closestPoint);
  float radiusSq = sphere.radius * sphere.radius;
  return distSq < radiusSq;
}

bool SphereOBB(const Sphere& sphere, const OBB& obb) {
  Point closestPoint = ClosestPoint(obb, sphere.position);
  float distSq = MagnitudeSq(sphere.position - closestPoint);
  float radiusSq = sphere.radius * sphere.radius;
  return distSq < radiusSq;
}

bool SpherePlane(const Sphere& sphere, const Plane& plane) {
  Point closestPoint = ClosestPoint(plane, sphere.position);
  float distSq = MagnitudeSq(sphere.position - closestPoint);
  float radiusSq = sphere.radius * sphere.radius;
  return distSq < radiusSq;
}

bool AABBAABB(const AABB& aabb1, const AABB& aabb2) {
  Point aMin = GetMin(aabb1);
  Point aMax = GetMax(aabb1);
  Point bMin = GetMin(aabb2);
  Point bMax = GetMax(aabb2);

  return  (aMin.x <= bMax.x && aMax.x >= bMin.x) &&
      (aMin.y <= bMax.y && aMax.y >= bMin.y) &&
      (aMin.z <= bMax.z && aMax.z >= bMin.z);
}

bool AABBOBB(const AABB& aabb, const OBB& obb) {
  const float* o = obb.orientation.asArray;

  vec3 test[15] = {
    vec3(1, 0, 0), // AABB axis 1
    vec3(0, 1, 0), // AABB axis 2
    vec3(0, 0, 1), // AABB axis 3
    vec3(o[0], o[1], o[2]),
    vec3(o[3], o[4], o[5]),
    vec3(o[6], o[7], o[8])
  };

  for (int i = 0; i < 3; ++i) { // Fill out rest of axis
    test[6 + i * 3 + 0] = Cross(test[i], test[0]);
    test[6 + i * 3 + 1] = Cross(test[i], test[1]);
    test[6 + i * 3 + 2] = Cross(test[i], test[2]);
  }

  for (int i = 0; i < 15; ++i) {
    if (!OverlapOnAxis(aabb, obb, test[i])) {
      return false; // Seperating axis found
    }
  }

  return true; // Seperating axis not found
}

bool OverlapOnAxis(const AABB& aabb, const OBB& obb, const vec3& axis) {
  Interval a = GetInterval(aabb, axis);
  Interval b = GetInterval(obb, axis);
  return ((b.min <= a.max) && (a.min <= b.max));
}

bool OverlapOnAxis(const OBB& obb1, const OBB& obb2, const vec3& axis) {
  Interval a = GetInterval(obb1, axis);
  Interval b = GetInterval(obb1, axis);
  return ((b.min <= a.max) && (a.min <= b.max));
}

bool OverlapOnAxis(const AABB& aabb, const Triangle& triangle, const vec3& axis) {
  Interval a = GetInterval(aabb, axis);
  Interval b = GetInterval(triangle, axis);
  return ((b.min <= a.max) && (a.min <= b.max));
}

bool OverlapOnAxis(const OBB& obb, const Triangle& triangle, const vec3& axis) {
  Interval a = GetInterval(obb, axis);
  Interval b = GetInterval(triangle, axis);
  return ((b.min <= a.max) && (a.min <= b.max));
}

bool OverlapOnAxis(const Triangle& t1, const Triangle& t2, const vec3& axis) {
  Interval a = GetInterval(t1, axis);
  Interval b = GetInterval(t2, axis);
  return ((b.min <= a.max) && (a.min <= b.max));
}

Interval GetInterval(const Triangle& triangle, const vec3& axis) {
  Interval result;

  result.min = Dot(axis, triangle.points[0]);
  result.max = result.min;
  for (int i = 1; i < 3; ++i) {
    float value = Dot(axis, triangle.points[i]);
    result.min = fminf(result.min, value);
    result.max = fmaxf(result.max, value);
  }

  return result;
}

Interval GetInterval(const OBB& obb, const vec3& axis) {
  vec3 vertex[8];

  vec3 C = obb.position;  // OBB Center
  vec3 E = obb.size;    // OBB Extents
  const float* o = obb.orientation.asArray;
  vec3 A[] = {      // OBB Axis
    vec3(o[0], o[1], o[2]),
    vec3(o[3], o[4], o[5]),
    vec3(o[6], o[7], o[8]),
  };

  vertex[0] = C + A[0] * E[0] + A[1] * E[1] + A[2] * E[2];
  vertex[1] = C - A[0] * E[0] + A[1] * E[1] + A[2] * E[2];
  vertex[2] = C + A[0] * E[0] - A[1] * E[1] + A[2] * E[2];
  vertex[3] = C + A[0] * E[0] + A[1] * E[1] - A[2] * E[2];
  vertex[4] = C - A[0] * E[0] - A[1] * E[1] - A[2] * E[2];
  vertex[5] = C + A[0] * E[0] - A[1] * E[1] - A[2] * E[2];
  vertex[6] = C - A[0] * E[0] + A[1] * E[1] - A[2] * E[2];
  vertex[7] = C - A[0] * E[0] - A[1] * E[1] + A[2] * E[2];

  Interval result;
  result.min = result.max = Dot(axis, vertex[0]);

  for (int i = 1; i < 8; ++i) {
    float projection = Dot(axis, vertex[i]);
    result.min = (projection < result.min) ? projection : result.min;
    result.max = (projection > result.max) ? projection : result.max;
  }

  return result;
}

Interval GetInterval(const AABB& aabb, const vec3& axis) {
  vec3 i = GetMin(aabb);
  vec3 a = GetMax(aabb);

  vec3 vertex[8] = {
    vec3(i.x, a.y, a.z),
    vec3(i.x, a.y, i.z),
    vec3(i.x, i.y, a.z),
    vec3(i.x, i.y, i.z),
    vec3(a.x, a.y, a.z),
    vec3(a.x, a.y, i.z),
    vec3(a.x, i.y, a.z),
    vec3(a.x, i.y, i.z)
  };

  Interval result;
  result.min = result.max = Dot(axis, vertex[0]);

  for (int i = 1; i < 8; ++i) {
    float projection = Dot(axis, vertex[i]);
    result.min = (projection < result.min) ? projection : result.min;
    result.max = (projection > result.max) ? projection : result.max;
  }

  return result;
}

bool AABBPlane(const AABB& aabb, const Plane& plane) {
  // Project the half extents of the AABB onto the plane normal
  float pLen =aabb.size.x * fabsf(plane.normal.x) +
        aabb.size.y * fabsf(plane.normal.y) +
        aabb.size.z * fabsf(plane.normal.z);
  // Find the distance from the center of the AABB to the plane
  float dist = Dot(plane.normal, aabb.position) - plane.distance;
  // Intersection occurs if the distance falls within the projected side
  return fabsf(dist) <= pLen;
}

bool OBBOBB(const OBB& obb1, const OBB& obb2) {
  const float* o1 = obb1.orientation.asArray;
  const float* o2 = obb2.orientation.asArray;

  vec3 test[15] = {
    vec3(o1[0], o1[1], o1[2]),
    vec3(o1[3], o1[4], o1[5]),
    vec3(o1[6], o1[7], o1[8]),
    vec3(o2[0], o2[1], o2[2]),
    vec3(o2[3], o2[4], o2[5]),
    vec3(o2[6], o2[7], o2[8])
  };

  for (int i = 0; i < 3; ++i) { // Fill out rest of axis
    test[6 + i * 3 + 0] = Cross(test[i], test[0]);
    test[6 + i * 3 + 1] = Cross(test[i], test[1]);
    test[6 + i * 3 + 2] = Cross(test[i], test[2]);
  }

  for (int i = 0; i < 15; ++i) {
    if (!OverlapOnAxis(obb1, obb2, test[i])) {
      return false; // Seperating axis found
    }
  }

  return true; // Seperating axis not found
}

bool OBBPlane(const OBB& obb, const Plane& plane) {
  // Local variables for readability only
  const float* o = obb.orientation.asArray;
  vec3 rot[] = { // rotation / orientation
    vec3(o[0], o[1], o[2]),
    vec3(o[3], o[4], o[5]),
    vec3(o[6], o[7], o[8]),
  };
  vec3 normal = plane.normal;

  // Project the half extents of the AABB onto the plane normal
  float pLen =obb.size.x * fabsf(Dot(normal, rot[0])) +
        obb.size.y * fabsf(Dot(normal, rot[1])) +
        obb.size.z * fabsf(Dot(normal, rot[2]));
  // Find the distance from the center of the OBB to the plane
  float dist = Dot(plane.normal, obb.position) - plane.distance;
  // Intersection occurs if the distance falls within the projected side
  return fabsf(dist) <= pLen;
}

bool PlanePlane(const Plane& plane1, const Plane& plane2) {
  // Compute direction of intersection line
  vec3 d = Cross(plane1.normal, plane2.normal);

  // Check the length of the direction line
  // if the length is 0, no intersection happened
  return !(CMP(Dot(d, d), 0));

  // We could have used the dot product here, instead of the cross product
}

bool Raycast(const Sphere& sphere, const Ray& ray, RaycastResult* outResult) {
  ResetRaycastResult(outResult);

  vec3 e = sphere.position - ray.origin;
  float rSq = sphere.radius * sphere.radius;

  float eSq = MagnitudeSq(e);
  float a = Dot(e, ray.direction); // ray.direction is assumed to be normalized
  float bSq = /*sqrtf(*/eSq - (a * a)/*)*/;
  float f = sqrt(fabsf((rSq)- /*(b * b)*/bSq));

  // Assume normal intersection!
  float t = a - f;

  // No collision has happened
  if (rSq - (eSq - a * a) < 0.0f) {
    return false;
  }
  // Ray starts inside the sphere
  else if (eSq < rSq) {
    // Just reverse direction
    t = a + f;
  }
  if (outResult != 0) {
    outResult->t = t;
    outResult->hit = true;
    outResult->point = ray.origin + ray.direction * t;
    outResult->normal = Normalized(outResult->point - sphere.position);
  }
  return true;
}

bool Raycast(const OBB& obb, const Ray& ray, RaycastResult* outResult) {
  ResetRaycastResult(outResult);

  const float* o = obb.orientation.asArray;
  const float* size = obb.size.asArray;

  vec3 p = obb.position - ray.origin;

  vec3 X(o[0], o[1], o[2]);
  vec3 Y(o[3], o[4], o[5]);
  vec3 Z(o[6], o[7], o[8]);

  vec3 f(
    Dot(X, ray.direction),
    Dot(Y, ray.direction),
    Dot(Z, ray.direction)
  );

  vec3 e(
    Dot(X, p),
    Dot(Y, p),
    Dot(Z, p)
  );

#if 1
  float t[6] = { 0, 0, 0, 0, 0, 0 };
  for (int i = 0; i < 3; ++i) {
    if (CMP(f[i], 0)) {
      if (-e[i] - size[i] > 0 || -e.x + size[i] < 0) {
        return false;
      }
      f[i] = 0.00001f; // Avoid div by 0!
    }

    t[i * 2 + 0] = (e[i] + size[i]) / f[i]; // tmin[x, y, z]
    t[i * 2 + 1] = (e[i] - size[i]) / f[i]; // tmax[x, y, z]
  }

  float tmin = fmaxf(fmaxf(fminf(t[0], t[1]), fminf(t[2], t[3])), fminf(t[4], t[5]));
  float tmax = fminf(fminf(fmaxf(t[0], t[1]), fmaxf(t[2], t[3])), fmaxf(t[4], t[5]));
#else
  // The above loop simplifies the below if statements
  // this is done to make sure the sample fits into the book
  if (CMP(f.x, 0)) {
    if (-e.x - obb.size.x > 0 || -e.x + obb.size.x < 0) {
      return -1;
    }
    f.x = 0.00001f; // Avoid div by 0!
  }
  else if (CMP(f.y, 0)) {
    if (-e.y - obb.size.y > 0 || -e.y + obb.size.y < 0) {
      return -1;
    }
    f.y = 0.00001f; // Avoid div by 0!
  }
  else if (CMP(f.z, 0)) {
    if (-e.z - obb.size.z > 0 || -e.z + obb.size.z < 0) {
      return -1;
    }
    f.z = 0.00001f; // Avoid div by 0!
  }

  float t1 = (e.x + obb.size.x) / f.x;
  float t2 = (e.x - obb.size.x) / f.x;
  float t3 = (e.y + obb.size.y) / f.y;
  float t4 = (e.y - obb.size.y) / f.y;
  float t5 = (e.z + obb.size.z) / f.z;
  float t6 = (e.z - obb.size.z) / f.z;

  float tmin = fmaxf(fmaxf(fminf(t1, t2), fminf(t3, t4)), fminf(t5, t6));
  float tmax = fminf(fminf(fmaxf(t1, t2), fmaxf(t3, t4)), fmaxf(t5, t6));
#endif

  // if tmax < 0, ray is intersecting AABB
  // but entire AABB is behing it's origin
  if (tmax < 0) {
    return false;
  }

  // if tmin > tmax, ray doesn't intersect AABB
  if (tmin > tmax) {
    return false;
  }

  // If tmin is < 0, tmax is closer
  float t_result = tmin;

  if (tmin < 0.0f) {
    t_result = tmax;
  }

  if (outResult != 0) {
    outResult->hit = true;
    outResult->t = t_result;
    outResult->point = ray.origin + ray.direction * t_result;

    vec3 normals[] = {
      X,      // +x
      X * -1.0f,  // -x
      Y,      // +y
      Y * -1.0f,  // -y
      Z,      // +z
      Z * -1.0f // -z
    };

    for (int i = 0; i < 6; ++i) {
      if (CMP(t_result, t[i])) {
        outResult->normal = Normalized(normals[i]);
      }
    }
  }
  return true;
}

void ResetRaycastResult(RaycastResult* outResult) {
  if (outResult != 0) {
    outResult->t = -1;
    outResult->hit = false;
    outResult->normal = vec3(0, 0, 1);
    outResult->point = vec3(0, 0, 0);
  }
}

bool Raycast(const AABB& aabb, const Ray& ray, RaycastResult* outResult) {
  ResetRaycastResult(outResult);

  vec3 min = GetMin(aabb);
  vec3 max = GetMax(aabb);

  // Any component of direction could be 0!
  // Address this by using a small number, close to
  // 0 in case any of directions components are 0
  float t1 = (min.x - ray.origin.x) / (CMP(ray.direction.x, 0.0f) ? 0.00001f : ray.direction.x);
  float t2 = (max.x - ray.origin.x) / (CMP(ray.direction.x, 0.0f) ? 0.00001f : ray.direction.x);
  float t3 = (min.y - ray.origin.y) / (CMP(ray.direction.y, 0.0f) ? 0.00001f : ray.direction.y);
  float t4 = (max.y - ray.origin.y) / (CMP(ray.direction.y, 0.0f) ? 0.00001f : ray.direction.y);
  float t5 = (min.z - ray.origin.z) / (CMP(ray.direction.z, 0.0f) ? 0.00001f : ray.direction.z);
  float t6 = (max.z - ray.origin.z) / (CMP(ray.direction.z, 0.0f) ? 0.00001f : ray.direction.z);

  float tmin = fmaxf(fmaxf(fminf(t1, t2), fminf(t3, t4)), fminf(t5, t6));
  float tmax = fminf(fminf(fmaxf(t1, t2), fmaxf(t3, t4)), fmaxf(t5, t6));

  // if tmax < 0, ray is intersecting AABB
  // but entire AABB is behing it's origin
  if (tmax < 0) {
    return false;
  }

  // if tmin > tmax, ray doesn't intersect AABB
  if (tmin > tmax) {
    return false;
  }

  float t_result = tmin;

  // If tmin is < 0, tmax is closer
  if (tmin < 0.0f) {
    t_result = tmax;
  }

  if (outResult != 0) {
    outResult->t = t_result;
    outResult->hit = true;
    outResult->point = ray.origin + ray.direction * t_result;

    vec3 normals[] = {
      vec3(-1, 0, 0),
      vec3(1, 0, 0),
      vec3(0, -1, 0),
      vec3(0, 1, 0),
      vec3(0, 0, -1),
      vec3(0, 0, 1)
    };
    float t[] = { t1, t2, t3, t4, t5, t6 };

    for (int i = 0; i < 6; ++i) {
      if (CMP(t_result, t[i])) {
        outResult->normal = normals[i];
      }
    }
  }

  return true;
}

bool Raycast(const Plane& plane, const Ray& ray, RaycastResult* outResult) {
  ResetRaycastResult(outResult);

  float nd = Dot(ray.direction, plane.normal);
  float pn = Dot(ray.origin, plane.normal);

  // nd must be negative, and not 0
  // if nd is positive, the ray and plane normals
  // point in the same direction. No intersection.
  if (nd >= 0.0f) {
    return false;
  }

  float t = (plane.distance - pn) / nd;

  // t must be positive
  if (t >= 0.0f) {
    if (outResult != 0) {
      outResult->t = t;
      outResult->hit = true;
      outResult->point = ray.origin + ray.direction * t;
      outResult->normal = Normalized(plane.normal);
    }
    return true;
  }

  return false;
}

bool Linetest(const Sphere& sphere, const Line& line) {
  Point closest = ClosestPoint(line, sphere.position);
  float distSq = MagnitudeSq(sphere.position - closest);
  return distSq <= (sphere.radius * sphere.radius);
}

bool Linetest(const Plane& plane, const Line& line) {
  vec3 ab = line.end - line.start;

  float nA = Dot(plane.normal, line.start);
  float nAB = Dot(plane.normal, ab);

  if (CMP(nAB, 0)) {
    return false;
  }

  float t = (plane.distance - nA) / nAB;
  return t >= 0.0f && t <= 1.0f;
}

bool Linetest(const AABB& aabb, const Line& line) {
  Ray ray;
  ray.origin = line.start;
  ray.direction = Normalized(line.end - line.start);
  RaycastResult raycast;
  if (!Raycast(aabb, ray, &raycast)) {
    return false;
  }
  float t = raycast.t;

  return t >= 0 && t * t <= LengthSq(line);
}

bool Linetest(const OBB& obb, const Line& line) {
  if (MagnitudeSq(line.end - line.start) < 0.0000001f) {
    return PointInOBB(line.start, obb);
  }
  Ray ray;
  ray.origin = line.start;
  ray.direction = Normalized(line.end - line.start);
  RaycastResult result;
  if (!Raycast(obb, ray, &result)) {
    return false;
  }
  float t = result.t;

  return t >= 0 && t * t <= LengthSq(line);
}

#ifndef NO_EXTRAS
bool Raycast(const Ray& ray, const Sphere& sphere, RaycastResult* outResult) {
  return Raycast(sphere, ray, outResult);
}

bool Raycast(const Ray& ray, const AABB& aabb, RaycastResult* outResult) {
  return Raycast(aabb, ray, outResult);
}

bool Raycast(const Ray& ray, const OBB& obb, RaycastResult* outResult) {
  return Raycast(obb, ray, outResult);
}

bool Raycast(const Ray& ray, const Plane& plane, RaycastResult* outResult) {
  return Raycast(plane, ray, outResult);
}

bool Linetest(const Line& line, const Sphere& sphere) {
  return Linetest(sphere, line);
}

bool Linetest(const Line& line, const AABB& aabb) {
  return Linetest(aabb, line);
}

bool Linetest(const Line& line, const OBB& obb) {
  return Linetest(obb, line);
}

bool Linetest(const Line& line, const Plane& plane) {
  return Linetest(plane, line);
}

vec3 Centroid(const Triangle& t) {
  vec3 result;
  result.x = t.a.x + t.b.x + t.c.x;
  result.y = t.a.y + t.b.y + t.c.y;
  result.z = t.a.z + t.b.z + t.c.z;
  result = result * (1.0f / 3.0f);
  return result;
}
#endif

bool PointInTriangle(const Point& p, const Triangle& t) {
  // Move the triangle so that the point is
  // now at the origin of the triangle
  vec3 a = t.a - p;
  vec3 b = t.b - p;
  vec3 c = t.c - p;

  // The point should be moved too, so they are both
  // relative, but because we don't use p in the
  // equation anymore, we don't need it!
  // p -= p; // This would just equal the zero vector!

  vec3 normPBC = Cross(b, c); // Normal of PBC (u)
  vec3 normPCA = Cross(c, a); // Normal of PCA (v)
  vec3 normPAB = Cross(a, b); // Normal of PAB (w)

  // Test to see if the normals are facing
  // the same direction, return false if not
  if (Dot(normPBC, normPCA) < 0.0f) {
    return false;
  }
  else if (Dot(normPBC, normPAB) < 0.0f) {
    return false;
  }

  // All normals facing the same way, return true
  return true;
}

#ifndef NO_EXTRAS
vec3 BarycentricOptimized(const Point& p, const Triangle& t) {
  vec3 v0 = t.b - t.a;
  vec3 v1 = t.c - t.a;
  vec3 v2 = p - t.a;

  float d00 = Dot(v0, v0);
  float d01 = Dot(v0, v1);
  float d11 = Dot(v1, v1);
  float d20 = Dot(v2, v0);
  float d21 = Dot(v2, v1);
  float denom = d00 * d11 - d01 * d01;

  if (CMP(denom, 0.0f)) {
    return vec3();
  }

  vec3 result;
  result.y = (d11 * d20 - d01 * d21) / denom;
  result.z = (d00 * d21 - d01 * d20) / denom;
  result.x = 1.0f - result.y - result.z;

  return result;
}
#endif

vec3 Barycentric(const Point& p, const Triangle& t) {
  vec3 ap = p - t.a;
  vec3 bp = p - t.b;
  vec3 cp = p - t.c;

  vec3 ab = t.b - t.a;
  vec3 ac = t.c - t.a;
  vec3 bc = t.c - t.b;
  vec3 cb = t.b - t.c;
  vec3 ca = t.a - t.c;

  vec3 v = ab - Project(ab, cb);
  float a = 1.0f - (Dot(v, ap) / Dot(v, ab));

  v = bc - Project(bc, ac);
  float b = 1.0f - (Dot(v, bp) / Dot(v, bc));

  v = ca - Project(ca, ab);
  float c = 1.0f - (Dot(v, cp) / Dot(v, ca));

#ifdef DO_SANITY_TESTS
#ifndef NO_EXTRAS
  vec3 barycentric = BarycentricOptimized(p, t);
  if (fabsf(a - barycentric.x) > 0.1f) {
    std::cout << "Expected a: " << a << " to be: " << barycentric.x << "\n";
  }
  if (fabsf(b - barycentric.y) > 0.1f) {
    std::cout << "Expected b: " << b << " to be: " << barycentric.y << "\n";
  }
  if (fabsf(c - barycentric.z) > 0.1f) {
    std::cout << "Expected c: " << c << " to be: " << barycentric.z << "\n";
  }
#endif
#endif

  return vec3(a, b, c);
}

Plane FromTriangle(const Triangle& t) {
  Plane result;
  result.normal = Normalized(Cross(t.b - t.a, t.c - t.a));
  result.distance = Dot(result.normal, t.a);
  return result;
}

Point ClosestPoint(const Triangle& t, const Point& p) {
  Plane plane = FromTriangle(t);
  Point closest = ClosestPoint(plane, p);

  // Closest point was inside triangle
  if (PointInTriangle(closest, t)) {
    return closest;
  }

  Point c1 = ClosestPoint(Line(t.a, t.b), closest); // Line AB
  Point c2 = ClosestPoint(Line(t.b, t.c), closest); // Line BC
  Point c3 = ClosestPoint(Line(t.c, t.a), closest); // Line CA

  float magSq1 = MagnitudeSq(closest - c1);
  float magSq2 = MagnitudeSq(closest - c2);
  float magSq3 = MagnitudeSq(closest - c3);

  if (magSq1 < magSq2 && magSq1 < magSq3) {
    return c1;
  }
  else if (magSq2 < magSq1 && magSq2 < magSq3) {
    return c2;
  }

  return c3;
}

bool TriangleSphere(const Triangle& t, const Sphere& s) {
  Point closest = ClosestPoint(t, s.position);
  float magSq = MagnitudeSq(closest - s.position);
  return magSq <= s.radius * s.radius;
}

bool TriangleAABB(const Triangle& t, const AABB& a) {
  // Compute the edge vectors of the triangle  (ABC)
  vec3 f0 = t.b - t.a;
  vec3 f1 = t.c - t.b;
  vec3 f2 = t.a - t.c;

  // Compute the face normals of the AABB
  vec3 u0(1.0f, 0.0f, 0.0f);
  vec3 u1(0.0f, 1.0f, 0.0f);
  vec3 u2(0.0f, 0.0f, 1.0f);

  vec3 test[13] = {
    // 3 Normals of AABB
    u0, // AABB Axis 1
    u1, // AABB Axis 2
    u2, // AABB Axis 3
    // 1 Normal of the Triangle
    Cross(f0, f1),
    // 9 Axis, cross products of all edges
    Cross(u0, f0),
    Cross(u0, f1),
    Cross(u0, f2),
    Cross(u1, f0),
    Cross(u1, f1),
    Cross(u1, f2),
    Cross(u2, f0),
    Cross(u2, f1),
    Cross(u2, f2)
  };

  for (int i = 0; i < 13; ++i) {
    if (!OverlapOnAxis(a, t, test[i])) {
      return false; // Seperating axis found
    }
  }

  return true; // Seperating axis not found
}

bool TriangleOBB(const Triangle& t, const OBB& o) {
  // Compute the edge vectors of the triangle  (ABC)
  vec3 f0 = t.b - t.a;
  vec3 f1 = t.c - t.b;
  vec3 f2 = t.a - t.c;

  // Compute the face normals of the AABB
  const float* orientation = o.orientation.asArray;
  vec3 u0(orientation[0], orientation[1], orientation[2]);
  vec3 u1(orientation[3], orientation[4], orientation[5]);
  vec3 u2(orientation[6], orientation[7], orientation[8]);

  vec3 test[13] = {
    // 3 Normals of AABB
    u0, // AABB Axis 1
    u1, // AABB Axis 2
    u2, // AABB Axis 3
    // 1 Normal of the Triangle
    Cross(f0, f1),
    // 9 Axis, cross products of all edges
    Cross(u0, f0),
    Cross(u0, f1),
    Cross(u0, f2),
    Cross(u1, f0),
    Cross(u1, f1),
    Cross(u1, f2),
    Cross(u2, f0),
    Cross(u2, f1),
    Cross(u2, f2)
  };

  for (int i = 0; i < 13; ++i) {
    if (!OverlapOnAxis(o, t, test[i])) {
      return false; // Seperating axis found
    }
  }

  return true; // Seperating axis not found
}

bool TriangleTriangle(const Triangle& t1, const Triangle& t2) {
#if 0
  vec3 axisToTest[] = {
    // Triangle 1, Normal
    SatCrossEdge(t1.a, t1.b, t1.b, t1.c),
    // Triangle 2, Normal
    SatCrossEdge(t2.a, t2.b, t2.b, t2.c),

    // Cross Product of edges
    SatCrossEdge(t2.a, t2.b, t1.a, t1.b),
    SatCrossEdge(t2.a, t2.b, t1.b, t1.c),
    SatCrossEdge(t2.a, t2.b, t1.c, t1.a),

    SatCrossEdge(t2.b, t2.c, t1.a, t1.b),
    SatCrossEdge(t2.b, t2.c, t1.b, t1.c),
    SatCrossEdge(t2.b, t2.c, t1.c, t1.a),

    SatCrossEdge(t2.c, t2.a, t1.a, t1.b),
    SatCrossEdge(t2.c, t2.a, t1.b, t1.c),
    SatCrossEdge(t2.c, t2.a, t1.c, t1.a),
  };
#else
  vec3 t1_f0 = t1.b - t1.a; // Edge 0
  vec3 t1_f1 = t1.c - t1.b; // Edge 1
  vec3 t1_f2 = t1.a - t1.c; // Edge 2

  vec3 t2_f0 = t2.b - t2.a; // Edge 0
  vec3 t2_f1 = t2.c - t2.b; // Edge 1
  vec3 t2_f2 = t2.a - t2.c; // Edge 2

  vec3 axisToTest[] = {
    // Triangle 1, Normal
    Cross(t1_f0, t1_f1),
    // Triangle 2, Normal
    Cross(t2_f0, t2_f1),

    // Cross Product of edges
    Cross(t2_f0, t1_f0),
    Cross(t2_f0, t1_f1),
    Cross(t2_f0, t1_f2),

    Cross(t2_f1, t1_f0),
    Cross(t2_f1, t1_f1),
    Cross(t2_f1, t1_f2),

    Cross(t2_f2, t1_f0),
    Cross(t2_f2, t1_f1),
    Cross(t2_f2, t1_f2),
  };
#endif

  for (int i = 0; i < 11; ++i) {
    if (!OverlapOnAxis(t1, t2, axisToTest[i])) {
      return false; // Seperating axis found
    }
  }

  return true; // Seperating axis not found
}

bool TriangleTriangleRobust(const Triangle& t1, const Triangle& t2) {
  vec3 axisToTest[] = {
    // Triangle 1, Normal
    SatCrossEdge(t1.a, t1.b, t1.b, t1.c),
    // Triangle 2, Normal
    SatCrossEdge(t2.a, t2.b, t2.b, t2.c),

    // Cross Product of edges
    SatCrossEdge(t2.a, t2.b, t1.a, t1.b),
    SatCrossEdge(t2.a, t2.b, t1.b, t1.c),
    SatCrossEdge(t2.a, t2.b, t1.c, t1.a),

    SatCrossEdge(t2.b, t2.c, t1.a, t1.b),
    SatCrossEdge(t2.b, t2.c, t1.b, t1.c),
    SatCrossEdge(t2.b, t2.c, t1.c, t1.a),

    SatCrossEdge(t2.c, t2.a, t1.a, t1.b),
    SatCrossEdge(t2.c, t2.a, t1.b, t1.c),
    SatCrossEdge(t2.c, t2.a, t1.c, t1.a),
  };

  for (int i = 0; i < 11; ++i) {
    if (!OverlapOnAxis(t1, t2, axisToTest[i])) {
      if (!CMP(MagnitudeSq(axisToTest[i]), 0)) {
        return false; // Seperating axis found
      }
    }
  }

  return true; // Seperating axis not found
}

vec3 SatCrossEdge(const vec3& a, const vec3& b, const vec3& c, const vec3& d) {
  vec3 ab = b - a;
  vec3 cd = d - c;

  vec3 result = Cross(ab, cd);
  if (!CMP(MagnitudeSq(result), 0)) { // Is ab and cd parallel?
    return result; // Not parallel!
  }
  else { // ab and cd are parallel
    // Get an axis perpendicular to AB
    vec3 axis = Cross(ab, c - a);
    result = Cross(ab, axis);
    if (!CMP(MagnitudeSq(result), 0)) { // Still parallel?
      return result; // Not parallel
    }
  }
  // New axis being tested is parallel too.
  // This means that a, b, c and d are on a line
  // Nothing we can do!
  return vec3();
}

Point debugRaycastResult;

bool Raycast(const Triangle& triangle, const Ray& ray, RaycastResult* outResult) {
  ResetRaycastResult(outResult);
  Plane plane = FromTriangle(triangle);

  RaycastResult planeResult;
  if (!Raycast(plane, ray, &planeResult)) {
    return false;
  }
  float t = planeResult.t;

  Point result = ray.origin + ray.direction * t;

  vec3 barycentric = Barycentric(result, triangle);
  if (barycentric.x >= 0.0f && barycentric.x <= 1.0f &&
    barycentric.y >= 0.0f && barycentric.y <= 1.0f &&
    barycentric.z >= 0.0f && barycentric.z <= 1.0f) {

#ifdef DO_SANITY_TESTS
    if (!PointInTriangle(result, triangle)) {
      std::cout << "Point in triangle and barycentric coordinates don't match!\n";
    }
#endif
    if (outResult != 0) {
      outResult->t = t;
      outResult->hit = true;
      outResult->point = ray.origin + ray.direction * t;
      outResult->normal = plane.normal;
    }

    return true;
  }
#ifdef DO_SANITY_TESTS
  else if (PointInTriangle(result, triangle)) {
    std::cout << "Point in triangle and barycentric coordinates don't match!\n";
  }
#endif

  return false;
}

bool Linetest(const Triangle& triangle, const Line& line) {
  Ray ray;
  ray.origin = line.start;
  ray.direction = Normalized(line.end - line.start);
  RaycastResult raycast;
  if (!Raycast(triangle, ray, &raycast)) {
    return false;
  }
  float t = raycast.t;

  return t >= 0 && t * t <= LengthSq(line);
}

void AccelerateMesh(Mesh& mesh) {
  if (mesh.accelerator != 0) {
    return;
  }

  vec3 min = mesh.vertices[0];
  vec3 max = mesh.vertices[0];

  for (int i = 1; i < mesh.numTriangles * 3; ++i) {
    min.x = fminf(mesh.vertices[i].x, min.x);
    min.y = fminf(mesh.vertices[i].y, min.y);
    min.z = fminf(mesh.vertices[i].z, min.z);

    max.x = fmaxf(mesh.vertices[i].x, max.x);
    max.y = fmaxf(mesh.vertices[i].y, max.y);
    max.z = fmaxf(mesh.vertices[i].z, max.z);
  }

  mesh.accelerator = new BVHNode();
  mesh.accelerator->bounds = FromMinMax(min, max);
  mesh.accelerator->children = 0;
  mesh.accelerator->numTriangles = mesh.numTriangles;
  mesh.accelerator->triangles = new int[mesh.numTriangles];
  for (int i = 0; i < mesh.numTriangles; ++i) {
    mesh.accelerator->triangles[i] = i;
  }

  SplitBVHNode(mesh.accelerator, mesh, 3);
}

void SplitBVHNode(BVHNode* node, const Mesh& model, int depth) {
  if (depth-- <= 0) { // Decrements depth
    return;
  }

  if (node->children == 0) {
    // Only split if this node contains triangles
    if (node->numTriangles > 0) {
      node->children = new BVHNode[8];

      vec3 c = node->bounds.position;
      vec3 e = node->bounds.size *0.5f;

      node->children[0].bounds = AABB(c + vec3(-e.x, +e.y, -e.z), e);
      node->children[1].bounds = AABB(c + vec3(+e.x, +e.y, -e.z), e);
      node->children[2].bounds = AABB(c + vec3(-e.x, +e.y, +e.z), e);
      node->children[3].bounds = AABB(c + vec3(+e.x, +e.y, +e.z), e);
      node->children[4].bounds = AABB(c + vec3(-e.x, -e.y, -e.z), e);
      node->children[5].bounds = AABB(c + vec3(+e.x, -e.y, -e.z), e);
      node->children[6].bounds = AABB(c + vec3(-e.x, -e.y, +e.z), e);
      node->children[7].bounds = AABB(c + vec3(+e.x, -e.y, +e.z), e);

    }
  }

  // If this node was just split
  if (node->children != 0 && node->numTriangles > 0) {
    for (int i = 0; i < 8; ++i) { // For each child
      // Count how many triangles each child will contain
      node->children[i].numTriangles = 0;
      for (int j = 0; j < node->numTriangles; ++j) {
        Triangle t = model.triangles[node->triangles[j]];
        if (TriangleAABB(t, node->children[i].bounds)) {
          node->children[i].numTriangles += 1;
        }
      }
      if (node->children[i].numTriangles == 0) {
        continue;
      }
      node->children[i].triangles = new int[node->children[i].numTriangles];
      int index = 0; // Add the triangles in the new child arrau
      for (int j = 0; j < node->numTriangles; ++j) {
        Triangle t = model.triangles[node->triangles[j]];
        if (TriangleAABB(t, node->children[i].bounds)) {
          node->children[i].triangles[index++] = node->triangles[j];
        }
      }
    }

    node->numTriangles = 0;
    delete[] node->triangles;
    node->triangles = 0;

    // Recurse
    for (int i = 0; i < 8; ++i) {
      SplitBVHNode(&node->children[i], model, depth);
    }
  }
}

void FreeBVHNode(BVHNode* node) {
  if (node->children != 0) {
    for (int i = 0; i < 8; ++i) {
      FreeBVHNode(&node->children[i]);
    }
    delete[] node->children;
    node->children = 0;
  }

  if (node->numTriangles != 0 || node->triangles != 0) {
    delete[] node->triangles;
    node->triangles = 0;
    node->numTriangles = 0;
  }
}

bool MeshAABB(const Mesh& mesh, const AABB& aabb) {
  if (mesh.accelerator == 0) {
    for (int i = 0; i < mesh.numTriangles; ++i) {
      if (TriangleAABB(mesh.triangles[i], aabb)) {
        return true;
      }
    }
  }
  else {
    std::list<BVHNode*> toProcess;
    toProcess.push_front(mesh.accelerator);

    // Recursivley walk the BVH tree
    while (!toProcess.empty()) {
      BVHNode* iterator = *(toProcess.begin());
      toProcess.erase(toProcess.begin());

      if (iterator->numTriangles >= 0) {
        // Iterate trough all triangles of the node
        for (int i = 0; i < iterator->numTriangles; ++i) {
          // Triangle indices in BVHNode index the mesh
          if (TriangleAABB(mesh.triangles[iterator->triangles[i]], aabb)) {
            return true;
          }
        }
      }

      if (iterator->children != 0) {
        for (int i = 8 - 1; i >= 0; --i) {
          // Only push children whos bounds intersect the test geometry
          if (AABBAABB(iterator->children[i].bounds, aabb)) {
            toProcess.push_front(&iterator->children[i]);
          }
        }
      }
    }
  }
  return false;
}

bool Linetest(const Mesh& mesh, const Line& line) {
  if (mesh.accelerator == 0) {
    for (int i = 0; i < mesh.numTriangles; ++i) {
      if (Linetest(mesh.triangles[i], line)) {
        return true;
      }
    }
  }
  else {
    std::list<BVHNode*> toProcess;
    toProcess.push_front(mesh.accelerator);

    // Recursivley walk the BVH tree
    while (!toProcess.empty()) {
      BVHNode* iterator = *(toProcess.begin());
      toProcess.erase(toProcess.begin());

      if (iterator->numTriangles >= 0) {
        // Iterate trough all triangles of the node
        for (int i = 0; i < iterator->numTriangles; ++i) {
          // Triangle indices in BVHNode index the mesh
          if (Linetest(mesh.triangles[iterator->triangles[i]], line)) {
            return true;
          }
        }
      }

      if (iterator->children != 0) {
        for (int i = 8 - 1; i >= 0; --i) {
          // Only push children whos bounds intersect the test geometry
          if (Linetest(iterator->children[i].bounds, line)) {
            toProcess.push_front(&iterator->children[i]);
          }
        }
      }
    }
  }
  return false;
}

bool MeshSphere(const Mesh& mesh, const Sphere& sphere) {
  if (mesh.accelerator == 0) {
    for (int i = 0; i < mesh.numTriangles; ++i) {
      if (TriangleSphere(mesh.triangles[i], sphere)) {
        return true;
      }
    }
  }
  else {
    std::list<BVHNode*> toProcess;
    toProcess.push_front(mesh.accelerator);

    // Recursivley walk the BVH tree
    while (!toProcess.empty()) {
      BVHNode* iterator = *(toProcess.begin());
      toProcess.erase(toProcess.begin());

      if (iterator->numTriangles >= 0) {
        // Iterate trough all triangles of the node
        for (int i = 0; i < iterator->numTriangles; ++i) {
          // Triangle indices in BVHNode index the mesh
          if (TriangleSphere(mesh.triangles[iterator->triangles[i]], sphere)) {
            return true;
          }
        }
      }

      if (iterator->children != 0) {
        for (int i = 8 - 1; i >= 0; --i) {
          // Only push children whos bounds intersect the test geometry
          if (SphereAABB(sphere, iterator->children[i].bounds)) {
            toProcess.push_front(&iterator->children[i]);
          }
        }
      }
    }
  }
  return false;
}

bool MeshOBB(const Mesh& mesh, const OBB& obb) {
  if (mesh.accelerator == 0) {
    for (int i = 0; i < mesh.numTriangles; ++i) {
      if (TriangleOBB(mesh.triangles[i], obb)) {
        return true;
      }
    }
  }
  else {
    std::list<BVHNode*> toProcess;
    toProcess.push_front(mesh.accelerator);

    // Recursivley walk the BVH tree
    while (!toProcess.empty()) {
      BVHNode* iterator = *(toProcess.begin());
      toProcess.erase(toProcess.begin());

      if (iterator->numTriangles >= 0) {
        // Iterate trough all triangles of the node
        for (int i = 0; i < iterator->numTriangles; ++i) {
          // Triangle indices in BVHNode index the mesh
          if (TriangleOBB(mesh.triangles[iterator->triangles[i]], obb)) {
            return true;
          }
        }
      }

      if (iterator->children != 0) {
        for (int i = 8 - 1; i >= 0; --i) {
          // Only push children whos bounds intersect the test geometry
          if (AABBOBB(iterator->children[i].bounds, obb)) {
            toProcess.push_front(&iterator->children[i]);
          }
        }
      }
    }
  }
  return false;
}

bool MeshPlane(const Mesh& mesh, const Plane& plane) {
  if (mesh.accelerator == 0) {
    for (int i = 0; i < mesh.numTriangles; ++i) {
      if (TrianglePlane(mesh.triangles[i], plane)) {
        return true;
      }
    }
  }
  else {
    std::list<BVHNode*> toProcess;
    toProcess.push_front(mesh.accelerator);

    // Recursivley walk the BVH tree
    while (!toProcess.empty()) {
      BVHNode* iterator = *(toProcess.begin());
      toProcess.erase(toProcess.begin());

      if (iterator->numTriangles >= 0) {
        // Iterate trough all triangles of the node
        for (int i = 0; i < iterator->numTriangles; ++i) {
          // Triangle indices in BVHNode index the mesh
          if (TrianglePlane(mesh.triangles[iterator->triangles[i]], plane)) {
            return true;
          }
        }
      }

      if (iterator->children != 0) {
        for (int i = 8 - 1; i >= 0; --i) {
          // Only push children whos bounds intersect the test geometry
          if (AABBPlane(iterator->children[i].bounds, plane)) {
            toProcess.push_front(&iterator->children[i]);
          }
        }
      }
    }
  }
  return false;
}

bool MeshTriangle(const Mesh& mesh, const Triangle& triangle) {
  if (mesh.accelerator == 0) {
    for (int i = 0; i < mesh.numTriangles; ++i) {
      if (TriangleTriangle(mesh.triangles[i], triangle)) {
        return true;
      }
    }
  }
  else {
    std::list<BVHNode*> toProcess;
    toProcess.push_front(mesh.accelerator);

    // Recursivley walk the BVH tree
    while (!toProcess.empty()) {
      BVHNode* iterator = *(toProcess.begin());
      toProcess.erase(toProcess.begin());

      if (iterator->numTriangles >= 0) {
        // Iterate trough all triangles of the node
        for (int i = 0; i < iterator->numTriangles; ++i) {
          // Triangle indices in BVHNode index the mesh
          if (TriangleTriangle(mesh.triangles[iterator->triangles[i]], triangle)) {
            return true;
          }
        }
      }

      if (iterator->children != 0) {
        for (int i = 8 - 1; i >= 0; --i) {
          // Only push children whos bounds intersect the test geometry
          if (TriangleAABB(triangle, iterator->children[i].bounds)) {
            toProcess.push_front(&iterator->children[i]);
          }
        }
      }
    }
  }
  return false;
}

#ifndef NO_EXTRAS
float Raycast(const Mesh& mesh, const Ray& ray) {
  return MeshRay(mesh, ray);
}
float Raycast(const Model& mesh, const Ray& ray) {
  return ModelRay(mesh, ray);
}
#endif

float MeshRay(const Mesh& mesh, const Ray& ray) {
  if (mesh.accelerator == 0) {
    for (int i = 0; i < mesh.numTriangles; ++i) {
      RaycastResult raycast;
      Raycast(mesh.triangles[i], ray, &raycast);
      float result = raycast.t;
      if (result >= 0) {
        return result;
      }
    }
  }
  else {
    std::list<BVHNode*> toProcess;
    toProcess.push_front(mesh.accelerator);

    // Recursivley walk the BVH tree
    while (!toProcess.empty()) {
      BVHNode* iterator = *(toProcess.begin());
      toProcess.erase(toProcess.begin());

      if (iterator->numTriangles >= 0) {
        // Iterate trough all triangles of the node
        for (int i = 0; i < iterator->numTriangles; ++i) {
          // Triangle indices in BVHNode index the mesh
          RaycastResult raycast;
          Raycast(mesh.triangles[iterator->triangles[i]], ray, &raycast);
          float r = raycast.t;
          if (r >= 0) {
            return r;
          }
        }
      }

      if (iterator->children != 0) {
        for (int i = 8 - 1; i >= 0; --i) {
          // Only push children whos bounds intersect the test geometry
          RaycastResult raycast;
          Raycast(iterator->children[i].bounds, ray, &raycast);
          if (raycast.t >= 0) {
            toProcess.push_front(&iterator->children[i]);
          }
        }
      }
    }
  }
  return -1;
}

bool TrianglePlane(const Triangle& t, const Plane& p) {
  float side1 = PlaneEquation(t.a, p);
  float side2 = PlaneEquation(t.b, p);
  float side3 = PlaneEquation(t.c, p);

  // On Plane
  if (CMP(side1, 0) && CMP(side2, 0) && CMP(side3, 0)) {
    return true;
  }

  // Triangle in front of plane
  if (side1 > 0 && side2 > 0 && side3 > 0) {
    return false;
  }

  // Triangle behind plane
  if (side1 < 0 && side2 < 0 && side3 < 0) {
    return false;
  }

  return true; // Intersection
}

void Model::SetContent(Mesh* mesh) {
  content = mesh;
  if (content != 0) {
    vec3 min = mesh->vertices[0];
    vec3 max = mesh->vertices[0];

    for (int i = 1; i < mesh->numTriangles * 3; ++i) {
      min.x = fminf(mesh->vertices[i].x, min.x);
      min.y = fminf(mesh->vertices[i].y, min.y);
      min.z = fminf(mesh->vertices[i].z, min.z);

      max.x = fmaxf(mesh->vertices[i].x, max.x);
      max.y = fmaxf(mesh->vertices[i].y, max.y);
      max.z = fmaxf(mesh->vertices[i].z, max.z);
    }
    bounds = FromMinMax(min, max);
  }
}

mat4 GetWorldMatrix(const Model& model) {
  mat4 translation = Translation(model.position);
  mat4 rotation = Rotation(model.rotation.x, model.rotation.y, model.rotation.z);
  mat4 localMat = /* Scale * */ rotation * translation;

  mat4 parentMat;
  if (model.parent != 0) {
    parentMat = GetWorldMatrix(*model.parent);
  }

  return localMat * parentMat;
}

OBB GetOBB(const Model& model) {
  mat4 world = GetWorldMatrix(model);
  AABB aabb = model.GetBounds();
  OBB obb;

  obb.size = aabb.size;
  obb.position = MultiplyPoint(aabb.position, world);
  obb.orientation = Cut(world, 3, 3);

  return obb;
}

float ModelRay(const Model& model, const Ray& ray) {
  mat4 world = GetWorldMatrix(model);
  mat4 inv = Inverse(world);
  Ray local;
  local.origin = MultiplyPoint(ray.origin, inv);
  local.direction = MultiplyVector(ray.direction, inv);
  local.NormalizeDirection();
  if (model.GetMesh() != 0) {
    return MeshRay(*(model.GetMesh()), local);
  }
  return -1;
}

bool Linetest(const Model& model, const Line& line) {
  mat4 world = GetWorldMatrix(model);
  mat4 inv = Inverse(world);
  Line local;
  local.start = MultiplyPoint(line.start, inv);
  local.end = MultiplyPoint(line.end, inv);
  if (model.GetMesh() != 0) {
    return Linetest(*(model.GetMesh()), local);
  }
  return false;
}

bool ModelSphere(const Model& model, const Sphere& sphere) {
  mat4 world = GetWorldMatrix(model);
  mat4 inv = Inverse(world);
  Sphere local;
  local.position = MultiplyPoint(sphere.position, inv);
  if (model.GetMesh() != 0) {
    return MeshSphere(*(model.GetMesh()), local);
  }
  return false;
}

bool ModelAABB(const Model& model, const AABB& aabb) {
  mat4 world = GetWorldMatrix(model);
  mat4 inv = Inverse(world);
  OBB local;
  local.size = aabb.size;
  local.position = MultiplyPoint(aabb.position, inv);
  local.orientation = Cut(inv, 3, 3);
  if (model.GetMesh() != 0) {
    return MeshOBB(*(model.GetMesh()), local);
  }
  return false;
}

bool ModelOBB(const Model& model, const OBB& obb) {
  mat4 world = GetWorldMatrix(model);
  mat4 inv = Inverse(world);
  OBB local;
  local.size = obb.size;
  local.position = MultiplyPoint(obb.position, inv);
  local.orientation = obb.orientation * Cut(inv, 3, 3);
  if (model.GetMesh() != 0) {
    return MeshOBB(*(model.GetMesh()), local);
  }
  return false;
}

bool ModelPlane(const Model& model, const Plane& plane) {
  mat4 world = GetWorldMatrix(model);
  mat4 inv = Inverse(world);
  Plane local;
  local.normal = MultiplyVector(plane.normal, inv);
  local.distance = plane.distance;
  if (model.GetMesh() != 0) {
    return MeshPlane(*(model.GetMesh()), local);
  }
  return false;
}

bool ModelTriangle(const Model& model, const Triangle& triangle) {
  mat4 world = GetWorldMatrix(model);
  mat4 inv = Inverse(world);
  Triangle local;
  local.a = MultiplyPoint(triangle.a, inv);
  local.b = MultiplyPoint(triangle.b, inv);
  local.c = MultiplyPoint(triangle.c, inv);
  if (model.GetMesh() != 0) {
    return MeshTriangle(*(model.GetMesh()), local);
  }
  return false;
}

Point Intersection(Plane p1, Plane p2, Plane p3) {
  /*return ((Cross(p2.normal, p3.normal) * -p1.distance) +
    (Cross(p3.normal, p1.normal) * -p2.distance) +
    (Cross(p1.normal, p2.normal) * -p3.distance)) /
    (Dot(p1.normal, Cross(p2.normal, p3.normal)));*/

#if 1
  mat3 D(
    p1.normal.x, p2.normal.x, p3.normal.x,
    p1.normal.y, p2.normal.y, p3.normal.y,
    p1.normal.z, p2.normal.z, p3.normal.z
  );
  vec3 A(-p1.distance, -p2.distance, -p3.distance);

  mat3 Dx = D, Dy = D, Dz = D;
  Dx._11 = A.x; Dx._12 = A.y; Dx._13 = A.z;
  Dy._21 = A.x; Dy._22 = A.y; Dy._23 = A.z;
  Dz._31 = A.x; Dz._32 = A.y; Dz._33 = A.z;

  float detD = Determinant(D);

  if (CMP(detD, 0)) {
    return Point();
  }

  float detDx = Determinant(Dx);
  float detDy = Determinant(Dy);
  float detDz = Determinant(Dz);

  return Point(detDx / detD, detDy / detD, detDz / detD);
#else
  vec3 m1(p1.normal.x, p2.normal.x, p3.normal.x);
  vec3 m2(p1.normal.y, p2.normal.y, p3.normal.y);
  vec3 m3(p1.normal.z, p2.normal.z, p3.normal.z);
  vec3 d(-p1.distance, -p2.distance, -p3.distance);

  vec3 u = Cross(m2, m3);
  vec3 v = Cross(m1, d);
  float denom = Dot(m1, u);

  if (CMP(denom, 0.0f)) {
    return Point();
  }

  Point result;
  result.x = Dot(d, u) / denom;
  result.y = Dot(m3, v) / denom;
  result.z = -Dot(m2, v) / denom;
  return result;
#endif
}

void GetCorners(const Frustum& f, vec3* outCorners) {
  outCorners[0] = Intersection(f._near, f.top,    f.left);
  outCorners[1] = Intersection(f._near, f.top,    f.right);
  outCorners[2] = Intersection(f._near, f.bottom, f.left);
  outCorners[3] = Intersection(f._near, f.bottom, f.right);
  outCorners[4] = Intersection(f._far,  f.top,    f.left);
  outCorners[5] = Intersection(f._far,  f.top,    f.right);
  outCorners[6] = Intersection(f._far,  f.bottom, f.left);
  outCorners[7] = Intersection(f._far,  f.bottom, f.right);
}

bool Intersects(const Frustum& f, const Point& p) {
  for (int i = 0; i < 6; ++i) {
    vec3 normal = f.planes[i].normal;
    float dist = f.planes[i].distance;
    float side = Dot(p, normal) + dist;
    if (side < 0.0f) {
      return false;
    }
  }

  return true;
}

bool Intersects(const Frustum& f, const Sphere& s) {
  for (int i = 0; i < 6; ++i) {
    vec3 normal = f.planes[i].normal;
    float dist = f.planes[i].distance;
    float side = Dot(s.position, normal) + dist;
    if (side < -s.radius) {
      return false;
    }
  }

  return true;
}

float Classify(const AABB& aabb, const Plane& plane) {
  // maximum extent in direction of plane normal
  float r = fabsf(aabb.size.x * plane.normal.x)
    + fabsf(aabb.size.y * plane.normal.y)
    + fabsf(aabb.size.z * plane.normal.z);

  // signed distance between box center and plane
  //float d = plane.Test(mCenter);
  float d = Dot(plane.normal, aabb.position) + plane.distance;

  // return signed distance
  if (fabsf(d) < r) {
    return 0.0f;
  }
  else if (d < 0.0f) {
    return d + r;
  }
  return d - r;
}

float Classify(const OBB& obb, const Plane& plane) {
  vec3 normal = MultiplyVector(plane.normal, obb.orientation);

  // maximum extent in direction of plane normal
  float r = fabsf(obb.size.x * normal.x)
    + fabsf(obb.size.y * normal.y)
    + fabsf(obb.size.z * normal.z);

  // signed distance between box center and plane
  //float d = plane.Test(mCenter);
  float d = Dot(plane.normal, obb.position) + plane.distance;

  // return signed distance
  if (fabsf(d) < r) {
    return 0.0f;
  }
  else if (d < 0.0f) {
    return d + r;
  }
  return d - r;
}

bool Intersects(const Frustum& f, const OBB& obb) {
  for (int i = 0; i < 6; ++i) {
    float side = Classify(obb, f.planes[i]);
    if (side < 0) {
      return false;
    }
  }
  return true;
}

bool Intersects(const Frustum& f, const AABB& aabb) {
  for (int i = 0; i < 6; ++i) {
    float side = Classify(aabb, f.planes[i]);
    if (side < 0) {
      return false;
    }
  }
  return true;
}

vec3 Unproject(const vec3& viewportPoint, const vec2& viewportOrigin, const vec2& viewportSize, const mat4& view, const mat4& projection) {
  // Step 1, Normalize the input vector to the view port
  float normalized[4] = {
    (viewportPoint.x - viewportOrigin.x) / viewportSize.x,
    (viewportPoint.y - viewportOrigin.y) / viewportSize.y,
    viewportPoint.z,
    1.0f
  };

  // Step 2, Translate into NDC space
  float ndcSpace[4] = {
    normalized[0], normalized[1],
    normalized[2], normalized[3]
  };
  // X Range: -1 to 1
  ndcSpace[0] = ndcSpace[0] * 2.0f - 1.0f;
  // Y Range: -1 to 1, our Y axis is flipped!
  ndcSpace[1] = 1.0f - ndcSpace[1] * 2.0f;
  // Z Range: 0 to 1
  if (ndcSpace[2] < 0.0f) {
    ndcSpace[2] = 0.0f;
  }
  if (ndcSpace[2] > 1.0f) {
    ndcSpace[2] = 1.0f;
  }

  // Step 3, NDC to Eye Space
  mat4 invProjection = Inverse(projection);
  float eyeSpace[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
  // eyeSpace = MultiplyPoint(ndcSpace, invProjection);
  Multiply(eyeSpace, ndcSpace, 1, 4, invProjection.asArray, 4, 4);

  // Step 4, Eye Space to World Space
  mat4 invView = Inverse(view);
  float worldSpace[4] = { 0.0f, 0.0f, 0.0f, 0.0f };
  // worldSpace = MultiplyPoint(eyeSpace, invView);
  Multiply(worldSpace, eyeSpace, 1, 4, invView.asArray, 4, 4);

  // Step 5, Undo perspective divide!
  if (!CMP(worldSpace[3], 0.0f)) {
    worldSpace[0] /= worldSpace[3];
    worldSpace[1] /= worldSpace[3];
    worldSpace[2] /= worldSpace[3];
  }

  // Return the resulting world space point
  return vec3(worldSpace[0], worldSpace[1], worldSpace[2]);
}

Ray GetPickRay(const vec2& viewportPoint, const vec2& viewportOrigin, const vec2& viewportSize, const mat4& view, const mat4& projection) {
  vec3 nearPoint(viewportPoint.x, viewportPoint.y, 0.0f);
  vec3 farPoint(viewportPoint.x, viewportPoint.y, 1.0f);

  vec3 pNear = Unproject(nearPoint, viewportOrigin, viewportSize, view, projection);
  vec3 pFar = Unproject(farPoint, viewportOrigin, viewportSize, view, projection);

  vec3 normal = Normalized(pFar - pNear);
  vec3 origin = pNear;

  return Ray(origin, normal);
}

// Chapter 15

void ResetCollisionManifold(CollisionManifold* result) {
  if (result != 0) {
    result->colliding = false;
    result->normal = vec3(0, 0, 1);
    result->depth = FLT_MAX;
    if (result->contacts.size() > 0) {
      result->contacts.clear();
    }
  }
}

std::vector<Point> GetVertices(const OBB& obb) {
  std::vector<vec3> v;
  v.resize(8);

  vec3 C = obb.position;  // OBB Center
  vec3 E = obb.size;    // OBB Extents
  const float* o = obb.orientation.asArray;
  vec3 A[] = {      // OBB Axis
    vec3(o[0], o[1], o[2]),
    vec3(o[3], o[4], o[5]),
    vec3(o[6], o[7], o[8]),
  };

  v[0] = C + A[0] * E[0] + A[1] * E[1] + A[2] * E[2];
  v[1] = C - A[0] * E[0] + A[1] * E[1] + A[2] * E[2];
  v[2] = C + A[0] * E[0] - A[1] * E[1] + A[2] * E[2];
  v[3] = C + A[0] * E[0] + A[1] * E[1] - A[2] * E[2];
  v[4] = C - A[0] * E[0] - A[1] * E[1] - A[2] * E[2];
  v[5] = C + A[0] * E[0] - A[1] * E[1] - A[2] * E[2];
  v[6] = C - A[0] * E[0] + A[1] * E[1] - A[2] * E[2];
  v[7] = C - A[0] * E[0] - A[1] * E[1] + A[2] * E[2];

  return v;
}

std::vector<Line> GetEdges(const OBB& obb) {
  std::vector<Line> result;
  result.reserve(12);
  std::vector<Point> v = GetVertices(obb);

  int index[][2] = { // Indices of edges
    { 6, 1 },{ 6, 3 },{ 6, 4 },{ 2, 7 },{ 2, 5 },{ 2, 0 },
    { 0, 1 },{ 0, 3 },{ 7, 1 },{ 7, 4 },{ 4, 5 },{ 5, 3 }
  };

  for (int j = 0; j < 12; ++j) {
    result.push_back(Line(
      v[index[j][0]], v[index[j][1]]
    ));
  }

  return result;
}

std::vector<Plane> GetPlanes(const OBB& obb) {
  vec3 c = obb.position;  // OBB Center
  vec3 e = obb.size;    // OBB Extents
  const float* o = obb.orientation.asArray;
  vec3 a[] = {      // OBB Axis
    vec3(o[0], o[1], o[2]),
    vec3(o[3], o[4], o[5]),
    vec3(o[6], o[7], o[8]),
  };

  std::vector<Plane> result;
  result.resize(6);

  result[0] = Plane(a[0]        ,  Dot(a[0], (c + a[0] * e.x)));
  result[1] = Plane(a[0] * -1.0f, -Dot(a[0], (c - a[0] * e.x)));
  result[2] = Plane(a[1]        ,  Dot(a[1], (c + a[1] * e.y)));
  result[3] = Plane(a[1] * -1.0f, -Dot(a[1], (c - a[1] * e.y)));
  result[4] = Plane(a[2]        ,  Dot(a[2], (c + a[2] * e.z)));
  result[5] = Plane(a[2] * -1.0f, -Dot(a[2], (c - a[2] * e.z)));

  return result;
}


bool ClipToPlane(const Plane& plane, const Line& line, Point* outPoint) {
  vec3 ab = line.end - line.start;

  float nA = Dot(plane.normal, line.start);
  float nAB = Dot(plane.normal, ab);

  if (CMP(nAB, 0)) {
    return false;
  }

  float t = (plane.distance - nA) / nAB;
  if (t >= 0.0f && t <= 1.0f) {
    if (outPoint != 0) {
      *outPoint = line.start + ab * t;
    }
    return true;
  }

  return false;
}

std::vector<Point> ClipEdgesToOBB(const std::vector<Line>& edges, const OBB& obb) {
  std::vector<Point> result;
  result.reserve(edges.size() * 3);
  Point intersection;

  std::vector<Plane>& planes = GetPlanes(obb);

  for (int i = 0; i < planes.size(); ++i) {
    for (int j = 0; j < edges.size(); ++j) {
      if (ClipToPlane(planes[i], edges[j], &intersection)) {
        if (PointInOBB(intersection, obb)) {
          result.push_back(intersection);
        }
      }
    }
  }

  return result;
}

float PenetrationDepth(const OBB& o1, const OBB& o2, const vec3& axis, bool* outShouldFlip) {
  Interval i1 = GetInterval(o1, Normalized(axis));
  Interval i2 = GetInterval(o2, Normalized(axis));

  if (!((i2.min <= i1.max) && (i1.min <= i2.max))) {
    return 0.0f; // No penerattion
  }

  float len1 = i1.max - i1.min;
  float len2 = i2.max - i2.min;
  float min = fminf(i1.min, i2.min);
  float max = fmaxf(i1.max, i2.max);
  float length = max - min;

  if (outShouldFlip != 0) {
    *outShouldFlip = (i2.min < i1.min);
  }

  return (len1 + len2) - length;
}

CollisionManifold FindCollisionFeatures(const OBB& A, const OBB& B) {
  CollisionManifold result; // Will return result of intersection!
  ResetCollisionManifold(&result);

  Sphere s1(A.position, Magnitude(A.size));
  Sphere s2(B.position, Magnitude(B.size));

  if (!SphereSphere(s1, s2)) {
    return result;
  }

  const float* o1 = A.orientation.asArray;
  const float* o2 = B.orientation.asArray;

  vec3 test[15] = {
    vec3(o1[0], o1[1], o1[2]),
    vec3(o1[3], o1[4], o1[5]),
    vec3(o1[6], o1[7], o1[8]),
    vec3(o2[0], o2[1], o2[2]),
    vec3(o2[3], o2[4], o2[5]),
    vec3(o2[6], o2[7], o2[8])
  };

  for (int i = 0; i < 3; ++i) { // Fill out rest of axis
    test[6 + i * 3 + 0] = Cross(test[i], test[0]);
    test[6 + i * 3 + 1] = Cross(test[i], test[1]);
    test[6 + i * 3 + 2] = Cross(test[i], test[2]);
  }

  vec3* hitNormal = 0;
  bool shouldFlip;

  for (int i = 0; i < 15; ++i) {
    if (test[i].x < 0.000001f) test[i].x = 0.0f;
    if (test[i].y < 0.000001f) test[i].y = 0.0f;
    if (test[i].z < 0.000001f) test[i].z = 0.0f;
    if (MagnitudeSq(test[i])< 0.001f) {
      continue;
    }

    float depth = PenetrationDepth(A, B, test[i], &shouldFlip);
    if (depth <= 0.0f) {
      return result;
    }
    else if (depth < result.depth) {
      if (shouldFlip) {
        test[i] = test[i] * -1.0f;
      }
      result.depth = depth;
      hitNormal = &test[i];
    }
  }

  if (hitNormal == 0) {
    return result;
  }
  vec3 axis = Normalized(*hitNormal);

  std::vector<Point> c1 = ClipEdgesToOBB(GetEdges(B), A);
  std::vector<Point> c2 = ClipEdgesToOBB(GetEdges(A), B);
  result.contacts.reserve(c1.size() + c2.size());
  result.contacts.insert(result.contacts.end(), c1.begin(), c1.end());
  result.contacts.insert(result.contacts.end(), c2.begin(), c2.end());

  Interval i = GetInterval(A, axis);
  float distance = (i.max - i.min)* 0.5f - result.depth * 0.5f;
  vec3 pointOnPlane = A.position + axis * distance;

  for (int i = result.contacts.size() - 1; i >= 0; --i) {
    vec3 contact = result.contacts[i];
    result.contacts[i] = contact + (axis * Dot(axis, pointOnPlane - contact));

    // This bit is in the "There is more" section of the book
    for (int j = result.contacts.size() - 1; j > i; --j) {
      if (MagnitudeSq(result.contacts[j] - result.contacts[i]) < 0.0001f) {
        result.contacts.erase(result.contacts.begin() + j);
        break;
      }
    }
  }

  result.colliding = true;
  result.normal = axis;

  return result;
}

CollisionManifold FindCollisionFeatures(const Sphere& A, const Sphere& B) {
  CollisionManifold result; // Will return result of intersection!
  ResetCollisionManifold(&result);

  float r = A.radius + B.radius;
  vec3 d = B.position - A.position;

  if (MagnitudeSq(d) - r * r > 0 || MagnitudeSq(d) == 0.0f) {
    return result;
  }
  Normalize(d);

  result.colliding = true;
  result.normal = d;
  result.depth = fabsf(Magnitude(d) - r) * 0.5f;

  // dtp - Distance to intersection point
  float dtp = A.radius - result.depth;
  Point contact = A.position + d * dtp;

  result.contacts.push_back(contact);

  return result;
}

CollisionManifold FindCollisionFeatures(const OBB& A, const Sphere& B) {
  CollisionManifold result; // Will return result of intersection!
  ResetCollisionManifold(&result);

  Point closestPoint = ClosestPoint(A, B.position);

  float distanceSq = MagnitudeSq(closestPoint - B.position);
  if (distanceSq > B.radius * B.radius) {
    return result;
  }

  vec3 normal;
  if (CMP(distanceSq, 0.0f)) {
    if (CMP(MagnitudeSq(closestPoint - A.position), 0.0f)) {
      return result;

    }
    // Closest point is at the center of the sphere
    normal = Normalized(closestPoint - A.position);
  }
  else {
    normal = Normalized(B.position - closestPoint);
  }

  Point outsidePoint = B.position - normal * B.radius;

  float distance = Magnitude(closestPoint - outsidePoint);

  result.colliding = true;
  result.contacts.push_back(closestPoint + (outsidePoint - closestPoint) * 0.5f);
  result.normal = normal;
  result.depth = distance * 0.5f;

  return result;
}



