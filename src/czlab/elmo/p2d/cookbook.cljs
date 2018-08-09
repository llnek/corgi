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
(defn Interval2D "" [& [minv maxv]]
  {:min (num?? minv 0) :max (num?? maxv 0)})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



