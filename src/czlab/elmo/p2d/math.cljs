;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.math

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
(def ACOS js/Math.acos)
(def ATAN2 js/Math.atan2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- CMP "" [x y]
  (<= (abs* (- x y))
      (* FLT_EPSILON (max 1 (max (abs* x) (abs* y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec-new*
  "" [cells & [type]]
  {:cells cells :size (count cells) :type type})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec-n "" [size & [type]]
  (when-not (neg? size)
    (vec-new* (clj->js (map (fn [_] 0) (range size))) type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec*
  "" [& [args]] (if (every? #(number? %) args) (vec-new* (clj->js args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec3 "" [& [x y z]]
  (vec-new* #js [(num?? x 0)(num?? y 0)(num?? z 0)] ::vec3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2 "" [& [x y]]
  (vec-new* #js [(num?? x 0)(num?? y 0)] ::vec2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- modDeg "" [deg]
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
(defn vec-eq? "" [v1 v2]
  (let [{c1 :cells s1 :size t1 :type} v1
        {c2 :cells s2 :size t2 :type} v2]
    (if (and (= t1 t2)
             (= s1 s2))
      (loop [[a & m1] c1
             [b & m2] c2 ok? true]
        (if (or (nil? a)
                (not ok?))
          ok?
          (recur m1 m2 (CMP a b)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XXXvec-eq? "" [v1 v2]
  (let [{c1 :cells s1 :size t1 :type} v1
        {c2 :cells s2 :size t2 :type} v2]
    (and (= t1 t2)
         (= s1 s2)
         (every? #(CMP (_1 %) (aget c2 (_2 %)))
                 (partition 2 (interleave c1 (range s1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-neq? "" [v1 v2] (not (vec-eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec-xxx "" [v1 v2 func res]
  (let [{c1 :cells s1 :size t1 :type} v1
        {c2 :cells s2 :size t2 :type} v2]
    (when (and (= t1 t2)
               (= s1 s2))
      (loop [i 0 SZ s1 out []]
        (if (>= i SZ)
          (res out t1)
          (recur (+ i 1)
                 SZ
                 (conj out (func (aget c1 i)(aget c2 i)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- wrap-as-new "" [out t] (vec-new* (clj->js out) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-add "" [v1 v2] (vec-xxx v1 v2 + wrap-as-new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-sub "" [v1 v2] (vec-xxx v1 v2 - wrap-as-new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-div "" [v1 v2] (vec-xxx v1 v2 / wrap-as-new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-mult "" [v1 v2] (vec-xxx v1 v2 * wrap-as-new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-scale "" [v n]
  (assert (number? n) "expected (vec, number)")
  (let [{:keys [cells size type]} v]
    (loop [i 0 SZ size out []]
      (if (>= i SZ)
        (wrap-as-new out type)
        (recur (+ 1 i) SZ (conj out (* n (aget cells i))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-plus "" [v n]
  (assert (number? n) "expected (vec, number)")
  (let [{:keys [cells size type]} v]
    (loop [i 0 SZ size out []]
      (if (>= i SZ)
        (wrap-as-new out type)
        (recur (+ 1 i) SZ (conj out (+ (aget cells i) n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-minus "" [v n]
  (assert (number? n) "expected (vec, number)")
  (let [{:keys [cells size type]} v]
    (loop [i 0 SZ size out []]
      (if (>= i SZ)
        (wrap-as-new out type)
        (recur (+ 1 i) SZ (conj out (- (aget cells i) n)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-dot "" [v1 v2]
  (vec-xxx v1 v2 * (fn [out & _]
                     (reduce (fn [sum n] (+ sum n)) 0 out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-lensq "" [v] (vec-dot v v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-len "" [v] (sqrt* (vec-dot v v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-distsq "" [v1 v2] (vec-lensq (vec-sub v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-dist "" [v1 v2] (vec-len (vec-sub v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-unit "" [v]
  (let [z (vec-len v)]
    (if (> z EPSILON) (vec-scale v (/ 1 z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- v2-rot "" [v angle center]
  (let [[cx cy] (or (:cells center) [0 0])
        [x y] (:cells v)
        x' (- x cx)
        y' (- y cy)
        cos (COS angle)
        sin (SIN angle)]
    (vec2 (+ cx (- (* x' cos) (* y' sin)))
          (+ cy (+ (* x' sin) (* y' cos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- v3-rot "" [v angle center] )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-rot "rotate counter-clockwise" [v angle & [center]]
  (let [{:keys [type]} v]
    (case type
      ::vec2  (v2-rot v angle center)
      ::vec3  (v3-rot v angle center) nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-xss
  "If positive, v2 is on top of v1,
  if negative, v2 below v1. Take the absolute value then it will
  be the sine of the angle between them."
  [v1 v2]
  (let [[x1 y1 ] (:cells v1)
        [x2 y2 ] (:cells v2)] (- (* x1 y2) (* y1 x2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v3-xss "" [v1 v2]
  (let [[x2 y2 z2] (:cells v2)
        [x1 y1 z1] (:cells v1)]
    (vec3 (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-xss "" [v1 v2]
  (let [{s1 :size t1 :type} v1
        {s2 :size t2 :type} v2]
    (if (and (= t1 t2)
             (= s1 s2))
      (case type
        ::vec2  (v2-xss v1 v2)
        ::vec3  (v3-xss v1 v2) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-angle "" [v1 v2]
  (let [{s1 :size t1 :type} v1
        {s2 :size t2 :type} v2]
    (when (and (= s1 s2)
               (= t1 t2))
      (ACOS (/ (vec-dot v1 v2)
               (sqrt* (* (vec-lensq v1) (vec-lensq v2))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-proj "" [length dir]
  (let [{s1 :size t1 :type} length
        {s2 :size t2 :type} dir]
    (when (and (= s1 s2)
               (= t1 t2))
      (vec-scale dir (/ (vec-dot length dir) (vec-lensq dir))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-perp "" [length dir]
  (let [{s2 :size t2 :type} dir
        {s1 :size t1 :type} length]
    (when (and (= s1 s2)
               (= t1 t2))
      (vec-sub length (vec-proj length dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-reflect "" [src normal]
  (let [{s1 :size t1 :type} src
        {s2 :size t2 :type} normal]
    (when (and (= s1 s2)
               (= t1 t2))
      (vec-sub src
               (vec-scale normal
                          (* 2 (vec-dot src normal)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-neg "" [v] (vec-scale v -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-normal "" [v]
  (let [[x y] (:cells v)] (vec2 y (- x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-min "" [v1 v2] (vec-xxx v1 v2 min wrap-as-new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-max "" [v1 v2] (vec-xxx v1 v2 max wrap-as-new))

;;kenl
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
  {:rows rows :cols cols
   :cells (clj->js (map (fn [_] 0) (range (* rows cols))))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat* "" [[rows cols] & args]
  (let [sz (* rows cols)
        az (count args)]
    (if (and (pos? az)
             (not= az sz))
      (assert false "expecting more args"))
    (if-not (every? #(number? %) args)
      (assert false "expecting all numbers"))
    (if (zero? az)
      (mx-new rows cols)
      (mx-new* rows cols (clj->js args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-ident "" [size]
  (let [sz (* size size)
        out (clj->js (map (fn [_] 0) (range sz)))]
    (dotimes [i size]
      (aset out (mx-pos size size i i) 1))
    (mx-new* size size out)))

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
(defn Point3D "" [& [x y z :as args]]
  (-> (if (= 1 (count args))
        x
        (vec3 (num?? x 0) (num?? y 0)(num?? z 0)))
      (assoc :type :point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Line3D "" [start end] {:type :line :start start :end end})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Ray
  "" [& [origin dir]]
  {:origin (or origin (Point3D 0 0 0))
   :type :ray
   :dir (if (some? dir) (v3-unit dir) (vec3 0 0 1))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Sphere
  "" [& [pos radius]]
  {:radius (if (some? radius) radius 1)
   :type :sphere
   :pos (if (some? pos) pos (Point3D 0 0 0))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn AABB
  ;;Rectangle3D
  "half size" [& [pos size]]
  {:size (if (some? size) size (vec3 1 1 1))
   :type :AABB
   :pos (if (some? pos) pos (Point3D 0 0 0))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn OBB
  "half size" [& [pos size orient]]
  {:size (if (some? size) size (vec3 1 1 1))
   :type :OBB
   :orient (if (some? orient) orient (mat3))
   :pos (if (some? pos) pos (Point3D 0 0 0))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Plane
  "" [& [normal dist]]
  {:dist (if (number? dist) dist 0)
   :type :plane
   :normal (if (some? normal) normal (vec3 1 0 0))})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Triangle "" [& [a b c :as args]]
  {:type :triangle
   :size 3 :cells (if (empty? args)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn planeEquation "" [point plane]
  (let [{:keys [normal dist]} plane] (- (v3-dot point normal) dist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pointInSphere? "" [point sphere]
  (let [{:keys [radius pos]} sphere]
    (< (v3-lensq (v3-sub point pos)) (sqr* radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pointOnPlane? "" [point plane]
  (let [{:keys [normal dist]} plane]
    (CMP (- (v3-dot point normal) dist) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pointInAABB? "" [point aabb]
  (let [{nx :x ny :y nz :z} (getMin3D aabb)
        {:keys [x y z]} point
        {xx :x xy :y xz :z} (getMax3D aabb)]
    (not (or (< x nx) (< y ny) (< z nz)
             (> x xx) (> y xy) (> z xz)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pointInOBB? "" [point obb]
  (let [{:keys [size pos orient]} obb
        {sx :x sy :y sz :z} size
        [r1 r2 r3 :as m3]
        (partition 3 (:cells orient))
        sizeL [sx sy sz]
        dir (v3-sub point pos)]
    (loop [i 0 SZ 3 ok? true]
      (if (or (>= i SZ)
              (not ok?))
        ok?
        (let [r (nth m3 i)
              axis (vec3 (nth r 0)(nth r 1)(nth r 2))
              distance (v3-dot dir axis)]
          (recur (+ 1 i)
                 SZ
                 (not (or (> distance (nth sizeL i))
                          (< distance  (- (nth sizeL i)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti closestPoint "" (fn [a & more] (:type a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod closestPoint :sphere [sphere point]
  (let [{:keys [pos radius]} sphere]
    (-> (v3-sub point pos) (v3-unit) (v3-scale radius) (v3-add pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod closestPoint :AABB [aabb point]
  (let [{nx :x ny :y nz :z} (getMin3D aabb)
        {xx :x xy :y xz :z} (getMax3D aabb)
        {:keys [x y z]} point
        rx (if (< x nx) nx x)
        ry (if (< y ny) ny y)
        rz (if (< z nz) nz z)]
    (Point3D (if (> rx xx) xx rx)
             (if (> ry xy) xy ry)
             (if (> rz xz) xz rz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod closestPoint :OBB [obb point]
  (let [{:keys [pos orient size]} obb
        [r1 r2 r3 :as m3]
        (partition 3 (:cells orient))
        sizeL [(:x size)(:y size)(:z size)]
        dir (v3-sub point pos)]
    (loop [i 0 SZ 3 result pos]
      (if (>= i SZ)
        result
        (let [r (nth m3 i)
              s (nth sizeL i)
              axis (vec3 (nth r 0)
                         (nth r 1)
                         (nth r 2))
              d (v3-dot dir axis)
              d (if (> d s) s d)
              d (if (< d (- s)) (- s) d)]
          (recur (+ 1 i)
                 SZ
                 (v3-add result (v3-scale axis d))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod closestPoint :plane [plane point]
  ;;This works assuming plane.Normal is normalized, which it should be
  (let [{:keys [normal dist]} plane
        distance (- (v3-dot normal point) dist)]
    ;;If the plane normal wasn't normalized, we'd need this:
    ;;distance = distance / DOT(plane.Normal, plane.Normal);
    (v3-sub point (v3-scale normal dist))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pointOnLine? "" [point line]
  (let [c (closestPoint line point)]
    (CMP (v3-lensq (v3-sub c point)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod closestPoint :line [line point]
  (let [{:keys [start end]} line
        lVec (v3-sub end start)
        ;;Project "point" onto the "Line Vector", computing:
        ;;closest(t) = start + t * (end - start)
        ;;T is how far along the line the projected point is
        t (/ (v3-dot (v3-sub point start) lVec) (v3-dot lVec lVec))
        ;; Clamp t to the 0 to 1 range
        t (max t 0)
        t (min t 1)]
    ;;return projected position of t
    (v3-add start (v3-scale lVec t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pointOnRay? "" [point ray]
  (let [{:keys [origin dir]} ray]
    (cond
      (v3-eq? point origin)
      true
      :else
      (let [norm (v3-unit (v3-sub point origin))
            diff (v3-dot norm dir)]
        ;;If vectors in the same dir, diff be 1
        (CMP diff 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod closestPoint :ray [ray point]
  (let [{:keys [origin dir]} ray
        ;;Project point onto ray,
        t (v3-dot (v3-sub point origin) dir)
        ;;We only want to clamp t in the positive direction.
        ;;The ray extends infinatley in this direction!
        t (max t 0)]
    ;; Compute the projected position from the clamped t
    ;; Notice we multiply r.Normal by t, not AB.
    ;; This is becuase we want the ray in the direction
    ;; of the normal, which technically the line segment is
    ;; but this is much more explicit and easy to read.
    (Point3D (v3-add origin (v3-scale dir t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sphereSphere? "" [s1 s2]
  (let [{r1 :radius p1 :pos} s1
        {r2 :radius p2 :pos} s2]
    (< (v3-lensq (v3-sub p1 p2)) (sqr* (+ r1 r2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sphereAABB? "" [sphere aabb]
  (let [{:keys [radius pos]} sphere
        cp (closestPoint aabb pos)]
    (< (v3-lensq (v3-sub pos cp)) (sqr* radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn sphereOBB? "" [sphere obb]
  (let [{:keys [radius pos]} sphere
        cp (closestPoint obb pos)]
    (< (v3-lensq (v3-sub pos cp)) (sqr* radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn spherePlane? "" [sphere plane]
  (let [{:keys [radius pos]} sphere
        cp (closestPoint plane pos)]
    (< (v3-lensq (v3-sub pos cp)) (sqr* radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aabbAABB? "" [aabb1 aabb2]
  (let [{anx :x any :y anz :z} (getMin3D aabb1)
        {axx :x axy :y axz :z} (getMax3D aabb1)
        {bnx :x bny :y bnz :z} (getMin3D aabb2)
        {bxx :x bxy :y bxz :z} (getMax3D aabb2)]
    (and (and (<= anx bxx)(>= axx bnx))
         (and (<= any bxy)(>= axy bny))
         (and (<= anz bxz)(>= axz bnz)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aabbOBB? "" [aabb obb]
  (let [{:keys [orient]} obb
        tmp (transient [])
        [r1 r2 r3 :as m3]
        (partition 3 (:cells orient))
        tmp' [(vec3 1 0 0);;AABB axis 1
              (vec3 0 1 0);;AABB axis 2
              (vec3 0 0 1);;AABB axis 3
              (vec3 (nth r1 0)(nth r1 1)(nth r1 2))
              (vec3 (nth r2 0)(nth r2 1)(nth r2 2))
              (vec3 (nth r3 0)(nth r3 1)(nth r3 2))]
        _ (dotimes [i 3]
            (conj! tmp (v3-xss (nth tmp' i)(nth tmp' 0)))
            (conj! tmp (v3-xss (nth tmp' i)(nth tmp' 1)))
            (conj! tmp (v3-xss (nth tmp' i)(nth tmp' 2))))
        tsts (concat tmp' (persistent! tmp))]
    ;;looking for Separating axis
    (loop [i 0 SZ (count tsts) ok? true]
      (if (or (>= i SZ)
              (not ok?))
        ok?
        (recur (+ 1 i)
               SZ
               (overlapOnAxis? aabb obb (nth tsts i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti overlapOnAxis? "" (fn [a b & more] [(:type a)(:type b)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod overlapOnAxis? [:AABB :OBB] [aabb obb axis]
  (let [{amin :min amax :max} (getInterval3D aabb axis)
        {bmin :min bmax :max} (getInterval3D obb axis)]
    (and (<= bmin amax) (<= amin bmax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod overlapOnAxis? [:OBB :OBB] [obb1 obb2 axis]
  (let [{amin :min amax :max} (getInterval3D obb1 axis)
        {bmin :min bmax :max} (getInterval3D obb2 axis)]
    (and (<= bmin amax) (<= amin bmax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod overlapOnAxis? [:AABB :triangle] [aabb triangle axis]
  (let [{amin :min amax :max} (getInterval3D aabb axis)
        {bmin :min bmax :max} (getInterval3D triangle axis)]
    (and (<= bmin amax) (<= amin bmax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod overlapOnAxis? [:OBB :triangle] [obb triangle axis]
  (let [{amin :min amax :max} (getInterval3D obb axis)
        {bmin :min bmax :max} (getInterval3D triangle axis)]
    (and (<= bmin amax) (<= amin bmax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod overlapOnAxis? [:triangle :triangle] [t1 t2 axis]
  (let [{amin :min amax :max} (getInterval3D t1 axis)
        {bmin :min bmax :max} (getInterval3D t2 axis)]
    (and (<= bmin amax) (<= amin bmax))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti getInterval3D (fn [a & more] (:type a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getInterval3D :triangle [triangle axis]
  (let [{:keys [points]} triangle
        v (v3-dot axis (aget points 0))]
    (loop [i 1 SZ 3 _min v _max v]
      (if (>= i SZ)
        (Interval _min _max)
        (let [v (v3-dot axis (aget points i))]
          (recur (+ 1 i)
                 SZ
                 (min _min v)(max _max v)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getInterval3D :OBB [obb axis]
  (let [{:keys [pos size orient]} obb
        C pos ;;OBB Center
        E [(:x size)(:y size)(:z size)] ;;OBB Extents
        [r1 r2 r3 :as m3]
        (partition 3 (:cells orient))
        A [;;OBB Axis
           (vec3 (nth r1 0)(nth r1 1)(nth r1 2))
           (vec3 (nth r2 0)(nth r2 1)(nth r2 2))
           (vec3 (nth r3 0)(nth r3 1)(nth r3 2))]
        vertex [(v3-add (v3-add (v3-add C (v3-scale (nth A 0) (nth E 0)))
                                (v3-scale (nth A 1) (nth E 1)))
                        (v3-scale (nth A 2) (nth E 2)))
                (v3-add (v3-add (v3-sub C (v3-scale (nth A 0)(nth E 0)))
                                (v3-scale (nth A 1)(nth E 1)))
                        (v3-scale (nth A 2)(nth E 2)))
                (v3-add (v3-sub (v3-add C (v3-scale (nth A 0)(nth E 0)))
                                (v3-scale (nth A 1)(nth E 1)))
                        (v3-scale (nth A 2)(nth E 2)))
                (v3-sub (v3-add (v3-add C (v3-scale (nth A 0)(nth E 0)))
                                (v3-scale (nth A 1)(nth E 1)))
                        (v3-scale (nth A 2)(nth E 2)))
                (v3-sub (v3-sub (v3-sub C (v3-scale (nth A 0)(nth E 0)))
                                (v3-scale (nth A 1)(nth E 1)))
                        (v3-scale (nth A 2)(nth E 2)))
                (v3-sub (v3-sub (v3-add C (v3-scale (nth A 0)(nth E 0)))
                                (v3-scale (nth A 1)(nth E 1)))
                        (v3-scale (nth A 2)(nth E 2)))
                (v3-sub (v3-add (v3-sub C (v3-scale (nth A 0)(nth E 0)))
                                (v3-scale (nth A 1)(nth E 1)))
                        (v3-scale (nth A 2)(nth E 2)))
                (v3-add (v3-sub (v3-sub C (v3-scale (nth A 0)(nth E 0)))
                                (v3-scale (nth A 1)(nth E 1)))
                        (v3-scale (nth A 2)(nth E 2)))]
        v (v3-dot axis (nth vertex 0))]
    (loop [i 1 SZ 8 _min v _max v]
      (if (>= i SZ)
        (Interval _min _max)
        (let [p (v3-dot axis (nth vertex i))]
          (recur (+ i 1)
                 SZ
                 (if (< p _min) p _min)
                 (if (> p _max) p _max)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getInterval3D :AABB [aabb axis]
  (let [{ix :x iy :y iz :z} (getMin3D aabb)
        {ax :x ay :y az :z} (getMax3D aabb)
        vertex [(vec3 ix ay az)
                (vec3 ix ay iz)
                (vec3 ix iy az)
                (vec3 ix iy iz)
                (vec3 ax ay az)
                (vec3 ax ay iz)
                (vec3 ax iy az)
                (vec3 ax iy iz)]
        v (v3-dot axis (nth vertex 0))]
    (loop [i 1 SZ 8 _min v _max v]
      (if (>= i SZ)
        (Interval _min _max)
        (let [p (v3-dot axis (nth vertex i))]
          (recur (+ i 1)
                 SZ
                 (if (< p _min) p _min)
                 (if (> p _max) p _max)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn aabbPlane? "" [aabb plane]
  ;;Project the half extents of the AABB onto the plane normal
  (let [{{nx :x ny :y nz :z :as normal} :normal {:keys [dist]}} plane
        {{zx :x sy :y sz :z} :size {:keys [pos]}} aabb
        pLen (+ (* sx (abs* nx))
                (* sy (abs* ny))(* sz (abs* nz)))
        ;;Find the distance from the center of the AABB to the plane
        d (- (v3-dot normal pos) dist)]
    ;;Intersection occurs if the distance falls within the projected side
    (<= (abs* d) pLen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn obbOBB? "" [obb1 obb2]
  (let [{o1 :orient} obb1
        {o2 :orient} obb2
        [o11 o12 o13 :as b1]
        (partition 3 (:cells o1))
        [o21 o22 o23 :as b2]
        (partition 3 (:cells o2))
        tmp (transient [])
        tmp' [(vec3 (nth o11 0)(nth o11 1)(nth o11 2))
              (vec3 (nth o12 0)(nth o12 1)(nth o12 2))
              (vec3 (nth o13 0)(nth o13 1)(nth o13 2))
              (vec3 (nth o21 0)(nth o21 1)(nth o21 2))
              (vec3 (nth o22 0)(nth o22 1)(nth o22 2))
              (vec3 (nth o23 0)(nth o23 1)(nth o23 2))]
        _ (dotimes [i 3]
            (conj! tmp (v3-xss (nth tmp' i)(nth tmp' 0)))
            (conj! tmp (v3-xss (nth tmp' i)(nth tmp' 1)))
            (conj! tmp (v3-xss (nth tmp' i)(nth tmp' 2))))
        tsts (concat tmp' (persistent! tmp))]
    (loop [i 0 SZ (count tsts) ok? true]
      (if (or (>= i SZ)
              (not ok?))
        ok?
        (recur (+ i 1)
               SZ
               (overlapOnAxis obb1 obb2 (nth tsts i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn obbPlane? "" [obb plane]
  (let [{:keys [pos size orient]} obb
        {sx :x sy :y sz :z} size
        {:keys [dist normal]} plane
        [r1 r2 r3 :as o3]
        (partition 3 (:cells orient))
        rot [(vec3 (nth r1 0)(nth r1 1)(nth r1 2))
             (vec3 (nth r2 0)(nth r2 1)(nth r2 2))
             (vec3 (nth r3 0)(nth r3 1)(nth r3 2))]
        ;;Project the half extents of the AABB onto the plane normal
        pLen (+ (* sx (abs* (v3-dot normal (nth rot 0))))
                (* sy (abs* (v3-dot normal (nth rot 1))))
                (* sz (abs* (v3-dot normal (nth rot 2)))))
        ;;Find the distance from the center of the OBB to the plane
        d (- (v3-dot normal pos) dist)]
    ;;Intersection occurs if the distance falls within the projected side
    (<= (abs* d) pLen)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn planePlane? "" [p1 p2]
  ;;Compute direction of intersection line
  (let [d (v3-xss (:normal p1)(:normal p2))]
    ;;Check the length of the direction line
    ;;if the length is 0, no intersection happened
    (not (CMP (v3-dot d d) 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- raycastResult "" []
  {:t -1 :hit? false :normal (vec3 0 0 1) :point (Point3D)})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti raycast "" (fn [a & more] (:type a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod raycast :sphere [sphere ray]
  (let [{:keys [radius pos]} sphere
        {:keys [dir origin]} ray
        e (v3-sub pos origin)
        rSq (sqr* radius)
        eSq (v3-lensq e)
        a (v3-dot e dir) ;;ray.direction is assumed to be normalized
        bSq (- eSq (sqr* a))
        f (sqrt* (abs* (- rSq bSq)))
        ;;Assume normal intersection!
        t (- a f)]
    ;;No collision has happened?
    (if-not (neg? (- rSq (- eSq (sqr* a))))
      ;;Ray starts inside the sphere
      (let [t (if (< eSq rSq) ;;Just reverse direction
                (+ a f) t)
            point (v3-add origin (v3-scale dir t))]
        (merge (raycastResult)
               {:t t :hit? true :point point
                :normal (v3-unit (v3-sub point pos))})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod raycast :OBB [obb ray]
  (let [{:keys [pos size orient]} obb
        {:keys [origin dir]} ray
        sizeL [(:x size)(:y size)(:z size)]
        [o1 o2 o3 :as m3]
        (partition 3 (:cells orient))
        p (v3-sub pos origin)
        X (vec3 (nth o1 0)(nth o1 1)(nth o1 2))
        Y (vec3 (nth o2 0)(nth o2 1)(nth o2 2))
        Z (vec3 (nth o3 0)(nth o3 1)(nth o3 2))
        f #js [(v3-dot X dir)
               (v3-dot Y dir)(v3-dot Z dir)]
        e #js [(v3-dot X p)(v3-dot Y p)(v3-dot Z p)]
        t #js [0 0 0 0 0 0]
        cont? (loop [i 0
                     SZ 3
                     ok? true]
                (if (or (>= i SZ) (not ok?))
                  ok?
                  (let [y? (if (CMP (aget f i) 0)
                             (if (or (pos? (- (- (aget e i))
                                              (aget sizeL i)))
                                     (neg? (+ (- (aget e 0))
                                              (aget sizeL i))))
                               false
                               (do->true (aset f i 0.00001))) ;Avoid div by 0!
                             true)]
                    (when y?
                      (aset t
                            (+ 0 (* i 2))
                            (/ (+ (aget e i)
                                  (aget sizeL i)) (aget f i)))  ;;tmin[x, y, z]
                      (aset t
                            (+ 1 (* i 2))
                            (/ (- (aget e i)
                                  (aget sizeL i)) (aget f i)))) ;;tmax[x, y, z]
                    (recur (+ 1 i) SZ y?))))]
    (if cont?
      (let [tmin (max (max (min (aget t 0) (aget t 1))
                           (min (aget t 2) (aget t 3)))
                      (min (aget t 4) (aget t 5)))
            tmax (min (min (max (aget t 0) (aget t 1))
                           (max (aget t 2) (aget t 3)))
                      (max (aget t 4) (aget t 5)))]
        ;;if tmax < 0, ray is intersecting AABB
        ;;but entire AABB is behind it's origin
        ;;if tmin > tmax, ray doesn't intersect AABB
        (if-not (or (neg? tmax)
                    (> tmin tmax))
          (let [;;If tmin is < 0, tmax is closer
                t_result (if (neg? tmin) tmax tmin)
                normals [X ;;+x
                         (v3-scale X -1) ;;-x
                         Y ;; +y
                         (v3-scale Y -1) ;;-y
                         Z ;; +z
                         (v3-scale Z -1)]
                _normal (loop [i 0 SZ 6 N nil]
                          (if (>= i SZ)
                            N
                            (recur (+ i 1)
                                   SZ
                                   (if (CMP t_result (aget t i))
                                     (v3-unit (nth normals i))
                                     N))))]
            (merge (raycastResult)
                   {:hit? true
                    :normal _normal
                    :t t_result
                    :point (v3-add origin (v3-scale dir t_result))})))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod raycast :AABB [aabb ray]
  (let [{nx :x ny :y nz :z} (getMin3D aabb)
        {xx :x xy :y xz :z} (getMax3D aabb)
        {:keys [origin dir]} ray
        {ox :x oy :y oz :z} origin
        {dx :x dy :y dz :z} dir
        {:keys []} aabb
        ;;Any component of direction could be 0!
        ;;Address this by using a small number, close to
        ;;0 in case any of directions components are 0
        t1 (/ (- nx ox) (if (CMP dx 0) 0.00001 dx))
        t2 (/ (- xx ox) (if (CMP dx 0) 0.00001 dx))
        t3 (/ (- ny oy) (if (CMP dy 0) 0.00001 dy))
        t4 (/ (- xy oy) (if (CMP dy 0) 0.00001 dy))
        t5 (/ (- nz oz) (if (CMP dz 0) 0.00001 dz))
        t6 (/ (- xz oz) (if (CMP dz 0) 0.00001 dz))
        tmin (max (max (min t1 t2)(min t3 t4)) (min t5 t6))
        tmax (min (min (max t1 t2)(max t3 t4)) (max t5 t6))]
    ;;if tmax < 0, ray is intersecting AABB
    ;;but entire AABB is behind it's origin
    ;;if tmin > tmax, ray doesn't intersect AABB

    (if-not (or (neg? tmax)
                (> tmin tmax))
      (let [;;If tmin is < 0, tmax is closer
            t_result (if (neg? tmin) tmax tmin)
            normals [(vec3 -1 0 0)
                     (vec3 1 0 0)
                     (vec3 0 -1 0)
                     (vec3 0 1 0)
                     (vec3 0 0 -1)
                     (vec3 0 0 1)]
            t [t1 t2 t3 t4 t5 t6]
            _normal (loop [i 0 SZ 6 N nil]
                      (if (>= i SZ)
                        N
                        (recur (+ 1 i)
                               SZ
                               (if (CMP t_result (nth t i))
                                 (nth normals i)
                                 N))))]
        (merge (raycastResult)
               {:t t_result :hit? true
                :point (v3-add origin (v3-scale dir t_result))})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod raycast :plane [plane ray]
  (let [{:keys [normal dist]} plane
        {:keys [origin dir]} ray
        nd (v3-dot dir normal)
        pn (v3-dot origin normal)]
    ;;nd must be negative, and not 0
    ;;if nd is positive, the ray and plane normals
    ;;point in the same direction. No intersection.
    (if (neg? nd)
      (let [t (/ (- dist pn) nd)]
        (if (>= t 0)
          (merge (raycastResult)
                 {:t t :hit? true
                  :point (v3-add origin (v3-scale dir t))
                  :normal (v3-unit normal)}))))))
;;kenl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
defmulti lineTest "" (fn [a & more] (:type a))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod lineTest :sphere [sphere line]
  (let [{:keys [radius pos]} sphere
        cp (closestPoint line pos)]
    (<= (v3-lensq (v3-sub pos cp)) (sqr* radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod lineTest :plane [plane line]
  (let [{:keys [start end]} line
        {:keys [normal dist]} plane
        ab (v3-sub end start)
        nA (v3-dot normal start)
        nAB (v3-dot normal ab)]
    (if-not (CMP nAB 0)
      (let [t (/ (- dist nA) nAB)]
        (and (>= t 0) (<= t 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod lineTest :AABB [aabb line]
  (let [{:keys []} aaaa
        {:keys [start end]} line
        ray (Ray start (v3-unit (v3-sub end start)))
        raycast (raycast aabb ray)]
    (if (some? raycast)
      (let [{:keys [t]} raycast]
        (and (>= t 0)
             (<= (sqr* t)
                 (v3-lensq line)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod lineTest :OBB [obb line]
  (let [{:keys []} obb
        {:keys [start end]} line]
    (if (< (v3-lensq (v3-sub end start)) 0.0000001)
      (pointInOBB? start obb)
      (let [ray (Ray start (v3-unit (v3-sub end start)))
            result (raycast obb ray)]
        (if (some? result)
          (let [{:keys [t]} result]
            (and (>= t 0)
                 (<= (sqr* t)
                     (v3-lensq line)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn centroid "" [triangle]
  (let [[p1 p2 p3] (:points triangle)
        {x1 :x y1 :y z1 :z} p1
        {x2 :x y2 :y z2 :z} p2
        {x3 :x y3 :y z3 :z} p3]
    (v3-scale (vec3 (+ x1 x2 x3)
                    (+ y1 y2 y3)
                    (+ z1 z2 z3)) (/ 1 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pointInTriangle? "" [p t]
  ;;Move the triangle so that the point is
  ;;now at the origin of the triangle
  (let [[p1 p2 p3] (:points t)
        a (v3-sub p1 p)
        b (v3-sub p2 p)
        c (v3-sub p3 p)
        ;;The point should be moved too, so they are both
        ;;relative, but because we don't use p in the
        ;;equation anymore, we don't need it!
        ;;p -= p; // This would just equal the zero vector!
        normPBC (v3-xss b c) ;;Normal of PBC (u)
        normPCA (v3-xss c a) ;;Normal of PCA (v)
        normPAB (v3-xss a b)] ;;Normal of PAB (w)
    ;;Test to see if the normals are facing
    ;;the same direction, return false if not
  (not (or (neg? (v3-dot normPBC normPCA))
           (neg? (v3-dot normPBC normPAB))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn baryCentricOptimized "" [p t]
  (let [[p1 p2 p3] (:points t)
        v0 (v3-sub p2 p1)
        v1 (v3-sub p3 p1)
        v2 (v3-sub p p1)
        d00 (v3-dot v0 v0)
        d01 (v3-dot v0 v1)
        d11 (v3-dot v1 v1)
        d20 (v3-dot v2 v0)
        d21 (v3-dot v2 v1)
        denom (- (* d00 d11)(* d01 d01))]
    (if (CMP denom 0)
      (vec3)
      (let [y (/ (- (* d11 d20)(* d01 d21)) denom)
            z (/ (- (* d00 d21)(* d01 d20)) denom)]
        (vec3 (- 1 y z) y z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn baryCentric "" [p t]
  (let [[p1 p2 p3] (:points t)
        ap (v3-sub p p1)
        bp (v3-sub p p2)
        cp (v3-sub p p3)
        ab (v3-sub p2 p1)
        ac (v3-sub p3 p1)
        bc (v3-sub p3 p2)
        cb (v3-sub p2 p3)
        ca (v3-sub p1 p3)
        v (v3-sub ab (project ab cb))
        a (- 1 (/ (v3-dot v ap)
                  (v3-dot v ab)))
        v (v3-sub bc (project bc ac))
        b (- 1 (/ (v3-dot v bp)(v3-dot v bc)))
        v (v3-sub ca (project ca ab))
        c (- 1 (/ (v3-dot v cp)(v3-dot v ca)))]
    (vec3 a b c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fromTriangle "" [t]
  (let [[p1 p2 p3] (:points t)
        normal (v3-unit (v3-xss (v3-sub p2 p1)
                                (v3-sub p3 p1)))]
    (Plane normal (v3-dot normal p1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod closestPoint :triangle [t p]
  (let [plane (fromTriangle t)
        cp (closestPoint plane p)]
    (if (pointInTriangle? cp t)
      cp
      (let [c1 (closestPoint (Line p1 p2) cp) ;;Line AB
            c2 (closestPoint (Line p2 p3) cp) ;;Line BC
            c3 (closestPoint (Line p3 p1) cp) ;;Line CA
            magSq1 (v3-lensq (v3-sub cp c1))
            magSq2 (v3-lensq (v3-sub cp c2))
            magSq3 (v3-lensq (v3-sub cp c3))]
        (cond
          (and (< magSq1 magSq2)(< magSq1 magSq3))
          c1
          (and (< magSq2 magSq1)(< magSq2 magSq3))
          c2
          :else c3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn triangleSphere? "" [t s]
  (let [{:keys [pos radius]} s
       cp (closestPoint t pos)]
    (<= (v3-lensq (v3-sub cp pos)) (sqr* radius))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn triangleAABB? "" [t aabb]
  (let [;;Compute the edge vectors of the triangle  (ABC)
        [p1 p2 p3] (:points t)
        f0 (v3-sub p2 p1)
        f1 (v3-sub p3 p2)
        f2 (v3-sub p1 p3)
        ;;Compute the face normals of the AABB
        u0 (vec3 1 0 0)
        u1 (vec3 0 1 0)
        u2 (vec3 0 0 1)
        tsts [;;3 Normals of AABB
              u0 ;;AABB Axis 1
              u1 ;;AABB Axis 2
              u2 ;;AABB Axis 3
              ;;1 Normal of the Triangle
              (v3-xss f0 f1)
              ;;9 Axis, cross products of all edges
              (v3-xss u0 f0)
              (v3-xss u0 f1)
              (v3-xss u0 f2)
              (v3-xss u1 f0)
              (v3-xss u1 f1)
              (v3-xss u1 f2)
              (v3-xss u2 f0)
              (v3-xss u2 f1)
              (v3-xss u2 f2)]]
    (loop [i 0 SZ (count tsts) ok? true]
      (if (or (>= i SZ)
              (not ok?))
        ok?
        (recur (+ 1 i)
               SZ
               (overlapOnAxis? aabb  t (nth tsts i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn triangleOBB? "" [t obb]
  (let [[p1 p2 p3] (:points t)
        {:keys [orient]} obb
        [o1 o2 o3]
        (partition 3 (:cells orient))
        ;;Compute the edge vectors of the triangle  (ABC)
        f0 (v3-sub p2 p1)
        f1 (v3-sub p3 p2)
        f2 (v3-sub p1 p3)
        ;;Compute the face normals of the AABB
        u0 (vec3 (nth o1 0)(nth o1 1)(nth o1 2))
        u1 (vec3 (nth o2 0)(nth o2 1)(nth o2 2))
        u2 (vec3 (nth o3 0)(nth o3 1)(nth o3 2))
        tsts [;;3 Normals of AABB
              u0 ;;AABB Axis 1
              u1 ;;AABB Axis 2
              u2 ;;AABB Axis 3
              ;;1 Normal of the Triangle
              (v3-xss f0 f1)
              ;;9 Axis, cross products of all edges
              (v3-xss u0 f0)
              (v3-xss u0 f1)
              (v3-xss u0 f2)
              (v3-xss u1 f0)
              (v3-xss u1 f1)
              (v3-xss u1 f2)
              (v3-xss u2 f0)
              (v3-xss u2 f1)
              (v3-xss u2 f2)]]
    (loop [i 0 SZ (count tsts) ok? true]
      (if (or (>= i SZ)
              (not ok?))
        ok?
        (recur (+ 1 i)
               SZ
               (overlapOnAxis? obb t (nth tsts i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn triangleTriangle? "" [t1 t2]
  (let [[p11 p12 p13] (:points t1)
        [p21 p22 p23] (:points t2)
        t1_f0 (v3-sub p12 p11) ;;Edge 0
        t1_f1 (v3-sub p13 p12) ;;Edge 1
        t1_f2 (v3-sub p11 p13) ;;Edge 2
        t2_f0 (v3-sub p22 p21) ;;Edge 0
        t2_f1 (v3-sub p23 p22) ;;Edge 1
        t2_f2 (v3-sub p21 p23) ;;Edge 2
        tsts [;;Triangle 1, Normal
              (v3-xss t1_f0 t1_f1)
              ;;Triangle 2, Normal
              (v3-xss t2_f0 t2_f1)
              ;;Cross Product of edges
              (v3-xss t2_f0 t1_f0)
              (v3-xss t2_f0 t1_f1)
              (v3-xss t2_f0 t1_f2)
              (v3-xss t2_f1 t1_f0)
              (v3-xss t2_f1 t1_f1)
              (v3-xss t2_f1 t1_f2)
              (v3-xss t2_f2 t1_f0)
              (v3-xss t2_f2 t1_f1)
              (v3-xss t2_f2 t1_f2)]]
    (loop [i 0 SZ (count tsts) ok? true]
      (if (or (>= i SZ)
              (not ok?))
        ok?
        (recur (+ 1 i)
               SZ
               (overlapOnAxis? t1 t2 (nth tsts i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn triangleTriangleRobust? "" [t1 t2]
  (let [[p11 p12 p13] (:points t1)
        [p21 p22 p23] (:points t2)
        tsts [;;Triangle 1, Normal
              (satCrossEdge p11 p12 p12 p13)
              ;;Triangle 2, Normal
              (satCrossEdge p21 p22 p22 p23)
              ;;Cross Product of edges
              (satCrossEdge p21 p22 p11 p12)
              (satCrossEdge p21 p22 p12 p13)
              (satCrossEdge p21 p22 p13 p11)
              (satCrossEdge p22 p23 p11 p12)
              (satCrossEdge p22 p23 p12 p13)
              (satCrossEdge p22 p23 p13 p11)
              (satCrossEdge p23 p21 p11 p12)
              (satCrossEdge p23 p21 p12 p13)
              (satCrossEdge p23 p21 p13 p11)]]
    (loop [i 0 SZ (count tsts) ok? true]
      (if (or (>= i SZ)
              (not ok?))
        ok?
        (recur (+ i 1)
               SZ
               (not (and (not (overlapOnAxis? t1 t2 (nth tsts i)))
                         (not (CMP (v3-lensq (nth tsts i)) 0)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- satCrossEdge "" [a b c d]
  (let [ab (v3-sub b a)
        cd (v3-sub d c)
        result (v3-xss ab cd)]
    ;;Is ab and cd parallel?
    (if-not (CMP (v3-lensq result) 0)
      result ;;not parallel
      (let [;;ab and cd are parallel
            ;;Get an axis perpendicular to AB
            axis (v3-xss ab (v3-sub c a))
            result (v3-xss ab axis)]
        ;;Still parallel?
        (if-not (CMP (v3-lensq result) 0)
          result ;;Not parallel
          ;;New axis being tested is parallel too.
          ;;This means that a, b, c and d are on a line
          ;;Nothing we can do!
          (vec3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod raycast :triangle [triangle ray]
  (let [plane (fromTriangle triangle)
        {:keys [origin dir]} ray
        planeResult (raycast plane ray)]
    (if (some? planeResult)
      (let [{:keys [t]} planeResult
            result (v3-add origin (v3-scale dir t))
            {bx :x by :y bz :z} (baryCentric result triangle)]
        (if (and (>= bx 0)(<= bx 1)
                 (>= by 0)(<= by 1)
                 (>= bz 0)(<= bz 1))
          (merge (raycastResult)
                 {:t t :hit? true
                  :normal (:normal plane)
                  :point (v3-add origin (v3-scale dir t))}))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod lineTest? :triangle [triangle line]
  (let [{:keys [start end]} line
        ray (Ray start (v3-unit (v3-sub end start)))
        raycast (raycast triangle ray)]
    (if (some? raycast)
      (let [{:keys [t]} raycast]
        (and (>= t 0)
             (<= (sqr* t) (v3-lensq line)))))))

;kenl
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn trianglePlane? "" [t p]
  (let [[p1 p2 p3] (:points t)
        side1 (planeEquation p1 p)
        side2 (planeEquation p2 p)
        side3 (planeEquation p3 p)]
    ;;on plane
    ;;(and (CMP side1 0)(CMP side2 0)(CMP side3 0))
    ;;Triangle in front of plane
    ;;Triangle behind plane
    (not (or (and (pos? side1)(pos? side2)(pos? side3))
             (and (neg? side1)(neg? side2)(neg? side3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn intersection "planes" [p1 p2 p3]
  (let [{{p1x :x p1y :y p1z :z} :normal d1 :dist} p1
        {{p2x :x p2y :y p2z :z} :normal d2 :dist} p2
        {{p3x :x p3y :y p3z :z} :normal d3 :dist} p3
        {:keys [cells] :as D} (mat3 p1x p2x p3x
                                    p1y p2y p3y
                                    p1z p2z p3z)
        {ax :x ay :y az :z :as A}
        (vec3 (- d1) (- d2) (- d3))
        Dx (aclone cells)
        Dy (aclone cells)
        Dz (aclone cells)
        _ (aset Dx (mx-pos 3 3 1 1) ax)
        _ (aset Dx (mx-pos 3 3 1 2) ay)
        _ (aset Dx (mx-pos 3 3 1 3) az)
        _ (aset Dy (mx-pos 3 3 2 1) ax)
        _ (aset Dy (mx-pos 3 3 2 2) ay)
        _ (aset Dy (mx-pos 3 3 2 3) az)
        _ (aset Dz (mx-pos 3 3 3 1) ax)
        _ (aset Dz (mx-pos 3 3 3 2) ay)
        _ (aset Dz (mx-pos 3 3 3 3) az)
        detD (m3-determ D)
        [x y z]
        (if (CMP detD 0)
          [0 0 0]
          [(/ (m3-determ (mx-new* 3 3 Dx)) detD)
           (/ (m3-determ (mx-new* 3 3 Dy)) detD)
           (/ (m3-determ (mx-new* 3 3 Dz)) detD)])]
    (Point x y z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti classify "" (fn [a & args] (:type a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod classify :AABB [aabb plane]
  ;;maximum extent in direction of plane normal
  (let [{:keys [dist normal]} plane
        {:keys [pos size]} aabb
        {sx :x sy :y sz :z} size
        {nx :x ny :y nz :z} normal
        r (+ (abs* (* sx nx))
             (abs* (* sy ny))
             (abs* (* sz nz)))
        ;;signed distance between box center and plane
        d (+ (v3-dot normal pos) dist)]
    (cond
      (< (abs* d) r)
      0
      (neg? d)
      (+ d r)
      :else
      (- d r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod classify :OBB [obb plane]
  (let [{:keys [pos size orient]} obb
        {:keys [normal dist]} plane
        {sx :x sy :y sz :z} size
        {nx :x ny :y nz :z} normal
        normal (m3-multVec3 normal orient)
        ;;maximum extent in direction of plane normal
        r (+ (abs* (* sx nx))
             (abs* (* sy ny))
             (abs* (* sz nz)))
        ;;signed distance between box center and plane
        d (+ (v3-dot normal pos) dist)]
    (cond
      (< (abs* d) r)
      0
      (neg? d)
      (+ d r)
      :else
      (- d r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getVertices "" [obb]
  (let [{C :pos E :size :keys [orient]} obb
        [o1 o2 o3] (partition 3 (:cells orient))
        c [(:x C)(:y C)(:z C)]
        e [(:x E)(:y E)(:z E)]
        A [;;OBB Axis
           (vec3 (nth o1 0)(nth o1 1)(nth o1 2))
           (vec3 (nth o2 0)(nth o2 1)(nth o2 2))
           (vec3 (nth o3 0)(nth o3 1)(nth o3 2))]]
    [(v3-add (v3-add (v3-add C (v3-scale (nth A 0)(nth e 0)))
                     (v3-scale (nth A 1)(nth e 1)))
             (v3-scale (nth A 2)(nth e 2)))
     (v3-add (v3-add (v3-sub C (v3-scale (nth A 0)(nth e 0)))
                     (v3-scale (nth A 1)(nth e 1)))
             (v3-scale (nth A 2)(nth e 2)))
     (v3-add (v3-sub (v3-add C (v3-scale (nth A 0)(nth e 0)))
                     (v3-scale (nth A 1)(nth e 1)))
             (v3-scale (nth A 2)(nth e 2)))
     (v3-sub (v3-add (v3-add C (v3-scale (nth A 0)(nth e 0)))
                     (v3-scale (nth A 1)(nth e 1)))
             (v3-scale (nth A 2)(nth e 2)))
     (v3-sub (v3-sub (v3-sub C (v3-scale (nth A 0)(nth e 0)))
                     (v3-scale (nth A 1)(nth e 1)))
             (v3-scale (nth A 2)(nth e 2)))
     (v3-sub (v3-sub (v3-add C (v3-scale (nth A 0)(nth e 0)))
                     (v3-scale (nth A 1)(nth e 1)))
             (v3-scale (nth A 2)(nth e 2)))
     (v3-sub (v3-add (v3-sub C (v3-scale (nth A 0)(nth e 0)))
                     (v3-scale (nth A 1)(nth e 1)))
             (v3-scale (nth A 2)(nth e 2)))
     (v3-add (v3-sub (v3-sub C (v3-scale (nth A 0)(nth e 0)))
                     (v3-scale (nth A 1)(nth e 1)))
             (v3-scale (nth A 2)(nth e 2)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getEdges "" [obb]
  (let [v (getVertices obb)
        ;;Indices of edges
        index [[6 1]
               [6 3]
               [6 4]
               [2 7]
               [2 5]
               [2 0]
               [0 1]
               [0 3]
               [7 1]
               [7 4]
               [4 5]
               [5 3]]]
    (loop [j 0 SZ (count index) out []]
      (if (>= j SZ)
        out
        (recur (+ j 1)
               SZ
               (conj out
                     (Line (nth v (_1 (nth index j)))
                           (nth v (_2 (nth index j))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn getPlanes "" [obb]
  (let [{C :pos E :size :keys [orient]} obb
        [o1 o2 o3] (partition 3 (:cells orient))
        e [(:x E)(:y E)(:z E)]
        ;;OBB axis
        a [(vec3 (nth o1 0)(nth o1 1)(nth o1 2))
           (vec3 (nth o2 0)(nth o2 1)(nth o2 2))
           (vec3 (nth o3 0)(nth o3 1)(nth o3 2))]]
    [(Plane (nth a 0)
            (v3-dot (nth a 0)
                    (v3-add C (v3-scale (nth a 0) ex))))
     (Plane (v3-scale (nth a 0) -1)
            (- (v3-dot (nth a 0)
                       (v3-sub C (v3-scale (nth a 0) ex)))))
     (Plane (nth a 1)
            (v3-dot (nth a 1)
                    (v3-add C (v3-scale (nth a 1) ey))))
     (Plane (v3-scale (nth a 1) -1)
            (- (v3-dot (nth a 1)
                       (v3-sub C (v3-scale (nth a 1) ey)))))
     (Plane (nth a 2)
            (v3-dot (nth a 2)
                    (v3-add C (v3-scale (nth a 2) ez))))
     (Plane (v3-scale (nth a 2) -1)
            (- (v3-dot (nth a 2)
                       (v3-sub C (v3-scale (nth a 2) ez)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clipToPlane "" [plane line]
  (let [{:keys [start end]} line
        {:keys [normal dist]} plane
        ab (v3-sub end start)
        nA (v3-dot normal start)
        nAB (v3-dot normal ab)]
  (if-not (CMP nAB 0)
    (let [t (/ (- dist nA) nAB)]
      (if (and (>= t 0)(<= t 1))
        (v3-add start (v3-scale ab t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clipEdgesToOBB "" [edges obb]
  (let [planes (getPlanes obb)
        out (transient [])]
    (dotimes [i (count planes)]
      (dotimes [j (count edges)]
        (if-some [pt (clipToPlane (nth planes i)
                                  (nth edges j))]
          (if (pointInOBB? pt obb) (conj! out pt)))))
    (persistent! out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn penetrationDepth "" [o1 o2 axis]
  (let [{n1 :min x1 :max} (getInterval3D o1 (v3-unit axis))
        {n2 :min x2 :max} (getInterval3D o2 (v3-unit axis))]

  (if (not (and (<= n2 x1)
                (<= n1 x2)))
    [0 false] ;;No penerattion
    (let [len1 (- x1 n1)
          len2 (- x2 n2)
          _min (min n1 n2)
          _max (max x1 x2)
          length (- _max _min)]
      [(- (+ len1 len2) length) (< n2 n1)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


