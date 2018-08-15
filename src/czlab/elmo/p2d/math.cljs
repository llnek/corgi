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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 _2 _3 do-with assoc!!]])

  (:require [czlab.elmo.afx.core :as ec :refer [sqrt* abs* sqr* n# num?? invert]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private EPSILON js/Number.EPSILON)
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
(defn- js-array [z] (make-array js/Number z))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- CMP "" [x y]
  (<= (abs* (- x y))
      (* EPSILON (max 1 (max (abs* x) (abs* y))))))

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
(defn- array-eq? "" [a1 a2]
  (let [out #js [1]]
    (dotimes [i (count a1)]
      (if-not (CMP (aget a1 i)
                   (aget a2 i)) (aset out 0 -1))) (pos? (aget out 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-eq? "" [v1 v2]
  (let [{c1 :cells s1 :size t1 :type} v1
        {c2 :cells s2 :size t2 :type} v2]
    (if (and (= t1 t2)
             (= s1 s2)) (array-eq? c1 c2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YYYvec-eq? "" [v1 v2]
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
(defn- mat-pos "" [rows cols r c] (+ (- c 1) (* (- r 1) cols)))
(defn- CELL "" [rows cols r c] (mat-pos rows cols r c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mat-new*
  "" [rows cols cells] {:rows rows :cols cols :cells cells})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mat-new "" [rows cols]
  (mat-new* rows
            cols
            (do-with [out (js-array (* rows cols))]
                     (dotimes [i (count out)] (aset out i 0)))))

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
      (mat-new rows cols)
      (mat-new* rows cols (clj->js args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-identity "" [size]
  (mat-new* size
            size
            (do-with [out (js-array (* size size))]
                     (dotimes [i (count out)]
                       (aset out (CELL size size i i) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-zero "" [size]
  (mat-new* size size (js-array (* size size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat2
  "" [_11 _12
      _21 _22] (mat* [2 2] _11 _12 _21 _22))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat3
  "" [_11 _12 _13
      _21 _22 _23
      _31 _32 _33] (mat* [3 3]
                         _11 _12 _13
                         _21 _22 _23
                         _31 _32 _33))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4
  "" [_11 _12 _13 _14
      _21 _22 _23 _24
      _31 _32 _33 _34
      _41 _42 _43 _44] (mat* [4 4]
                             _11 _12 _13 _14
                             _21 _22 _23 _24
                             _31 _32 _33 _34
                             _41 _42 _43 _44))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-eq? "" [a b]
  (let [{ra :rows ca :cols va :cells} a
        {rb :rows cb :cols vb :cells} b]
    (if (and (= ra rb)
             (= ca cb)) (array-eq? va vb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-neq? "" [a b] (not (mat-eq? a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-xpose "" [m]
  (let [{:keys [rows cols cells]} m
        tmp (transient [])
        cs (partition cols cells)]
    (dotimes [i rows]
      (doseq [c cs] (conj! tmp (nth c i))))
    (mat-new* cols rows (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XXXmat-xpose "" [m]
  (let [tmp (transient [])
        {:keys [rows cols cells]} m]
    (dotimes [c cols]
      (dotimes [r rows]
        (conj! tmp (nth cells
                        (+ c (* rows
                                (mod r rows)))))))
    (mat-new* cols rows (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat3-fastInverse "" [m] (mat-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-fastInverse "" [m]
  (let [{ocells :cells :as out} (mat-xpose m)
        {:keys [rows cols cells]} m
        [m1 m2 m3 m4] (partition cols cells)
        right (vec3 (_1 m1)(_2 m1)(_3 m1))
        up (vec3 (_1 m2)(_2 m2)(_3 m2))
        forward (vec3 (_1 m3)(_2 m3)(_3 m3))
        position (vec3 (_1 m4)(_2 m4)(_3 m4))]
    (aset ocells (CELL 4 4 1 4) 0)
    (aset ocells (CELL 4 4 2 4) 0)
    (aset ocells (CELL 4 4 3 4) 0)
    (aset ocells (CELL 4 4 4 1) (- (vec-dot right position)))
    (aset ocells (CELL 4 4 4 2) (- (vec-dot up position)))
    (aset ocells (CELL 4 4 4 3) (- (vec-dot forward position))) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-scale "" [m n]
  (let [{:keys [rows cols cells]} m]
    (mat-new* rows cols
              (clj->js (map #(* n %) cells)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-multAB "" [a b]
  (let [{aRows :rows aCols :cols aCells :cells} a
        {bRows :rows bCols :cols bCells :cells} b
        _ (assert (= aCols bRows)
                  "mismatch matrices")
        out (js-array (* aRows bCols))]
    (dotimes [i aRows]
      (dotimes [j bCols]
        (aset out
              (+ j (* i bCols))
              (reduce (fn [sum k]
                        (+ sum
                           (* (aget aCells (+ k (* i aCols)))
                              (aget bCells (+ j (* k bCols)))))) 0 (range bRows)))))
    (mat-new* aRows bCols out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-determ "" (fn [m] [(:rows m)(:cols m)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-determ [2 2] [m]
  (let [{:keys [cells]} m]
    (- (* (aget cells 0) (aget cells 3))
       (* (aget cells 1) (aget cells 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mat-cut "" [m row col]
  (let [{:keys [rows cols cells]} m
        ;change to zero indexed
        row' (- 1 row)
        col' (- 1 col)
        tmp (transient [])]
    (dotimes [i rows]
      (dotimes [j cols]
        (when-not (or (= i row')
                      (= j col'))
          (conj! tmp (aget cells (+ j (* i cols)))))))
    (mat-new* (- 1 rows) (- 1 cols) (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-minor "" (fn [m] [(:rows m)(:cols m)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-minor [3 3] [m]
  (let [{:keys [rows cols cells]} m
        tmp (transient [])]
    (dotimes [i rows]
      (dotimes [j cols]
        (conj! tmp (mat-determ (mat-cut m i j)))))
    (mat-new* rows cols (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-minor [2 2] [m]
  (let [{:keys [cells]} m]
    (mat2 (aget cells 3) (aget cells 2)
          (aget cells 1) (aget cells 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mx-cofactor "" [minor]
  (let [{:keys [rows cols cells]} minor
        tmp (js-array (* rows cols))]
    (dotimes [i rows]
      (dotimes [j cols]
        (let [k (+ i (* j cols))]
          (aset tmp
                k
                (* (aget cells k)
                   (js/Math.pow -1 (+ i j)))))))
    (mat-new* rows cols tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-cofactor "" [m] (mx-cofactor (mat-minor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-determ [3 3] [m]
  (let [{mCells :cells size :rows} m
        {cCells :cells} (mat-cofactor m)]
    (reduce (fn [sum j]
              (+ sum
                 (aget mCells (+ j (* size 0)))
                 (aget cCells (+ j 0)))) 0 (range size))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-minor [4 4] [m]
  (let [tmp (transient [])
        {:keys [cells rows cols]} m]
    (dotimes [i rows]
      (dotimes [j cols]
        (conj! tmp (mat-determ (mat-cut m i j)))))
    (mat-new* rows cols (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-determ [4 4] [m]
  (let [{:keys [cells] size :rows} m
        {cCells :cells} (mat-cofactor m)]
    (reduce (fn [sum j]
              (+ sum
                 (aget cells (+ j (* size 0)))
                 (aget cCells (+ j 0)))) 0 size)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-adjugate "" [m] (mat-xpose (mat-cofactor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-inverse "" [m]
  (let [d (mat-determ m)
        {:keys [rows cols]} m]
    (if (CMP d 0)
      (mat-identity rows)
      (mat-scale (mat-adjugate m) (/ 1 d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-fromColMajor "" [m] (mat-xpose m))
(defn mat-toColMajor "" [m] (mat-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-txlate "" [v3]
  (let [[x y z] (:cells v3)
        {:keys [cells] :as out} (mat-identity 4)]
    (aset cells (CELL 4 4 4 1) x)
    (aset cells (CELL 4 4 4 2) y)
    (aset cells (CELL 4 4 4 3) z) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-fromMX "" (fn [m] [(:rows m)(:cols m)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-fromMX [3 3] [m]
  (let [{:keys [rows cols cells]} m
        [r1 r2 r3] (partition cols cells)]
    (mat-new* (+ 1 rows)
             (+ 1 cols)
             (clj->js (concat r1 [0] r2 [0] r3 [0] [0 0 0 1])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti getTranslation "" (fn [m] [(:rows m)(:cols m)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getTranslation [4 4] [m]
  (let [{:keys [cols cells]} m]
    (vec3 (aget cells (CELL 4 4 4 1))
          (aget cells (CELL 4 4 4 1))
          (aget cells (CELL 4 4 4 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-fromVX "" (fn [v] (:size v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-fromVX 3 [v]
  (let [[x y z] (:cells v)
        {:keys [cells] :as out} (mat-identity 4)]
    (aset cells (CELL 4 4 1 1) x)
    (aset cells (CELL 4 4 2 2) y)
    (aset cells (CELL 4 4 3 3) z) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti getScaleFromMX "" (fn [m] [(:rows m)(:cols m)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getScaleFromMX [4 4] [m]
  (let [{:keys [cols cells]} m
        [r1 r2 r3 _] (partition cols cells)]
    (vec3 (nth r1 0) (nth r2 1) (nth r3 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotation2x2 "" [angle]
  (mat2 (COS angle) (SIN angle)
        (- (SIN angle)) (COS angle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-yawPitchRoll "" [yaw pitch roll]
  (mat-new* 4 4
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
  (mat-new* 4 4
           #js [1 0 0 0
                0 (COS rad) (SIN rad) 0
                0 (- (SIN rad)) (COS rad) 0
                0 0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XRotation3x3 "" [rad]
  (mat-new* 3 3
           #js [1 0 0
                0 (COS rad) (SIN rad)
                0 (- (SIN rad)) (COS rad)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YRotation "" [rad]
  (mat-new* 4 4
           #js [(COS rad) 0 (- (SIN rad)) 0
                0 1 0 0
                (SIN rad) 0 (COS rad) 0
                0 0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YRotation3x3 "" [rad]
  (mat-new* 3 3
           #js [(COS rad) 0 (- (SIN rad))
                0 1 0
                (SIN rad) 0 (COS rad)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ZRotation "" [rad]
  (mat-new* 4 4
           #js [(COS rad) (SIN rad) 0 0
                (- (SIN rad)) (COS rad) 0 0
                0 0 1 0
                0 0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ZRotation3x3 "" [rad]
  (mat-new* 3 3
           #js [(COS rad) (SIN rad) 0
                (- (SIN rad)) (COS rad) 0
                0 0 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-rotation "" [pitch yaw roll]
  (mat-multAB (mat-multAB (ZRotation roll)
                          (XRotation pitch)) (YRotation yaw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotation3x3 "" [pitch yaw roll]
  (mat-multAB (mat-multAB (ZRotation3x3 roll)
                          (XRotation3x3 pitch)) (YRotation3x3 yaw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-orthogonal "" (fn [m] [(:rows m)(:cols m)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod  mat-orthogonal [4 4] [m]
  (let [{:keys [rows cols cells]} m
        [r1 r2 r3 r4] (partition cols cells)
        xAxis (vec3 (nth r1 0) (nth r1 1) (nth r1 2))
        yAxis (vec3 (nth r2 0) (nth r2 1) (nth r2 2))
        zAxis (vec-xss xAxis yAxis)
        [xx xy xz] (:cells (vec-xss yAxis zAxis))
        [yx yy yz] (:cells (vec-xss zAxis xAxis))
        [zx zy zz] (:cells (vec-xss xAxis yAxis))]
    (mat-new* 4 4
             #js [xx xy xz (nth r1 3)
                  yx yy yz (nth r2 3)
                  zx zy zz (nth r3 3)
                  (nth r4 0) (nth r4 1) (nth r4 2) (nth r4 3)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-orthogonal [3 3] [m]
  (let [{:keys [rows cols cells]} m
        [r1 r2 r3] (partition cols cells)
        xAxis (vec3 (nth r1 0) (nth r1 1) (nth r1 2))
        yAxis (vec3 (nth r2 0) (nth r2 1) (nth r2 2))
        zAxis (vec-xss xAxis yAxis)
        [xx xy xz] (:cells (vec-xss yAxis zAxis))
        [yx yy yz] (:cells (vec-xss zAxis xAxis))
        [zx zy zz] (:cells (vec-xss xAxis yAxis))]
    (mat-new* 3 3
             #js [xx xy xz
                  yx yy yz
                  zx zy zz])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-axisAngle "" [axis rad]
  (let [[x' y' z'] (:cells axis)
        c (COS rad)
        s (SIN rad)
        t (- 1 c)
        [x y z]
        (if-not (CMP (vec-lensq axis) 1)
          (let [ilen (invert (vec-len axis))]
            [(* x' ilen) (* y' ilen) (* z' ilen)])
          [x' y' z'])]
  (mat-new* 4 4
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
  (let [[x' y' z'] (:cells axis)
        c (COS rad)
        s (SIN rad)
        t (- 1 c)
        [x y z]
        (if-not (CMP (vec-lensq axis) 1)
          (let [ilen (invert (vec-len axis))]
            [(* x' ilen)(* y' ilen)(* z' ilen)])
          [x' y' z'])]
    (mat-new* 3 3
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
(defn mat-multPoint "" [v3 m4]
  (let [[x y z] (:cells v3)
        [r1 r2 r3 r4]
        (partition 4 (:cells m4))]
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
(defmulti mat3-multVX "" (fn [v m] [(:rows m)(:cols m)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat3-multVX [4 4] [v3 m4]
  (let [[x y z] (:cells v3)
        [r1 r2 r3 r4]
        (partition 4 (:cells m4))]
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
(defmethod mat3-multVX [3 3] [v3 m3]
  (let [[x y z] (:cells v3)
        [r1 r2 r3] (partition 3 (:cells m3))]
    (vec3 (vec-dot v3 (vec3 (nth r1 0)(nth r2 0)(nth r3 0)))
          (vec-dot v3 (vec3 (nth r1 1)(nth r2 1)(nth r3 1)))
          (vec-dot v3 (vec3 (nth r1 2)(nth r2 2)(nth r3 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat4-txform "" (fn [a b c] (cond (vector? b) :axis-angle
                                           (map? b) :rotation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat4-txform :rotation [scale eulerRotation translate]
  (let [[x y z] (:cells eulerRotation)]
    (mat-multAB (mat-multAB (mat-fromVX scale)
                            (mat4-rotation x y z)) (mat4-txlate translate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat4-txform :axis-angle [scale rotationAxis rotationAngle translate]
  (mat-multAB (mat-multAB (mat-fromVX scale)
                          (mat4-axisAngle rotationAxis rotationAngle))
              (mat4-txlate translate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-lookAt "" [pos target up]
  (let [[fx fy fz :as forward] (:cells (vec-unit (vec-sub target pos)))
        [rx ry rz :as right] (:cells (vec-unit (vec-xss up forward)))
        [nx ny nz :as newUp] (:cells (vec-xss forward right))]
    (mat-new* 4 4
             #js [rx nx fx 0
                  ry ny fy 0
                  rz nz fz 0
                  (- (vec-dot right pos))
                  (- (vec-dot newUp pos))
                  (- (vec-dot forward pos)) 1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;https://msdn.microsoft.com/en-us/library/windows/desktop/bb147302(v=vs.85).aspx
;;
(defn mat4-proj "" [fov aspect zNear zFar]
  (let [tanHalfFov (TAN (* fov 0.5))
        fovY (invert tanHalfFov) ;;cot(fov/2)
        fovX (/ fovY aspect) ;;cot(fov/2) / aspect
        r33 (/ zFar (- zFar zNear)) ;;far/range
        {:keys [cells] :as ret} (mat-identity 4)]
    (aset cells (CELL 4 4 1 1) fovX)
    (aset cells (CELL 4 4 2 2) fovY)
    (aset cells (CELL 4 4 3 3) r33)
    (aset cells (CELL 4 4 3 4) 1)
    (aset cells (CELL 4 4 4 3) (* (- zNear) r33)) ;-near * (far / range)
    (aset cells (CELL 4 4 4 4) 0)
    ret))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Derived following: http://www.songho.ca/opengl/gl_projectionmatrix.html
;;Above was wrong, it was OpenGL style, our matrices are DX style
;;Correct impl:
;;https://msdn.microsoft.com/en-us/library/windows/desktop/bb205347(v=vs.85).aspx
(defn mat4-ortho "" [left right bottom top zNear zFar]
  (let [_11 (/ 2 (- right left))
        _22 (/ 2 (- top bottom))
        _33 (/ 1 (- zFar zNear))
        _41 (/ (+ left right) (- left right))
        _42 (/ (+ top bottom) (- bottom top))
        _43 (/ zNear (- zNear zFar))]
    (mat-new* 4 4
             #js [_11 0 0 0
                  0 _22 0 0
                  0 0 _33 0
                  _41 _42 _43 1])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-decompose "" (fn [m] [(:rows m)(:cols m)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-decompose [3 3] [rot1]
  (let [rot (mat-xpose rot1)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


