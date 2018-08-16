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
(def TWO-PI (* 2 js/Math.PI))
(def PI js/Math.PI)
(def DEG-2PI 360.0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private ATAN2 js/Math.atan2)
(def ^:private ACOS js/Math.acos)
(def ^:private COS js/Math.cos)
(def ^:private SIN js/Math.sin)
(def ^:private TAN js/Math.tan)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- jsa* [z & [v]]
  (do-with [out (make-array js/Number z)]
    (let [v (num?? v 0)] (dotimes [i z] (aset out i v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- CMP "" [x y]
  (<= (abs* (- x y))
      (* EPSILON (max 1 (max (abs* x) (abs* y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec*
  "" [& [args]] (if (every? #(number? %) args) (clj->js args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2
  "" [& [x y]] #js [(num?? x 0)(num?? y 0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec3
  "" [& [x y z]] #js [(num?? x 0)(num?? y 0)(num?? z 0)])

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
    (dotimes [i (n# a1)]
      (if-not (CMP (aget a1 i)
                   (aget a2 i)) (aset out 0 -1))) (pos? (aget out 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-eq? "" [v1 v2]
  (if (= (n# v1)
         (n# v2)) (array-eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YYYvec-eq? "" [v1 v2]
  (if (= (n# v1)(n# v2))
      (loop [[a & m1] v1 [b & m2] v2 ok? true]
        (if (or (nil? a)
                (not ok?)) ok? (recur m1 m2 (CMP a b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XXXvec-eq? "" [v1 v2]
  (and (= (n# v1)(n# v2))
       (every? #(CMP (_1 %) (aget v2 (_2 %)))
                 (partition 2 (interleave v1 (range (n# v1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-neq? "" [v1 v2] (not (vec-eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec-xxx "" [v1 v2 func res]
  (let [size (n# v1)]
    (when (= size (n# v2))
      (let [out (jsa* size)]
        (dotimes [i size]
          (aset out i (func (aget v1 i)(aget v2 i)))) (res out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-add "" [v1 v2] (vec-xxx v1 v2 + identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-sub "" [v1 v2] (vec-xxx v1 v2 - identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-div "" [v1 v2] (vec-xxx v1 v2 / identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-mult "" [v1 v2] (vec-xxx v1 v2 * identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec-nnn "" [v func res]
  (let [size (n# v) out (jsa* size)]
    (dotimes [i size] (aset out i (func (aget v i)))) (res out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-scale "" [v n]
  {:pre [number? n]}
  (vec-nnn v (fn [i] (* i n)) identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-plus "" [v n]
  {:pre [number? n]}
  (vec-nnn v (fn [i] (+ i n)) identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-minus "" [v n]
  {:pre [number? n]}
  (vec-nnn v (fn [i] (- i n)) identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-dot "" [v1 v2]
  (vec-xxx v1 v2 * (fn [out]
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
    (if (> z EPSILON) (vec-scale v (invert z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti vec-rot "" (fn [v angle & [center]] (n# v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod vec-rot 2 [v angle & [center]]
  (let [[cx cy] (or center [0 0])
        [x y] v
        x' (- x cx)
        y' (- y cy)
        cos (COS angle)
        sin (SIN angle)]
    (vec2 (+ cx (- (* x' cos) (* y' sin)))
          (+ cy (+ (* x' sin) (* y' cos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod vec-rot 3 [v angle & [center]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti vec-xss "" (fn [v1 v2] (n# v1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;If positive, v2 is on top of v1,
;;if negative, v2 below v1. Take the absolute value then it will
;;be the sine of the angle between them."
(defmethod vec-xss 2 [v1 v2]
  (assert (= 2 (n# v2)))
  (let [[x1 y1 ] v1
        [x2 y2 ] v2] (- (* x1 y2) (* y1 x2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod vec-xss 3 [v1 v2]
  (assert (= 3 (n# v2)))
  (let [[x2 y2 z2] v2
        [x1 y1 z1] v1]
    (vec3 (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-angle "" [v1 v2]
  (if (= (n# v1)(n# v2))
    (ACOS (/ (vec-dot v1 v2)
             (sqrt* (* (vec-lensq v1) (vec-lensq v2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-proj "" [length dir]
  (if (= (n# length)(n# dir))
    (vec-scale dir (/ (vec-dot length dir)(vec-lensq dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-perp "" [length dir]
  (if (= (n# length)(n# dir))
    (vec-sub length (vec-proj length dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-reflect "" [src normal]
  (if (= (n# src)(n# normal))
    (vec-sub src
             (vec-scale normal
                        (* 2 (vec-dot src normal))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-neg "" [v] (vec-scale v -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti vec-normal "" (fn [v & more] (n# v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod vec-normal 2 [v & [dir]]
  (let [[x y] v
        left-turn (vec2 (- y) x)
        right-turn (vec2 y (- x))] (if (= dir :left) left-turn right-turn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-min "" [v1 v2] (vec-xxx v1 v2 min identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-max "" [v1 v2] (vec-xxx v1 v2 max identity))

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
  "" [rows cols cells] {:dim [rows cols] :arr cells})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mat-new
  "" [rows cols] (mat-new* rows cols (jsa* (* rows cols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat* "" [[rows cols] & args]
  {:pre [(every? #(number? %) args)]}
  (if (empty? args)
    (mat-new rows cols)
    (mat-new* rows cols (clj->js args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-identity "" [sz]
  {:pre [(pos? sz)]}
  (mat-new* sz
            sz
            (do-with [out (jsa* (* sz sz))]
              (dotimes [i sz]
                (aset out (CELL sz sz (+ 1 i) (+ 1 i)) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-zero "" [sz]
  {:pre [(pos? sz)]}
  (mat-new* sz sz (jsa* (* sz sz))))

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
  (let [{da :dim va :arr} a
        {db :dim vb :arr} b]
    (if (= da db) (array-eq? va vb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-neq? "" [a b] (not (mat-eq? a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YYYmat-xpose "" [m]
  (let [{:keys [arr] [rows cols] :dim} m
        tmp (transient [])
        cs (partition cols arr)]
    (dotimes [i cols]
      (doseq [c cs] (conj! tmp (nth c i))))
    (mat-new* cols rows (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-xpose "" [m]
  (let [tmp (transient [])
        {:keys [arr] [rows cols] :dim} m]
    (dotimes [i (* rows cols)]
      (conj! tmp (aget arr
                       (+ (int (/ i rows))
                          (* cols (mod i rows))))))
    (mat-new* cols rows (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat3-fastInverse "" [m] (mat-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-fastInverse "" [m]
  (let [{o_arr :arr :as out} (mat-xpose m)
        {:keys [arr] [rows cols] :dim} m
        [m1 m2 m3 m4] (partition cols arr)
        right (vec3 (_1 m1)(_2 m1)(_3 m1))
        up (vec3 (_1 m2)(_2 m2)(_3 m2))
        forward (vec3 (_1 m3)(_2 m3)(_3 m3))
        position (vec3 (_1 m4)(_2 m4)(_3 m4))]
    (aset o_arr (CELL 4 4 1 4) 0)
    (aset o_arr (CELL 4 4 2 4) 0)
    (aset o_arr (CELL 4 4 3 4) 0)
    (aset o_arr (CELL 4 4 4 1) (- (vec-dot right position)))
    (aset o_arr (CELL 4 4 4 2) (- (vec-dot up position)))
    (aset o_arr (CELL 4 4 4 3) (- (vec-dot forward position))) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-scale "" [m n]
  (let [{:keys [arr] [rows cols] :dim} m]
    (mat-new* rows cols
              (clj->js (map #(* n %) arr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-multAB "" [a b]
  (let [{[aRows aCols] :dim aCells :arr} a
        {[bRows bCols] :dim bCells :arr} b
        _ (assert (= aCols bRows)
                  "mismatch matrices")
        out (jsa* (* aRows bCols))]
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
(defmulti mat-det "" (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-det [2 2] [m]
  (let [{:keys [arr]} m]
    (- (* (aget arr 0) (aget arr 3))
       (* (aget arr 1) (aget arr 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mat-cut "" [m row col]
  (let [{:keys [arr] [rows cols] :dim} m
        ;change to zero indexed
        row' (- row 1)
        col' (- col 1)
        tmp (transient [])]
    (dotimes [i rows]
      (dotimes [j cols]
        (when-not (or (= i row')
                      (= j col'))
          (conj! tmp (aget arr (+ j (* i cols)))))))
    (mat-new* (- rows 1)
              (- cols 1)
              (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-minor "" (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-minor [3 3] [m]
  (let [tmp (transient [])
        {:keys [arr] [rows cols] :dim} m]
    (dotimes [i rows]
      (dotimes [j cols]
        (conj! tmp (mat-det (mat-cut m i j)))))
    (mat-new* rows cols (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-minor [2 2] [m]
  (let [{:keys [arr]} m]
    (mat2 (aget arr 3) (aget arr 2)
          (aget arr 1) (aget arr 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mx-cofactor "" [minor]
  (let [{:keys [arr] [rows cols] :dim} minor
        tmp (jsa* (* rows cols))]
    (dotimes [i rows]
      (dotimes [j cols]
        (let [k (+ i (* j cols))]
          (aset tmp
                k
                (* (aget arr k)
                   (js/Math.pow -1 (+ i j)))))))
    (mat-new* rows cols tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-cofactor "" [m] (mx-cofactor (mat-minor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-det [3 3] [m]
  (let [{mCells :arr} m
        {cCells :arr} (mat-cofactor m)]
    (reduce (fn [sum j]
              (+ sum
                 (aget mCells (+ j (* 3 0)))
                 (aget cCells (+ j 0)))) 0 (range 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-minor [4 4] [m]
  (let [tmp (transient [])
        {:keys [arr] [rows cols] :dim} m]
    (dotimes [i rows]
      (dotimes [j cols]
        (conj! tmp
               (mat-det (mat-cut m i j)))))
    (mat-new* rows
              cols
              (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-det [4 4] [m]
  (let [{:keys [arr]} m
        {cCells :arr} (mat-cofactor m)]
    (reduce (fn [sum j]
              (+ sum
                 (aget arr (+ j (* 4 0)))
                 (aget cCells (+ j 0)))) 0 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-adjugate "" [m] (mat-xpose (mat-cofactor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-inverse "" [m]
  (let [d (mat-det m)
        {[rows cols] :dim} m]
    (if (CMP d 0)
      (mat-identity rows)
      (mat-scale (mat-adjugate m) (invert d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-fromColMajor "" [m] (mat-xpose m))
(defn mat-toColMajor "" [m] (mat-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-txlate "" [v3]
  {:pre [(array? v3)(= 3 (n# v3))]}
  (let [[x y z] v3
        {:keys [arr] :as out} (mat-identity 4)]
    (aset arr (CELL 4 4 4 1) x)
    (aset arr (CELL 4 4 4 2) y)
    (aset arr (CELL 4 4 4 3) z) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-fromMX "" (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-fromMX [3 3] [m]
  (let [{:keys [arr]
         [rows cols] :dim} m
        [r1 r2 r3] (partition cols arr)]
    (mat-new* (+ 1 rows)
              (+ 1 cols)
              (clj->js (concat r1 [0] r2 [0] r3 [0] [0 0 0 1])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti getTranslation "" (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getTranslation [4 4] [m]
  (let [{:keys [arr]} m]
    (vec3 (aget arr (CELL 4 4 4 1))
          (aget arr (CELL 4 4 4 1))
          (aget arr (CELL 4 4 4 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-fromVX "" (fn [v] (n# v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-fromVX 3 [v]
  (let [[x y z] v
        {:keys [arr] :as out} (mat-identity 4)]
    (aset arr (CELL 4 4 1 1) x)
    (aset arr (CELL 4 4 2 2) y)
    (aset arr (CELL 4 4 3 3) z) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti getScaleFromMX "" (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getScaleFromMX [4 4] [m]
  (let [{:keys [arr] [_ cols] :dim} m
        [r1 r2 r3 _] (partition cols arr)]
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
(defmulti mat-orthogonal "" (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod  mat-orthogonal [4 4] [m]
  (let [{:keys [arr] [rows cols] :dim} m
        [r1 r2 r3 r4]
        (partition cols arr)
        xAxis (vec3 (nth r1 0) (nth r1 1) (nth r1 2))
        yAxis (vec3 (nth r2 0) (nth r2 1) (nth r2 2))
        zAxis (vec-xss xAxis yAxis)
        [xx xy xz] (vec-xss yAxis zAxis)
        [yx yy yz] (vec-xss zAxis xAxis)
        [zx zy zz] (vec-xss xAxis yAxis)]
    (mat-new* 4 4
              #js [xx xy xz (nth r1 3)
                   yx yy yz (nth r2 3)
                   zx zy zz (nth r3 3)
                   (nth r4 0) (nth r4 1) (nth r4 2) (nth r4 3)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-orthogonal [3 3] [m]
  (let [{:keys [arr] [rows cols] :dim} m
        [r1 r2 r3]
        (partition cols arr)
        xAxis (vec3 (nth r1 0) (nth r1 1) (nth r1 2))
        yAxis (vec3 (nth r2 0) (nth r2 1) (nth r2 2))
        zAxis (vec-xss xAxis yAxis)
        [xx xy xz] (vec-xss yAxis zAxis)
        [yx yy yz] (vec-xss zAxis xAxis)
        [zx zy zz] (vec-xss xAxis yAxis)]
    (mat-new* 3 3
              #js [xx xy xz yx yy yz zx zy zz])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-axisAngle "" [axis rad]
  {:pre [(array? axis)(= 3 (n# axis))]}
  (let [[x' y' z'] axis
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
  {:pre [(array? axis)(= 3 (n# axis))]}
  (let [[x' y' z'] axis
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
  {:pre [(array? v3)(= 3 (n# v3))]}
  (let [[x y z] v3
        [r1 r2 r3 r4]
        (partition 4 (:arr m4))]
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
(defmulti mat3-multVX "" (fn [v m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat3-multVX [4 4] [v3 m4]
  (assert (and (array? v3)(= 3 (n# v3))))
  (let [[x y z] v3
        [r1 r2 r3 r4]
        (partition 4 (:arr m4))]
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
  (assert (and (array? v3)(= 3 (n# v3))))
  (let [[x y z] v3
        [r1 r2 r3] (partition 3 (:arr m3))]
    (vec3 (vec-dot v3 (vec3 (nth r1 0)(nth r2 0)(nth r3 0)))
          (vec-dot v3 (vec3 (nth r1 1)(nth r2 1)(nth r3 1)))
          (vec-dot v3 (vec3 (nth r1 2)(nth r2 2)(nth r3 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat4-txform "" (fn [a b c] (cond (vector? b) :axis-angle
                                           (array? b) :rotation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat4-txform
  :rotation [scale eulerRotation translate]
  (let [[x y z] eulerRotation]
    (mat-multAB (mat-multAB (mat-fromVX scale)
                            (mat4-rotation x y z)) (mat4-txlate translate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat4-txform
  :axis-angle [scale [rotationAxis rotationAngle] translate]
  (mat-multAB (mat-multAB (mat-fromVX scale)
                          (mat4-axisAngle rotationAxis rotationAngle))
              (mat4-txlate translate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-lookAt "" [pos target up]
  (let [[fx fy fz :as forward] (vec-unit (vec-sub target pos))
        [rx ry rz :as right] (vec-unit (vec-xss up forward))
        [nx ny nz :as newUp] (vec-xss forward right)]
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
        {:keys [arr] :as ret} (mat-identity 4)]
    (aset arr (CELL 4 4 1 1) fovX)
    (aset arr (CELL 4 4 2 2) fovY)
    (aset arr (CELL 4 4 3 3) r33)
    (aset arr (CELL 4 4 3 4) 1)
    (aset arr (CELL 4 4 4 3) (* (- zNear) r33)) ;-near * (far / range)
    (aset arr (CELL 4 4 4 4) 0)
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
(defmulti mat-decompose "" (fn [m] (:dim m)))

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


