;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.math

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [_1 _2 _3 do-with assoc!!]])

  (:require [czlab.mcfud.afx.core
             :as ec :refer [pos?? n# sqr* sqrt* abs* num?? numFlip]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private EPSILON (or 0.0000000001 js/Number.EPSILON))
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
(defn xmod

  "Proper modulo."
  [x N]

  (if (< x 0)
    (- x (* -1 (+ N (* (js/Math.floor (/ (- x) N)) N)))) (rem x N)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; <= instead of < for NaN comparison safety
(defn fuzzyEq?

  "Best guess if 2 numbers are the same."
  [a b]

  (<= (abs* (- a b)) EPSILON))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fuzzyZero?

  "Best guess if number is zero."
  [n]

  (fuzzyEq? n 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;javascript array
(defn- jsa*

  "Create a javascript array of length z and filled with value v."
  [z & [v]]

  (do-with
    [out (array)]
    (let [v (num?? v 0)] (dotimes [i z] (.push out v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;VECTORS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- CMP-EQ

  "Fuzzy match if 2 numbers are equal."
  [x y]

  (<= (abs* (- x y))
      (* EPSILON (max 1 (max (abs* x) (abs* y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec2

  "Create a 2 row vector."
  [& [x y]]

  #js [(num?? x 0)(num?? y 0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec3

  "Create a 3 row vector."
  [& [x y z]]

  #js [(num?? x 0)(num?? y 0)(num?? z 0)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-zero

  "Create a array and filled with zero."
  [sz]

  (jsa* sz))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- modDeg

  "Normalize the degree."
  [deg]

  (comment
    (let [d (if (> deg DEG-2PI)
              (modDeg (- deg DEG-2PI)) deg)]
      (if (< d NEG-DEG-2PI) (modDeg (+ deg DEG-2PI)) d)))
  (mod deg DEG-2PI))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rad->deg

  "Convert radian to degree."
  [r]

  (modDeg (* DEG-2PI (/ r TWO-PI))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn deg->rad

  "Convert degree to radian."
  [d]

  (* TWO-PI (/ (modDeg d) DEG-2PI)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- array-eq?

  "True if these 2 arrays are equal."
  [a1 a2]

  (loop [i 0
         SZ (n# a1)
         diff? false]
    (cond
      (zero? SZ)
      true
      (or (>= i SZ) diff?)
      (not diff?)
      :else
      (recur (+ 1 i)
             SZ
             (not (fuzzyEq? (aget a1 i)
                            (aget a2 i)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-eq?

  "True if 2 arrays are equal."
  [v1 v2]

  (if (= (n# v1)
         (n# v2)) (array-eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YYYvec-eq?

  "not-used."
  [v1 v2]

  (if (= (n# v1)(n# v2))
      (loop [[a & m1] v1 [b & m2] v2 ok? true]
        (if (or (nil? a)
                (not ok?)) ok? (recur m1 m2 (CMP-EQ a b))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XXXvec-eq?

  "not-used."
  [v1 v2]

  (and (= (n# v1)(n# v2))
       (every? #(CMP-EQ (_1 %) (aget v2 (_2 %)))
                 (partition 2 (interleave v1 (range (n# v1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-neq?

  "True if arrays not equal."
  [v1 v2]

  (not (vec-eq? v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec-xxx

  "Apply a function to each value in the arrays, and then apply the
  result function to the result."
  [v1 v2 func res]

  (let [size (n# v1)]
    (when (= size (n# v2))
      (let [out (jsa* size)]
        (dotimes [i size]
          (aset out i (func (aget v1 i)(aget v2 i)))) (res out)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-add

  "Add 2 vectors."
  [v1 v2]

  (vec-xxx v1 v2 + identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-sub

  "Subtract 2 vectors."
  [v1 v2]

  (vec-xxx v1 v2 - identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-div

  "Divide v1 by v2."
  [v1 v2]

  (vec-xxx v1 v2 / identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-mult

  "Multiple 2 vectors."
  [v1 v2]

  (vec-xxx v1 v2 * identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- vec-nnn

  "Apply function to the array, then apply result function to result."
  [v func res]

  (let [size (n# v)
        out (jsa* size)]
    (dotimes [i size] (aset out i (func (aget v i)))) (res out)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-scale

  "Scalar multiplication of vector."
  [v n]
  {:pre [(number? n)]}

  (vec-nnn v (fn [i] (* i n)) identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-plus

  "Scalar addition of vector."
  [v n]
  {:pre [(number? n)]}

  (vec-nnn v (fn [i] (+ i n)) identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-minus

  "Scalar subtraction of vector."
  [v n]
  {:pre [(number? n)]}

  (vec-nnn v (fn [i] (- i n)) identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-dot

  "Dot product of vectors, returning a scalar."
  [v1 v2]

  (vec-xxx v1 v2 * (fn [out]
                     (reduce (fn [sum n] (+ sum n)) 0 out))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-lensq

  "Length of vector, squared."
  [v]

  (vec-dot v v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-len

  "Length of vector."
  [v]

  (sqrt* (vec-dot v v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-distsq

  "Distance between 2 vectors, squared."
  [v1 v2]

  (vec-lensq (vec-sub v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-dist

  "Distance between 2 vectors."
  [v1 v2]

  (vec-len (vec-sub v1 v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-unit

  "Finf unit-vector of this vector."
  [v]

  (let [z (vec-len v)]
    (if (> z EPSILON) (vec-scale v (numFlip z)) (aclone v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti vec-rot

  "Rotate a vector."

  (fn [v angle & [center]] (n# v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod vec-rot

  2
  [v angle & [center]]

  (let [[cx cy] (or center [0 0])
        [x y] v
        x' (- x cx)
        y' (- y cy)
        cos (COS angle)
        sin (SIN angle)]
    (vec2 (+ cx (- (* x' cos) (* y' sin)))
          (+ cy (+ (* x' sin) (* y' cos))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod vec-rot

  3
  [v angle & [center]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn v2-xss*

  "Find normal to this 2d-vector."
  [a v]
  {:pre [(number? a)]}

  (let [[x y] v] (vec2 (* (- a) y) (* a x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti vec-xss

  ""

  (fn [v1 v2] (n# v1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;If positive, v2 is on top of v1,
;;if negative, v2 below v1. Take the absolute value then it will
;;be the sine of the angle between them."
(defmethod vec-xss

  2
  [v1 v2]

  (assert (= 2 (n# v2)))
  (let [[x1 y1 ] v1
        [x2 y2 ] v2] (- (* x1 y2) (* y1 x2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod vec-xss

  3
  [v1 v2]

  (assert (= 3 (n# v2)))
  (let [[x2 y2 z2] v2
        [x1 y1 z1] v1]
    (vec3 (- (* y1 z2) (* z1 y2))
          (- (* z1 x2) (* x1 z2))
          (- (* x1 y2) (* y1 x2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-angle

  "Find angle between these 2 vectors."
  [v1 v2]

  (if (= (n# v1)(n# v2))
    (ACOS (/ (vec-dot v1 v2)
             (sqrt* (* (vec-lensq v1) (vec-lensq v2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-proj

  "Find projection."
  [length dir]

  (if (= (n# length)(n# dir))
    (vec-scale dir (/ (vec-dot length dir)(vec-lensq dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-perp

  "Find the perpedicular vector."
  [length dir]

  (if (= (n# length)(n# dir))
    (vec-sub length (vec-proj length dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-reflect

  "Reflect a normal."
  [src normal]

  (if (= (n# src)(n# normal))
    (vec-sub src
             (vec-scale normal
                        (* 2 (vec-dot src normal))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-neg

  "Negate a vector,"
  [v]

  (vec-scale v -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti vec-normal

  "Normal of a vector."

  (fn [v & more] (n# v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod vec-normal

  2
  [[x y] & [dir]]

  (let [left-turn (vec2 (- y) x)
        right-turn (vec2 y (- x))]
    (if (= dir :left) left-turn right-turn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-min

  "Find minimum values of vectors."
  [v1 v2]

  (vec-xxx v1 v2 min identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn vec-max

  "Find maximum values of vectors."
  [v1 v2]

  (vec-xxx v1 v2 max identity))

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
(defn- mat-pos

  "Find index position where matrix is mapped to 1D array."
  [rows cols r c]

  (+ (- c 1) (* (- r 1) cols)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- CELL

  "Find the cell position."
  [rows cols r c]

  (mat-pos rows cols r c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mat-new*

  "Create a new matrix with these cells."
  [rows cols cells]

  {:dim [rows cols] :arr cells})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mat-new

  "Create a new matrix."
  [rows cols]

  (mat-new* rows cols (jsa* (* rows cols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat*

  "Create a matrix, optionally with data."
  [[rows cols] & args]
  {:pre [(every? #(number? %) args)]}

  (if (empty? args)
    (mat-new rows cols)
    (let [sz (* rows cols)]
      (assert (= sz (n# args))
              "size don't match")
      (mat-new* rows cols (clj->js args)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-identity

  "Create a identity matrix."
  [sz]
  {:pre [(pos? sz)]}

  (mat-new* sz
            sz
            (do-with [out (jsa* (* sz sz))]
              (dotimes [i sz]
                (aset out (CELL sz sz (+ 1 i) (+ 1 i)) 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-zero

  "Create a matrix with all zeroes."
  [sz]
  {:pre [(pos?? sz)]}

  (mat-new* sz sz (jsa* (* sz sz))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat2

  "Create a 2x2 matrix."
  [_11 _12
   _21 _22]

  (mat* [2 2] _11 _12 _21 _22))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat3

  "Create a 3x3 matrix."
  [_11 _12 _13
   _21 _22 _23
   _31 _32 _33]

  (mat* [3 3] _11 _12 _13 _21 _22 _23 _31 _32 _33))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4

  "Create a 4x4 matrix."
  [_11 _12 _13 _14
   _21 _22 _23 _24
   _31 _32 _33 _34
   _41 _42 _43 _44]

  (mat* [4 4] _11 _12 _13 _14 _21 _22 _23 _24 _31 _32 _33 _34 _41 _42 _43 _44))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-eq?

  "True if these matrices are equals."
  [a b]

  (let [{da :dim va :arr} a
        {db :dim vb :arr} b]
    (if (= da db) (array-eq? va vb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-neq?

  "True if these matrices are different."
  [a b]

  (not (mat-eq? a b)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YYYmat-xpose

  "not-used."
  [m]

  (let [{:keys [arr] [rows cols] :dim} m
        tmp (transient [])
        cs (partition cols arr)]
    (dotimes [i cols]
      (doseq [c cs] (conj! tmp (nth c i))))
    (mat-new* cols rows (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-xpose

  "Transpose a matrix."
  [m]

  (let [tmp (transient [])
        {:keys [arr] [rows cols] :dim} m]
    (dotimes [i (* rows cols)]
      (conj! tmp (aget arr
                       (+ (int (/ i rows))
                          (* cols (mod i rows))))))
    (mat-new* cols rows (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat3-fastInverse

  "Inverse a 3x3 matrix - fast."
  [m]

  (mat-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-fastInverse

  "Inverse a 4x4 matrx - fast."
  [m]

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
(defn mat-scale

  "Scalar multiply a matrix."
  [m n]

  (let [{:keys [arr] [rows cols] :dim} m]
    (mat-new* rows
              cols
              (clj->js (map #(* n %) arr)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-multAB

  "Multiply 2 matrices."
  [a b]

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
(defmulti mat-det

  "Matrix determinent."

  (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-det

  [2 2]
  [m]

  (let [{:keys [arr]} m]
    (- (* (aget arr 0) (aget arr 3))
       (* (aget arr 1) (aget arr 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-cut

  "Extract a portion of a matrix."
  [m row col]

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
(defmulti mat-minor

  "Matrix minor."

  (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-minor

  :default
  [m]

  (let [tmp (transient [])
        {:keys [arr] [rows cols] :dim} m]
    (dotimes [i rows]
      (dotimes [j cols]
        ;mat-cut is 1-indexed
        (conj! tmp (mat-det (mat-cut m
                                     (+ 1 i)
                                     (+ 1 j))))))
    (mat-new* rows cols (clj->js (persistent! tmp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-minor

  [2 2]
  [m]

  (let [{:keys [arr]} m]
    (mat2 (aget arr 3) (aget arr 2)
          (aget arr 1) (aget arr 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-cofactor

  "Matrix co-factor."
  [m]

  (let [{:keys [arr] [rows cols] :dim} (mat-minor m)
        tmp (aclone arr)]
    (dotimes [i (* rows cols)]
      (if (odd? i) (aset tmp i (* -1 (aget tmp i)))))
    (mat-new* rows cols tmp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-det

  :default
  [m]

  (let [tmp (transient [])
        {:keys [arr] [rows cols] :dim} m]
    (dotimes [c cols]
      (->> (mat-cut m 1 (+ 1 c)) (mat-det) (conj! tmp)))
    (reduce (fn [sum j]
              (let [v (nth tmp j)]
                (+ sum
                   (* (aget arr j)
                      (if (odd? j) (* -1 v) v))))) 0 (range cols))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-adjugate

  "Matrix adjugate."
  [m]

  (mat-xpose (mat-cofactor m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-inverse

  "Inverse of matrix."

  (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-inverse

  [2 2]
  [m]

  (let [{[a b c d] :arr
         [rows cols] :dim} m
        det (- (* a d) (* b c))]
    (when-not (fuzzyZero? det)
      (let [det' (numFlip det)]
        (mat2 (* det' d) (* det' (- b)) (* det' (- c)) (* det' a))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-inverse

  :default
  [m]

  (let [d (mat-det m)
        {[rows cols] :dim} m]
    (if (fuzzyZero? d)
      (mat-identity rows)
      (mat-scale (mat-adjugate m) (numFlip d)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-fromColMajor

  "Matrix from column major."
  [m]

  (mat-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-toColMajor

  "Matrix to column major."
  [m]

  (mat-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-txlate

  "Translate a 3x3 matrix."
  [v3]
  {:pre [(array? v3)(= 3 (n# v3))]}

  (let [[x y z] v3
        {:keys [arr] :as out} (mat-identity 4)]
    (aset arr (CELL 4 4 4 1) x)
    (aset arr (CELL 4 4 4 2) y)
    (aset arr (CELL 4 4 4 3) z) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-fromMX

  "Matrix from matrix-translation."

  (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-fromMX

  [3 3]
  [m]

  (let [{:keys [arr]
         [rows cols] :dim} m
        [r1 r2 r3] (partition cols arr)]
    (mat-new* (+ 1 rows)
              (+ 1 cols)
              (clj->js (concat r1 [0] r2 [0] r3 [0] [0 0 0 1])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti getTranslation

  "Get the translation of a matrix."

  (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getTranslation

  [4 4]
  [m]

  (let [{:keys [arr]} m]
    (vec3 (aget arr (CELL 4 4 4 1))
          (aget arr (CELL 4 4 4 2))
          (aget arr (CELL 4 4 4 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-fromVX

  "Matrix from vector-translation."

  (fn [v] (n# v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-fromVX

  3
  [v]

  (let [[x y z] v
        {:keys [arr] :as out} (mat-identity 4)]
    (aset arr (CELL 4 4 1 1) x)
    (aset arr (CELL 4 4 2 2) y)
    (aset arr (CELL 4 4 3 3) z) out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti getScaleFromMX

  "Get scale from matrix-translation."

  (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod getScaleFromMX

  [4 4]
  [m]

  (let [{:keys [arr] [_ cols] :dim} m
        [r1 r2 r3 _] (partition cols arr)]
    (vec3 (nth r1 0) (nth r2 1) (nth r3 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-vmult

  "Multiply matrix and  vector."
  [m v]
  {:pre [(= (_2 (:dim m))(n# v))]}

  (let [m2 (mat-multAB m
                       (mat-new* (n# v) 1 v))] (:arr m2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotation2x2

  "Rotate a 2x2 matrix, counter-clockwise"
  [angle]

  (mat2 (COS angle) (- (SIN angle)) (SIN angle) (COS angle)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-yawPitchRoll

  "3D rotation."
  [yaw pitch roll]

  (mat4 (+ (* (COS roll) (COS yaw))
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
        0 0 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XRotation

  "Rotate on x-axis in 4D."
  [rad]

  (mat4 1 0 0 0
        0 (COS rad) (SIN rad) 0
        0 (- (SIN rad)) (COS rad) 0
        0 0 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn XRotation3x3

  "Rotate on x-axis in 3D."
  [rad]

  (mat3 1 0 0
        0 (COS rad) (SIN rad)
        0 (- (SIN rad)) (COS rad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YRotation

  "Rotate on y-axis in 4D."
  [rad]

  (mat4 (COS rad) 0 (- (SIN rad)) 0
        0 1 0 0
        (SIN rad) 0 (COS rad) 0
        0 0 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn YRotation3x3

  "Rotate on y-axis in 3D."
  [rad]

  (mat3 (COS rad) 0 (- (SIN rad))
        0 1 0
        (SIN rad) 0 (COS rad)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ZRotation

  "Rotate in z-axis in 4D."
  [rad]

  (mat4 (COS rad) (SIN rad) 0 0
        (- (SIN rad)) (COS rad) 0 0
        0 0 1 0
        0 0 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn ZRotation3x3

  "Rotate in z-axis in 3D."
  [rad]

  (mat3 (COS rad) (SIN rad) 0
        (- (SIN rad)) (COS rad) 0
        0 0 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-rotation

  "Rotation in 4D."
  [pitch yaw roll]

  (mat-multAB (mat-multAB (ZRotation roll)
                          (XRotation pitch)) (YRotation yaw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn rotation3x3

  "Rotation in 3D."
  [pitch yaw roll]

  (mat-multAB (mat-multAB (ZRotation3x3 roll)
                          (XRotation3x3 pitch)) (YRotation3x3 yaw)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-orthogonal

  "Orthogonal of matrix."

  (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-orthogonal

  [4 4]
  [m]

  (let [{:keys [arr] [rows cols] :dim} m
        [r1 r2 r3 r4]
        (partition cols arr)
        xAxis (vec3 (nth r1 0) (nth r1 1) (nth r1 2))
        yAxis (vec3 (nth r2 0) (nth r2 1) (nth r2 2))
        zAxis (vec-xss xAxis yAxis)
        [xx xy xz] (vec-xss yAxis zAxis)
        [yx yy yz] (vec-xss zAxis xAxis)
        [zx zy zz] (vec-xss xAxis yAxis)]
    (mat4 xx xy xz (nth r1 3)
          yx yy yz (nth r2 3)
          zx zy zz (nth r3 3)
          (nth r4 0) (nth r4 1) (nth r4 2) (nth r4 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-orthogonal

  [3 3]
  [m]

  (let [{:keys [arr] [rows cols] :dim} m
        [r1 r2 r3]
        (partition cols arr)
        xAxis (vec3 (nth r1 0) (nth r1 1) (nth r1 2))
        yAxis (vec3 (nth r2 0) (nth r2 1) (nth r2 2))
        zAxis (vec-xss xAxis yAxis)
        [xx xy xz] (vec-xss yAxis zAxis)
        [yx yy yz] (vec-xss zAxis xAxis)
        [zx zy zz] (vec-xss xAxis yAxis)]
    (mat3 xx xy xz yx yy yz zx zy zz)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-axisAngle

  "Rotate on this axis by this angle in 4D."
  [axis rad]
  {:pre [(array? axis)(= 3 (n# axis))]}

  (let [[x' y' z'] axis
        c (COS rad)
        s (SIN rad)
        t (- 1 c)
        [x y z]
        (if-not (fuzzyEq? (vec-lensq axis) 1)
          (let [ilen (numFlip (vec-len axis))]
            [(* x' ilen) (* y' ilen) (* z' ilen)])
          [x' y' z'])]
  (mat4 (+ c (* t x x)) (+ (* t x y) (* s z)) (- (* t x z) (* s y)) 0
        (- (* t x y) (* s z)) (+ c (* t y y)) (+ (* t y z) (* s x)) 0
        (+ (* t x z)(* s y)) (- (* t y z)(* s x)) (+ c (* t z z)) 0
        0 0 0 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn axisAngle3x3

  "Rotate on this axis by this angle in 3D."
  [axis rad]
  {:pre [(array? axis)(= 3 (n# axis))]}

  (let [[x' y' z'] axis
        c (COS rad)
        s (SIN rad)
        t (- 1 c)
        [x y z]
        (if-not (fuzzyEq? (vec-lensq axis) 1)
          (let [ilen (numFlip (vec-len axis))]
            [(* x' ilen)(* y' ilen)(* z' ilen)])
          [x' y' z'])]
    (mat3 (+ c (* t x x)) (+ (* t x y)(* s z)) (- (* t x z)(* s y))
          (- (* t x y)(* s z)) (+ c (* t y y)) (+ (* t y z)(* s x))
          (+ (* t x z)(* s y)) (- (* t y z)(* s x)) (+ c (* t z z)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat-multPoint

  "Multiply vector and 4x4 matrix."
  [v3 m4]
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
(defmulti mat3-multVX

  "Multiply vector and 3x3 matrix."

  (fn [v m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat3-multVX

  [4 4]
  [v3 m4]

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
(defmethod mat3-multVX

  [3 3]
  [v3 m3]
  (assert (and (array? v3)(= 3 (n# v3))))

  (let [[x y z] v3
        [r1 r2 r3] (partition 3 (:arr m3))]
    (vec3 (vec-dot v3 (vec3 (nth r1 0)(nth r2 0)(nth r3 0)))
          (vec-dot v3 (vec3 (nth r1 1)(nth r2 1)(nth r3 1)))
          (vec-dot v3 (vec3 (nth r1 2)(nth r2 2)(nth r3 2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat4-txform

  "Transform a 4x4 matrix."

  (fn [a b c] (cond (vector? b) :axis-angle
                    (array? b) :rotation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat4-txform

  :rotation
  [scale eulerRotation translate]

  (let [[x y z] eulerRotation]
    (mat-multAB (mat-multAB (mat-fromVX scale)
                            (mat4-rotation x y z)) (mat4-txlate translate))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat4-txform

  :axis-angle
  [scale [rotationAxis rotationAngle] translate]

  (mat-multAB (mat-multAB (mat-fromVX scale)
                          (mat4-axisAngle rotationAxis rotationAngle))
              (mat4-txlate translate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn mat4-lookAt

  "View of a 4D matrix."
  [pos target up]

  (let [[fx fy fz :as forward] (vec-unit (vec-sub target pos))
        [rx ry rz :as right] (vec-unit (vec-xss up forward))
        [nx ny nz :as newUp] (vec-xss forward right)]
    (mat4 rx nx fx 0
          ry ny fy 0
          rz nz fz 0
          (- (vec-dot right pos))
          (- (vec-dot newUp pos))
          (- (vec-dot forward pos)) 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;https://msdn.microsoft.com/en-us/library/windows/desktop/bb147302(v=vs.85).aspx
;;
(defn mat4-proj

  "4D projection."
  [fov aspect zNear zFar]

  (let [tanHalfFov (TAN (* fov 0.5))
        fovY (numFlip tanHalfFov) ;;cot(fov/2)
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
(defn mat4-ortho

  "Orthogonal to this 4x4 matrix."
  [left right bottom top zNear zFar]

  (let [_11 (/ 2 (- right left))
        _22 (/ 2 (- top bottom))
        _33 (/ 1 (- zFar zNear))
        _41 (/ (+ left right) (- left right))
        _42 (/ (+ top bottom) (- bottom top))
        _43 (/ zNear (- zNear zFar))]
    (mat4 _11 0 0 0
          0 _22 0 0
          0 0 _33 0
          _41 _42 _43 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti mat-decompose

  "Decompose this matrix."

  (fn [m] (:dim m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod mat-decompose

  [3 3]
  [rot1]

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
(defn pythagSQ

  "Hypotenuse squared."
  [x y]

  (+ (* x x) (* y y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn pythag

  "Hypotenuse."
  [x y]

  (sqrt* (pythagSQ x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn wrap??

  "Find the modulo of the next increment."
  [i len]

  (mod (+ 1 i) len))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn biasGreater?

  "Is it more a or b?"
  [a b]

  (let [biasRelative 0.95
        biasAbsolute 0.01]
    (>= a (+ (* b biasRelative) (* a biasAbsolute)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


