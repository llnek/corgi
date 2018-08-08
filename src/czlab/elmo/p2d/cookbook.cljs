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
;;EOF
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

;;kenl
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
  (let [out (mx-new cols rows)
        {:keys [rows cols cells]} m]
    (dotimes [i (* rows cols)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-fastInverse "" [m] (m3-xpose m))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-fastInverse "" [m]
  (let [{:keys [_11 _12 _13 _14
                _21 _22 _23 _24
                _31 _32 _33 _34
                _41 _42 _43 _44]} m
        inverse (-> (m4-xpose m)
                    (assoc :_41 0 :_14 0
                           :_42 0 :_24 0
                           :_43 0 :_34 0))
        right (vec3 _11 _12 _13)
        up (vec3 _21 _22 _23)
        forward (vec3 _31 _32 _33)
        position (vec3 _41 _42 _43)]
    (assoc inverse
           :_41 (- (v3-dot right position))
           :_42 (- (v3-dot up position))
           :_43 (- (v3-dot forward position)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-xpose "" [m]
  (let [{:keys [_11 _12
                _21 _22]} m] (mat2 _11 _21
                                   _12 _22)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-xpose "" [m]
  (let [{:keys [_11 _12 _13
                _21 _22 _23
                _31 _32 _33]} m] (mat3 _11 _21 _31
                                       _12 _22 _32
                                       _13 _23 _33)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-xpose "" [m]
  (let [{:keys [_11 _12 _13 _14
                _21 _22 _23 _24
                _31 _32 _33 _34
                _41 _42 _43 _44]} m] (mat4 _11 _21 _31 _41
                                           _12 _22 _32 _42
                                           _13 _23 _33 _43
                                           _14 _24 _34 _44)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m2-scale "" [m n]
  (let [{:keys [_11 _12
                _21 _22]} m] (mat2 (* n _11) (* n _21)
                                   (* n _12) (* n _22))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m3-scale "" [m n]
  (let [{:keys [_11 _12 _13
                _21 _22 _23
                _31 _32 _33]} m] (mat3 (* n _11) (* n _21) (* n _31)
                                       (* n _12) (* n _22) (* n _32)
                                       (* n _13) (* n _23) (* n _33))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn m4-scale "" [m n]
  (let [{:keys [_11 _12 _13 _14
                _21 _22 _23 _24
                _31 _32 _33 _34
                _41 _42 _43 _44]} m] (mat4 (* n _11) (* n _21) (* n _31) (* n _41)
                                           (* n _12) (* n _22) (* n _32) (* n _42)
                                           (* n _13) (* n _23) (* n _33) (* n _43)
                                           (* n _14) (* n _24) (* n _34) (* n _44))))


bool Multiply(float* out, const float* matA, int aRows, int aCols, const float* matB, int bRows, int bCols) {
  if (aCols != bRows) {
    return false;
  }

  for (int i = 0; i < aRows; ++i) {
    for (int j = 0; j < bCols; ++j) {
      out[bCols * i + j] = 0.0f;
      for (int k = 0; k < bRows; ++k) {
        out[bCols * i + j] += matA[aCols * i + k] * matB[bCols * k + j];
      }
    }
  }

  return true;
}

mat2 operator*(const mat2& matrixA, const mat2& matrixB) {
  mat2 result;
  Multiply(result.asArray, matrixA.asArray, 2, 2, matrixB.asArray, 2, 2);
  return result;
}

mat3 operator*(const mat3& matrixA, const mat3& matrixB) {
  mat3 result;
  Multiply(result.asArray, matrixA.asArray, 3, 3, matrixB.asArray, 3, 3);
  return result;
}

mat4 operator*(const mat4& matrixA, const mat4& matrixB) {
  mat4 result;
  Multiply(result.asArray, matrixA.asArray, 4, 4, matrixB.asArray, 4, 4);
  return result;
}

float Determinant(const mat2& matrix) {
  return matrix._11 * matrix._22 - matrix._12 * matrix._21;
}

mat2 Cut(const mat3& mat, int row, int col) {
  mat2 result;
  int index = 0;

  for (int i = 0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {
      if (i == row || j == col) {
        continue;
      }
      result.asArray[index++] = mat.asArray[3 * i + j];
    }
  }

  return result;
}

mat3 Cut(const mat4& mat, int row, int col) {
  mat3 result;
  int index = 0;

  for (int i = 0; i < 4; ++i) {
    for (int j = 0; j < 4; ++j) {
      if (i == row || j == col) {
        continue;
      }
      result.asArray[index++] = mat.asArray[4 * i + j];
    }
  }

  return result;
}

mat3 Minor(const mat3& mat) {
  mat3 result;

  for (int i = 0; i < 3; ++i) {
    for (int j = 0; j < 3; ++j) {
      result[i][j] = Determinant(Cut(mat, i, j));
    }
  }

  return result;
}

mat2 Minor(const mat2& mat) {
  return mat2(
    mat._22, mat._21,
    mat._12, mat._11
  );
}

void Cofactor(float* out, const float* minor, int rows, int cols) {
  for (int i = 0; i < rows; ++i) {
    for (int j = 0; j < cols; ++j) {
      out[cols * j + i] = minor[cols * j + i] * powf(-1.0f, i + j);
    }
  }
}

mat2 Cofactor(const mat2& mat) {
  mat2 result;
  Cofactor(result.asArray, Minor(mat).asArray, 2, 2);
  return result;
}

mat3 Cofactor(const mat3& mat) {
  mat3 result;
  Cofactor(result.asArray, Minor(mat).asArray, 3, 3);
  return result;
}

float Determinant(const mat3& mat) {
  float result = 0.0f;

  /*float A = mat.asArray[3 * 0 + 0] * Determinant(Cut(mat, 0, 0));
  float B = mat.asArray[3 * 0 + 1] * Determinant(Cut(mat, 0, 1));
  float C = mat.asArray[3 * 0 + 2] * Determinant(Cut(mat, 0, 2));
  result = A - B + C;*/

  /*for (int j = 0; j < 3; ++j) {
    result += mat.asArray[3 * 0 + j] * Determinant(Cut(mat, 0, j)) * powf(-1, 0 + j);
  }*/

  mat3 cofactor = Cofactor(mat);
  for (int j = 0; j < 3; ++j) {
    result += mat.asArray[3 * 0 + j] * cofactor[0][j];
  }

  return result;
}

mat4 Minor(const mat4& mat) {
  mat4 result;

  for (int i = 0; i < 4; ++i) {
    for (int j = 0; j < 4; ++j) {
      result[i][j] = Determinant(Cut(mat, i, j));
    }
  }

  return result;
}

mat4 Cofactor(const mat4& mat) {
  mat4 result;
  Cofactor(result.asArray, Minor(mat).asArray, 4, 4);
  return result;
}

float Determinant(const mat4& mat) {
  float result = 0.0f;

  mat4 cofactor = Cofactor(mat);
  for (int j = 0; j < 4; ++j) {
    result += mat.asArray[4 * 0 + j] * cofactor[0][j];
  }

  return result;
}

mat2 Adjugate(const mat2& mat) {
  return Transpose(Cofactor(mat));
}

mat3 Adjugate(const mat3& mat) {
  return Transpose(Cofactor(mat));
}

mat4 Adjugate(const mat4& mat) {
  return Transpose(Cofactor(mat));
}

mat2 Inverse(const mat2& mat) {
  float det = Determinant(mat);
  if (CMP(det, 0.0f)) { return mat2(); }
  return Adjugate(mat) * (1.0f / det);

  /*float det = mat._11 * mat._22 - mat._12 * mat._21;
  if (CMP(det, 0.0f)) {
    return mat2();
  }
  float i_det = 1.0f / det;
  mat2 result;
  result._11 =  mat._22 * i_det;
  result._12 = -mat._12 * i_det;
  result._21 = -mat._21 * i_det;
  result._22 =  mat._11 * i_det;
  return result;*/
}

mat3 Inverse(const mat3& mat) {
  float det = Determinant(mat);
  if (CMP(det, 0.0f)) { return mat3(); }
  return Adjugate(mat) * (1.0f / det);
}

mat4 Inverse(const mat4& m) {
  /*float det = Determinant(m);
  if (CMP(det, 0.0f)) { return mat4(); }
  return Adjugate(m) * (1.0f / det);*/

  // The code below is the expanded form of the above equation.
  // This optimization avoids loops and function calls

  float det
    = m._11 * m._22 * m._33 * m._44 + m._11 * m._23 * m._34 * m._42 + m._11 * m._24 * m._32 * m._43
    + m._12 * m._21 * m._34 * m._43 + m._12 * m._23 * m._31 * m._44 + m._12 * m._24 * m._33 * m._41
    + m._13 * m._21 * m._32 * m._44 + m._13 * m._22 * m._34 * m._41 + m._13 * m._24 * m._31 * m._42
    + m._14 * m._21 * m._33 * m._42 + m._14 * m._22 * m._31 * m._43 + m._14 * m._23 * m._32 * m._41
    - m._11 * m._22 * m._34 * m._43 - m._11 * m._23 * m._32 * m._44 - m._11 * m._24 * m._33 * m._42
    - m._12 * m._21 * m._33 * m._44 - m._12 * m._23 * m._34 * m._41 - m._12 * m._24 * m._31 * m._43
    - m._13 * m._21 * m._34 * m._42 - m._13 * m._22 * m._31 * m._44 - m._13 * m._24 * m._32 * m._41
    - m._14 * m._21 * m._32 * m._43 - m._14 * m._22 * m._33 * m._41 - m._14 * m._23 * m._31 * m._42;

  if (CMP(det, 0.0f)) {
    return mat4();
  }
  float i_det = 1.0f / det;

  mat4 result;
  result._11 = (m._22 * m._33 * m._44 + m._23 * m._34 * m._42 + m._24 * m._32 * m._43 - m._22 * m._34 * m._43 - m._23 * m._32 * m._44 - m._24 * m._33 * m._42) * i_det;
  result._12 = (m._12 * m._34 * m._43 + m._13 * m._32 * m._44 + m._14 * m._33 * m._42 - m._12 * m._33 * m._44 - m._13 * m._34 * m._42 - m._14 * m._32 * m._43) * i_det;
  result._13 = (m._12 * m._23 * m._44 + m._13 * m._24 * m._42 + m._14 * m._22 * m._43 - m._12 * m._24 * m._43 - m._13 * m._22 * m._44 - m._14 * m._23 * m._42) * i_det;
  result._14 = (m._12 * m._24 * m._33 + m._13 * m._22 * m._34 + m._14 * m._23 * m._32 - m._12 * m._23 * m._34 - m._13 * m._24 * m._32 - m._14 * m._22 * m._33) * i_det;
  result._21 = (m._21 * m._34 * m._43 + m._23 * m._31 * m._44 + m._24 * m._33 * m._41 - m._21 * m._33 * m._44 - m._23 * m._34 * m._41 - m._24 * m._31 * m._43) * i_det;
  result._22 = (m._11 * m._33 * m._44 + m._13 * m._34 * m._41 + m._14 * m._31 * m._43 - m._11 * m._34 * m._43 - m._13 * m._31 * m._44 - m._14 * m._33 * m._41) * i_det;
  result._23 = (m._11 * m._24 * m._43 + m._13 * m._21 * m._44 + m._14 * m._23 * m._41 - m._11 * m._23 * m._44 - m._13 * m._24 * m._41 - m._14 * m._21 * m._43) * i_det;
  result._24 = (m._11 * m._23 * m._34 + m._13 * m._24 * m._31 + m._14 * m._21 * m._33 - m._11 * m._24 * m._33 - m._13 * m._21 * m._34 - m._14 * m._23 * m._31) * i_det;
  result._31 = (m._21 * m._32 * m._44 + m._22 * m._34 * m._41 + m._24 * m._31 * m._42 - m._21 * m._34 * m._42 - m._22 * m._31 * m._44 - m._24 * m._32 * m._41) * i_det;
  result._32 = (m._11 * m._34 * m._42 + m._12 * m._31 * m._44 + m._14 * m._32 * m._41 - m._11 * m._32 * m._44 - m._12 * m._34 * m._41 - m._14 * m._31 * m._42) * i_det;
  result._33 = (m._11 * m._22 * m._44 + m._12 * m._24 * m._41 + m._14 * m._21 * m._42 - m._11 * m._24 * m._42 - m._12 * m._21 * m._44 - m._14 * m._22 * m._41) * i_det;
  result._34 = (m._11 * m._24 * m._32 + m._12 * m._21 * m._34 + m._14 * m._22 * m._31 - m._11 * m._22 * m._34 - m._12 * m._24 * m._31 - m._14 * m._21 * m._32) * i_det;
  result._41 = (m._21 * m._33 * m._42 + m._22 * m._31 * m._43 + m._23 * m._32 * m._41 - m._21 * m._32 * m._43 - m._22 * m._33 * m._41 - m._23 * m._31 * m._42) * i_det;
  result._42 = (m._11 * m._32 * m._43 + m._12 * m._33 * m._41 + m._13 * m._31 * m._42 - m._11 * m._33 * m._42 - m._12 * m._31 * m._43 - m._13 * m._32 * m._41) * i_det;
  result._43 = (m._11 * m._23 * m._42 + m._12 * m._21 * m._43 + m._13 * m._22 * m._41 - m._11 * m._22 * m._43 - m._12 * m._23 * m._41 - m._13 * m._21 * m._42) * i_det;
  result._44 = (m._11 * m._22 * m._33 + m._12 * m._23 * m._31 + m._13 * m._21 * m._32 - m._11 * m._23 * m._32 - m._12 * m._21 * m._33 - m._13 * m._22 * m._31) * i_det;

#ifdef  DO_SANITY_TESTS
#ifndef NO_EXTRAS
  if (result * m != mat4()) {
    std::cout << "ERROR! Expecting matrix x inverse to equal identity!\n";
  }
#endif
#endif

  return result;
}

mat4 ToColumnMajor(const mat4& mat) {
  return Transpose(mat);
}

mat3 ToColumnMajor(const mat3& mat) {
  return Transpose(mat);
}

mat4 FromColumnMajor(const mat4& mat) {
  return Transpose(mat);
}

mat3 FromColumnMajor(const mat3& mat) {
  return Transpose(mat);
}

mat4 FromColumnMajor(const float* mat) {
  return Transpose(mat4(
    mat[0],  mat[1],  mat[2],  mat[3],
    mat[4],  mat[5],  mat[6],  mat[7],
    mat[8],  mat[9],  mat[10], mat[11],
    mat[12], mat[13], mat[14], mat[15]
  ));
}

mat4 Translation(float x, float y, float z) {
  return mat4(
    1.0f, 0.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 1.0f, 0.0f,
       x,    y,    z, 1.0f
  );
}
mat4 Translation(const vec3& pos) {
  return mat4(
    1.0f, 0.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 1.0f, 0.0f,
    pos.x,pos.y,pos.z,1.0f
  );
}

#ifndef NO_EXTRAS
mat4 Translate(float x, float y, float z) {
  return mat4(
    1.0f, 0.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 1.0f, 0.0f,
    x, y, z, 1.0f
  );
}

mat4 Translate(const vec3& pos) {
  return mat4(
    1.0f, 0.0f, 0.0f, 0.0f,
    0.0f, 1.0f, 0.0f, 0.0f,
    0.0f, 0.0f, 1.0f, 0.0f,
    pos.x, pos.y, pos.z, 1.0f
  );
}
#endif

mat4 FromMat3(const mat3& mat) {
  mat4 result;

  result._11 = mat._11;
  result._12 = mat._12;
  result._13 = mat._13;

  result._21 = mat._21;
  result._22 = mat._22;
  result._23 = mat._23;

  result._31 = mat._31;
  result._32 = mat._32;
  result._33 = mat._33;

  return result;
}

vec3 GetTranslation(const mat4& mat) {
  return vec3(mat._41, mat._42, mat._43);
}

mat4 Scale(float x, float y, float z) {
  return mat4(
       x, 0.0f, 0.0f, 0.0f,
    0.0f,    y, 0.0f, 0.0f,
    0.0f, 0.0f,    z, 0.0f,
    0.0f, 0.0f, 0.0f, 1.0f
  );
}

mat4 Scale(const vec3& vec) {
  return mat4(
    vec.x,0.0f, 0.0f, 0.0f,
    0.0f, vec.y,0.0f, 0.0f,
    0.0f, 0.0f, vec.z,0.0f,
    0.0f, 0.0f, 0.0f, 1.0f
  );
}

vec3 GetScale(const mat4& mat) {
  return vec3(mat._11, mat._22, mat._33);
}

mat4 Rotation(float pitch, float yaw, float roll) {
  return  ZRotation(roll) * XRotation(pitch) * YRotation(yaw);
}

mat3 Rotation3x3(float pitch, float yaw, float roll) {
  return ZRotation3x3(roll) * XRotation3x3(pitch) * YRotation3x3(yaw);
}

#ifndef NO_EXTRAS
mat2 Rotation2x2(float angle) {
  return mat2(
    cosf(angle), sinf(angle),
    -sinf(angle), cosf(angle)
    );
}

mat4 YawPitchRoll(float yaw, float pitch, float roll) {
  yaw = DEG2RAD(yaw);
  pitch = DEG2RAD(pitch);
  roll = DEG2RAD(roll);

  mat4 out; // z * x * y
  out._11 = (cosf(roll) * cosf(yaw)) + (sinf(roll) * sinf(pitch) * sinf(yaw));
  out._12 = (sinf(roll) * cosf(pitch));
  out._13 = (cosf(roll) * -sinf(yaw)) + (sinf(roll) * sinf(pitch) * cosf(yaw));
  out._21 = (-sinf(roll) * cosf(yaw)) + (cosf(roll) * sinf(pitch) * sinf(yaw));
  out._22 = (cosf(roll) * cosf(pitch));
  out._23 = (sinf(roll) * sinf(yaw)) + (cosf(roll) * sinf(pitch) * cosf(yaw));
  out._31 = (cosf(pitch) * sinf(yaw));
  out._32 = -sinf(pitch);
  out._33 = (cosf(pitch) * cosf(yaw));
  out._44 = 1;
  return out;
}
#endif

mat4 XRotation(float angle) {
  angle = DEG2RAD(angle);
  return mat4(
    1.0f, 0.0f, 0.0f, 0.0f,
    0.0f, cosf(angle), sinf(angle), 0.0f,
    0.0f, -sinf(angle), cosf(angle), 0.0f,
    0.0f, 0.0f, 0.0f, 1.0f
  );
}

mat3 XRotation3x3(float angle) {
  angle = DEG2RAD(angle);
  return mat3(
    1.0f, 0.0f, 0.0f,
    0.0f, cosf(angle), sinf(angle),
    0.0f, -sinf(angle), cosf(angle)
  );
}

mat4 YRotation(float angle) {
  angle = DEG2RAD(angle);
  return mat4(
    cosf(angle), 0.0f, -sinf(angle), 0.0f,
    0.0f, 1.0f, 0.0f, 0.0f,
    sinf(angle), 0.0f, cosf(angle), 0.0f,
    0.0f, 0.0f, 0.0f, 1.0f
  );
}

mat3 YRotation3x3(float angle) {
  angle = DEG2RAD(angle);
  return mat3(
    cosf(angle), 0.0f, -sinf(angle),
    0.0f, 1.0f, 0.0f,
    sinf(angle), 0.0f, cosf(angle)
  );
}

mat4 ZRotation(float angle) {
  angle = DEG2RAD(angle);
  return mat4(
    cosf(angle), sinf(angle), 0.0f, 0.0f,
    -sinf(angle), cosf(angle), 0.0f, 0.0f,
    0.0f, 0.0f, 1.0f, 0.0f,
    0.0f, 0.0f, 0.0f, 1.0f
  );
}

mat3 ZRotation3x3(float angle) {
  angle = DEG2RAD(angle);
  return mat3(
    cosf(angle), sinf(angle), 0.0f,
    -sinf(angle), cosf(angle), 0.0f,
    0.0f, 0.0f, 1.0f
  );
}

#ifndef NO_EXTRAS

mat4 Orthogonalize(const mat4& mat) {
  vec3 xAxis(mat._11, mat._12, mat._13);
  vec3 yAxis(mat._21, mat._22, mat._23);
  vec3 zAxis = Cross(xAxis, yAxis);

  xAxis = Cross(yAxis, zAxis);
  yAxis = Cross(zAxis, xAxis);
  zAxis = Cross(xAxis, yAxis);

  return mat4(
    xAxis.x, xAxis.y, xAxis.z, mat._14,
    yAxis.x, yAxis.y, yAxis.z, mat._24,
    zAxis.x, zAxis.y, zAxis.z, mat._34,
    mat._41, mat._42, mat._43, mat._44
  );
}

mat3 Orthogonalize(const mat3& mat) {
  vec3 xAxis(mat._11, mat._12, mat._13);
  vec3 yAxis(mat._21, mat._22, mat._23);
  vec3 zAxis = Cross(xAxis, yAxis);

  xAxis = Cross(yAxis, zAxis);
  yAxis = Cross(zAxis, xAxis);
  zAxis = Cross(xAxis, yAxis);

  return mat3(
    xAxis.x, xAxis.y, xAxis.z,
    yAxis.x, yAxis.y, yAxis.z,
    zAxis.x, zAxis.y, zAxis.z
  );
}
#endif

mat4 AxisAngle(const vec3& axis, float angle) {
  angle = DEG2RAD(angle);
  float c = cosf(angle);
  float s = sinf(angle);
  float t = 1.0f - cosf(angle);

  float x = axis.x;
  float y = axis.y;
  float z = axis.z;
  if (!CMP(MagnitudeSq(axis), 1.0f)) {
    float inv_len = 1.0f / Magnitude(axis);
    x *= inv_len;
    y *= inv_len;
    z *= inv_len;
  }

  return mat4(
    t * (x * x) + c, t * x * y + s * z, t * x * z - s * y, 0.0f,
    t * x * y - s * z, t * (y * y) + c, t * y * z + s * x, 0.0f,
    t * x * z + s * y, t * y * z - s * x, t * (z * z) + c, 0.0f,
    0.0f, 0.0f, 0.0f, 1.0f
  );
}

mat3 AxisAngle3x3(const vec3& axis, float angle) {
  angle = DEG2RAD(angle);
  float c = cosf(angle);
  float s = sinf(angle);
  float t = 1.0f - cosf(angle);

  float x = axis.x;
  float y = axis.y;
  float z = axis.z;
  if (!CMP(MagnitudeSq(axis), 1.0f)) {
    float inv_len = 1.0f / Magnitude(axis);
    x *= inv_len;
    y *= inv_len;
    z *= inv_len;
  }

  return mat3(
    t * (x * x) + c, t * x * y + s * z, t * x * z - s * y,
    t * x * y - s * z, t * (y * y) + c, t * y * z + s * x,
    t * x * z + s * y, t * y * z - s * x, t * (z * z) + c
  );
}

vec3 MultiplyPoint(const vec3& vec, const mat4& mat) {
  vec3 result;
  result.x = vec.x * mat._11 + vec.y * mat._21 + vec.z * mat._31 + 1.0f * mat._41;
  result.y = vec.x * mat._12 + vec.y * mat._22 + vec.z * mat._32 + 1.0f * mat._42;
  result.z = vec.x * mat._13 + vec.y * mat._23 + vec.z * mat._33 + 1.0f * mat._43;
  return result;
}

vec3 MultiplyVector(const vec3& vec, const mat4& mat) {
  vec3 result;
  result.x = vec.x * mat._11 + vec.y * mat._21 + vec.z * mat._31 + 0.0f * mat._41;
  result.y = vec.x * mat._12 + vec.y * mat._22 + vec.z * mat._32 + 0.0f * mat._42;
  result.z = vec.x * mat._13 + vec.y * mat._23 + vec.z * mat._33 + 0.0f * mat._43;
  return result;
}

vec3 MultiplyVector(const vec3& vec, const mat3& mat) {
  vec3 result;
  result.x = Dot(vec, vec3{ mat._11, mat._21, mat._31 });
  result.y = Dot(vec, vec3{ mat._12, mat._22, mat._32 });
  result.z = Dot(vec, vec3{ mat._13, mat._23, mat._33 });
  return result;
}

mat4 Transform(const vec3& scale, const vec3& eulerRotation, const vec3& translate) {
  return Scale(scale) *
    Rotation(eulerRotation.x, eulerRotation.y, eulerRotation.z) *
    Translation(translate);
}

mat4 Transform(const vec3& scale, const vec3& rotationAxis, float rotationAngle, const vec3& translate) {
  return Scale(scale) *
    AxisAngle(rotationAxis, rotationAngle) *
    Translation(translate);
}

mat4 LookAt(const vec3& position, const vec3& target, const vec3& up) {
  vec3 forward = Normalized(target - position);
  vec3 right = Normalized(Cross(up, forward));
  vec3 newUp = Cross(forward, right);

#ifdef DO_SANITY_TESTS
  mat4 viewPosition = Translation(position);
  mat4 viewOrientation = mat4(
    right.x, right.y, right.z, 0.0f,
    newUp.x, newUp.y, newUp.z, 0.0f,
    forward.x, forward.y, forward.z, 0.0f,
    0.0f, 0.0f, 0.0f, 1.0f
  );

  // I had this implemented originally, it was wrong!
  // It's scale * orientation * transform
  // That's why it needed two inverses :(
  //mat4 view = Inverse(viewPosition) * Inverse(viewOrientation);
  //std::cout << "View: " << view << "\n\n";
  //std::cout << "Alt: " << Inverse(viewOrientation * viewPosition) << "\n\n";
  // Turns out it's the same as one inverse in the correct order
  mat4 view = Inverse(viewOrientation * viewPosition);
  mat4 result =
#else
  return
#endif
    mat4(
    right.x, newUp.x, forward.x, 0.0f,
    right.y, newUp.y, forward.y, 0.0f,
    right.z, newUp.z, forward.z, 0.0f,
    -Dot(right, position), -Dot(newUp, position), -Dot(forward, position), 1.0f
  );
#ifdef DO_SANITY_TESTS
#ifndef NO_EXTRAS
  if (result != view) {
    std::cout << "Error, result and view do not match in an expected manner!\n";
    std::cout << "view: \n" << view << "\n\n";
    std::cout << "result: \n" << result << "\n\n";
  }
#endif
  return result;
#endif
}

// https://msdn.microsoft.com/en-us/library/windows/desktop/bb147302(v=vs.85).aspx
mat4 Projection(float fov, float aspect, float zNear, float zFar) {
  /* https://msdn.microsoft.com/en-us/library/windows/desktop/bb205350(v=vs.85).aspx
  float yScale = 1.0f / tanf(DEG2RAD((fov * 0.5f)));
  float xScale = yScale / aspect;
  float zf = zFar;
  float zn = zNear;

  return mat4(
    xScale,     0,          0,               0,
    0,        yScale,       0,               0,
    0,          0,       zf / (zf - zn),         1,
    0,          0, - zn*zf / (zf - zn),     0
  ); */

  float tanHalfFov = tanf(DEG2RAD((fov * 0.5f)));

  mat4 result; // There are MANY different ways to derive a projection matrix!

#if 0
    result._11 = 1.0f / (aspect * tanHalfFov);
    result._22 = 1.0f / tanHalfFov;
    result._33 = (-zNear - zFar) / (zNear - zFar);
    result._44 = 0.0f;
    result._34 = 1.0f;
    result._43 = (2.0f * zFar * zNear) / (zNear - zFar);
#else
    float fovY = 1.0f / tanHalfFov; // cot(fov/2)
    float fovX = fovY / aspect; // cot(fov/2) / aspect

    result._11 = fovX;
    result._22 = fovY;
    result._33 = zFar / (zFar - zNear); // far / range
    result._34 = 1.0f;
    result._43 = -zNear * result._33; // - near * (far / range)
    result._44 = 0.0f;
#endif

  // result._43 *= -1.0f;

  return result;
}

// Derived following: http://www.songho.ca/opengl/gl_projectionmatrix.html
// Above was wrong, it was OpenGL style, our matrices are DX style
// Correct impl: https://msdn.microsoft.com/en-us/library/windows/desktop/bb205347(v=vs.85).aspx
mat4 Ortho(float left, float right, float bottom, float top, float zNear, float zFar) {
  float _11 = 2.0f / (right - left);
  float _22 = 2.0f / (top - bottom);
  float _33 = 1.0f / (zFar - zNear);
  float _41 = (left + right) / (left - right);
  float _42 = (top + bottom) / (bottom - top);
  float _43 = (zNear) / (zNear - zFar);

  return mat4(
     _11, 0.0f, 0.0f, 0.0f,
    0.0f,  _22, 0.0f, 0.0f,
    0.0f, 0.0f,  _33, 0.0f,
     _41,  _42,  _43, 1.0f
  );

  /*
  2 / (r - l)     0         0       0
  0         2 / (t - b)     0       0
  0         0         1 / (zf - zn)   0
  (l + r) / (l - r) (t + b) / (b - t) zn / (zn - zf)  1
  */

  /*float w = right - left;
  float h = bottom - top;

  return mat4(
    w * 0.5f, 0.0f, 0.0f, 0.0f,
    0.0f, h * 0.5f, 0.0f, 0.0f,
    0.0f, 0.0f, 1.0f / (zFar - zNear), 0.0f,
    0.0f, 0.0f, zNear / (zNear - zFar), 1.0f
  );*/
}

vec3 Decompose(const mat3& rot1) {
  mat3 rot = Transpose(rot1);

  float sy = sqrt(rot._11 * rot._11 + rot._21 * rot._21);

  bool singular = sy < 1e-6; // If

  float x, y, z;
  if (!singular) {
    x = atan2(rot._32, rot._33);
    y = atan2(-rot._31, sy);
    z = atan2(rot._21, rot._11);
  }
  else {
    x = atan2(-rot._23, rot._22);
    y = atan2(-rot._31, sy);
    z = 0;
  }

  return vec3(x, y, z);
}







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


