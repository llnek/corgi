;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.test

  (:require [czlab.elmo.p2d.math
             :as pm :refer [vec-neq? vec-eq? vec2 vec3 PI
                            mat-neq? mat-eq? mat* mat2 mat3 mat4]]
            [clojure.string :as cs]
            [czlab.elmo.afx.core
             :as ec :refer [deftest if-some+ when-some+
                            ensureThrown _1 _2 _3
                            runtest ensure?? raise!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def TMPVAR (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest test-math

  (ensure?? (let [[x y] (vec2 1 2)]
              (and (= 1 x)(= 2 y))) "vec2 ctor")

  (ensure?? (let [[x y z] (vec3 1 2 3)]
              (and (= 1 x)(= 2 y)(= 3 z))) "vec3 ctor")

  (ensure?? (vec-neq? (vec3 2 3 0)(vec3 1 2 3)) "v3,v3 neq?")
  (ensure?? (vec-neq? (vec3 1 2 3)(vec2 2 3)) "v3 neq?")
  (ensure?? (vec-neq? (vec2 2 3)(vec3 1 2 3)) "v2 neq?")

  (ensure?? (vec-eq? (vec2 2 3)(vec2 2 3)) "v2 eq?")
  (ensure?? (vec-eq? (vec3 1 2 3)(vec3 1 2 3)) "v3 eq?")

  (ensure?? (vec-eq? (vec3 5 7 9)
                        (pm/vec-add (vec3 1 2 3)(vec3 4 5 6))) "vec-add")

  (ensure?? (vec-eq? (vec2 1 3)
                        (pm/vec-sub (vec2 2 3)(vec2 1 0))) "vec-sub")

  (ensure?? (vec-eq? (vec2 4 15)
                        (pm/vec-mult (vec2 2 3)(vec2 2 5))) "vec-mult")

  (ensure?? (vec-eq? (vec2 3 4)
                        (pm/vec-div (vec2 9 8)(vec2 3 2))) "vec-div")

  (ensure?? (vec-eq? (vec2 27 24)
                        (pm/vec-scale (vec2 9 8) 3 )) "vec-scale")
  (ensure?? (vec-eq? (vec3 27 24 -21)
                        (pm/vec-scale (vec3 9 8 -7) 3 )) "vec-scale")

  (ensure?? (vec-eq? (vec2 6 5)
                        (pm/vec-minus (vec2 9 8) 3 )) "vec-minus")
  (ensure?? (vec-eq? (vec2 12 11)
                        (pm/vec-plus (vec2 9 8) 3 )) "vec-plus")

  (ensure?? (= -42 (pm/vec-dot (vec2 -9 8)(vec2 2 -3))) "v2-dot")
  (ensure?? (= 49 (pm/vec-dot (vec3 9 8 -7)(vec3 -2 4 -5))) "v3-dot")

  (ensure?? (= 11 (pm/vec-xss (vec2 -9 8)(vec2 2 -3))) "v2-xss")
  (ensure?? (vec-eq? (vec3 -12 59 52)
                        (pm/vec-xss (vec3 9 8 -7)(vec3 -2 4 -5))) "v3-xss")

  (ensure?? (= 14 (pm/vec-lensq (vec3 1 2 -3))) "v3-lensq")
  (ensure?? (= 13 (pm/vec-lensq (vec2 2 3))) "v2-lensq")
  (ensure?? (= 5 (pm/vec-len (vec2 3 4))) "vec-len")

  (ensure?? (= 116 (pm/vec-distsq (vec3 7 6 5)(vec3 1 2 -3))) "v3-distsq")
  (ensure?? (= 202 (pm/vec-distsq (vec2 2 3)(vec2 -9 -6))) "v2-distsq")

  (ensure?? (vec-eq? (vec2 0.6 0.8)
                        (pm/vec-unit (vec2 3 4))) "vec-unit")

  (ensure?? (vec-eq? (vec2 -4 3)
                        (pm/vec-rot (vec2 3 4) (/ PI 2))) "v2-rot")

  (ensure?? (= 90 (pm/rad->deg
                    (pm/vec-angle (vec2 3 4)(vec2 -4 3)))) "vec-angle")

  (ensure?? (vec-eq? (vec2 3 -4)
                        (pm/vec-neg (vec2 -3 4))) "v2-neg")

  (ensure?? (vec-eq? (vec2 -4 3)
                        (pm/vec-normal (vec2 3 4) :left)) "vec-normal")
  (ensure?? (vec-eq? (vec2 4 -3)
                        (pm/vec-normal (vec2 3 4))) "vec-normal")

  (ensure?? (vec-eq? (vec2 -1 4)
                        (pm/vec-min (vec2 5 4)(vec2 -1 7))) "vec-min")
  (ensure?? (vec-eq? (vec3 9 3 4)
                     (pm/vec-max (vec3 8 -3 4)(vec3 9 3 0))) "vec-max")

  (ensure?? (mat-eq? (mat3 1 0 0 0 1 0 0 0 1)
                     (pm/mat-identity 3)) "mat-identity")

  (ensure?? (mat-eq? (mat3 0 0 0 0 0 0 0 0 0)
                     (pm/mat-zero 3)) "mat-zero")

  (ensure?? (mat-neq? (mat3 1 0 0 0 1 0 1 0 1)
                      (mat3 1 0 1 0 1 0 0 0 1)) "mat-neq?")

  (ensure?? (mat-eq? (mat2 1 0 0 1)
                     (pm/mat-identity 2)) "mat2")

  (ensure?? (mat-eq? (mat3 1 0 0 0 1 0 0 0 1)
                     (pm/mat-identity 3)) "mat3")

  (ensure?? (mat-eq? (pm/mat4 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1)
                     (pm/mat-identity 4)) "mat4")

  (ensure?? (mat-eq? (mat* [3 2] 1 4 2 5 3 6)
                     (pm/mat-xpose (mat* [2 3] 1 2 3 4 5 6))) "mat-xpose")

  (ensure?? (mat-eq? (mat3 -4 -8 -4 -6 -12 -6 -2 -4 -2)
                     (pm/mat-minor (mat3 1 2 3 3 4 5 7 8 9))) "mat-minor")

  (ensure?? (mat-eq? (mat3 -4 8 -4 6 -12 6 -2 4 -2)
                     (pm/mat-cofactor (mat3 1 2 3 3 4 5 7 8 9))) "mat-cofactor")

  (ensure?? (= -2 (pm/mat-det (mat2 1 2 3 4))) "mat-det")
  (ensure?? (= -64 (pm/mat-det (mat3 1 2 3 3 4 5 7 -8 9))) "mat-det")

  (ensure?? (mat-eq? (mat* [2 3]
                           -12 10 8 -6 -4 2)
                     (pm/mat-scale
                       (mat* [2 3] 6 -5 -4 3 2 -1) -2)) "mat-scale")

  (ensure?? (mat-eq? (mat3 12 9 6 30 23 16 48 37 26)
                     (pm/mat-multAB (mat* [3 2] 1 2 3 4 5 6)
                                    (mat* [2 3] 6 5 4 3 2 1))) "mat-multAB")

  (ensure?? (mat-eq? (mat3 93 42 3 -6 -30 18 67 6 -13)
                     (pm/mat-adjugate
                       (mat3 1 2 3 4 -5 -6 7 8 -9))) "mat-adjugate")

  (ensure?? (mat-eq? (mat3 (/ 31 94) (/ 7 47) (/ 1 94)
                           (/ -1 47) (/ -5 47) (/ 3 47)
                           (/ 67 282) (/ 1 47) (/ -13 282))
                     (pm/mat-inverse
                       (mat3 1 2 3 4 -5 -6 7 8 -9))) "mat-inverse")

  (ensure?? (mat-eq? (mat2 0.6 -0.7 -0.2 0.4)
                     (pm/mat-inverse (mat2 4 7 2 6))) "mat-inverse")

  (ensure?? (mat-eq? (mat4 1 0 0 0 0 1 0 0 0 0 1 0 4 7 2 1)
                     (pm/mat4-txlate (vec3 4 7 2))) "mat4-txlate")

  (ensure?? (vec-eq? (vec3 4 7 2)
                     (pm/getTranslation
                       (mat4 1 0 0 0 0 1 0 0 0 0 1 0 4 7 2 1))) "getTranslation")

  (ensure?? (mat-eq? (mat4 4 7 2 0 5 6 7 0 9 0 3 0 0 0 0 1)
                     (pm/mat-fromMX (mat3 4 7 2 5 6 7 9 0 3))) "mat-fromMX")

  (ensure?? (mat-eq? (mat4 4 0 0 0 0 7 0 0 0 0 2 0 0 0 0 1)
                     (pm/mat-fromVX (vec3 4 7 2))) "mat-fromVX")

  (ensure?? (vec-eq? (vec3 4 7 2)
                     (pm/getScaleFromMX
                       (mat4 4 0 0 0 0 7 0 0 0 0 2 0 0 0 0 1))) "getScaleFromMX")

  (ensure?? (vec-eq? (vec2 -23 42)
                     (pm/mat-vmult (mat2 3 -5 7 2) (vec2 4 7))) "mat-vmult")

  (ensure?? (mat-eq? (mat2 0 -1 1 0)
                     (pm/rotation2x2 (/ PI 2))) "rotation2x2"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(js/console.log (runtest test-math "elmo test-math"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


