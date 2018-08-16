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
             :as pm :refer [vec-eq? vec2 vec3 PI]]
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

  (ensure?? (pm/vec-neq? (vec3 2 3 0)(vec3 1 2 3)) "v3,v3 neq?")
  (ensure?? (pm/vec-neq? (vec3 1 2 3)(vec2 2 3)) "v3 neq?")
  (ensure?? (pm/vec-neq? (vec2 2 3)(vec3 1 2 3)) "v2 neq?")

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

  (ensureThrown js/Error (raise! "hello" "world") "raise!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(js/console.log (runtest test-math "elmo test-math"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


