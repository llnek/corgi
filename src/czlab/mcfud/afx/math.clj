;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.math)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro V3 "Call vec3" [a b c] `(czlab.mcfud.afx.math/vec3 ~a ~b ~c))
(defmacro V2 "Call vec2" [a b] `(czlab.mcfud.afx.math/vec2 ~a ~b))

(defmacro vz2 "Call vec-zero 2." [] `(czlab.mcfud.afx.math/vec-zero 2))
(defmacro vz3 "Call vec-zero 3." [] `(czlab.mcfud.afx.math/vec-zero 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


