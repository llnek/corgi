(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.impulse

  (:require-macros [czlab.elmo.afx.core :as ec :refer [n#]])

  (:require [czlab.elmo.afx.core :as ec :refer [invert]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [vec2]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def MaxPolyVertexCount 64)

(defmulti copyShape "" (fn [a] (:type a)))
(defmulti initShape "" (fn [a] (:type a)))
(defmulti computeMass "" (fn [a density] (:type a)))
(defmulti setOrient "" (fn [a radians] (:type a)))
(defmulti drawShape "" (fn [a] (:type a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Shape "" [e]
  (atom {:type e :body nil :radius 0 :u nil}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Circle "" [r]
  (let [c (Shape :circle)]
    (swap! c #(assoc % :radius r)) c))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod initShape :circle [c] (computeMass c 1))
(defmethod copyShape :circle [c] (atom @c))
(defmethod computeMass :circle [c density]
  (let [{:keys [body radius]} @c
        r2 (* radius radius)
        bm (* PI r2 density)
        I (* bm r2)]
    (swap! body
           #(assoc %
                   :m bm
                   :im (invert bm)
                   :I I
                   :iI = (invert I))) c))
(defmethod setOrient :circle [c radians] c)
(defmethod drawShape :circle [c ctx] c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Polygon "" []
  (let [s (Shape :polygon)]
    s))

(defmethod initShape :polygon [p] (computeMass p 1))
(defmethod copyShape :polygon [p] (atom @p))
(defmethod computeMass :polygon [p density]
  (let [{:keys [edges]} @p
        inv3 (invert 3)
        sz (n# edges)
        ;;calculate centroid and moment of interia
        [C M I]
        (loop [i 0 c V2_ZERO area 0 I 0]
          (if (>= i sz)
            [(v2-scale c (invert area)) (* density area) (* density I)]
            (let [{p2 :v1} @(->> (mod (+ 1 i) sz)
                                 (nth edges))
                  {p1 :v1} @(nth edges i)
                  {x1 :x y1 :y} p1
                  {x2 :x y2 :y} p2
                  D (v2-xss p1 p2)
                  ;;triangle vertices, third vertex implied as (0, 0)
                  triArea (* 0.5 D)
                  x' (+ (* x1 x1) (* x2 x1) (* x2 x2))
                  y' (+ (* y1 y1) (* y2 y1) (* y2 y2))]
              ;;use area to weight the centroid average, not just vertex position
              (recur (+ 1 i)
                     (v2-add c (v2-scale (v2-add p1 p2) (* triArea inv3)))
                     (+ area triArea)
                     (+ I (* 0.25f * inv3 * D (+ x' y')))))))]
    (swap! p
           #(assoc %
                   :m M :im (invert M) :I I :iI (invert I)))
    ;;;translate vertices to centroid (make the centroid (0, 0)
    ;;for the polygon in model space)
    ;;Not really necessary
    (loop [i 0 lastE nil]
      (if (>= i sz)
        (swap! lastE #(assoc % :v2 (:v1 @(nth edges 0))))
        (let [e (nth edges i)
              {:keys [v1 v2]} @e
              v1' (v2-sub v1 C)
              e' (if (pos? i) (nth edges (- i 1)))]
          (swap! e #(assoc % :v1 v1'))
          (if (some? e') (swap! e' #(assoc % :v2 v1')))
          (recur (+ 1 i) e))))) p)

(defmethod setOrient :polygon [p radians]
  (let [{:keys [u]} @p]
    (.Set u radians ) p))

(defmethod drawShape :polygon [p] p)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setBox "" [p hw hh]
  (swap! p
         #(assoc %
                 :edges [(Point2D -hw -hh)
                         (Point2D hw -hh)
                         (Point2D hw hh)
                         (Point2D -hw hh)]
                 :normal [(vec2 0 -1)
                          (vec2 1 0) (vec2 0 1) (vec2 -1 0)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn set "" [p vertices]
  (let [sz (n# vertices)
        ;;Find the right most point on the hull
        rightMost
        (loop [i 1 rightMost 0 cx (:x (_1 vertices))]
          (if (>= i sz)
            rightMost
            (let [x (:x (nth vertices i))
                  [r x'] (cond (> x cx) [i x]
                               :else
                               (if (and (= x cx)
                                        (< (:y (nth vertices i))
                                           (:y (nth vertices rightMost))))
                                 [i cx]
                                 [rightMost cx]))]
              (recur (inc i) r x'))))
        [numVerts]
        (loop [hull [rightMost] outCount 0 indexHull rightMost loop? true]
          (if-not loop?
            [outCount]
            (let [nextHullIndex
                  (loop [i 1 nextHullIndex 0]
                    (if (>= i sz)
                      nextHullIndex
                      (recur (+ 1 i)
                             (if (= nextHullIndex indexHull)
                               i
                               (let [v' (nth vertices (nth hull outCount))
                                     e1 (v2-sub (nth vertices nextHullIndex) v')
                                     e2 (v2-sub (nth vertices i) v')
                                     c (v2-xss e1 e2)]
                                 (if (or (neg? c)
                                         (and (zero? c)
                                              (> (v2-lensq e2)
                                                 (v2-lensq e1)))) i nextHullIndex))))))]
              (recur hull
                     (+ 1 outCount)
                     nextHullIndex
                     (not= nextHullIndex rightMost)))))
        verts (transient [])]
    (dotimes [i numVerts]
      (conj! verts (nth vertices (nth hull i))))
    (swap! p #(assoc % :vertices (persistent! verts)))
    (calcFaceNormals p)))

(defn calcFaceNormals "" [p]
  (let [{:keys [vertices]} @p
        ns (transient [])
        sz (n# vertices)]
    (dotimes [i sz]
      (let [i2 (mod (+ 1 i) sz)
            face (v2-sub (nth vertices i2) (nth vertices i))]
        ;;ensure no zero-length edges
        (assert (> (v2-lensq face) (* EPSILON EPSILON)))
        ;;calculate normal with 2D cross product between vector and scalar
        (conj! ns (v2-norm (vec2 (:y face) (- (:x face)))))))
    (swap! p #(assoc % :normals (persistent! ns)))))



  // The extreme point along a direction within a polygon
  Vec2 GetSupport( const Vec2& dir )
  {
    real bestProjection = -FLT_MAX;
    Vec2 bestVertex;

    for(uint32 i = 0; i < m_vertexCount; ++i)
    {
      Vec2 v = m_vertices[i];
      real projection = Dot( v, dir );

      if(projection > bestProjection)
      {
        bestVertex = v;
        bestProjection = projection;
      }
    }

    return bestVertex;
  }



