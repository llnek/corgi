(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.p2d.impulse

  (:require-macros [czlab.elmo.afx.core :as ec :refer [_1 n#]])

  (:require [czlab.elmo.afx.core :as ec :refer [invert]]
            [czlab.elmo.afx.gfx2d
             :as gx :refer [PI TWO-PI V2_ZERO Point2D EPSILON
                            *pos-inf* *neg-inf*
                            vec2 v2-add v2-sub v2-dot v2-xss]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmulti computeMass "" (fn [a density] (:type a)))
(defmulti setOrient "" (fn [a radians] (:type a)))
(defmulti copyShape "" (fn [a] (:type a)))
(defmulti initShape "" (fn [a] (:type a)))
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
(defn- calcFaceNormals "" [p]
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;The extreme point along a direction within a polygon
(defn getSupport "" [p dir]
  (let [{:keys [vertices]} @p
        sz (n# vertices)]
    (loop [i 0 proj *neg-inf* bv nil]
      (if (>= i sz)
        bv
        (let [v (nth vertices i)
              p' (v2-dot v dir)
              b? (> p' proj)]
          (recur (inc i) (if b? p' proj) (if b? v bv)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn Body "" [shape x y]
  (let [body (atom {})]
    (swap! body
           #(assoc %
                   :pos (Point2D x y)
                   :type :body
                   :vel V2_ZERO
                   :ii 0
                   :im 0
                   :i 0
                   :m 0
                   :angVel 0
                   :torque 0
                   :angle 0
                   :force V2_ZERO
                   :staticFriction 0.5
                   :dynamicFriction 0.3
                   :bounce 0.2))
    (swap! shape #(assoc % :body body))
    (initShape shape)
    body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn applyForce "" [b f]
  (swap! b (fn [{:keys [force] :as root}]
             (assoc root :force (+ force f)))) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn applyImpulse "" [b impulse contactVector]
  (let [{:keys [vel im ii angVel]} @b]
    (swap! b
           #(assoc %
                   :vel (v2-add vel (v2-scale impulse im))
                   :angVel (+ angVel
                              (* ii (v2-xss contactVector impulse))))) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn setStatic "" [b]
  (swap! b #(assoc %
                   :ii 0 :im 0 :m 0 :i 0)) b)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod setOrient :body [obj radians]
  (swap! obj #(assoc % :angle radians))
  (setOrient (:shape @obj) radians) obj)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
(defn- integrateForces "" [b dt]
  (let [{:keys [im ii vel force angVel torque]} @b
        dt' (/ dt 2)
        {:keys [gravity]} @*gWorld*]
    (when-not (zero? im)
      (swap! b
             #(assoc %
                     :angVel (+ angVel (* dt' torque ii))
                     :vel (+ vel (* dt' (+ (* im force) gravity)))))) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- integrateVelocity "" [b dt]
  (let [{:keys [pos im vel angVel angle]} @b]
    (when-not (zero? im)
      (swap! b
             #(assoc %
                     :angle (+ angle (* dt angVel))
                     :pos (v2-add pos (v2-scale vel dt))))
      (setOrient b (:angle @b))
      (integrateForces b dt))
    b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step "" []
  (let [contacts (transient [])
        {:keys [samples]} @*gWorld*
        sz (ec/countStore samples)]
    (dotimes [i sz]
      (let [A (ec/nthStore samples i)]
        (loop [j (+ i 1)]
          (when (< j sz)
            (let [B (ec/nthStore samples j)]
              (when-not (and (zero? (:im @A))
                             (zero? (:im @B)))
                (let [m (solve (manifold A B))]
                  (if (pos? (:contact_count m)) (conj! contacts m))))
              (recur (inc j)))))))

  // Integrate forces
  for(uint32 i = 0; i < bodies.size( ); ++i)
    IntegrateForces( bodies[i], m_dt );

  // Initialize collision
  for(uint32 i = 0; i < contacts.size( ); ++i)
    contacts[i].Initialize( );

  // Solve collisions
  for(uint32 j = 0; j < m_iterations; ++j)
    for(uint32 i = 0; i < contacts.size( ); ++i)
      contacts[i].ApplyImpulse( );

  // Integrate velocities
  for(uint32 i = 0; i < bodies.size( ); ++i)
    IntegrateVelocity( bodies[i], m_dt );

  // Correct positions
  for(uint32 i = 0; i < contacts.size( ); ++i)
    contacts[i].PositionalCorrection( );

  // Clear all forces
  for(uint32 i = 0; i < bodies.size( ); ++i)
  {
    Body *b = bodies[i];
    b->force.Set( 0, 0 );
    b->torque = 0;
  }
}

void Scene::Render( void )
{
  for(uint32 i = 0; i < bodies.size( ); ++i)
  {
    Body *b = bodies[i];
    b->shape->Draw( );
  }

  glPointSize( 4.0f );
  glBegin( GL_POINTS );
  glColor3f( 1.0f, 0.0f, 0.0f );
  for(uint32 i = 0; i < contacts.size( ); ++i)
  {
    Manifold& m = contacts[i];
    for(uint32 j = 0; j < m.contact_count; ++j)
    {
      Vec2 c = m.contacts[j];
      glVertex2f( c.x, c.y );
    }
  }
  glEnd( );
  glPointSize( 1.0f );

  glBegin( GL_LINES );
  glColor3f( 0.0f, 1.0f, 0.0f );
  for(uint32 i = 0; i < contacts.size( ); ++i)
  {
    Manifold& m = contacts[i];
    Vec2 n = m.normal;
    for(uint32 j = 0; j < m.contact_count; ++j)
    {
      Vec2 c = m.contacts[j];
      glVertex2f( c.x, c.y );
      n *= 0.75f;
      c += n;
      glVertex2f( c.x, c.y );
    }
  }
  glEnd( );
}

Body *Scene::Add( Shape *shape, uint32 x, uint32 y )
{
  assert( shape );
  Body *b = new Body( shape, x, y );
  bodies.push_back( b );
  return b;
}




