;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.rygel.pong.core

  (:require [czlab.mcfud.afx.core :as c :refer [_1 _2 fn_1
                                                fn_2 fn_*
                                                do->nil let->nil]]
            [oops.core :as oc]
            [czlab.mcfud.cc.dialog :as g]
            [czlab.mcfud.afx.ebus :as u]
            [czlab.mcfud.cc.ccsx :as x :refer [CV-X CV-O xcfg G-NET]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- rot-flat
  "" [obj] (c/call-js! obj "setRotation" 90) obj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- write-status [msg])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- write-score [who score])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- clamp-ball! [ball]
  (let->nil
    [{:keys [walls]} (:game @xcfg)
     {:keys [n s e w]} walls
     bx (x/bbox ball)
     hh (/ (_2 (x/r-> bx)) 2)]
    (if (x/collide? n bx)
      (x/posY! ball (- (x/minY n) hh)))
    (if (x/collide? s bx)
      (x/posY! ball (+ (x/maxY s) hh)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clamp-paddle! [pad & more]
  (let->nil
    [{:keys [walls]} (:game @xcfg)
     {:keys [n s e w]} walls
     f (fn_1 (let [px (x/bbox ____1)
                   hh (/ (_2 (x/r-> px)) 2)]
               (if (x/collide? px n)
                 (x/posY! ____1 (- (x/minY n) hh)))
               (if (x/collide? px s)
                 (x/posY! ____1 (+ (x/maxY s) hh)))))]
    (f pad)
    (doseq [x more] (f x))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- on-end []
  (let->nil
    [{{:keys [scene]} :game :keys [start-scene]} @xcfg
     hud (x/gcbyn scene :hud)
     gl (x/gcbyn scene :arena)]
    (js/cc.eventManager.pauseTarget gl true)
    (js/cc.eventManager.pauseTarget hud true)
    (g/pop-dlg scene
               {:msg (x/l10n "%playMore")
                :yes #(x/run-scene (start-scene))
                :cleanup #(do (js/cc.eventManager.resumeTarget gl true)
                              (js/cc.eventManager.resumeTarget hud true))})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- next-point []
  (let->nil
    [{:keys [scene b-vel]} (:game @xcfg)
     ball (x/gcbyn+ scene :arena :ball)
     velObj (c/get-js ball "____vel")
     [bx by] (x/p-> b-vel)
     [vx vy] [(* bx (c/rand-sign))
              (* by (c/rand-sign))]]
    (c/set-js! velObj "x" 0)
    (c/set-js! velObj "y" 0)
    (x/pos! ball (x/mid-rect))
    (c/call-js!
      ball
      "runAction"
      (new js/cc.Sequence
           (js/cc.scaleBy 2 3 3)
           (new js/cc.CallFunc
                (fn_* (c/set-js! velObj "x" vx)
                      (c/set-js! velObj "y" vy)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- won-game [who]
  (let->nil
    [{:keys [pmap] :as G} (:game @xcfg)
     pk (pmap who)
     {:keys [pid]} (G pk)]
    (x/sfx-effect :game-end)
    (write-status (x/l10n "%winGame" pid))
    (on-end)
    (swap! xcfg
           #(assoc-in % [:game :running?] false))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- check-game-state []
  (let->nil
    [{:keys [scene scores walls num-points]} (:game @xcfg)
     {:keys [n w e s]} walls
     ball (x/gcbyn+ scene :arena :ball)
     bb (x/bbox ball)]
    (when-some [win (cond (x/collide? bb e) CV-X
                          (x/collide? bb w) CV-O)]
      (let [wpath [:game :scores win]
            s (+ 1 (get-in @xcfg wpath))]
        (swap! xcfg #(assoc-in % wpath s))
        (write-score win s)
        (if (>= s num-points) (won-game win) (next-point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- on-click [topic msgTopic evt]
  (let->nil
    [{:keys [pmap scene]} (:game @xcfg)
     gl (x/gcbyn scene :arena)
     [kx ko] (x/pkeys pmap)
     [px po] (x/gcbyn* gl kx ko)]
    (cond (= msgTopic x/MOUSE-DOWN)
          (let [mp (c/call-js! evt "getLocation")
                r1 (x/bbox px)
                r2 (x/bbox po)
                x? (x/contains-pt? r1 mp)
                o? (x/contains-pt? r2 mp)]
            (if (or x? o?)
              (swap! xcfg
                     (fn_1 (update-in ____1
                                      [:game]
                                      #(assoc % :x-grabbed? x? :o-grabbed? o?))))))
          (= msgTopic x/MOUSE-UP)
          (swap! xcfg (fn_1 (update-in ____1
                                       [:game]
                                       #(assoc %
                                               :x-grabbed? false
                                               :o-grabbed? false))))
          (= msgTopic x/MOUSE-MOVE)
          (let [{:keys [x-grabbed? o-grabbed?]} (:game @xcfg)
                [_ dy] (x/p-> (c/call-js! evt "getDelta"))]
            (if o-grabbed? (x/posY! po (+ (x/posY po) dy)))
            (if x-grabbed? (x/posY! px (+ (x/posY px) dy)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- on-touch "" [topic msgTopic & msgs])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- repos-ball []
  (let->nil
    [{:keys [b-vel scene]} (:game @xcfg)
     ball (x/gcbyn+ scene :arena :ball)
     [vx vy] (x/p-> b-vel)
     velObj (c/get-js ball "____vel")]
    (x/pos! ball (x/mid-rect))
    (c/set-js! velObj "x" (* vx (c/rand-sign)))
    (c/set-js! velObj "y" (* vy (c/rand-sign)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- create-ball []
  (let->nil
    [{:keys [scene]} (:game @xcfg)
     gl (x/gcbyn scene :arena)
     ball (x/sprite* "#pongball.png")]
    (c/set-js! ball "____vel" (x/ccp* 0 0))
    (x/add-> gl ball "ball")
    (repos-ball)
    (swap! xcfg
           #(assoc-in % [:game :b-size] (x/bbox ball)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- repos-paddles []
  (let->nil
    [{:keys [walls scene pmap p-size]} (:game @xcfg)
     {:keys [e w]} walls
     gl (x/gcbyn scene :arena)
     [kx ko] (x/pkeys pmap)
     pw2 (/ (x/oget-width p-size) 2)
     [_ ey] (x/mid-rect* e)
     [_ wy] (x/mid-rect* w)]
    (x/pos! (x/gcbyn gl ko) (+ (x/maxX e) pw2) ey)
    (x/pos! (x/gcbyn gl kx) (+ (x/maxX w) pw2) wy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- create-paddles []
  (let->nil
    [{:keys [pmap imap scene]} (:game @xcfg)
     gl (x/gcbyn scene :arena)
     [kx ko] (x/pkeys pmap)
     ;which icon image?
     ix (imap CV-X)
     io (imap CV-O)
     x (rot-flat (x/sprite* ix))
     o (rot-flat (x/sprite* io))]
    (x/add-> gl x (name kx))
    (x/add-> gl o (name ko))
    (swap! xcfg
           #(assoc-in % [:game :p-size] (x/bbox x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn init []
  (let [{{:keys [gmode evQ]} :game :keys [ebus]} @xcfg]
    (u/sub+ ebus x/TOUCH-ONE-MOVE on-touch)
    (u/sub+ ebus x/MOUSE-MOVE on-click)
    (create-ball)
    (create-paddles)
    ;always player 1 for mode 1, and create the bot
    (when (= x/G-ONE gmode)
      (swap! xcfg #(assoc-in % [:game :bot] nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- move-via-key [keybd pad up down dy]
  (let [y' (+ (if (aget keybd down) (- dy) 0)
              (if (aget keybd up) dy 0))]
    (when-not (zero? y')
      (x/posY! pad (+ y' (x/posY pad))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- handle-keys [dt]
  (let->nil
    [{:keys [scene pmap p-vel kmap keybd]} (:game @xcfg)
     [vx vy] (x/p-> p-vel)
     [dx dy] (c/mapfv * dt vx vy)
     gl (x/gcbyn scene :arena)
     [kx ko] (x/pkeys pmap)
     [mx mo] [(kmap CV-X) (kmap CV-O)]]
    (move-via-key keybd (x/gcbyn gl kx) (nth mx 0) (nth mx 1) dy)
    (move-via-key keybd (x/gcbyn gl ko) (nth mo 0) (nth mo 1) dy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- motion-objs [dt]
  (let->nil
    [{:keys [scene]} (:game @xcfg)
     ball (x/gcbyn+ scene :arena :ball)
     [px py] (x/p-> (x/pos* ball))
     [vx vy] (x/p-> (c/get-js ball "____vel"))]
    (x/pos! ball (+ px (* dt vx)) (+ py (* dt vy)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- resolve-hit [ball pad]
  (let->nil
    [velObj (c/get-js ball "____vel")
     hw (/ (_1 (x/r-> (x/bbox ball))) 2)
     [cx _] (x/mid-rect*)
     [px _] (x/p-> (x/pos* pad))]
    (c/set-js! velObj
               "x" (- (x/oget-x velObj)))
    (cond (< px cx)
          (x/posX! ball (+ (x/maxX pad) hw))
          (> px cx)
          (x/posX! ball (- (x/minX pad) hw)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- bounce-wall [ball wall w]
  (let->nil
    [velObj (c/get-js ball "____vel")
     hh (/ (_2 (x/r-> (x/bbox ball))) 2)]
    (c/set-js! velObj "y" (- (x/oget-y velObj)))
    (cond (= :s w)
          (x/posY! ball (+ (x/maxY wall) hh))
          (= :n w)
          (x/posY! ball (- (x/minY wall) hh)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- check-collide []
  (let [{:keys [scene pmap walls]} (:game @xcfg)
        {:keys [n e s w]} walls
        [kx ko] (x/pkeys pmap)
        [pb px po]
        (-> (x/gcbyn scene :arena)
            (x/gcbyn* :ball kx ko))
        [bx bo bb] (x/bbox* px po pb)]
    (clamp-paddle! px po)
    (cond (x/collide? bb bx)
          (resolve-hit pb px)
          (x/collide? bb bo)
          (resolve-hit pb po)
          (x/collide? bb n)
          (bounce-wall pb n :n)
          (x/collide? bb s)
          (bounce-wall pb s :s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- pre-start []
  (do->nil
    (repos-paddles)
    (next-point)
    (swap! xcfg #(assoc-in % [:game :inited?] true))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- do-step [dt]
  (handle-keys dt)
  (motion-objs dt)
  (check-collide)
  (check-game-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn step [dt]
  (let->nil
    [{:keys [running? inited?]} (:game @xcfg)]
    (when running? (if-not inited? (pre-start) (do-step dt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF
