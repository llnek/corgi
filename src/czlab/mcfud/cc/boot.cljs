;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.cc.boot

  (:require [czlab.mcfud.afx.core
             :as c :refer [n# _1 _2 fn_0 fn_* fn_1 cc+ do-with]]
            [clojure.string :as cs]
            [czlab.mcfud.cc.ccsx
             :as x :refer [gres+ gres* gicfg r-> p->
                           native? not-native? xcfg]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- handle-devices []
  (let [{:keys [policy size landscape?]} (:game @xcfg)
        [w h] (x/frame-size*)
        [dw dh] (r-> size)
        [X Y dir] (cond (or (>= w 2048)
                            (>= h 2048))
                        [2048 1536 :hdr]
                        (or (>= w 1136)
                            (>= h 1136))
                        [1136 640 :hds]
                        (or (>= w 1024)
                            (>= h 1024))
                        [1024 768 :hds]
                        (or (>= w 960)
                            (>= h 960))
                        [960 640 :hds]
                        :else [480 320 :sd])
        [X Y] (if (> dw dh) [X Y] [Y X])]
    (swap! xcfg #(assoc-in % [:game :resdir] dir))
    (x/design-res! dw dh policy)
    ;;device window size or canvas size.
    (x/debug* "view.frameSize = [" w ", " h "]")
    (x/debug* "game.size = [" dw ", " dh "]")
    ;;need to prefix "assets" for andriod
    (reduce #(do-with [acc %1]
               (.push acc
                      (str "assets/" %2)) (.push acc %2))
            (js/jsb.fileUtils.getSearchPaths) ["res/" "src/"])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- core-assets []
  #js [(x/gres+ :assets :loader :czlab)
       (x/gres+ :assets :loader :preloader)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- game-assets []
  (let [{{:keys [images tiles
                 fonts sheets sounds]} :assets} @xcfg
        r #(cs/replace %1 %2 ".png")
        f (fn [acc c x]
            (reduce (fn [b [_ v]] (x b v)) acc c))]
    (clj->js (c/ps!
               (-> (f (c/tvec*) tiles #(conj! %1 (x/gres* %2)))
                   (f images #(conj! %1 (x/gres* %2)))
                   (f sounds #(conj! %1 (x/gres* %2)))
                   (f fonts #(conj! %1
                                    (x/gres* %2)
                                    (x/gres* (r %2 #"\.fnt$"))))
                   (f sheets #(conj! %1
                                     (x/gres* %2)
                                     (x/gres* (r %2 #"\.plist$")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- precache-atlases []
  (doseq [[_ v]
          (get-in @xcfg [:assets :sheets])]
    (js/cc.spriteFrameCache.addSpriteFrames (x/gres* v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nice-fade-out [scene]
  (let [{:keys [run-once start-scene]} @xcfg
        logo (x/gcbyn scene :lg)
        f (fn_0 (precache-atlases)
                (x/remove! logo)
                (run-once)
                (x/run-scene (start-scene)))]
    ;(x/debug* "fade out! run next scene!!!!!")
    (c/call-js! scene "unscheduleUpdate")
    (x/remove! (x/gcbyn scene :pg))
    (c/call-js! logo
                "runAction"
                (js/cc.Sequence.create
                  (js/cc.FadeOut.create 1.2)
                  (js/cc.CallFunc.create f nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private CHUNK 36)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prelaunch-4 [scene]
  "Load resources.
  We have to load chunk by chunk because
  the array of resources can't be too big, else jsb complains"
  (x/debug* "inside prelaunch-4()")
  (let [[logo pbar] (core-assets)
        assets (game-assets)
        ;;[count, head, tail] snapshot info used by
        ;;each iteration as we chunk up the input
        state #js [0 0 0]
        R (x/vrect)
        pg (x/add-> scene
                    (->> (x/sprite* pbar)
                         (new js/cc.ProgressTimer)) :pg)
        cf #(let [[_ s e] state
                  arr (.slice assets s e)]
              ;(debug* "start s = " s ", e = " e)
              ;(debug* (js/JSON.stringify #js{:arr assets}))
              (if (pos? (n# arr))
                (js/cc.loader.load arr
                                   (fn [res sum cnt]
                                     (aset state 0 (+ 1 (_1 state))))
                                   (fn [] nil))))
        cb #(let [len (n# assets)
                  [cnt _ _] state]
              (c/call-js! pg
                          "setPercentage"
                          (min (* (/ cnt len) 100) 100))
              (if (< cnt len)
                (let [[_ _ head] state;get last tail
                      tail (+ head (min CHUNK (- len head)))]
                  (aset state 1 head)
                  (aset state 2 tail)
                  (cf))
                (nice-fade-out scene)))
        logo' (x/center-image R scene logo :lg)
        [mx my] (x/mid-rect* R)
        [_ height] (x/bsize* logo')]
    (doto pg
      (c/call-js! "setType" js/cc.ProgressTimer.TYPE_BAR)
      (c/call-js! "setScaleX" .8)
      (c/call-js! "setScaleY" .3)
      (x/pos! mx (- my (* .6 height))))
    (x/attr* scene #js{:update cb})
    (c/call-js! scene "scheduleUpdate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prelaunch-3 [scene]
  (x/debug* "inside prelaunch-3()")
  (fn_* (x/pos! (x/add-> scene
                         (x/clayer* 0 0 0) :bg -1)) (prelaunch-4 scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prelaunch-2
  "Hack to suppress the showing of cocos2d's logo,
  instead, we load our own logo and progress bar.
  Then we run another loading scene which actually
  loads the game assets - updating the progress bar."
  [scene]
  (x/debug* "inside prelaunch-2()")
  (let [f (fn_* (js/cc.loader.load
                  (core-assets) c/fn-nil (prelaunch-3 scene)))]
    (x/attr* scene
             #js {;:init #(.call js/cc.Scene.prototype.init scene)
                  ;:onExit #(.call js/cc.Node.prototype.onExit scene)
                  :onEnter (fn_0 (js/cc.Node.prototype.onEnter.call scene)
                                 (c/call-js! scene "scheduleOnce" f 0.3))})
    (x/run-scene scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prelaunch []
  (x/debug* "inside prelaunch()")
  (if-some [e (x/gebyid "cocosLoading")] (js/document.body.removeChild e))
  (js/cc.director.setProjection js/cc.Director.PROJECTION_2D)
  ;;for IOS
  (js/cc.view.enableRetina (= js/cc.sys.os js/cc.sys.OS_IOS))
  ;;for mobile web
  (if (and js/cc.sys.isMobile
           (not= js/cc.sys.browserType js/cc.sys.BROWSER_TYPE_BAIDU)
           (not= js/cc.sys.browserType js/cc.sys.BROWSER_TYPE_WECHAT))
    (js/cc.view.enableAutoFullScreen true))
  ;;
  (let [{{:keys [debug? size
                 frame-rate policy]} :game} @xcfg
        [width height] (r-> size)]
    (if (native?)
      (handle-devices)
      (let [[w h] (x/frame-size*)]
        (x/debug* "browser size, w= " w ", h= " h)
        (x/design-res! width height policy)
        (js/cc.view.adjustViewPort true)
        (js/cc.view.resizeWithBrowserSize true)))
    ;if we have a framerate, set it by inverting
    (if (c/pos?? frame-rate)
      (-> (c/flip frame-rate)
          (js/cc.director.setAnimationInterval)))
    ;maybe set debug on?
    (if debug? (js/cc.director.setDisplayStats true))
    ;;hack to suppress the showing of cocos2d's logo
    ;;and instead, show our own!
    (prelaunch-2 (x/scene*))
    (x/debug* "vrect = " (c/jsonize (x/vrect)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hook the start
(set! js/cc.game.____bootstrap (fn_* (x/bootstrap prelaunch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


