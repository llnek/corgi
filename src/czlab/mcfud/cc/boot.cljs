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
             :as c :refer [n# _1 _2 fn_* fn_1 cc+ do-with]]
            [clojure.string :as cs]
            [czlab.mcfud.cc.ccsx
             :as x :refer [gres+ gres* gicfg r-> p->
                           debug* native? not-native? xcfg]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- handle-multi-devices []
  (let [{:keys [policy size landscape?]} (:game @xcfg)
        [w h] (r-> (x/frame-size))
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
                        :else [480 320 :sd])]
    (swap! xcfg #(assoc-in % [:game :resdir] dir))
    (x/set-dev-res! dw dh policy)
    ;;device window size or canvas size.
    (debug* "view.frameSize = [" w ", " h "]")
    (debug* "game.designSize = [" dw ", " dh "]")
    ;;need to prefix "assets" for andriod
    (do-with [searchs (js/jsb.fileUtils.getSearchPaths)]
      (doseq [p (map #(str % dir)
                     ["assets/res/" "res/"])] (.push searchs p))
      (doseq [p ["assets/src" "src"]] (.push searchs p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- core-assets []
  #js [(gres+ :assets :loader ::czlab)
       (gres+ :assets :loader ::preloader)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- game-assets []
  (let [r #(cs/replace %1 %2 ".png")
        f (fn [acc c x]
            (reduce (fn [b [_ v]] (x b v)) acc c))
        {{:keys [images tiles
                 fonts sheets sounds]} :assets} @xcfg]
    (-> (f (c/tvec*) tiles #(conj! %1 (gres* %2)))
        (f images #(conj! %1 (gres* %2)))
        (f sounds #(conj! %1 (gres* %2)))
        (f fonts #(conj! %1
                         (gres* %2)
                         (gres* (r %2 #"\.fnt$"))))
        (f sheets #(conj! %1
                          (gres* %2)
                          (gres* (r %2 #"\.plist$"))))
        (c/pert!)
        (clj->js))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- precache-atlases []
  (doseq [[_ v]
          (get-in @xcfg [:assets :sheets])]
    (js/cc.spriteFrameCache.addSpriteFrames (gres* v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nice-fade-out [scene]
  (let [{:keys [run-once start-scene]} @xcfg
        logo (x/gcbyn scene "lg")
        f #(do (precache-atlases)
               (x/remove! logo)
               (run-once)
               (x/run-scene (start-scene)))]
    (debug* "fade out! run next scene!!!!!")
    (ocall! scene "unscheduleUpdate")
    (x/remove! (x/gcbyn scene "pg"))
    (ocall! logo
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
  (let [[logo pbar] (core-assets)
        assets (game-assets)
        ;;[count, head, tail] snapshot info used by
        ;;each iteration as we chunk up the unput
        state #js [0 0 0]
        pg (x/add-> scene
                    (new js/cc.ProgressTimer (x/sprite* pbar)) "pg")
        cf (fn_* (let [[_ s e] state
                       arr (.slice assets s e)]
                   ;(debug* "start s = " s ", e = " e)
                   ;(debug* (js/JSON.stringify #js{:arr assets}))
                   (if (pos? (n# arr))
                     (js/cldr.load arr
                                   (fn [res sum cnt]
                                     ;(debug* "total = " sum ", cnt = " cnt)
                                     (aset state 0 (+ 1 (_1 state))))
                                   (fn []
                                     ;(debug* "done = " (_1 state))
                                     nil)))))
        cb (fn_* (let [len (n# assets)
                       [cnt _ _] state]
                   (->> (min (* (/ cnt len) 100) 100)
                        (ocall! pg "setPercentage"))
                   (if (< cnt len)
                     (let [[_ _ head] state;get last tail
                           tail (+ head (min CHUNK (- len head)))]
                       (aset state 1 head)
                       (aset state 2 tail)
                       (cf))
                     (nice-fade-out scene))))
        _ (x/attr* scene #js{:update cb})
        logo' (x/center-image scene logo "lg")
        [mx my] (p-> (x/mid-rect (x/vrect)))
        [_ height] (r-> (x/bsize logo'))]
    (ocall! pg "setType" js/cc.ProgressTimer.TYPE_BAR)
    (ocall! pg "setScaleX" .8)
    (ocall! pg "setScaleY" .3)
    (x/pos! pg mx (- my (* .6 height)))
    (ocall! scene "scheduleUpdate")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prelaunch-3 [scene]
  (fn_* (x/pos! (x/add-> scene
                         (x/clayer* 0 0 0) "bg" -1)) (prelaunch-4 scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prelaunch-2
  "Hack to suppress the showing of cocos2d's logo,
  instead, we load our own logo and progress bar.
  Then we run another loading scene which actually
  loads the game assets - updating the progress bar."
  [scene]
  (let [f (fn_* (js/cldr.load
                  (core-assets) nil (prelaunch-3 scene)))]
    (x/attr* scene
             #js {:init #(.call js/sproto.init scene)
                  :onExit #(.call js/nproto.onExit scene)
                  :onEnter #(do (.call js/nproto.onEnter scene)
                                (ocall scene "scheduleOnce" f 0.3))})
    (x/run-scene scene)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- prelaunch []
  (if-some [e (x/gebyid "cocosLoading")] (js/dbody.removeChild e))
  (js/dtor.setProjection js/Dtor.PROJECTION_2D)
  ;;for IOS
  (js/view.enableRetina (= js/sys.os js/sys.OS_IOS))
  ;;for mobile web
  (if (and js/sys.isMobile
           (not= js/sys.browserType js/sys.BROWSER_TYPE_BAIDU)
           (not= js/sys.browserType js/sys.BROWSER_TYPE_WECHAT))
    (js/view.enableAutoFullScreen true))
  ;;
  (let [{{:keys [debug? size
                 frame-rate policy]} :game} @xcfg
        [width height] (r-> size)]
    (if (native?)
      (-> (clj->js (handle-multi-devices))
          (js/jsb.fileUtils.setSearchPaths))
      (do (js/view.resizeWithBrowserSize true)
          (js/view.adjustViewPort true)
          (x/set-dev-res! width height policy)))
    (if (c/pos?? frame-rate)
      (-> (c/num-flip frame-rate)
          (js/dtor.setAnimationInterval)))
    (if debug? (js/dtor.setDisplayStats true))
    (swap! xcfg
           #(assoc-in % [:game :vert?] (x/is-portrait?)))
    ;;hack to suppress the showing of cocos2d's logo
    ;;and instead, show our own!
    (prelaunch-2 (x/scene*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hook the start
(->> (fn_*
       (let [lang (keyword js/sys.language)]
         (x/debug* "boot-loader called")
         (x/debug* "locale = " (name lang))
         (swap! xcfg
                #(assoc (c/deep-merge %
                                      (js/cc.game.____configurator)) :lang lang))
         (prelaunch)
         (x/sfx-music-vol! (get-in @xcfg [:audio :volume]))
         (let [[w h] (r-> (js/view.getDesignResolutionSize))]
           (debug* "design = [" w ", " h "]")
           (debug* "loaded and running. OK"))))
     (set! js/cc.game.____bootloader))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


