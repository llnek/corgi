;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.boot

  (:require-macros [])
  (:require []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn handleMultiDevices "" []
  (let [{:keys [width height]} (js->clj (cx/screenSize))
        policy (get-in @xcfg [:resolution :policy])
        flat? (get-in @*xcfg* [:game :landscape?])
        [X Y dir] (cond (or (>= width 2048)
                            (>= height 2048))
                        [2048 1536 :hdr]
                        (or (>= width 1136)
                            (>= height 1136))
                        [1136 640 :hds]
                        (or (>= width 1024)
                            (>= height 1024))
                        [1024 768 :hds]
                        (or (>= width 960)
                            (>= height 960))
                        [960 640 :hds]
                        :else [480 320 :sd])]
    (apply cx/setDevRes! (conj (if flat? [X Y] [Y X]) policy))
    (swap! *xcfg* #(assoc-in % [:resolution :resDir] dir))
    ;;device window size or canvas size.
    (cx/info* "view.frameSize = [" width ", " height "]")
    ;;need to prefix "assets" for andriod
    (do-with [searchPaths (js/jsb.fileUtils.getSearchPaths)]
             (doseq [p (map #(str % dir) ["assets/res/" "res/"])] (.push searchPaths p))
             (doseq [p ["assets/src" "src"]] (.push searchPaths p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- _startLoading "" [scene]
  (let [cb (oget scene "_callback")]
    (js/cc.loader.load (clj->js (oget scene "_resources"))
                       (fn [result cnt loadedCount] nil) (fn [] (cb)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn MyLoaderScene "" [resources callback]
  (let [scene (js/cc.Scene)
        func #(_startLoading scene %)]
    (->>
      #js{:_callback (or callback ec/noopy)
          :_resources (or resources [])
          :init (fn [] true)
          :onEnter
          #(this-as me
                    (do (.call js/cc.Node.prototype.onEnter me)
                        (ocall me "scheduleOnce" func 0.3)))
          :onExit #(this-as me (.call js/cc.Node.prototype.onExit me))}
      (attrs* scene))
    scene))

//////////////////////////////////////////////////////////////////////////////
(defn- preLaunch "" []
  (if (and (not-native?)
           (js/document.getElementById "cocosLoading"))
    (js/document.body.removeChild (js/document.getElementById "cocosLoading")))
  (js/cc.director.setProjection js/cc.Director.PROJECTION_2D)
  ;;for IOS
  (js/cc.view.enableRetina (= js/cc.sys.os js/cc.sys.OS_IOS))
  ;;for mobile web
  (if (and js/cc.sys.isMobile
           (not= js/cc.sys.browserType js/cc.sys.BROWSER_TYPE_BAIDU)
           (not= js/cc.sys.browserType js/cc.sys.BROWSER_TYPE_WECHAT))
    (js/cc.view.enableAutoFullScreen true))
  (let [{:keys [debug? frameRate size policy]} (:game @*xcfg*)]
    (if (native?)
      (->> (clj->js (handleMultiDevices))
           (js/jsb.fileUtils.setSearchPaths))
      (do (js/cc.view.resizeWithBrowserSize true)
          (js/cc.view.adjustViewPort true)
          (setDevRes! (:width size) (:height size) policy)))
    (if (number? frameRate)
      (js/cc.director.setAnimationInterval (/ 1 frameRate)))
    (if debug?
      (js/cc.director.setDisplayStats true))
    ;;hack to suppress the showing of cocos2d's logo
    (let [res ["cocos2d/pics/ZotohLab.png"
               "cocos2d/pics/preloader_bar.png"]
          cb #(ldr.preload (pvGatherPreloads)
                           #(let [{:keys [runOnce startScene]} @*xcfg*]
                              (runOnce)
                              (js/cc.director.runScene (startScene))))
          s (MyLoaderScene res cb)]
      (set! js/cc.loaderScene s)
      (js/cc.director.runScene s))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- startFunc "" []
  (cx/info* "game.start called")
  (preLaunch)
  (l10nInit)
  (sfxInit)
  (let [rs (js/cc.view.getDesignResolutionSize)]
    (cx/info* "DesignResolution, = [" (oget-width rs) ", " (oget-height rs) "]")
    (cx/info* "loaded and running. OK")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.onStartFunc startFunc)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


