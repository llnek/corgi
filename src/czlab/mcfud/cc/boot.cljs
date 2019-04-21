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

  (:require-macros [czlab.mcfud.afx.core
                    :as ec :refer [defvoid defvoid- f#* do-with]]
                   [czlab.mcfud.cc.ccsx
                    :as cx :refer [oget-width oget-height
                                   not-native? gebyid native? attr*]])
  (:require [czlab.mcfud.afx.core
             :as ec :refer [nil-fn pos?? numFlip deepMerge]]
            [czlab.mcfud.cc.ccsx :as cx :refer [xcfg setDevRes!]]
            [czlab.mcfud.afx.gfx2d :refer [Area]]
            [czlab.mcfud.afx.math :refer [vec2]]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- handleMultiDevices

  "Prepare assets for a device."
  []

  (let [{:keys [policy landscape?]} (:game @xcfg)
        {:keys [wide tall]} (cx/frameSize)
        [X Y dir] (cond (or (>= wide 2048)
                            (>= tall 2048))
                        [2048 1536 :hdr]
                        (or (>= wide 1136)
                            (>= tall 1136))
                        [1136 640 :hds]
                        (or (>= wide 1024)
                            (>= tall 1024))
                        [1024 768 :hds]
                        (or (>= wide 960)
                            (>= tall 960))
                        [960 640 :hds]
                        :else [480 320 :sd])]
    (-> (if landscape? (Area X Y) (Area Y X)) (setDevRes! policy))
    (swap! xcfg #(assoc-in % [:game :resDir] dir))
    ;;device window size or canvas size.
    (cx/debug* "view.frameSize = [" wide ", " tall "]")
    ;;need to prefix "assets" for andriod
    (do-with [searchs (js/jsb.fileUtils.getSearchPaths)]
      (doseq [p (map #(str % dir)
                     ["assets/res/" "res/"])] (.push searchs p))
      (doseq [p ["assets/src" "src"]] (.push searchs p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preloadLogoScene

  "Hack to suppress the showing of cocos2d's logo,
  instead, we load our own logo and progress bar.
  Then we run another loading scene which actually
  loads the game assets - updating the progress bar."
  []

  (do-with [scene (new js/cc.Scene)]
    (let [func (f#* (js/cc.loader.load (clj->js (cx/gldr->imgs))
                                       nil-fn
                                       (cx/preloader)))]
      (attr* scene
             #js {:init #(.call js/cc.Scene.prototype.init scene)
                  :onEnter #(do (.call js/cc.Node.prototype.onEnter scene)
                                (ocall scene "scheduleOnce" func 0.3))
                  :onExit #(.call js/cc.Node.prototype.onExit scene)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- preLaunch

  "Prelaunch of game."
  []

  (if-some [e (gebyid "cocosLoading")] (js/document.body.removeChild e))

  (js/cc.director.setProjection js/cc.Director.PROJECTION_2D)
  ;;for IOS
  (js/cc.view.enableRetina (= js/cc.sys.os js/cc.sys.OS_IOS))
  ;;for mobile web
  (if (and js/cc.sys.isMobile
           (not= js/cc.sys.browserType js/cc.sys.BROWSER_TYPE_BAIDU)
           (not= js/cc.sys.browserType js/cc.sys.BROWSER_TYPE_WECHAT))
    (js/cc.view.enableAutoFullScreen true))
  ;;
  (let [{:keys [debug? frameRate size policy]} (:game @xcfg)]
    (if (native?)
      (-> (clj->js (handleMultiDevices))
          (js/jsb.fileUtils.setSearchPaths))
      (do (js/cc.view.resizeWithBrowserSize true)
          (js/cc.view.adjustViewPort true)
          (cx/setDevRes! size policy)))
    (if (pos?? frameRate) (-> (numFlip frameRate)
                              (js/cc.director.setAnimationInterval)))
    (if debug? (js/cc.director.setDisplayStats true))
    (swap! xcfg
           #(assoc-in % [:game :vert?] (cx/isPortrait?)))
    ;;hack to suppress the showing of cocos2d's logo
    ;;and instead, show our own!
    (cx/run* (preloadLogoScene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvoid- boot-loader

  "Bootload the game."
  [& xs]

  (cx/debug* "boot-loader called")
  (swap! xcfg
         #(deepMerge % (js/cc.game.____configurator)))

  (cx/debug* (js/JSON.stringify (-> (dissoc @xcfg :l10nTable) (clj->js))))
  (preLaunch)
  (cx/l10nInit)
  (cx/sfxInit)
  (let [rs (js/cc.view.getDesignResolutionSize)]
    (cx/debug* "DesignResolution, = [" (oget-width rs) ", " (oget-height rs) "]")
    (cx/debug* "loaded and running. OK")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hook the start
(set! js/cc.game.____bootloader boot-loader)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


