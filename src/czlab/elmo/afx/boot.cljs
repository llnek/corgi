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

  (:require-macros [czlab.elmo.afx.core :as ec :refer [f#* do-with]]
                   [czlab.elmo.afx.ccsx
                    :as cx :refer [oget-width oget-height
                                   not-native? native? attr*]])
  (:require [czlab.elmo.afx.ccsx :as cx :refer [*xcfg*]]
            [czlab.elmo.afx.core :as ec]
            [oops.core :refer [oget oset! ocall oapply ocall! oapply!]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- handleMultiDevices "" []
  (let [{:keys [policy landscape?]} (:game @*xcfg*)
        {:keys [width height]} (cx/screenBox)
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
    (apply cx/setDevRes!
           (conj (if landscape? [X Y] [Y X]) policy))
    (swap! *xcfg* #(assoc-in % [:game :resDir] dir))
    ;;device window size or canvas size.
    (cx/info* "view.frameSize = [" width ", " height "]")
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
    (let [func (f#* (js/cc.loader.load
                      (clj->js (cx/gldr->imgs))
                      (constantly nil) (cx/preloader)))]
    (attr* scene
           #js{:init #(.call js/cc.Scene.prototype.init scene)
               :onEnter #(do (.call js/cc.Node.prototype.onEnter scene)
                             (ocall scene "scheduleOnce" func 0.3))
               :onExit #(.call js/cc.Node.prototype.onExit scene)}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
          (cx/setDevRes! (:width size) (:height size) policy)))
    (if (number? frameRate)
      (js/cc.director.setAnimationInterval (/ 1 frameRate)))
    (if debug?
      (js/cc.director.setDisplayStats true))
    (swap! *xcfg*
           #(assoc-in % [:game :vert?] (cx/isPortrait?)))
    ;;hack to suppress the showing of cocos2d's logo
    ;;and instead, show our own!
    (cx/run* (preloadLogoScene))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(set! js/cc.game.startElmo
      (f#*
        (cx/info* "game.start called")
        (swap! *xcfg*
               #(ec/deepMerge % (js/cc.game.configElmo)))
        (cx/info* (js/JSON.stringify
                    (-> (deref *xcfg*)
                        (dissoc :l10nTable) (clj->js))))
        (preLaunch)
        (cx/l10nInit)
        (cx/sfxInit)
        (let [rs (js/cc.view.getDesignResolutionSize)]
          (cx/info* "DesignResolution, = ["
                    (oget-width rs) ", " (oget-height rs) "]")
          (cx/info* "loaded and running. OK"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


