(ns czlab.elmo.tictactoe.app
  (:require [czlab.elmo.tictactoe.resource :refer [g_resources res]]))

(js/console.log "ttt.app loaded")

(def HelloWorldLayer
  (js/cc.Layer.extend
    #js {
      :sprite nil
      :ctor
      #(this-as this
                 (let [_ (._super this)
                       size js/cc.winSize
                       helloLabel (new js/cc.LabelTTF "Hello World" "Arial" 38)]
                   (set! helloLabel.x (/ size.width 2))
                   (set! helloLabel.y (+ 200 (/ size.height 2)))
                   (.addChild this helloLabel 5)
                   (set! this.sprite (new js/cc.Sprite (:HelloWorld_png res)))
                   (.attr this.sprite
                          #js {:x (/ size.width 2)
                               :y (/ size.height  2) })
                   (.addChild this this.sprite 0)
                   this)) }))

(def HelloWorldScene
  (js/cc.Scene.extend
    #js {:onEnter
         #(this-as this
                 (let [_ (._super this)]
                   (.addChild this (new HelloWorldLayer)))) }))

(set! js/cc.game.onStartFunc
        #(this-as this
                 (let [sys js/cc.sys]
                   (js/console.log "game.onStart called")
                   (if (and (not sys.isNative)
                            (js/document.getElementById "cocosLoading"))
                     (js/document.body.removeChild (js/document.getElementById "cocosLoading")))
                   (js/cc.view.enableRetina (= sys.os sys.OS_IOS))
                   (if (and sys.isMobile
                            (not= sys.browserType sys.BROWSER_TYPE_BAIDU)
                            (not= sys.browserType sys.BROWSER_TYPE_WECHAT))
                     (js/cc.view.enableAutoFullScreen true))
                   (js/cc.view.adjustViewPort true)
                   (js/cc.view.setDesignResolutionSize 960 640 js/cc.ResolutionPolicy.SHOW_ALL)
                   (js/cc.view.resizeWithBrowserSize true)
                   (js/cc.LoaderScene.preload g_resources
                                              (fn [] (js/cc.director.runScene (new HelloWorldScene)))))))



