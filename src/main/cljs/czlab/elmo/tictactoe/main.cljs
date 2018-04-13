(ns czlab.elmo.tictactoe.main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn onStart []
  (this-as this
           (let [sys cc.sys]
             (if (and (not sys.isNative)
                      (.getElementById document "cocosLoading")) ;;If referenced loading.js, please remove it
               (.removeChild document.body (.getElementById document "cocosLoading")))

             ;; Pass true to enable retina display, on Android disabled by default to improve performance
             (.enableRetina cc.view (= sys.os sys.OS_IOS))

             ;; Disable auto full screen on baidu and wechat, you might also want to eliminate sys.BROWSER_TYPE_MOBILE_QQ
             (if (and sys.isMobile
                      (not= sys.browserType sys.BROWSER_TYPE_BAIDU)
                      (not= sys.browserType sys.BROWSER_TYPE_WECHAT))
               (.enableAutoFullScreen cc.view true))

             ;; Adjust viewport meta
             (.adjustViewPort cc.view true)

             ;; Uncomment the following line to set a fixed orientation for your game
             ;; cc.view.setOrientation(cc.ORIENTATION_PORTRAIT);

             ;; Setup the resolution policy and design resolution size
             (.setDesignResolutionSize cc.view 960 640 cc.ResolutionPolicy.SHOW_ALL)

             ;; The game will be resized when browser size change
             (.resizeWithBrowserSize cc.view true)

             ;;load resources
             (.preload cc.LoaderScene
                       g_resources #(.runScene cc.director (new HelloWorldScene)) this))))

(set! cc.game.onStart onStart)

(.run cc.game)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


