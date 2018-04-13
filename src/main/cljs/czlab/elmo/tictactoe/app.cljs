(ns ^{:doc ""
      :author "Kenneth Leung"}
  czlab.elmo.tictactoe.app)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def HelloWorldLayer
  (cc.Layer.extend
    (js-obj "sprite" nil
            "ctor" (fn []
                     (this-as this
                              (let [_ (._super this)
                                    size  cc.winSize
                                    helloLabel (new cc.LabelTTF "Hello World" "Arial" 38)]
                                (set! helloLabel.x  (/ size.width 2))
                                (set! helloLabel.y (+ 200 (/ size.height 2)))
                                (.addChild this helloLabel 5)
                                (set! this.sprite (new cc.Sprite res.HelloWorld_png))
                                (.attr this.sprite (js-obj "x" (/ size.width 2)
                                                  "y" (/ size.height 2)))
                                (.addChild this this.sprite 0)
                                true))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def HelloWorldScene
  (cc.Scene.extend
    (js-obj "onEnter" (fn []
                        (this-as this
                                 (let [_ (._super this)]
                                   (.addChild this (new HelloWorldLayer))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


