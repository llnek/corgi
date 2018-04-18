var cc = {
  winSize: { x : 0, y : 0 },
  sys: { isNative : {}, os : {} , OS_IOS : {},  isMobile : {},
                            browserType: {}, BROWSER_TYPE_BAIDU : {},
                            BROWSER_TYPE_WECHAT : {}
              },
  view: { enableRetina: function(){},
             enableAutoFullScreen:function(){},
             adjustViewPort:function(){},
             setDesignResolutionSize:function(){},
             resizeWithBrowserSize:function(){} },
  ResolutionPolicy : { SHOW_ALL : {} },
  director : { runScene:function(){} },
  LoaderScene : { preload:function(){} }
};

cc.Sprite=function(){};
cc.Sprite.prototype.attr=function(){};

cc.LabelTTF= function() {};

cc.Layer=function(){};
cc.Layer.prototype.extend=function() {};
cc.Layer.prototype.addChild=function(){};
cc.Layer.prototype._super=function(){};
cc.Layer.prototype.sprite=cc.Sprite;

cc.Scene=function(){};
cc.Scene.prototype.extend=function() {};
cc.Scene.prototype.addChild=function(){};
cc.Scene.prototype._super=function(){};


cc.game={
  onStartFunc :function(){},
  run :function(){}
};


