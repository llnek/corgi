var cc = {
  rectIntersectsRect: function() {},
  size :function(){},
  p:function(){},
  rect:function(){},

  capabilities: {},

  winSize: { x : 0, y : 0 },
  sys: { isNative : {}, os : {} , OS_IOS : {},  isMobile : {},
                            browserType: {}, BROWSER_TYPE_BAIDU : {},
                            BROWSER_TYPE_WECHAT : {}
              },
  view: { enableRetina: function(){},
          enableAutoFullScreen:function(){},
          getVisibleSize:function(){},
          getFrameSize:function(){},
          getVisibleOrigin:function(){},
          adjustViewPort:function(){},
          setDesignResolutionSize:function(){},
          resizeWithBrowserSize:function(){} },
  ResolutionPolicy : { SHOW_ALL : {} },
  director : { getRunningScene: function(){},
               getWinSize:function(){},
               runScene:function(){} },

  eventManager: {
    addListener: function(){}
  },
  LoaderScene : { preload:function(){} }
};

cc.EventListener= function(){};
cc.EventListener.KEYBOARD={};
cc.EventListener.MOUSE={};
cc.EventListener.TOUCH_ALL_AT_ONCE={};
cc.EventListener.TOUCH_ONE_BY_ONE={};


cc.EventMouse=function(){};
cc.EventMouse.BUTTON_LEFT={};


cc.spriteFrameCache= {
  getSpriteFrame:function(){}
};

cc.Sprite=function(){};
cc.Sprite.prototype.attr=function(){};

cc.TransitionCrossFade= function() {};
cc.TransitionScene=function() {};

cc.DelayTimer= function() {};
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


