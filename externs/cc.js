var cc = {
  rectIntersectsRect: function() {},
  size :function(){},
  p:function(){},
  rect:function(){},

  capabilities: {},

  winSize: { x : 0, y : 0 },
  sys: { isNative : {}, os : {} , OS_IOS : {},  isMobile : {},
         language : {},
         browserType: {}, BROWSER_TYPE_BAIDU : {}, BROWSER_TYPE_WECHAT : {} },
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
               pushScene:function(){},
               popToRootScene:function(){},
               popScene:function(){},
               runScene:function(){} },

  eventManager: {
    addListener: function(){}
  },
  LoaderScene : { preload:function(){} }
};

cc.Node= {
  setColor:function(){},
  setPosition:function(){},
  setVisible:function(){},
  setScale:function(){},
  addChild:function(){},
  setAnchorPoint:function(){}
};

cc.audioEngine= {
  setMusicVolume:function(){},
  stopAllEffects:function(){},
  stopMusic:function(){},
  playMusic:function(){},
  playEffect:function(){}
};

cc.MenuItemToggle=function() {};
cc.MenuItemSprite=function() {};
cc.MenuItemLabel=function() {};
cc.MenuItemLabel.prototype.setColor=function(){};
cc.LabelBMFont=function(){};
cc.LabelBMFont.prototype.setColor=function(){};

cc.Menu=function(){};
cc.Menu.prototype.alignItemsHorizontally=function(){};
cc.Menu.prototype.alignItemsVertically=function(){};
cc.Menu.prototype.addChild=function(){};
cc.Menu.prototype.alignItemsHorizontallyWithPadding=function(){};
cc.Menu.prototype.alignItemsVerticallyWithPadding=function(){};


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

cc.SpriteBatchNode=function(){};
cc.textureCache={
  addImage:function(){}
};

cc.game={
  onStartFunc :function(){},
  run :function(){}
};

var Cookies= {
  set:function(){},
  get:function(){}
};

var Mustache= {
  render:function(){}
};

var LZString= {
  locale: {},
  defaultLocale: {},
  toLocaleString:function(){} };


