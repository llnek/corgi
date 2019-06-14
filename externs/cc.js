//this is funky since goog optimizes some code and names a var or
//function as gl.  and this gl will shadow the window.gl that
//cocos2dx internally creates.  so need to do this so goog won't
//touch the name gl
var gl={};
var cp={
  BoxShape: function(){},
  Space: function(){},
  Body: function(){},
  v: function(){},
  momentForBox: function(){}
};
var cc = {
  rectIntersectsRect: function() {},
  rectContainsPoint: function() {},
  rectContainsRect: function() {},
  rectGetMidX: function() {},
  rectGetMidY: function() {},

  size :function(){},
  color:function(){},
  p:function(){},
  rect:function(){},

  KEY: { left : {}, right : {}, down: {}, up : {}, a : {}, d : {}, s : {}, w : {} },

  loaderScene: {},

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
          getDesignResolutionSize:function(){},
          resizeWithBrowserSize:function(){} },
  ResolutionPolicy : { SHOW_ALL : {},
                       FIXED_HEIGHT: {} },
  director : { getRunningScene: function(){},
               setProjection:function(){},
               setAnimationInterval:function(){},
               getAnimationInterval:function(){},
               setDisplayStats:function(){},
               getWinSize:function(){},
               pushScene:function(){},
               popToRootScene:function(){},
               popScene:function(){},
               runScene:function(){} },

  eventManager: {
    resumeTarget: function(){},
    pauseTarget: function(){},
    addListener: function(){},
    removeListener: function(){}
  },
  LoaderScene : { preload:function(){} }
};

var jsb={
  fileUtils: {
    getSearchPaths:function(){},
    setSearchPaths:function(){}
  }
};

cc.color.WHITE={};
cc.color.RED={};

cc.Node=function(){};
cc.Node.prototype.onEnter=function(){};
cc.Node.prototype.onExit=function(){};
cc.Node.prototype.setColor=function(){};
cc.Node.prototype.setPosition=function(){};
cc.Node.prototype.setVisible=function(){};
cc.Node.prototype.setScale=function(){};
cc.Node.prototype.addChild=function(){};
cc.Node.prototype.setAnchorPoint=function(){};

cc.DrawNode=function(){};
cc.Sequence=function(){};
cc.Sequence.create=function(){};
cc.FadeOut=function(){};
cc.FadeOut.create=function(){};
cc.CallFunc=function(){};
cc.CallFunc.create=function(){};

cc.MenuItemFont=function(){};
cc.MenuItemFont.setFontName=function() {};
cc.MenuItemFont.setFontSize=function(){};


cc.Director=function(){};
cc.Director.PROJECTION_2D={};

cc.loader={
  load:function(){}
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

cc.ProgressTimer=function(){};
cc.ProgressTimer.TYPE_BAR={};
cc.LayerColor=function(){};

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
  getSpriteFrame:function(){},
  addSpriteFrames:function(){}
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
  ____configurator:function(){},
  ____bootstrap:function(){},
  onStart:function() {},
  run:function(){}
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


