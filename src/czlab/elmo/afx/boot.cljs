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
(defn- handleMultiDevices* "" [xcfg]
  (let [searchPaths (js/jsb.fileUtils.getSearchPaths)
        flat? (get-in @xcfg [:game :landscape?])
        pcy (get-in @xcfg [:resolution :policy])
        fsz (cx/screenSize)]


  // need to prefix "assets" for andriod
  if (fsz.width >= 2048 || fsz.height >= 2048) {
    ps = ['assets/res/hdr', 'res/hdr'];
    xcfg.resolution.resDir = 'hdr';
    ccsx.setdr(landscape, 2048, 1536, pcy);
  }
  else
  if (fsz.width >= 1136 || fsz.height >= 1136) {
    ps = ['assets/res/hds', 'res/hds'];
    xcfg.resolution.resDir= 'hds';
    ccsx.setdr(landscape, 1136, 640, pcy);
  }
  else
  if (fsz.width >= 1024 || fsz.height >= 1024) {
    ps = ['assets/res/hds', 'res/hds'];
    xcfg.resolution.resDir= 'hds';
    ccsx.setdr(landscape, 1024, 768, pcy);
  }
  else
  if (fsz.width >= 960 || fsz.height >= 960) {
    ps = ['assets/res/hds', 'res/hds'];
    xcfg.resolution.resDir= 'hds';
    ccsx.setdr(landscape, 960, 640, pcy);
  }
  else {
    ps = ['assets/res/sd', 'res/sd'];
    xcfg.resolution.resDir= 'sd';
    ccsx.setdr(landscape, 480, 320, pcy);
  }

  ps= ps.concat(['assets/src', 'src']);

  for (let n=0; n < ps.length; ++n) {
    searchPaths.push(ps[n]);
  }

  sjs.loggr.info("Resource search paths: " + searchPaths);
  return searchPaths;
}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- handleMultiDevices
  "Sort out what resolution to use for this device." [xcfg]
  (let [pcy (get-in @xcfg [:resolution :policy])
        fh (get-in @xcfg [:handleDevices])
        fsz (cx/screenSize)]
    ;;device window size or canvas size.
    (cx/info* "view.frameSize = ["
              (oget-width fsz) ", " (oget-height fsz) "]")
    ;;if handler provided, call it and go.
    (if (fn? fh) (fh) (handleMultiDevices* xcfg))))

//////////////////////////////////////////////////////////////////////////////
let pvLoadSound = (sh, xcfg, k,v) => {
  return sh.sanitizeUrl( v + '.' + xcfg.game.sfx );
}
let pvLoadSprite = (sh, xcfg, k, v) => {
  return sh.sanitizeUrl(v[0]);
}
let pvLoadImage = (sh, xcfg, k,v) => {
  return sh.sanitizeUrl(v);
}
let pvLoadTile = (sh, xcfg, k,v) => {
  return sh.sanitizeUrl(v);
}
let pvLoadAtlas = (sh, xcfg, k,v) => {
  return [sh.sanitizeUrl( v + '.plist'),
          sh.sanitizeUrl( v + '.png') ];
}

//////////////////////////////////////////////////////////////////////////////
//
let pvLoadLevels = (sjs, sh, xcfg) => {
  let rc = [],
  f1= (k) => {
    return (v, n) => {
      const a = sjs.reduceObj( (memo, item, key) => {
        const z= [k, n, key].join('.');
        switch (n) {
          case 'sprites':
            memo.push( pvLoadSprite( sh, xcfg, z, item));
            xcfg.assets.sprites[z] = item;
          break;
          case 'images':
            memo.push( pvLoadImage( sh, xcfg, z, item));
            xcfg.assets.images[z] = item;
          break;
          case 'tiles':
            memo.push( pvLoadTile(sh, xcfg,  z, item));
            xcfg.assets.tiles[z] = item;
          break;
        }
        return memo;
      }, [], v);
      rc = rc.concat(a);
    };
  };

  sjs.eachObj((v,k) => { sjs.eachObj(f1(k), v); }, xcfg.levels);
  return rc;
}

/////////////////////////////////////////////////////////////////////////////
let pvGatherPreloads = (sjs, sh, xcfg) => {
  let assets= xcfg.assets,
  p,
  rc= [

    R.values(R.mapObjIndexed((v,k) => {
      return pvLoadSprite(sh,xcfg,k,v);
    }, assets.sprites)),

    R.values(R.mapObjIndexed((v,k) => {
      return pvLoadImage(sh,xcfg,k,v);
    }, assets.images)),

    R.values(R.mapObjIndexed((v,k) => {
      return pvLoadSound(sh,xcfg,k,v);
    }, assets.sounds)),

    sjs.reduceObj((memo, v,k) => {
      // value is array of [ path, image , xml ]
      p= sh.sanitizeUrl(v[0]);
      return memo.concat([p+'/'+v[1], p+'/'+v[2]]);
    }, [], assets.fonts),

    sjs.reduceObj((memo, v,k) => {
      return memo.concat( pvLoadAtlas(sh, xcfg, k,v));
    }, [], assets.atlases),

    R.values(R.mapObjIndexed((v,k) => {
      return pvLoadTile(sh, xcfg, k,v);
    }, assets.tiles)),

    xcfg.game.preloadLevels ? pvLoadLevels(sjs, sh, xcfg) : []
  ];

  return R.reduce((memo,v) => {
    sjs.loggr.info('Loading ' + v);
    memo.push( v );
    return memo;
  }, [], R.flatten(rc));
}

/////////////////////////////////////////////////////////////////////////////
/**
 * @class MyLoaderScene
 */
const MyLoaderScene = cc.Scene.extend(/** @lends MyLoaderScene# */{

  init() { return true; },

  _startLoading() {
    const res = this.resources,
    self=this;

    self.unschedule(self._startLoading);
    cc.loader.load(res,
                   (result, count, loadedCount) => {},
                   () => {
                     if (sjs.isfunc(self.cb)) {
                       self.cb();
                     }
                   });
  },

  initWithResources(resources, cb) {
    this.resources = resources || [];
    this.cb = cb;
  },

  onEnter() {
    const self = this;
    cc.Node.prototype.onEnter.call(self);
    self.schedule(self._startLoading, 0.3);
  },

  onExit() {
    cc.Node.prototype.onExit.call(this);
  }

});

//////////////////////////////////////////////////////////////////////////////
let preLaunchApp = (sjs, sh, xcfg, ldr,  ss1) => {
  let fz= ccsx.screen(),
  paths,
  sz,
  pfx,
  rs, pcy;

  if (cc.sys.isNative) {
    paths= handleMultiDevices();
    if (!!paths) {
      jsb.fileUtils.setSearchPaths(paths);
    }
  } else {
    sz= xcfg.game[xcfg.resolution.resDir];
    pcy = xcfg.resolution.web;
    cc.view.setDesignResolutionSize(sz.width, sz.height, pcy);
  }

  rs= cc.view.getDesignResolutionSize();
  xcfg.handleResolution(rs);
  sjs.loggr.info('DesignResolution, = [' +
                 rs.width + ", " +
                 rs.height + "]" +
                 ", scale = " + xcfg.game.scale);

  cc.director.setProjection(cc.Director.PROJECTION_2D);
  if (cc.sys.isNative) {
    pfx= "";
  } else {
    cc.view.resizeWithBrowserSize(true);
    cc.view.adjustViewPort(true);
    pfx = "/public/ig/res/";
  }

  //cc.director.setAnimationInterval(1 / sh.xcfg.game.frameRate);
  if (xcfg.game.debug) {
    cc.director.setDisplayStats(xcfg.game.showFPS);
  }

  rs= [ pfx + 'cocos2d/pics/preloader_bar.png',
        pfx + 'cocos2d/pics/ZotohLab.png' ];
  // hack to suppress the showing of cocos2d's logo
  cc.loaderScene = new MyLoaderScene();
  cc.loaderScene.init();
  cc.loaderScene.initWithResources(rs, () => {
    ldr.preload(pvGatherPreloads(sjs, sh, xcfg), () => {
      xcfg.runOnce();
      cc.director.runScene( sh.protos[ss1].reify() );
    });
  });
  cc.director.runScene(cc.loaderScene);
}

sjs.loggr.info("About to create Cocos2D HTML5 Game");

preLaunchApp(sjs, sh, xcfg, loader, ss1);
sh.l10nInit(),
sh.sfxInit();

//sjs.merge(me.xcfg.game, global.document.ccConfig);
sjs.loggr.debug(sjs.jsonfy(xcfg.game));
sjs.loggr.info("Registered game start state - " + ss1);
sjs.loggr.info("Loaded and running. OK");

/*@@
return xbox;
@@*/

//////////////////////////////////////////////////////////////////////////////
//EOF

