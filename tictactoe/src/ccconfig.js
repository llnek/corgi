//used by web debug mode only.
document.ccConfig = {

  loadExtension: false,
  chipmunk: true,
  box2d: false,
  showFPS: false,
  frameRate: 30,

  id: 'gameArea',

  renderMode: 0,
  debugMode: 1,

  engineDir: ['/public/extlibs/cocos2d-html5'],
  modules: [ 'cocos2d', 'ccui' ],

  jsList: [

    "/public/vendors/ramda/ramda.js",
    "/public/vendors/l10njs/l10n.js",
    "/public/vendors/mustache/mustache.js",
    "/public/vendors/helpers/dbg.js",

    "/public/vendors/js-signals/signals.js",
    "/public/vendors/ash-js/ash.js",

    "/public/vendors/rxjs/rx.all.js",

    "/public/vendors/cherimoia/skaro.js",
    "/public/vendors/cherimoia/caesar.js",
    "/public/vendors/cherimoia/ebus.js",

    "/public/ig/lib/zotohlab/asx/asterix.js",
    "/public/ig/lib/zotohlab/asx/cfg.js",
    "/public/ig/lib/zotohlab/asx/ccsx.js",
    "/public/ig/lib/zotohlab/asx/odin.js",

    "/public/ig/lib/zotohlab/asx/scenes.js",
    "/public/ig/lib/zotohlab/asx/pool.js",
    "/public/ig/lib/zotohlab/asx/loader.js",

    '/public/ig/lib/zotohlab/asx/negamax.js',

    "/public/ig/lib/zotohlab/gui/msgbox.js",
    "/public/ig/lib/zotohlab/gui/ynbox.js",
    "/public/ig/lib/zotohlab/gui/online.js",

    '/public/ig/lib/game/tictactoe/p/config.js',
    '/public/ig/lib/game/tictactoe/i18n/l10n.js',

    '/public/ig/lib/game/tictactoe/n/board.js',
    '/public/ig/lib/game/tictactoe/n/cobjs.js',
    '/public/ig/lib/game/tictactoe/n/gnodes.js',

    '/public/ig/lib/game/tictactoe/s/factory.js',
    '/public/ig/lib/game/tictactoe/s/utils.js',
    '/public/ig/lib/game/tictactoe/s/resolve.js',
    '/public/ig/lib/game/tictactoe/s/stager.js',
    '/public/ig/lib/game/tictactoe/s/logic.js',
    '/public/ig/lib/game/tictactoe/s/motion.js',
    '/public/ig/lib/game/tictactoe/s/sysobjs.js',

    '/public/ig/lib/game/tictactoe/p/splash.js',
    '/public/ig/lib/game/tictactoe/p/mmenu.js',
    '/public/ig/lib/game/tictactoe/p/hud.js',
    '/public/ig/lib/game/tictactoe/p/game.js',
    '/public/ig/lib/game/tictactoe/p/protos.js',


    '/public/ig/lib/zotohlab/p/boot.js'
  ]

};

