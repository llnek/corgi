// This library is distributed in  the hope that it will be useful but without
// any  warranty; without  even  the  implied  warranty of  merchantability or
// fitness for a particular purpose.
// The use and distribution terms for this software are covered by the Eclipse
// Public License 1.0  (http://opensource.org/licenses/eclipse-1.0.php)  which
// can be found in the file epl-v10.html at the root of this distribution.
// By using this software in any  fashion, you are agreeing to be bound by the
// terms of this license. You  must not remove this notice, or any other, from
// this software.
// Copyright (c) 2013-2015, Ken Leung. All rights reserved.

"use strict";/**
 * @requires zotohlab/asx/asterix
 * @requires zotohlab/asx/scenes
 * @requires zotohlab/asx/ccsx
 * @requires zotohlab/asx/odin
 * @requires p/hud
 * @requires n/cobjs
 * @requires s/sysobjs
 * @module p/game
 */

import scenes from 'zotohlab/asx/scenes';
import sh from 'zotohlab/asx/asterix';
import ccsx from 'zotohlab/asx/ccsx';
import odin from 'zotohlab/asx/odin';
import huds from 'p/hud';
import cobjs from 'n/cobjs';
import sobjs from 's/sysobjs';


//////////////////////////////////////////////////////////////////////////
let evts= odin.Events,
sjs= sh.skarojs,
xcfg = sh.xcfg,
csts= xcfg.csts,
R = sjs.ramda,
undef,
//////////////////////////////////////////////////////////////////////////
/** @class BackLayer */
BackLayer = scenes.XLayer.extend({
  setup() {
    this.centerImage(sh.getImage('game.bg'));
  },
  rtti() { return 'BackLayer'; }
}),
//////////////////////////////////////////////////////////////////////////////
/** * @class GameLayer */
GameLayer = scenes.XGameLayer.extend({
  /**
   * @method pkInput
   * @protected
   */
  pkInput() {
    ccsx.onTouchOne(this.ebus);
    ccsx.onMouse(this.ebus);
  },
  /**
   * @method replay
   * @private
   */
  replay() {
    if (sjs.isobj(this.options.wsock)) {
      // request server to restart a new game
      this.options.wsock.send({
        type: evts.MSG_SESSION,
        code: evts.REPLAY
      });
    } else {
      this.play(false);
    }
  },
  /**
   * @method play
   * @private
   */
  play(newFlag) {

    csts.CELLS = this.options.size*this.options.size;
    csts.GRID_SIZE= this.options.size;

    // sort out names of players
    let p1ids, p2ids;
    sjs.eachObj((v,k) => {
      if (v[0] === 1) {
        p1ids= [k, v[1] ];
      } else {
        p2ids= [k, v[1] ];
      }
    }, this.options.ppids);

    // clean slate
    this.reset(newFlag);
    this.initPlayers();

    this.initEngine( sobjs.systems, sobjs.entityFactory);
    this.getHUD().regoPlayers(csts.P1_COLOR, p1ids,
                              csts.P2_COLOR, p2ids);
    this.options.running=true;
    this.options.msgQ = [];
  },
  /**
   * @method onNewGame
   * @private
   */
  onNewGame(mode) {
    //sh.sfxPlay('start_game');
    this.setGameMode(mode);
    this.play(true);
  },
  /**
   * @method reset
   * @private
   */
  reset(newFlag) {
    if (!sjs.isempty(this.atlases)) {
      sjs.eachObj( v => { v.removeAllChildren(); }, this.atlases);
    } else {
      this.regoAtlas('game-pics');
      this.regoAtlas('lang-pics');
    }
    this.options.lastWinner=undef;
    if (newFlag) {
      this.getHUD().resetAsNew();
    } else {
      this.getHUD().reset();
    }
  },
  /**
   * @method updateHUD
   * @private
   */
  updateHUD() {
    if (this.options.running) {
      this.getHUD().drawStatus(this.actor);
    } else {
      this.getHUD().drawResult(this.lastWinner);
    }
  },
  /**
   * @method playTimeExpired
   * @private
   */
  playTimeExpired(msg) {
    this.options.msgQ.push("forfeit");
  },
  /**
   * @method initPlayers
   * @private
   */
  initPlayers() {
    let p2cat, p1cat,
    p2, p1;

    switch (this.options.mode) {
      case sh.gtypes.ONLINE_GAME:
        p2cat = csts.NETP;
        p1cat = csts.NETP;
      break;
      case sh.gtypes.P1_GAME:
        p1cat= csts.HUMAN;
        p2cat= csts.BOT;
      break;
      case sh.gtypes.P2_GAME:
        p2cat= csts.HUMAN;
        p1cat= csts.HUMAN;
      break;
    }
    p1= new cobjs.Player(p1cat, csts.CV_X, 1, csts.P1_COLOR);
    p2= new cobjs.Player(p2cat, csts.CV_O, 2, csts.P2_COLOR);
    this.options.players = [null,p1,p2];
    this.options.colors={};
    this.options.colors[csts.P1_COLOR] = p1;
    this.options.colors[csts.P2_COLOR] = p2;
  },
  /**
   * @method overAndDone
   * @private
   */
  overAndDone(winner) {
    this.getHUD().endGame(winner);
  }

});

/** @alias module:p/game */
const xbox = /** @lends xbox# */{
  /**
   * @property {String} rtti
   */
  rtti: sh.ptypes.game,
  /**
   * @method reify
   * @param {Object} options
   * @return {cc.Scene}
   */
  reify(options) {
    const scene = new scenes.XSceneFactory([
      BackLayer, GameLayer,
      huds.HUDLayer
    ]).reify(options);

    scene.onmsg('/hud/showmenu', msg => {
      scenes.showMenu();
    }).
    onmsg('/hud/replay', msg => {
      sh.main.replay();
    }).
    onmsg('/hud/timer/show', msg => {
      sh.main.getHUD().showTimer();
    }).
    onmsg('/net/restart', msg => {
      sh.main.getHUD().killTimer();
      sh.main.play(false);
    }).
    onmsg('/net/stop', msg => {
      sh.main.overAndDone(msg.status);
    }).
    onmsg('/hud/timer/hide', msg => {
      sh.main.getHUD().killTimer();
    }).
    onmsg('/hud/score/update', msg => {
      sh.main.getHUD().updateScore(msg.color, msg.score);
    }).
    onmsg('/hud/end', msg => {
      sh.main.overAndDone(msg.winner);
    }).
    onmsg('/hud/update', msg => {
      sh.main.getHUD().update(msg.running, msg.pnum);
    }).
    onmsg('/player/timer/expired', msg => {
      sh.main.playTimeExpired(msg);
    });

    return scene;
  }
};

sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF

