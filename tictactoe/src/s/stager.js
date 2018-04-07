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
 * @requires zotohlab/asx/ccsx
 * @requires zotohlab/asx/odin
 * @requires s/utils
 * @requires n/gnodes
 * @module s/stager
 */

import sh from 'zotohlab/asx/asterix';
import ccsx from 'zotohlab/asx/ccsx';
import odin from 'zotohlab/asx/odin';
import utils from 's/utils';
import gnodes from 'n/gnodes';

//////////////////////////////////////////////////////////////////////////////
let evts= odin.Events,
sjs= sh.skarojs,
xcfg=sh.xcfg,
csts= xcfg.csts,
R=sjs.ramda,
undef,
//////////////////////////////////////////////////////////////////////////////
/** * @class Stager */
Stager = sh.Ashley.sysDef({
  /**
   * @memberof module:s/stager~Stager
   * @method constructor
   * @param {Object} options
   */
  constructor(options) {
    this.state= options;
    this.inited=false;
  },
  /**
   * @memberof module:s/stager~Stager
   * @method removeFromEngine
   * @param {Ash.Engine} engine
   */
  removeFromEngine(engine) {
    this.board=null;
  },
  /**
   * @memberof module:s/stager~Stager
   * @method addToEngine
   * @param {Ash.Engine} engine
   */
  addToEngine(engine) {
    engine.addEntity(sh.factory.reifyBoard(sh.main,
                                           this.state));
    this.board= engine.getNodeList(gnodes.BoardNode);
  },
  /**
   * @memberof module:s/stager~Stager
   * @method update
   * @param {Number} dt
   */
  update(dt) {
    if (ccsx.isTransitioning()) { return false; }
    const node= this.board.head;
    if (this.state.running &&
        !!node) {
      if (! this.inited) {
        this.onceOnly(node, dt);
        this.inited=true;
      } else {
        this.doit(node,dt);
      }
    }
  },
  /**
   * @method showGrid
   * @private
   */
  showGrid(node) {
    let mgs = utils.mapGridPos(),
    cs=node.view.cells,
    pos=0,
    sp;

    R.forEach( mp => {
      sp= ccsx.createSprite('z.png');
      sp.setPosition(ccsx.vboxMID(mp));
      sh.main.addAtlasItem('game-pics',sp);
      cs[pos++]=[sp, sp.getPositionX(), sp.getPositionY(), csts.CV_Z];
    }, mgs);
  },

  /**
   * @method onceOnly
   * @private
   */
  onceOnly(node,dt) {

    this.showGrid(node);

    if (sjs.isobj(this.state.wsock)) {
      // online play
      sjs.loggr.debug("reply to server: session started ok");
      this.state.wsock.send({
        type: evts.MSG_SESSION,
        code: evts.STARTED
      });
      this.state.actor= 0;
    } else {
      //randomly pick a player to start the game.
      let pnum = sjs.randSign() > 0 ? 1 : 2;
      this.state.actor=pnum;
      if (this.state.players[pnum].category === csts.HUMAN) {
        sh.fire('/hud/timer/show');
      }
      else
      if (this.state.players[pnum].category === csts.BOT) {
      }
    }

    sh.main.pkInput();
  },
  /**
   * @method doit
   * @private
   */
  doit(node,dt) {

    let active = this.state.running,
    actor = this.state.actor;

    if (!active) {
      actor= this.state.lastWinner;
    }

    sh.fire('/hud/update', {
      running: active,
      pnum: actor
    });
  }

}, {
/**
 * @memberof module:s/stager~Stager
 * @property {Number} Priority
 */
Priority: xcfg.ftypes.PreUpdate
});


/** @alias module:s/stager */
const xbox = {
  /**
   * @property {Stager} Stager
   */
  Stager : Stager
};
sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF


