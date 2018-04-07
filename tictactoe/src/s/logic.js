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
 * @requires n/gnodes
 * @module s/logic
 */

import sh from 'zotohlab/asx/asterix';
import ccsx from 'zotohlab/asx/ccsx';
import odin from 'zotohlab/asx/odin';
import gnodes from 'n/gnodes';

//////////////////////////////////////////////////////////////////////////////
let evts= odin.Events,
sjs= sh.skarojs,
xcfg= sh.xcfg,
csts= xcfg.csts,
undef,
//////////////////////////////////////////////////////////////////////////////
/** * @class Logic */
Logic = sh.Ashley.sysDef({
  /**
   * @memberof module:s/logic~Logic
   * @method constructor
   * @param {Object} options
   */
  constructor(options) {
    this.state= options;
    this.botTimer=null;
  },
  /**
   * @memberof module:s/logic~Logic
   * @method removeFromEngine
   * @param {Ash.Engine} engine
   */
  removeFromEngine(engine) {
    this.board=null;
  },
  /**
   * @memberof module:s/logic~Logic
   * @method addToEngine
   * @param {Ash.Engine} engine
   */
  addToEngine(engine) {
    this.board = engine.getNodeList(gnodes.BoardNode);
  },
  /**
   * @memberof module:s/logic~Logic
   * @method update
   * @param {Number} dt
   */
  update(dt) {
    const node= this.board.head;
    if (this.state.running &&
        !!node) {
      this.doit(node, dt);
    }
  },
  /**
   * @method doit
   * @private
   */
  doit(node, evt) {
    const ps= this.state.players,
    cp= ps[this.state.actor],
    board= node.board,
    grid= node.grid,
    bot= node.robot,
    sel= node.selection;

    //handle online play
    if (sjs.isobj(this.state.wsock)) {
      //if the mouse click is from the valid user, handle it
      if (!!cp && (this.state.pnum === cp.pnum)) {
        this.enqueue(sel.cell,cp.value,grid);
      }
    }
    else
    if (cp.category === csts.BOT) {
      // for the bot, create some small delay...
      if (!!this.botTimer) {
        if (ccsx.timerDone(this.botTimer)) {
          let bd= bot.algo.getGameBoard(),
          rc;
          bd.syncState(grid.values, this.state.players[this.state.actor].value);
          rc= bd.getFirstMove();
          if (!sjs.echt(rc)) { rc = bot.algo.eval(); }
          this.enqueue(rc,cp.value,grid);
          this.botTimer=ccsx.undoTimer(this.botTimer);
        }
      } else {
        this.botTimer = ccsx.createTimer(sh.main, 0.6);
      }
    }
    else
    if (sel.cell >= 0) {
      //possibly a valid click ? handle it
      this.enqueue(sel.cell,cp.value,grid);
    }

    sel.cell= -1;
  },
  /**
   * @method enqueue
   * @private
   */
  enqueue(pos, value, grid) {

    if ((pos >= 0 && pos < grid.values.length) &&
        csts.CV_Z === grid.values[pos]) {

      let snd, pnum;

      sh.fire('/hud/timer/hide');

      if (sjs.isobj(this.state.wsock)) {
        this.onEnqueue(grid,this.state.actor,pos);
      } else {
        if (this.state.actor === 1) {
          snd= 'x_pick';
          pnum = 2;
        } else {
          snd= 'o_pick';
          pnum = 1;
        }
        grid.values[pos] = value;
        this.state.actor = pnum;
        sh.sfxPlay(snd);
        if (this.state.players[pnum].category === csts.HUMAN) {
          sh.fire('/hud/timer/show');
        }
      }
    }
  },
  /**
   * @method onEnqueue
   * @private
   */
  onEnqueue(grid,pnum,cell) {
    const src= {
      color: this.state.players[pnum].color,
      value: this.state.players[pnum].value,
      grid: grid.values,
      cell: cell
    },
    snd = pnum===1 ? 'x_pick' : 'o_pick',
    evt= {
      source: sjs.jsonfy(src),
      type: evts.MSG_SESSION,
      code: evts.PLAY_MOVE
    };
    this.state.wsock.send(evt);
    this.state.actor=0;
    sh.sfxPlay(snd);
  }
},{
/**
 * @memberof module:s/logic~Logic
 * @property {Number} Priority
 */
Priority : xcfg.ftypes.Logic
});


/** @alias module:s/logic */
const xbox = /** @lends xbox# */{
  /**
   * @property {Logic} Logic
   */
  Logic : Logic
};
sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF

