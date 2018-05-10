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
 * @requires Rx
 * @module s/motion
 */

import sh from 'zotohlab/asx/asterix';
import ccsx from 'zotohlab/asx/ccsx';
import odin from 'zotohlab/asx/odin';
import gnodes from 'n/gnodes';
import Rx from 'Rx';

//////////////////////////////////////////////////////////////////////////////
let evts= odin.Events,
sjs= sh.skarojs,
xcfg= sh.xcfg,
csts = xcfg.csts,
undef,
//////////////////////////////////////////////////////////////////////////////
/** * @class Motions */
Motions = sh.Ashley.sysDef({
  /**
   * @memberof module:s/motion~Motions
   * @method constructor
   * @param {Object} options
   */
  constructor(options) {
    this.state= options;
    this.inited=false;
  },
  /**
   * @memberof module:s/motion~Motions
   * @method removefromEngine
   * @param {Ash.Engine} engine
   */
  removeFromEngine(engine) {
    this.netplay=null;
    this.stream=null;
    this.evQ=null;
    this.gui=null;
  },
  /**
   * @memberof module:s/motion~Motions
   * @method addToEngine
   * @param {Ash.Engine} engine
   */
  addToEngine(engine) {
    this.netplay = engine.getNodeList(gnodes.NetPlayNode);
    this.gui = engine.getNodeList(gnodes.GUINode);
    this.evQ=[];
  },
  /**
   * @method onceOnly
   * @private
   */
  onceOnly() {
    let ws= this.state.wsock,
    t, m, s;
    if (sjs.isobj(ws)) {
      ws.cancelAll();
      s=Rx.Observable.create( obj => {
        ws.listenAll( msg => {
          obj.onNext({group:'net',
                      event: msg});
        });
      });
    } else {
      s= Rx.Observable.never();
    }
    t=Rx.Observable.create( obj => {
      sh.main.signal('/touch/one/end',
                     msg => obj.onNext(msg));
    });
    m=Rx.Observable.create( obj => {
      sh.main.signal('/mouse/up',
                     msg => obj.onNext(msg));
    });
    this.stream= Rx.Observable.merge(m,t,s);
    this.stream.subscribe( msg => {
      if (!!this.evQ) {
        this.evQ.push(msg);
      }
    });
  },
  /**
   * @memberof module:s/motion~Motions
   * @method update
   * @param {Number} dt
   */
  update(dt) {
    const evt= this.evQ.length > 0 ? this.evQ.shift() : undef,
    n= this.netplay.head,
    g= this.gui.head;

    if (!this.inited) {
      this.onceOnly();
      this.inited=true;
    }
    else if (!!evt) {
      if (evt.group === 'net') {
        if (!!n) { this.onnet(n, evt.event); }
      } else {
        if (!!g) { this.ongui(g,evt); }
      }
    }
  },
  /**
   * @method onnet
   * @private
   */
  onnet(node, evt) {
    switch (evt.type) {
      case evts.MSG_NETWORK:
        this.onnetw(node, evt);
      break;
      case evts.MSG_SESSION:
        this.onsess(node, evt);
      break;
    }
  },
  /**
   * @method onnetw
   * @private
   */
  onnetw(node, evt) {
    switch (evt.code) {
      case evts.RESTART:
        sjs.loggr.debug("restarting a new game...");
        sh.fire('/net/restart');
      break;
      case evts.STOP:
        if (this.state.running) {
          sjs.loggr.debug("game will stop");
          sh.fire('/hud/timer/hide');
          this.onsess(node,evt);
          sh.fire('/net/stop', evt);
        }
      break;
    }
  },
  /**
   * @method onsess
   * @private
   */
  onsess(node, evt) {
    let cmd= evt.source.cmd,
    snd, pnum,
    grid=node.grid,
    vs=grid.values;

    if (sjs.isobj(cmd) &&
        sjs.isnum(cmd.cell) &&
        cmd.cell >= 0 &&
        cmd.cell < vs.length) {

      if (this.state.players[1].value === cmd.value) {
        snd= 'x_pick';
      } else {
        snd= 'o_pick';
      }
      vs[cmd.cell] = cmd.value;
      sh.sfxPlay(snd);
    }

    pnum= sjs.isnum(evt.source.pnum) ? evt.source.pnum : -1;
    if (pnum === 1 || pnum === 2) {} else { return; }
    switch (evt.code) {
      case evts.POKE_MOVE:
        sjs.loggr.debug("player " + pnum + ": my turn to move");
        sh.fire('/hud/timer/show');
        this.state.actor= pnum;
      break;
      case evts.POKE_WAIT:
        sjs.loggr.debug("player " + pnum + ": my turn to wait");
        sh.fire('/hud/timer/hide');
        // toggle color
        this.state.actor= pnum===1 ? 2 : 1;
      break;
    }
  },
  /**
   * @method ongui
   * @private
   */
  ongui(node, evt) {
    if (!this.state.running) {return;}
    let sel = node.selection,
    map = node.view.gridMap,
    rect,
    sz= map.length;

    //set the mouse/touch position
    sel.px = evt.loc.x;
    sel.py = evt.loc.y;
    sel.cell= -1;

    if (this.state.actor === 0) {
      return;
    }

    //which cell did he click on?
    for (let n=0; n < sz; ++n) {
      rect = map[n];
      if (sel.px >= rect.left && sel.px <= rect.right &&
          sel.py >= rect.bottom && sel.py <= rect.top) {
        sel.cell= n;
        break;
      }
    }
  }

}, {
/**
 * @memberof module:s/motion~Motions
 * @property {Number} Priority
 */
Priority: xcfg.ftypes.Motion
});


/** @alias module:s/motion */
const xbox = /** @lends xbox# */{
  /**
   * @property {Motions} Motions
   */
  Motions: Motions
};
sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF

