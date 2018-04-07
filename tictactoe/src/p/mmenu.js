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
 * @module p/mmenu
 */

import scenes from 'zotohlab/asx/scenes';
import sh from 'zotohlab/asx/asterix';
import ccsx from 'zotohlab/asx/ccsx';

//////////////////////////////////////////////////////////////////////////////
let sjs=sh.skarojs,
xcfg = sh.xcfg,
csts= xcfg.csts,
R=sjs.ramda,
undef,
SEED= {
  ppids: {},
  grid: [
    0,0,0,
    0,0,0,
    0,0,0
  ],
  size: 3,
  pnum: 1,
  mode: 0
},
//////////////////////////////////////////////////////////////////////////////
/** * @class MainMenuLayer */
MainMenuLayer = scenes.XMenuLayer.extend({
  /**
   * @method title
   * @private
   */
  title() {
    const wb=ccsx.vbox(),
    cw= ccsx.center(),
    tt=ccsx.bmfLabel({
      fontPath: sh.getFont('font.JellyBelly'),
      text: sh.l10n('%mmenu'),
      pos: cc.p(cw.x, wb.top * 0.9),
      color: cc.color('#F6B17F'),
      scale: xcfg.game.scale
    });
    this.addItem(tt);
  },
  /**
   * @method onnetplay
   * @private
   */
  onnetplay(msg) {
    const gl= sh.protos[sh.ptypes.game],
    ol= sh.protos[sh.ptypes.online],
    mm= sh.protos[sh.ptypes.mmenu];
    msg.onback= () => { ccsx.runScene( mm.reify()); };
    msg.yes= (wss,pnum,startmsg) => {
      const m= sjs.mergeEx(R.omit(['yes',
                                   'onback'], msg), {
        wsock: wss,
        pnum: pnum
      });
      sjs.merge(m, startmsg);
      ccsx.runScene( gl.reify(m));
    }
    ccsx.runScene(ol.reify(msg));
  },
  /**
   * @method onplay
   * @private
   */
  onplay(msg) {
    ccsx.runScene(sh.protos[sh.ptypes.game].reify(msg));
  },
  /**
   * @method setup
   * @protected
   */
  setup() {
    this.centerImage(sh.getImage('gui.mmenu.menu.bg'));
    this.incIndexZ();
    this.title();
    const color= cc.color('#5E3178'),
    cw = ccsx.center(),
    wb= ccsx.vbox(),
    me=this,
    p={},
    menu= ccsx.vmenu([
      { nnn: '#online.png',
        cb() {
          me.onnetplay(sjs.mergeEx(SEED,
                                   { mode: sh.gtypes.ONLINE_GAME}));
        }},
      { nnn: '#player2.png',
        cb() {
          p[ sh.l10n('%p1') ] = [ 1, sh.l10n('%player1') ];
          p[ sh.l10n('%p2') ] = [ 2, sh.l10n('%player2') ];
          me.onplay(sjs.mergeEx(SEED,
                                {ppids: p,
                                 mode: sh.gtypes.P2_GAME }));
        }},
      { nnn: '#player1.png',
        cb() {
          p[ sh.l10n('%cpu') ] = [ 2, sh.l10n('%computer') ];
          p[ sh.l10n('%p1') ] = [ 1,  sh.l10n('%player1') ];
          me.onplay(sjs.mergeEx(SEED,
                                {ppids: p,
                                 mode: sh.gtypes.P1_GAME }));
        }}
    ],
    { pos: cw });
    this.addItem(menu);

    this.mkBackQuit(false, [{
        nnn: '#icon_back.png',
        color: color,
        cb() {
          me.options.onback();
        }},
      { nnn: '#icon_quit.png',
        color: color,
        cb() { me.onQuit(); }
      }],
      (m,z) => {
        m.setPosition(wb.left + csts.TILE + z.width * 1.1,
                      wb.bottom + csts.TILE + z.height * 0.45);
    });

    this.mkAudio({
      pos: cc.p(wb.right - csts.TILE,
                wb.bottom + csts.TILE),
      color: color,
      anchor: ccsx.acs.BottomRight
    });
  },
  /**
   * @method ctor
   * @private
   */
  ctor(options) {
    this._super(options);
  }
});

/** @alias module:p/mmenu */
const xbox= /** @lends xbox# */{
  /**
   * @property {String} rtti
   */
  rtti: sh.ptypes.mmenu,
  /**
   * Create the Main Menu screen.
   * @method reify
   * @param {Object} options
   * @return {cc.Scene}
   */
  reify(options) {
    return new scenes.XSceneFactory([
      MainMenuLayer
    ]).reify(options);
  }
};

sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF

