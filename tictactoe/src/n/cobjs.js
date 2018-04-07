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
 * @requires zotohlab/asx/negamax
 * @requires zotohlab/asx/ccsx
 * @requires s/utils
 * @module n/cobjs
 */

import negax from 'zotohlab/asx/negamax';
import sh from 'zotohlab/asx/asterix';
import ccsx from 'zotohlab/asx/ccsx';
import utils from 's/utils';

//////////////////////////////////////////////////////////////////////////////
/** @alias module:n/cobjs */
let xbox= {},
sjs= sh.skarojs,
xcfg = sh.xcfg,
csts= xcfg.csts,
undef;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class SmartAlgo
 */
const SmartAlgo = sh.Ashley.casDef({
  /**
   * @memberof module:n/ccobjs~SmartAlgo
   * @method constructor
   * @param {GameBoard} board
   */
  constructor(board) {
    this.algo= new negax.NegaMax(board);
  }
});
/**
 * @property {SmartAlgo} SmartAlgo
 */
xbox.SmartAlgo = SmartAlgo;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class Board
 */
const Board = sh.Ashley.casDef({
  /**
   * @memberof module:n/ccobjs~Board
   * @method constructor
   * @param {Number} size
   * @param {Array} goals
   */
  constructor(size, goals) {
    this.GOALSPACE= goals;
    this.size=size;
  }
});
/**
 * @property {GameBoard} Board
 */
xbox.Board = Board;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class Grid
 */
const Grid = sh.Ashley.casDef({
  /**
   * @memberof module:n/ccobjs~Grid
   * @method constructor
   * @param {Number} size
   * @param {Array} seed
   */
  constructor(size,seed) {
    this.values= sjs.makeArray(size * size, csts.CV_Z);
    this.size=size;
  }
});
/**
 * @property {Grid} Grid
 */
xbox.Grid = Grid;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class GridView
 */
const GridView = sh.Ashley.casDef({
  /**
   * @memberof module:n/ccobjs~GridView
   * @method constructor
   * @param {Number} size
   * @param {cc.Layer} layer
   */
  constructor(size, layer) {
    const sp = ccsx.createSprite('z.png'),
    sz= sp.getContentSize();
    this.cells= sjs.makeArray(size * size, null);
    this.layer= layer;
    this.width= sz.width;
    this.height= sz.height;
    this.url= "";
    this.gridMap= utils.mapGridPos();
  }
});
/**
 * @property {GridView} GridView
 */
xbox.GridView = GridView;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class NetPlay
 */
const NetPlay = sh.Ashley.casDef({
  /**
   * @memberof module:n/ccobjs~NetPlay
   * @method constructor
   */
  constructor() {
    this.event= null;
  }
});
/**
 * @property {NetPlay} NetPlay
 */
xbox.NetPlay = NetPlay;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class Player
 */
const Player = sh.Ashley.casDef({
  /**
   * @memberof module:n/ccobjs~Player
   * @method constructor
   * @param {Number} category
   * @param {Number} value
   * @param {Number} id
   * @param {Number} color
   */
  constructor(category,value,id,color) {
    this.color= color;
    this.pnum=id;
    this.category= category;
    this.value= value;
    this.offset = id === 1 ? 0 : 1;
  }
});
/**
 * @property {Player} Player
 */
xbox.Player = Player;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class UISelection
 */
const UISelection = sh.Ashley.casDef({
  /**
   * @memberof module:n/ccobjs~UISelection
   * @method constructor
   */
  constructor() {
    this.cell = -1;
    this.px = -1;
    this.py = -1;
  }
});
/**
 * @property {UISelection} UISelection
 */
xbox.UISelection = UISelection;


sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/

//////////////////////////////////////////////////////////////////////////////
//EOF

