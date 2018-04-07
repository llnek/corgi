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
 * @requires n/cobjs
 * @module n/gnodes
 */

import sh from 'zotohlab/asx/asterix';
import cobjs from 'n/cobjs';

//////////////////////////////////////////////////////////////////////////////
/** @alias module:n/gnodes */
let xbox= {},
sjs=sh.skarojs,
undef,
//////////////////////////////////////////////////////////////////////////////
/**
 * @class BoardNode
 */
BoardNode = sh.Ashley.nodeDef({
  /**
   * @memberof module:n/gnodes~BoardNode
   * @property {UISelection} selection
   */
  selection: cobjs.UISelection,
  /**
   * @memberof module:n/gnodes~BoardNode
   * @property {Board} board
   */
  board: cobjs.Board,
  /**
   * @memberof module:n/gnodes~BoardNode
   * @property {SmartAlgo} robot
   */
  robot: cobjs.SmartAlgo,
  /**
   * @memberof module:n/gnodes~BoardNode
   * @property {Grid} grid
   */
  grid: cobjs.Grid,
  /**
   * @memberof module:n/gnodes~BoardNode
   * @property {GridView} view
   */
  view: cobjs.GridView
});
/**
 * @property {BoardNode} BoardNode
 */
xbox.BoardNode = BoardNode;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class GUINode
 */
const GUINode = sh.Ashley.nodeDef({
  /**
   * @memberof module:n/gnodes~GUINode
   * @property {UISelection} selection
   */
  selection: cobjs.UISelection,
  /**
   * @memberof module:n/gnodes~GUINode
   * @property {GridView} view
   */
  view: cobjs.GridView
});
/**
 * @property {GUINode} GUINode
 */
xbox.GUINode = GUINode;

//////////////////////////////////////////////////////////////////////////////
/**
 * @class NetPlayNode
 */
const NetPlayNode = sh.Ashley.nodeDef({
  /**
   * @memberof module:n/gnodes~NetPlayNode
   * @property {NetPlay} playcmd
   */
  playcmd: cobjs.NetPlay,
  /**
   * @memberof module:n/gnodes~Grid
   * @property {Grid} grid
   */
  grid: cobjs.Grid
});
/**
 * @property {NetPlayNode} NetPlayNode
 */
xbox.NetPlayNode = NetPlayNode;


sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF

