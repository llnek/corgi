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
 * @requires n/board
 * @requires n/gnodes
 * @module s/factory
 */

import sh from 'zotohlab/asx/asterix';
import cobjs from 'n/cobjs';
import gboard from 'n/board';
import gnodes from 'n/gnodes';

//////////////////////////////////////////////////////////////////////////////
let sjs= sh.skarojs,
xcfg = sh.xcfg,
csts= xcfg.csts,
undef,

//////////////////////////////////////////////////////////////////////////////
// returns array of winning combinations.
mapGoalSpace = size => {
  const rows = [],
  cols = [],
  dx = [],
  dy = [];
  let h, v;

  for (let r=0; r < size; ++r) {
    h = [];
    v = [];
    for (let c=0; c < size; ++c) {
      h.push(r * size + c);
      v.push(c * size + r);
    }
    rows.push(h);
    cols.push(v);
    dx.push(r * size + r);
    dy.push((size - r - 1) * size + r);
  }
  return [dx, dy].concat(rows, cols);
},
//////////////////////////////////////////////////////////////////////////////
/** * @class EntityFactory */
EntityFactory = sh.Ashley.casDef({
  /**
   * @memberof module:s/factory~EntityFactory
   * @method constructor
   * @param {Ash.Engine} engine
   */
  constructor(engine) {
    this.engine=engine;
  },
  /**
   * @memberof module:s/factory~EntityFactory
   * @method reifyBoard
   * @param {cc.Layer} layer
   * @param {Object} options
   * @return {Ash.Entity}
   */
  reifyBoard(layer, options) {
    const goals= mapGoalSpace(options.size),
    bd= new gboard.GameBoard(options.size,
                      csts.CV_Z,
                      csts.CV_X,
                      csts.CV_O, goals),
    ent = sh.Ashley.newEntity();

    ent.add(new cobjs.Grid(options.size, options.seed));
    ent.add(new cobjs.Board(options.size, goals));
    ent.add(new cobjs.UISelection());
    ent.add(new cobjs.SmartAlgo(bd));
    ent.add(new cobjs.NetPlay());
    ent.add(new cobjs.GridView(options.size, layer));

    options.GOALSPACE=goals;
    return ent;
  }

});

/** @alias module:s/factory */
const xbox = /** @lends xbox# */{
  EntityFactory : EntityFactory
};

sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF

