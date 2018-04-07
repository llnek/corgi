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
 * @requires s/utils
 * @requires s/factory
 * @requires s/resolve
 * @requires s/stager
 * @requires s/logic
 * @requires s/motion
 * @module s/sysobjs
 */

import sh from 'zotohlab/asx/asterix';
import utils from 's/utils';
import fact from 's/factory';
import res from 's/resolve';
import visor from 's/stager';
import turn from 's/logic';
import motion from 's/motion';

const sjs = sh.skarojs,
/** @alias module:s/sysobjs */
xbox= /** @lends xbox# */{
  systems: [visor.Stager,
  motion.Motions,
  turn.Logic,
  res.Resolve,
  utils],
  /**
   * @method entityFactory
   * @param {Ash.Engine}
   * @return {EntityFactory}
   */
  entityFactory(engine) {
    sh.factory = new fact.EntityFactory(engine);
    return sh.factory;
  }

};

sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF

