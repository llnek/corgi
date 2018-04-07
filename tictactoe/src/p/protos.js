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
 * @requires zotohlab/gui/online
 * @requires zotohlab/gui/msgbox
 * @requires zotohlab/gui/ynbox
 * @requires p/splash
 * @requires p/mmenu
 * @requires p/game
 * @module p/protos
 */

import online from 'zotohlab/gui/online';
import msgbox from 'zotohlab/gui/msgbox';
import ynbox from 'zotohlab/gui/ynbox';
import sh from 'zotohlab/asx/asterix';
import splash from 'p/splash';
import mmenu from 'p/mmenu';
import arena from 'p/game';

//////////////////////////////////////////////////////////////////////////////
let ps= [online, splash, mmenu, msgbox, ynbox, arena],
protos= sh.protos,
sjs= sh.skarojs,
/** @alias module:p/protos */
xbox = protos,
xcfg = sh.xcfg,
R = sjs.ramda,
undef;

R.forEach((obj) => {
  protos[obj.rtti] = obj;
}, ps);

sjs.merge(exports, protos);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF

