;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author ""}

  czlab.elmo.tictactoe.game

  (:require-macros [czlab.elmo.afx.core :as ec :refer [f#*]]
                   [czlab.elmo.afx.ccsx :as cx :refer []])
  (:require [czlab.elmo.afx.ccsx :as cx]
            [czlab.elmo.afx.core :as ec]
            [czlab.elmo.tictactoe.splash :as splash]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- HUDLayer "" []
  (do-with [hud (new js/cc.Layer)]
    (let [color (js/cc.color "#5e3178")
    (oset! hud "!p2Long" "")
    (oset! hud "!p1Long" "")
    (oset! hud "!p2ID" "")
    (oset! hud "!p1ID" "")
    this.options.i_menu= {
      cb() { sh.fire('/hud/showmenu'); },
      nnn: '#icon_menu.png',
      where: ccsx.acs.Bottom,
      color: color,
      scale: scale
    };
    this.options.i_replay = {
      cb() { sh.fire('/hud/replay'); },
      where: ccsx.acs.Bottom,
      nnn: '#icon_replay.png',
      color: color,
      scale : scale,
      visible: false
    };
    this.color= color;
    this.initScores();
  },
  /**
   * @method initScores
   * @protected
   */
  initScores() {
    this.scores= {};
    this.scores[csts.P2_COLOR] =  0;
    this.scores[csts.P1_COLOR] =  0;
  },
  /**
   * @method initLabels
   * @protected
   */
  initLabels() {
    const cw= ccsx.center(),
    c= this.color,
    wb= ccsx.vbox();

    this.title = ccsx.bmfLabel({
      fontPath: sh.getFont('font.JellyBelly'),
      text: '',
      color: c,
      anchor: ccsx.acs.Top,
      scale: xcfg.game.scale * 0.6,
      pos: cc.p(cw.x, wb.top - 2*csts.TILE)
    });
    this.addItem(this.title);

    this.score1= ccsx.bmfLabel({
      fontPath: sh.getFont('font.SmallTypeWriting'),
      text: '0',
      scale: xcfg.game.scale,
      color: ccsx.white,
      pos: cc.p(csts.TILE + csts.S_OFF + 2,
                wb.top - csts.TILE - csts.S_OFF),
      anchor: ccsx.acs.TopLeft
    });
    this.addItem(this.score1);

    this.score2= ccsx.bmfLabel({
      fontPath: sh.getFont('font.SmallTypeWriting'),
      text: '0',
      scale: xcfg.game.scale,
      color: ccsx.white,
      pos: cc.p(wb.right - csts.TILE - csts.S_OFF,
                wb.top - csts.TILE - csts.S_OFF),
      anchor: ccsx.acs.TopRight
    });
    this.addItem(this.score2);

    this.status= ccsx.bmfLabel({
      fontPath: sh.getFont('font.CoffeeBuzzed'),
      text: '',
      color: ccsx.white,
      scale: xcfg.game.scale * 0.3,// 0.06,
      pos: cc.p(cw.x, wb.bottom + csts.TILE * 10)
    });
    this.addItem(this.status);

    this.result= ccsx.bmfLabel({
      fontPath: sh.getFont('font.CoffeeBuzzed'),
      color: ccsx.white,
      text: '',
      scale: xcfg.game.scale * 0.3,// 0.06,
      pos: cc.p(cw.x, wb.bottom + csts.TILE * 10),
      visible: false
    });
    this.addItem(this.result);
  },
  /**
   * @method showTimer
   * @private
   */
  showTimer() {
    const cw= ccsx.center(),
    wb= ccsx.vbox();

    // timer is already showing, go away
    if (this.countDownState) {
      return;
    }

    if (! this.countDown) {
      this.countDown= ccsx.bmfLabel({
        fontPath: sh.getFont('font.AutoMission'),
        text: '',
        scale: xcfg.game.scale * 0.5,
        color: ccsx.white,
        pos: cc.p(cw.x,
                  wb.top - 10*csts.TILE),
        anchor: ccsx.acs.Center
      });
      this.addItem(this.countDown);
    }

    this.countDownValue= csts.PLAYER_THINK_TIME;
    this.showCountDown();

    this.schedule(this.updateTimer, 1.0);
    this.countDownState= true;
  },
  /**
   * @method updateTimer
   * @private
   */
  updateTimer(dt) {
    if (!this.countDownState) { return; } else {
      this.countDownValue -= 1;
    }
    if (this.countDownValue < 0) {
      this.killTimer();
      sh.fire('/player/timer/expired');
    }
    else {
      this.showCountDown();
    }
  },
  /**
   * @method showCountDown
   * @private
   */
  showCountDown(msg) {
    if (!!this.countDown) {
      this.countDown.setString(msg || '' + this.countDownValue);
    }
  },
  /**
   * @method killTimer
   * @private
   */
  killTimer() {
    if (this.countDownState) {
      this.unschedule(this.updateTimer);
      this.showCountDown(' ');
    }
    this.countDownState=false;
    this.countDownValue=0;
  },
  /**
   * @method updateScore
   * @private
   */
  updateScore(pcolor, value) {
    this.scores[pcolor] += value;
    this.drawScores();
  },
  /**
   * @method update
   * @protected
   */
  update(running, pnum) {
    if (running) {
      this.drawStatus(pnum);
    } else {
      this.drawResult(pnum);
    }
  },
  /**
   * @method endGame
   * @private
   */
  endGame(winner) {
    this.killTimer();
    this.replayBtn.setVisible(true);
    this.result.setVisible(true);
    this.status.setVisible(false);
    this.drawResult(winner);
  },
  /**
   * @method drawStatusText
   * @private
   */
  drawStatusText(obj, msg) {
    obj.setString(msg || '');
  },
  /**
   * @method drawScores
   * @private
   */
  drawScores() {
    const s2 = this.scores[this.play2],
    s1 = this.scores[this.play1],
    n2 = ''+s2,
    n1 = ''+s1;

    this.score1.setString(n1);
    this.score2.setString(n2);
  },
  /**
   * @method drawResult
   * @private
   */
  drawResult(pnum) {
    let msg = sh.l10n('%whodraw');

    if (sjs.isnum(pnum)) {
      switch (pnum) {
        case 2: msg= sh.l10n('%whowin', { who: this.p2Long}); break;
        case 1: msg= sh.l10n('%whowin', { who: this.p1Long}); break;
      }
    }

    this.drawStatusText(this.result, msg);
  },
  /**
   * @method drawStatus
   * @private
   */
  drawStatus(pnum) {
    if (sjs.isnum(pnum)) {
      const pfx = pnum === 1 ? this.p1Long : this.p2Long;
      this.drawStatusText(this.status,
                          sh.l10n('%whosturn', {
        who: pfx
      }));
    }
  },
  /**
   * @method regoPlayers
   * @private
   */
  regoPlayers(color1,p1ids,color2,p2ids) {
    this.play2= color2;
    this.play1= color1;
    this.p2Long= p2ids[1];
    this.p1Long= p1ids[1];
    this.p2ID= p2ids[0];
    this.p1ID= p1ids[0];
    this.title.setString(this.p1ID + " / " + this.p2ID);
  },
  /**
   * @method resetAsNew
   * @protected
   */
  resetAsNew() {
    this.initScores();
    this.reset();
  },
  /**
   * @method reset
   * @protected
   */
  reset() {
    this.replayBtn.setVisible(false);
    this.result.setVisible(false);
    this.status.setVisible(true);
  }
});

/** @alias module:p/hud */
const xbox = /** @lends xbox# */{
  /**
   * @property {HUDLayer} HUDLayer
   */
  HUDLayer: HUDLayer
};

sjs.merge(exports, xbox);
/*@@
return xbox;
@@*/
//////////////////////////////////////////////////////////////////////////////
//EOF


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn gameScene "" []
  (do-with [scene (new js/cc.Scene)]
    (let [y0 (new js/cc.Layer)
          _ (cx/addItem scene y0 -1)
          bg (sprite* (cx/getImage :game-bg))
          _ (setXXX! bg {:pos cp})
          _ (cx/addItem y0 bg)]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- gameLayer "" []
  (do-with [y (new js/cc.Layer)]
    (let []
      (->>
        #js{:ebus ebus
            :init
            #(do ()
                 (cx/onTouchOne ebus)
                 (cx/onMouse ebus)) }
        (attr* y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- replay "" [gl]
  ;;request server to restart a new game?
  (if-some [w (oget gl "?____wss")]
    (odn/odinSend w {:etype *msg-session*
                     :ecode *evt-replay*})
    (play false)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- play "" []

  (initPlayers)
  (initEngine)
  (initHUD csts.P1_COLOR p1ids
           csts.P2_COLOR p2ids)
  (oset! gl "!lastWinner" nil)
  (oset! gl "!runningQ" true)
  (oset! gl "!msgBuf" []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- updateHUD "" []
  (if (oget gl "?runningQ")
    (drawStatus hud this.actor)
    (drawResult hud this.lastWinner)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- playTimeExpired "" [gl msg]
  (if-some [buf (oget gl "?msgBuf")]
    (.push buf "forfeit")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- initPlayers "" []
  (let [mode (oget gl "?game_mode")
        [p1cat p2cat] (case mode
                        3 [3 3]
                        2 [1 1]
                        1 [1 2]
                        (raise! "crap!"))
        p1 {:category p1cat
            :value csts.CV_X
            :pnum 1
            :color csts.P1_COLOR
            :offset 0}
        p2 {:category p2cat
            :value csts.CV_O
            :pnum 2
            :color csts.P2_COLOR
            :offset 1}]
    (oset! gl "!players" [nil p1 p2])
    (oset! gl "!colors"
           {:X p1 :O p2})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- overAndDone "" [winner]
  (endGame hud winner))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn xxx "" []
  (ebus/sub+ ebus
             "hud.showmenu"
             (f#* (showMenu)))
  (ebus/sub+ ebus
             "hud.replay"
             (f#* (replay)))

  (ebus/sub+ ebus
             "hud.timer.show"
             (f#* (showTimer hud)))

  (ebus/sub+ ebus
             "net.restart"
             (f#* (killTimer hud) (play)))

  (ebus/sub+ ebus
             "net.stop"
             #(overAndDone (:status %)))

  (ebus/sub+ ebus "hud.timer.hide"
             (f#* (killTimer)))

  (ebus/sub+ ebus
             "hud.score.update"
             (fn [msg]
               (updateScore hud
                            (:color msg)
                            (:score msg))))

  (ebus/sub+ ebus "hud.end"
      #(overAndDone (:winner %)))

  (ebus/sub+ ebus
             "hud.update"
             (fn [msg]
               (update hud
                       (:runningQ msg)
                       (:pnum msg))))

  (ebus/sub+ ebus
             "player.timer.expired"
             #(playTimeExpired %))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF



