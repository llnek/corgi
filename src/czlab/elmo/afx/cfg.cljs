;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.cfg

  (:require [czlab.elmo.afx.core :as ec]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *xcfg*
  (atom {:urlPrefix "/public/elmo/"
         :appid ""
         :version ""
         :trackingID ""
         :color (js/cc.color 0 0 0)
         :levels {}
         :images {:czlab "core/ZotohLab.png"
                  :preloader "core/preloader_bar.png"}
         :sprites { }
         :tiles { }
         :sounds {}
         :fonts {}
         :game {:policy js/cc.ResolutionPolicy.SHOW_ALL
                :preloadLevels? true
                :size {:width 0 :height 0}
                :resDir ""
                :landscape? true
                :scale 1
                :sfx :mp3
                :gravity 0 }
         :l10nTable {:en {"%mobileStart" "Press Anywhere To Start!"
                          "%webStart" "Press Spacebar To Start!"
                          "%passwd" "Password"
                          "%userid" "UserId"
                          "%player2" "Player 2"
                          "%player1" "Player 1"
                          "%computer" "Computer"
                          "%cpu" "CPU"
                          "%2players" "2 Players"
                          "%1player" "1 Player"
                          "%online" "Online"
                          "%gameover" "Game Over"
                          "%quit!" "Quit"
                          "%back" "Back"
                          "%ok" "OK"
                          "%mmenu" "Main Menu"
                          "%replay" "REPLAY"
                          "%play" "PLAY"
                          "%waitothers" "Waiting...\nfor other players."
                          "%waitother" "Waiting...\nfor another player."
                          "%signinplay" "Please sign in to play."
                          "%quit?" "Continue and quit game?" } }
         :csts {:CV-X (ocall "X" "charCodeAt" 0)
                :CV-O (ocall "0" "charCodeAt" 0)
                :P2-COLOR "O"
                :P1-COLOR "X"
                :NETP 3
                :HUMAN 1
                :BOT 2
                :GAME-MODE 1
                :TILE 8
                :S-OFF 4
                :GAME-ID "" }
         :audio {:volume 0.5
                 :open? false
                 :track nil }

        :startScene (fn [] nil)

        :runOnce (fn [] nil)
}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


