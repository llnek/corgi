;; Copyright © 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.mcfud.afx.odin

  (:require [czlab.mcfud.afx.ebus :as e]
            [czlab.mcfud.afx.core
             :as c :refer [do-with defenum trye!
                           let#nil debug* warn* fn_1]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defenum net not-connected 0 connected)
(defenum msg network 1 session)
(defenum evt
  playreq 1
  joinreq
  start
  stop
  restart
  replay
  poke-move
  poke-wait
  poke-rumble
  play-move
  quit-game
  await-start
  sync-arena)

(defenum evt
  playreq-ok 100
  joinreq-ok
  playreq-nok
  joinreq-nok
  user-nok
  game-nok
  room-nok
  room-filled
  rooms-full
  player-joined
  started
  connected
  error
  closed)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkevent
  "New event object."
  [eventType code payload]
  {:time-stamp (system-time) :etype eventType :ecode code :source payload})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mk-play-request
  "Create a PLAY REQUEST event."
  [game user pwd]
  (mkevent evt-playreq -1 {:game game :user user :password pwd}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mk-join-request
  "Create a JOIN REQUEST event."
  [room user pwd]
  (mkevent evt-joinreq -1 {:room room :user user :password pwd}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- json-decode
  "Decode the input json string."
  [input]
  (merge {:etype -1 :ecode -1}
         (trye! (c/s->clj input))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-play-request
  "Get the PLAY REQUEST as json string."
  [game user password]
  (c/jsonize (mk-play-request game user password)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;connecting 0 open 1 closing 2 closed 3
(defn odin-connect!
  "Connect to this url and request a websocket upgrade."
  [odin url]
  (do-with [odin]
    (let [ws (new js/WebSocket url)
          {:keys [ebus game user password]} @odin]
      (c/set-js! ws
                 "onopen"
                 (fn_1 (c/call-js! ws
                                   "send"
                                   (get-play-request game user password))))
      (c/set-js! ws
                 "onmessage"
                 (fn_1 (let [{:keys [etype ecode] :as evt}
                             (json-decode (c/get-js ____1 "data"))]
                         (case etype
                           (msg-network | msg-session)
                           (e/pub ebus (str etype "." ecode) evt)
                           (warn* "unhandled evt: " etype ", code= " ecode)))))
      (c/set-js! ws "onclose" (fn_1 (debug* "closing websocket.")))
      (c/set-js! ws "onerror" (fn_1 (debug* "websocket error: " ____1)))
      (swap! odin #(assoc % :wsock ws)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odin-session
  "Create a session."
  [config]
  (atom (merge {:subcs #{}
                :wsock nil
                :ebus (e/new-event-bus)} config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ready?
  "If socket ready?"
  [wsock]
  (= 1 (c/get-js wsock "readyState")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odin-send
  "Send this event through the socket."
  [odin evt]
  (let#nil
    [{:keys [wsock]} @odin]
    (if (and (some? wsock)
             (ready? wsock)) (c/call-js! wsock "send" (c/jsonize evt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odin-listen
  "Listen to this message-type and event."
  [odin msgType evtCode callback]
  (let#nil
    [{:keys [ebus subcs]} @odin
     h (e/sub+ ebus
               (str msgType "." evtCode) callback)]
    (swap! odin
           #(update-in %
                       [:subcs]
                       (fn_1 (conj ____1 h)))) h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odin-listen+
  "Listen to all message events."
  [odin callback]
  [(odin-listen odin msg-network ">" callback)
   (odin-listen odin msg-session ">" callback)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odin-cancel-all!
  "Cancel and remove all subscribers."
  [odin]
  (do-with [odin]
    (swap! odin
           (fn [{:keys [ebus] :as root}]
             (e/unsub-all! ebus)
             (assoc root :subcs #{})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odin-cancel!
  "Cancel this subscriber."
  [odin subid]
  (do-with [odin]
    (swap! odin
           (fn [{:keys [ebus subcs] :as root}]
             (e/unsub! ebus subid)
             (assoc root :subcs (disj subcs subid))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- odin-close!
  "Close the connection to the socket."
  [odin]
  (odin-cancel-all! odin)
  (do-with [odin]
    (swap! odin
           (fn [{:keys [wsock] :as root}]
             (if (and (some? wsock)
                      (ready? wsock))
               (trye! (c/call-js! wsock "close")))
             (assoc root :wsock nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odin-disconnect!
  "Close the socket." [odin] (odin-close! odin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


