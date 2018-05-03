;; Copyright ©  2013-2018, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc ""
      :author "Kenneth Leung"}

  czlab.elmo.afx.odin

  (:require-macros
    [czlab.elmo.afx.core
     :as ec :refer [trye!]])
  (:require [czlab.elmo.afx.ebus :as ebus]
            [czlab.elmo.afx.core
             :as ec
             :refer [info* warn* debug* objectize jsonize]]
            [oops.core :refer [oget oset! ocall oapply
                               ocall! oapply! oget+
                               oset!+ ocall+ oapply+ ocall!+ oapply!+]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def *msg-network* 1)
(def *msg-session* 2)

(def *evt-playgame-req* 3)
(def *evt-joingame-req* 4)

(def *evt-playreq-nok* 10)
(def *evt-joinreq-nok* 11)
(def *evt-user-nok* 12)
(def *evt-game-nok* 13)
(def *evt-room-nok* 14)
(def *evt-room-filled* 15)
(def *evt-rooms-full* 16)

(def *evt-playreq-ok* 30)
(def *evt-joinreq-ok* 31)

(def *evt-await-start* 40)
(def *evt-sync-arena* 45)
(def *evt-poke-rumble* 46)

(def *evt-restart* 50)
(def *evt-start* 51)
(def *evt-stop* 52)
(def *evt-poke-move* 53)
(def *evt-poke-wait* 54)
(def *evt-play-move* 55)
(def *evt-replay* 56)

(def *evt-quit-game* 60)

(def *evt-player-joined* 90)
(def *evt-started* 95)
(def *evt-connected* 98)
(def *evt-error* 99)
(def *evt-closed* 100)

(def *net-not-connected* 0)
(def *net-connected* 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- noop "" [& xs] nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkEvent "" [eventType code payload]
  {:timeStamp (system-time) :etype eventType :ecode code :source payload})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkPlayRequest "" [game user pwd]
  (mkEvent *evt-playgame-req*
           -1 {:game game :user user :password pwd}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- mkJoinRequest "" [room user pwd]
  (mkEvent *evt-joingame-req*
           -1 {:room room :user user :password pwd}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- json-decode "" [input]
  (let [obj (or (try
                  (objectize input)
                  (catch js/Error e1 nil)
                  (catch js/Object e2 nil)) #js{})]
    (merge {:etype -1 :ecode -1} (js->clj obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- getPlayRequest "" [game user password]
  (jsonize (clj->js
             (mkPlayRequest game user password))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;connecting 0 open 1 closing 2 closed 3
(defn odinConnect
  "Connect to this url and request a websocket upgrade."
  [odin url]
  (let [ws (new js/WebSocket url)
        {:keys [::ebus game user password]} @odin]
    (->>
      (fn [e]
        (ocall ws
               "send"
               (getPlayRequest game user password)))
      (oset! ws "onopen"))

    (->>
      (fn [e]
        (let [{:keys [etype ecode] :as evt}
              (json-decode (oget e "data"))]
          (case etype
            (*msg-network* | *msg-session*)
            (ebus/pub ebus (str etype "." ecode) evt)
            (warn* "unhandled server event: "
                   etype ", code = " ecode))))
      (oset! ws "onmessage"))

    (->>
      (fn [e] (debug* "closing websocket."))
      (oset! ws "onclose"))

    (->>
      (fn [e] (debug* "websocket error: " e))
      (oset! ws "onerror"))

    (swap! odin #(assoc % ::wsock ws))
    odin))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odinSession "" [config]
  (atom (merge {::ebus (ebus/createEvBus)
                ::subcs #{}
                ::wsock nil} config)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odinSend
  "Send this event through the socket." [odin evt]
  (let [{:keys [::wsock]} @odin]
    (if (and (some? wsock)
             (= 1 (oget wsock "readyState")))
      (ocall wsock "send" (jsonize (clj->js evt))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odinListen
  "Listen to this message-type and event."
  [odin msgType evtCode callback]
  (let [{:keys [::ebus ::subcs]} @odin
        h (ebus/sub+ ebus
                     (str msgType "." evtCode) callback)]
    (swap! odin
           #(update-in %
                       [::subcs]
                       (fn [c] (conj c h)))) h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odinListen+
  "Listen to all message events."
  [odin callback]
  [(odinListen odin *msg-network* ">" callback)
   (odinListen odin *msg-session* ">" callback)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odinCancelAll!
  "Cancel and remove all subscribers." [odin]
  (swap! odin
         (fn [{:keys [::ebus] :as root}]
           (ebus/unsubAll! ebus)
           (assoc root ::subcs #{}))) odin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odinCancel!
  "Cancel this subscriber." [odin subid]
  (swap! odin
         (fn [{:keys [::ebus ::subcs] :as root}]
           (ebus/unsub ebus subid)
           (assoc root
                  ::subcs
                  (disj subcs subid)))) odin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn odinClose
  "Close the connection to the socket." [odin]

  (odinCancelAll! odin)
  (swap! odin
         (fn [{:keys [::wsock] :as root}]
           (if (and (some? wsock)
                    (= 1 (oget wsock "readyState")))
             (try
               (ocall wsock "close")
               (catch js/Error e1 nil)
               (catch js/Object e2 nil)))
           (assoc root ::wsock nil)))
  odin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn disconnect
  "Disconnect from the socket." [odin] (odinClose odin))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


