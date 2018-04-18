(ns czlab.elmo.tictactoe.resource)

(js/console.log "ttt.resource loaded")

(def res {:HelloWorld_png "res/HelloWorld.png" })
(def g_resources #js [])
(doseq [[k v] res] (.push g_resources v))

