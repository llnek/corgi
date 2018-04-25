(require '[cljs.build.api :as b])

(println "Building ...")

(let [start (System/nanoTime)]
  (b/build "test"
    {:output-to "tst/test.js"
     :output-dir "tst"
     ;:externs ["externs/cc.js"]
     :verbose true})
  (println "... done. Elapsed" (/ (- (System/nanoTime) start) 1e9) "seconds"))


