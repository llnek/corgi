(require '[cljs.build.api :as b])

(println "Building ...")

(let [start (System/nanoTime)]
  (b/build "test"
    {:output-to "rtst/test.js"
     :output-dir "rtst"
     :externs ["externs/cc.js"]
     :optimizations :whitespace
     ;:optimizations :advanced
     :verbose true})
  (println "... done. Elapsed" (/ (- (System/nanoTime) start) 1e9) "seconds"))

