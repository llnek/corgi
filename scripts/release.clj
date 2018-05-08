(require '[cljs.build.api :as b])

(println "Building ...")

(let [start (System/nanoTime)]
  (b/build "src"
    {:output-to "release/ttt.js"
     :output-dir "release"
     :externs ["externs/cc.js"]
     ;:optimizations :simple
     :optimizations :whitespace
     ;:optimizations :advanced
     :verbose true})
  (println "... done. Elapsed" (/ (- (System/nanoTime) start) 1e9) "seconds"))

