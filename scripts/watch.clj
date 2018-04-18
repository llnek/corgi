(require '[cljs.build.api :as b])

(b/watch "src"
  {:main 'a.core
   :output-to "out/a.js"
   :output-dir "out"})
