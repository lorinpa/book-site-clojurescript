(defproject book-review "V 3.0"
  :description "Book Club Site"
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.439"]
                 [doo "0.1.10"]]
  :url "http://public-action.org"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :plugins [[lein-cljsbuild "1.1.7"] [lein-doo "0.1.10"]]
  :doo {
    :build "test-build" :alias {:default [:firefox-headless]}
    :paths {:karma "node_modules/karma/bin/karma" }
  }
  :clean-targets [:target-path "out/js"]
  :cljsbuild {
    :builds
    [ {
        ; The path to the top-level ClojureScript source directory:
        :source-paths ["src/cljs" "test" ]
        ; The standard ClojureScript compiler options:
        ; (See the ClojureScript compiler documentation for details.)
        :compiler {
          :output-to "out/js/main.js"
          :optimizations :advanced
          :pretty-print false }
   }
   {:id "test-build"
             :source-paths ["src/cljs" "test"]
             :compiler {:output-to "out/test/testable.js"
                        :main book-review.runner
                        :optimizations :whitespace} }
    ]})
