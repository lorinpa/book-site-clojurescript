(defproject book-review "V 2.0"
  :description "Book Club Site "
  :url "http://public-action.org"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [org.clojure/clojurescript "0.0-1859"]
                 [com.cemerick/clojurescript.test "0.2.1"]
                 ]
  :plugins [[lein-cljsbuild "0.3.2"] [com.cemerick/clojurescript.test "0.2.1"][marginalia "0.7.1"] [lein-marginalia "0.7.1"]]
  :cljsbuild
  {
    :builds [
             {:source-paths ["src/cljs"]
              :compiler {:output-to "resources/public/books_cljs.js"
                         :optimizations :advanced
                         :pretty-print true}}
             {:source-paths ["src/cljs" "test" ]
              :compiler {:output-to "target/cljs/wstestable.js"
                         :optimizations :whitespace
                         :pretty-print true}}
           ]
          :test-commands {
                    "phantom-ws" ["phantomjs" :runner
                                          "window.literal_js_was_evaluated=true"
                                          "target/cljs/wstestable.js"
                                          "test/cemerick/cljs/test/extra_test_command_file.js"]
          } 
  }
)
