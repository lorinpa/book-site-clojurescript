(defproject book-review "Rel 1.0"
  :description "Book Review Site "
  :url "http://public-action.org"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]]
  :plugins [[lein-cljsbuild "0.3.2"]]
  :cljsbuild {
    :builds [{:source-paths ["src/cljs"]
              :compiler {:output-to "resources/public/books_cljs.js"
                         :libs ["closure/library/third_party/closure"]
                         :optimizations :advanced
                         :pretty-print true}}]}
)
