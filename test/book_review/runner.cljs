(ns book-review.runner
  (:require [cljs.test :as test]
            [doo.runner :refer-macros [doo-all-tests doo-tests]]
            [book-review.books_test]))

(doo-tests 'book-review.books_test)