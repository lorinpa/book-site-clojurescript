(ns book-review.books_test
   (:require  [book-review.books :as p] [cljs.test :refer-macros [async deftest is testing]  ]  )
)


(deftest add-authors
  (testing "add authors"
    (p/addAuthor 0 "Mark" "Twain")
    (p/addAuthor 1 "Sam" "Shepard")
    (let [ 
          cnt (count  (deref p/AuthorList))
    ]
      (is (= cnt 2)) 
    )
  )  
)


(deftest get-author
  (testing "add authors"
    (p/addAuthor 2 "Ann" "Sanders")
    (let [ 
          cnt (count  (deref p/AuthorList))
          ann (p/get-list-item-by-id p/AuthorList 2)
    ]
      (is (= cnt 3)) 
      (is (= (:first_name ann) "Ann"))
    )
  )  
)

(deftest delete-author 
 (testing "delete an author"
    (p/remove-item p/AuthorList 2)
    (is (= (count (deref p/AuthorList)) 2))
  ) 
)  

(deftest add-author-with-json 
  (testing "adding an author with a json object"
    (let
      [ jsonObj "{\"id\": 4, \"first_name\":\"Mary\",\"last_name\": \"Wallace\"}"]  
      (p/addAuthor jsonObj)
      (is (= (count (deref p/AuthorList)) 3))
    )
  ) 
)

(deftest modify-author
  (testing "modify author"
       (p/modifyAuthor (p/Author. 1 "Sammy" "Foo"))
       (is
           (=
             (:first_name  (p/get-list-item-by-id p/AuthorList 1)) 
             "Sammy"
          )
         )
   )  
  ) 
