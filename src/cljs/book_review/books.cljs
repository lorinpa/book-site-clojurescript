(ns book-review.books
  (:require 
    [goog.net.XhrManager  :as manager]
    [goog.net.XhrIo :as xhr]
    [goog.dom :as gdom] 
    [goog.object :as gobject]
    [goog.json :as gjson]
    [goog.structs :as gstructs]
  )
)

(defn format-name [last first]
  (format "%s, %s" last first )
)

(defn by-id [id]
  (.getElementById js/document (name id)))

(defn append [parent & children]
  (apply gdom/append parent children) parent)

(defn addAttribute [elem key value]
  (let [ propObj ( gobject/create key value)]
    (gdom/setProperties elem propObj) elem)
)

(defn set-elem-text [elem text]
    (aset  elem  "textContent" text)
)

(defn create-doc-fragment []
  (.createDocumentFragment js/document)
)

(defn create-elem [name]
  (gdom/createElement name)
)

(defn create-input-elem [type value id-key id-value]
  (
    let [ input-control (create-elem type) ]
    (addAttribute input-control "type" "input")
    (addAttribute input-control id-key id-value)
    (addAttribute input-control "value" value)
    input-control
  )
)

(defn create-label-elem [for-value text-value] 
    ( let [ label-elem (create-elem "label")]
      (addAttribute label-elem "for" for-value)
      (addAttribute label-elem "title" text-value)
      (set-elem-text label-elem text-value)
      label-elem
    )
)

(defn star-rating [elem num-stars]
  (cond
      (= num-stars 1) (aset  elem "textContent" "★ " )
      (= num-stars 2) (aset  elem "textContent" "★ ★" )
      (= num-stars 3) (aset  elem "textContent" "★ ★ ★ " )
      (= num-stars 4) (aset  elem "textContent" "★ ★ ★ ★ " )
      (= num-stars 5) (aset  elem "textContent" "★ ★ ★ ★ ★ " )
  )
 elem
)

(defrecord Book [ id title category_count review_count author_first_name author_last_name author_id])
(def BookList (atom []))

(defrecord Author [ id  first_name last_name book_count])
(def AuthorList (atom []))

(defrecord Review [ id body stars book_id author_id])
(def ReviewList (atom []))

(defrecord Category [ id title ])
(def CategoryList (atom []))

(defrecord BookCategory [ id book_id category_id ])
(def BookCategoryList (atom []))

(defn jsonToBook [jsonObj]
  (let [  id (aget jsonObj "id")  title  (aget jsonObj "title")
          category_count (aget jsonObj "categories_count") 
          review_count (aget jsonObj "review_count") 
          author_first_name (aget jsonObj "author_first_name")
          author_last_name  (aget jsonObj "author_last_name") 
          author_id (aget jsonObj "author_id" )]
    (Book. id title category_count review_count author_first_name author_last_name author_id)))

(defn jsonToAuthor [jsonObj]
  (let [  id (aget jsonObj "id") book_count (aget jsonObj "book_count")
           first_name (aget jsonObj "first_name") last_name  (aget jsonObj "last_name")]
    (Author. id  first_name last_name book_count )))

(defn jsonToReview [jsonObj]
  (let [  id (aget jsonObj "id") 
          book_id (aget jsonObj "book_id") 
          author_id (aget jsonObj "author_id")
          body (aget jsonObj "body") stars  (aget jsonObj "stars")]
    (Review. id body stars book_id author_id)))

(defn jsonToCategory [jsonObj]
  (let [id (aget jsonObj "id") title (aget jsonObj "title")]
    (Category. id title)))

(defn jsonToBookCategory [jsonObj]
  (let [id (aget jsonObj "id") 
        book_id (aget jsonObj "book_id") 
        category_id (aget jsonObj "category_id")]
    (BookCategory. id book_id category_id)))

(defn addBook
  ([id title category_count review_count author_first_name author_last_name author_id] 
    (swap! BookList conj (Book. id title category_count review_count author_first_name author_last_name author_id)))
  ([jsonObj] (swap! BookList conj (jsonToBook jsonObj)) )
)

(defn addAuthor
  ([id first_name last_name book_count]  
    (swap! AuthorList conj (Author. id first_name last_name book_count)))
  ([jsonObj] (swap! AuthorList conj (jsonToAuthor jsonObj)) )
)

(defn addReview
  ([id body stars book_id author_id]  
    (swap! ReviewList conj (Review. id body stars book_id author_id)))
  ([jsonObj] (swap! ReviewList conj (jsonToReview jsonObj)) )
)

(defn addCategory
  ([id title]  
    (swap! CategoryList conj (Category. id title)))
  ([jsonObj] (swap! CategoryList conj (jsonToCategory jsonObj)) )
)

(defn addBookCategory
  ([id book_id category_id]  
    (swap! BookCategoryList conj (BookCategory. id book_id category_id) ))
  ([jsonObj] (swap! BookCategoryList conj (jsonToBookCategory jsonObj)) )
)

(defn clear-report-details []
  (let[ report (by-id "report") report-details (by-id "report-details") ]
      (.removeChild report report-details))
)

(defn get-list-item [list id]
  (let [  derefed-list (deref list)
          results (filter ( fn[n]  (= (str (:id n )) id)) derefed-list )]
      (first results))
)

;this function returns the first related item
(defn get-related-list-item [list id]
  (let [  derefed-list (deref list)
          results (filter ( fn[n]  (=  (:id n ) id)) derefed-list )]
      (first results))
)

(defn set-menu-header 
  ([text]
    (let [heading-elem (by-id "menu-header")]
      (aset  heading-elem  "textContent" text)
      ))

  ([text attr-key attr-value]
  (let [heading-elem (by-id "menu-header")]
    (aset heading-elem  "textContent" text)
    (addAttribute heading-elem attr-key attr-value)
    ))
)

(defn empty-elem [elem]
  (let [  child-list (aget elem "children")
          child-seq (array-seq child-list)
          cnt (-count child-seq)
          index-vals (range cnt)]
          (doseq [index index-vals]
            (do
                (gdom/removeNode  (-nth child-seq index))
                (gobject/clear  (-nth child-seq index))
            )
          )))

(defn clear-menu-list []
  (let [
        report-details (by-id "report-details")
              ]
    (empty-elem report-details)
    (gdom/removeChildren  report-details)

))

;we want an option to specify the li element as the recepient
;of the attr-key value set (in addition to the child). That way complex menu rows
;can be captured as click transmitters
(defn render-menu-link ([elem attr-key attr-value]
    (
     let [  li (.createElement js/document "li") a-link (.createElement js/document "a")]
      (addAttribute a-link "href" "#")
      (addAttribute a-link attr-key attr-value)
      (addAttribute li attr-key attr-value)
      (append a-link elem)
      (append li a-link)
      li))
  )


(defn render-menu-item 

  ([href text attr-key attr-value]
    (let [  li (.createElement js/document "li") a-link (.createElement js/document "a")]
      (addAttribute a-link "href" href)
      (addAttribute a-link attr-key attr-value)
      (aset  a-link  "textContent" text)
      (append li a-link)
      li))

  ([child-elem  attr-key attr-value]
    (let [ li (.createElement js/document "li") a-link (.createElement js/document "a") ]
      (addAttribute a-link "href" "#")
      (addAttribute a-link attr-key attr-value)
      (append a-link child-elem)
      (append li a-link)
      li))
 
  ([child-elem ]
    (let [ li (.createElement js/document "li") a-link (.createElement js/document "a") ]
      (addAttribute a-link "href" "#")
      (append a-link child-elem)
      (append li a-link)
      li))
)

(defn labeled-elem  [text child-elem]
    (let [ label-elem (.createElement js/document "div")]
        (aset  label-elem  "textContent" text)
        (addAttribute label-elem "class" "pure-u-1-1")
        (append label-elem child-elem)
      label-elem
    )
)

(defn labeled-row
  ;; one row 2 text columns
  ( [label text]
    (let [  row-elem (.createElement js/document "div")
            label-elem (.createElement js/document "div")
            text-elem (.createElement js/document "div")
          ]
        (aset label-elem  "textContent" label)
        (aset text-elem  "textContent" text)
        (addAttribute label-elem "class" "pure-u-1-3")
        (addAttribute text-elem "class" "pure-u-1-3")
        (append row-elem label-elem)
        (append row-elem text-elem)
        row-elem
    )
  )
  ;; one row 1 text column 1 child element
  ( [label text child-elem]
    (let [  row-elem (.createElement js/document "div")
            label-elem (.createElement js/document "div")
          ]
        (aset label-elem  "textContent" label)
        (addAttribute label-elem "class" "pure-u-1-3")
        (addAttribute child-elem "class" "pure-u-1-3")
        (append row-elem label-elem)
        (append row-elem child-elem)
        row-elem
    )
  )
)

(defn table-elem [] 
    (let [ table-elem (create-elem "table") ]
      (addAttribute table-elem "class" "review pure-table")
    table-elem)
)

(defn message-table-row 
  ([message]
  (let [ tr (create-elem "tr")
         td (create-elem "td")
        ]
    (aset td "textContent" message)
    (append tr td)
    tr
  ))

  ([message colspan]
  (let [ tr (create-elem "tr")
         td (create-elem "td")
        ]
    (addAttribute td "colspan" colspan)
    (aset  td "textContent" message)
    (append tr td)
    tr
  ))
)

(defn review-details-td [review-id]
    (let[ 
        details-td (create-elem "td")
        button (create-elem "button")
      ] 
      (addAttribute details-td "align" "right")
      (addAttribute button "class" "pure-button pure-button-c small-button")
      (addAttribute button "data-review-id" review-id)
      (aset  button "textContent" "Details" )
      (append details-td button)
      details-td
    )
)

(defn review-row
  ( [num-stars review-id]
    ( let [ row (create-elem "tr") 
        label-td (create-elem "td")
        ratings-td (create-elem "td") 
        details-td (review-details-td review-id)]
        (addAttribute label-td "class" "soft")  
        (star-rating ratings-td num-stars)
        (addAttribute ratings-td "class" "star-rating")
        (aset label-td "textContent" "Rating:")
        (append row label-td) 
        (append row ratings-td) 
        (append row details-td) 
        row
    )
   )

  ( [num-stars]
    (let [ row (create-elem "tr") 
        label-td (create-elem "td")
        ratings-td (create-elem "td") ]
        (addAttribute label-td "class" "soft")  
        (star-rating ratings-td num-stars)
        (addAttribute ratings-td "class" "star-rating")
        (aset  label-td "textContent" "Rating:")
        (append row label-td) 
        (append row ratings-td) 
        row
    )
   )
)

(defn book-tr [book-title col-span]
  (let [
        tr (create-elem "tr")
        td (create-elem "td")
      ]
      (aset td "textContent" book-title )
      (addAttribute td "colspan" col-span)
      (append tr td)
      tr
    )  
)

(defn author-row [last_name first_name author-id]
  (let
    [ tr (create-elem "tr") label-td (create-elem "td")
      name-td (create-elem "td" )]
    (aset  name-td "textContent" (format-name last_name first_name))
    (aset  label-td "textContent" "Author:")
    (addAttribute name-td "data-author-id" author-id)
    (append tr label-td)
    (append tr  name-td)
    tr))

(defn review-header [ book-title num-stars review-id]
  (let [ table (table-elem) title-row (book-tr book-title 3)
         rating-row (review-row num-stars review-id)]
    (append table title-row)
    (append table rating-row) 
    table))



(defn no-data-message-row [table  message ]
  (let [ childNodes (aget table "childNodes") num_nodes (aget  childNodes "length")]
  (if (= 0 num_nodes)
    (append table (message-table-row message)))))

(defn no-data-message [fragment label-text message ]
  (let [ childNodes (aget fragment "childNodes" ) num_nodes(aget childNodes "length") ]
      (if (= 0 num_nodes)
        (append fragment  (render-menu-item (labeled-row label-text message ))))))

(defn render-book-list[]
  (let[ report-details  (by-id "report-details")
    fragment (.createDocumentFragment js/document)]
    (set-menu-header "Books")
    (clear-menu-list)
    (doseq [b (deref BookList)]
      (append fragment (render-menu-item "#" (:title b) "data-book-id"  (:id b) )))
    (append report-details fragment)))

(defn render-book-details [book]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document)
        category-count-elem (labeled-row "# Categories:" (:category_count book))
        review-count-elem (labeled-row "# Reviews:" (:review_count book))
        author-elem (labeled-row "Author:" (format-name  (:author_last_name book) (:author_first_name book)))]
    (set-menu-header (:title book))
    (clear-menu-list)
    (append fragment (render-menu-link author-elem "data-author-id" (:author_id book) ))
    (append fragment (render-menu-link category-count-elem "data-categories-for-book-id" (:id book) ))
    (append fragment (render-menu-item review-count-elem "data-reviews-for-book-id" (:id book) ))
    (append report-details fragment)
  )
)

(defn render-author-list[]
  (let[ report-details  (by-id "report-details") 
       fragment (.createDocumentFragment js/document)]
    (clear-menu-list)
    (set-menu-header "Authors")
    (doseq [author (deref AuthorList)]
      (append fragment (render-menu-item "#" (format-name (:last_name author) (:first_name author) )
        "data-author-id"  (:id author) ))
    )
    (append report-details fragment)
   )
)

(defn render-author-details [author]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document)
        book-count-elem (labeled-row "# Books:" (:book_count author))]
    (clear-menu-list)
    (set-menu-header (format-name (:last_name author) (:first_name author) ))
    (append fragment (render-menu-item book-count-elem "reviews-by-book" (:id author) ))
    (doseq [b (deref BookList)]
      (if (= (:author_id b) (:id author) )
        (append fragment (render-menu-item "#" (:title b) "data-book-id"  (:id b) ))
      )
    )
    (append report-details fragment)
  )
)

(defn render-review-details [review]
  (let [ report-details  (by-id "report-details")
        table (table-elem)
        author (get-related-list-item AuthorList (:author_id review))
        author-tr (author-row (:last_name author) (:first_name author) (:id author) )
        review-tr (review-row (:stars review))
        body-tr (message-table-row (:body review) 2)
        book (get-related-list-item BookList (:book_id review)) 
       ]
    (append table author-tr)
    (append table review-tr)
    (append table body-tr)
    (clear-menu-list)
    (set-menu-header (:title book) )
    (append report-details table) 
  )
)

(defn render-review-list []
  (let[ report-details  (by-id "report-details") 
        fragment (.createDocumentFragment js/document)
        table (create-elem "table")
       ]
    (clear-menu-list)
    (set-menu-header "Reviews")
    (doseq [review (deref ReviewList)]
      (append fragment (book-tr (:title (get-related-list-item BookList (:book_id review))) 3 ))
       (append fragment (review-row (:stars review) (:id review)))
    )
    (append table fragment)
    (append report-details table)))

(defn render-category-list []
  (let[ report-details  (by-id "report-details") 
       fragment (.createDocumentFragment js/document)]
    (clear-menu-list)
    (set-menu-header "Categories")
    (doseq [category (deref CategoryList)]
      (append fragment (render-menu-item 
        (:title  category)
        "data-category-id" (:id category)    
      ))
    )
    (append report-details fragment)))

(defn render-category-details [category]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document) id (:id category)]
      (doseq [i (deref BookCategoryList)]
        (if  (= (:category_id i) id)
          (do
             (doseq [b (deref BookList)]
                (if (= (:book_id i) (:id b) )
                  (append fragment (render-menu-item (:title b) "data-book-id" (:id b)))
                )
              )
          )
        )
      )
      (clear-menu-list)
      (set-menu-header (format "%s - %s"  (:title category) "Books" ))
      (no-data-message fragment "No books found for this category." "")
      (append report-details fragment)))

(defn render-categories-for-book [book]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document) id (:id book)]
      (doseq [book-category (deref BookCategoryList)]
        (if  (= (:book_id book-category) id)
          (do
             (doseq [category (deref CategoryList)]
                (if (= (:category_id book-category) (:id category) )
                  (append fragment (render-menu-item (:title category) "data-category-id" (:id category)))
                )
              )
          )
        )
      )
      (clear-menu-list)
      (set-menu-header (format "Categories for: %s" (:title book) ) "data-book-id" (:id book)  )
      (no-data-message fragment "No Categoried Found this Book." "")
      (append report-details fragment)))

(defn render-reviews-for-book [book]
  (let[ report-details  (by-id "report-details") id (:id book)
        table (table-elem)]
      (doseq [review (deref ReviewList)]
        (if  (= (:book_id review) id)
           (append table (review-row (:stars review) (:id review)))
        )
      )
      (clear-menu-list)
      (set-menu-header (format "Reviews of: %s" (:title book) ) "data-book-id" (:id book)  )
      (no-data-message-row table "No Reviews Found." )
      (append report-details table)))


(defn get-parent-dataset [elem]
  (let[ 
        parent (aget elem "parentElement")
        parent (aget parent "parentElement")
        dataset (aget parent "dataset")
      ]
      dataset
    )
)

(defn list-click-listener [ event]
  (let [ target-elem (aget event "target") tag-name (aget target-elem "tagName")
          dataset (aget target-elem "dataset")
          parent-dataset (get-parent-dataset target-elem)
        ]
    (.preventDefault event)
    (cond
      (.hasOwnProperty dataset "bookId")
          (render-book-details  (get-list-item BookList (aget dataset "bookId")))
      (.hasOwnProperty dataset "authorId")
          (render-author-details (get-list-item AuthorList (aget dataset "authorId")))
      (.hasOwnProperty dataset "reviewId")
          (render-review-details (get-list-item ReviewList (aget dataset "reviewId")))
      (.hasOwnProperty dataset "categoryId")
          (render-category-details (get-list-item CategoryList (aget dataset "categoryId")))
      (.hasOwnProperty parent-dataset "categoriesForBookId")
          (render-categories-for-book (get-list-item BookList (aget parent-dataset "categoriesForBookId")))
      (.hasOwnProperty parent-dataset "reviewId")
          (render-review-details (get-list-item ReviewList (aget parent-dataset "reviewId")))
      (.hasOwnProperty parent-dataset "reviewsForBookId")
          (render-reviews-for-book (get-list-item BookList (aget parent-dataset "reviewsForBookId")))
      (.hasOwnProperty parent-dataset "authorId")
          (render-author-details (get-list-item AuthorList (aget parent-dataset "authorId")))
    )
  )
)

(defn menu-listener [event]
  (let
    [ target-elem (. event -target) target-id (. target-elem -id)]
    (cond
      (= target-id "author-list-menu-item") (render-author-list)  
      (= target-id "book-list-menu-item") (render-book-list)
      (= target-id "category-list-menu-item") (render-category-list)
      (= target-id "review-list-menu-item") (render-review-list)
    )
   )  
)

(defn import-books [raw-list]
  (doseq [b raw-list]
    (addBook b)
  )
  (render-book-list)
)

(defn import-list [raw-list add-function]
  (doseq [i raw-list]
    (add-function i)
  )
)

(defn add-router []
  (let [ 
        report (by-id "report")
        menu (by-id "footer-menu")
      ]
      (.addEventListener report "click" list-click-listener true)
      (.addEventListener menu "click" menu-listener)
  )
)

(defn ajax-response-handler [content]
  
  (let [
        targ (aget content "currentTarget")
        readyState (aget targ "readyState")
        resp (aget targ "response")
        readyState-ok (= readyState 4)
        ]
  
      (if readyState-ok  
        (if (= (aget targ "status") 200 )
          (do
            (def raw_list  (.parse js/JSON resp))
            (cond
              (.hasOwnProperty raw_list "books") 
                 (import-books (array-seq (aget raw_list "books")))
              (.hasOwnProperty raw_list "authors") (import-list (array-seq (aget raw_list "authors")) addAuthor) 
              (.hasOwnProperty raw_list "reviews") (import-list (array-seq (aget raw_list "reviews") ) addReview ) 
              (.hasOwnProperty raw_list "categories") (import-list (array-seq (aget raw_list "categories") ) addCategory ) 
              (.hasOwnProperty raw_list "book_categories") (import-list (array-seq (aget raw_list "book_categories") ) addBookCategory ) 
            )
          )
        )
      )
  )
)

(defn doget [request-type url handler-function ]
  (let [x  (js/XMLHttpRequest.)  ]

    (aset  x "onreadystatechange" handler-function )
    (.open x request-type url)
    (.setRequestHeader x "Content-Type" "application/json" )
    (.setRequestHeader x "Accept" "application/json" )
    (.send x)
  )
)

(defn doc-ready-handler []
  (let[ ready-state (. js/document -readyState)]
    (if (= "complete" ready-state)
      (do
        (add-router)
        (doget "GET" "/books" ajax-response-handler)
        (doget "GET" "/authors" ajax-response-handler)
        (doget "GET" "/reviews" ajax-response-handler)
        (doget "GET" "/categories" ajax-response-handler)
        (doget "GET" "/book_categories" ajax-response-handler)
      )
    )
  )
)

(defn on-doc-ready []
  (aset  js/document "onreadystatechange" doc-ready-handler )  
)

(on-doc-ready)
