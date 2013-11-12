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

;; Creates a formatted string of [last name, first name]
;; Used to Author names.
(defn format-name [last first]
  (format "%s, %s" last first ))

;; Selects and returns an HTML element by it's ID attribute
(defn by-id [id]
  (.getElementById js/document (name id)))

;; Appends a parent HTML element with a child element.
;; E.G. <table> parent - <tr> child 
(defn append [parent & children]
  (apply gdom/append parent children) parent)

;; Adds or Sets an HTML attribute
;; E.G. <p id="somevalue"> elem is "p", "id" is key, "somevalue" is value
(defn addAttribute [elem key value]
  (let [  propObj ( gobject/create key value)]
          (gdom/setProperties elem propObj) elem))

;; Prepares data for HTTP transport
;; E.G. replaces space characters with %20
(defn encode [data]
  (js/encodeURIComponent  data)  )

;; Sends an asynchronous request to a remote server. 
;; The "handler-function" is attached to the request (as a reponse callback listener).
;; Sets the request and response content type to JSON.
(defn do-ajax [request-type url handler-function ]
  (let [x  (js/XMLHttpRequest.)  ]
        (aset  x "onreadystatechange" handler-function )
        (.open x request-type url)
        (.setRequestHeader x "Content-Type" "application/json" )
        (.setRequestHeader x "Accept" "application/json" )
        (.send x)))

;; Creates an HTML element which is not attached to the HTML document
(defn create-doc-fragment []
  (.createDocumentFragment js/document))

;; Creates an HTML element. The name parameter sets the elemet type.
;; E.G. "p" creates a "<p></p>".
;; The attr-key and attr-value parameters adds an attribute key/value to the element.
;; E.G. "id" "author-first-name" creates "<p id="author-first-name"></p>"
;; The property-key and property-value parameters add an additional key/value to the element.
;; E.G. "data-authorId" "4" creates <p data-authorId="4"></p>
(defn create-elem 
  ([name]
    (gdom/createElement name))

  ([name attr-key attr-value]
    (let [ elem  (create-elem name) ]
     (addAttribute elem attr-key attr-value)
     elem))
  
  ([name attr-key attr-value property-key property-value]
    (let [ elem  (create-elem name attr-key attr-value) ]
     (aset elem property-key property-value)
     elem))
)

;; Creates a read-only HTML form element.
;; Hard-codes the style attribute to enforce a consistent UI.
;; The elem-type parameter sets the form element's type. 
;; E.G. "text" creates <input type="text">
;; The property-key and property value set, adds a attribute key/value to
;; the form element. E.G. "data-bookId" "1" creates <input type="text" data-bookId="1" disabled="true">
(defn create-read-only-control 
    ([ elem-type ]
    (let [elem  (create-elem elem-type) ]
    (addAttribute elem "disabled" "true")  
    (addAttribute elem "style" "color:blue;")
    elem))

    ([ elem-type propert-key property-value]
    (let [elem  (create-read-only-control elem-type) ]
    (aset elem propert-key property-value)
    elem)) )

;; Creates an HTML form element.
;; The type parameter sets the element type. E.G. "text" create <input type='text'/>
;; The value parameter sets the element's value attribute. E.G. "Twain" creates <input value="Twain"/>
;; The id-value parameter sets the "id" attribute value. E.G. "lastname" creates  <input id="lastname"/>
(defn create-input-elem [type value id-value]
  ( let [ input-control (create-elem "input") ]
    (addAttribute input-control "type" type)
    (addAttribute input-control  "id" id-value)
    (addAttribute input-control "value" value)
    input-control))

;; Creates an HTML form label.
;; The for-value parameter sets the for attribute value. E.G. "firstname" creates <label for="firstname">
;; The text-value parameter append a text node value. E.G. "Twain" creates <label>Twain</label>
(defn create-label-elem [for-value text-value]  
    ( let [ label-elem (create-elem "label")]
      (addAttribute label-elem "for" for-value)
      (append label-elem text-value)
      label-elem
    )
)

;; speciality elements dedicated to pure css and form building

;; Creates an HTML form.
;; Hard codes the PURE UI CSS class names to enforce a consistent UI.
(defn create-form []
  (let [form (create-elem "form")]
      (addAttribute form "class" "pure-form pure-form-aligned")
      form))

(defn create-form-control-group 
  ([ ]
    (let [  div (create-elem "div")]
            (addAttribute div "class" "pure-control-group")
            div))
  ( [label control]
    (let [  div (create-elem "div")]
            (addAttribute div "class" "pure-control-group")
            (append div label)
            (append div control)
            div))
)

(defn create-command-button [command data-key data-value id-value   ]
  (let[  button (create-elem "button")]
      (append button command)
      (addAttribute button "class" "pure-button pure-button-primary")
      (addAttribute button data-key data-value)
      (addAttribute button "id" id-value)
      button))

(defn create-read-only-control-group [text-value id-key label-value]
  (let [  div (create-form-control-group)
          input-elem (create-input-elem "text" text-value  id-key)
          input-label (create-label-elem id-key label-value)]
          (addAttribute input-elem "disabled" "true")  
          (addAttribute input-elem "style" "color:blue;")
          (append div input-label)
          (append div input-elem)
          div)  )

(defn create-input-control-group [text-value id-key label-value]
  (let [  div (create-form-control-group)
          input-elem (create-input-elem "text" text-value  id-key)
          input-label (create-label-elem id-key label-value)]
          (append div input-label)
          (append div input-elem)
          div))

(defn get-input-control-value [id-value]
  (let [  input-elem (by-id id-value) input-val (aget input-elem "value")]
          input-val) )

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

(defrecord Book [ id title author_id])
(def BookList (atom []))

(defrecord Author [ id  first_name last_name])
(def AuthorList (atom []))

(defrecord Review [ id body stars book_id])
(def ReviewList (atom []))

(defrecord Category [ id title ])
(def CategoryList (atom []))

(defrecord BookCategory [ id book_id category_id ])
(def BookCategoryList (atom []))

(defn jsonToBook [jsonObj]
  (let [  id (aget jsonObj "id")  title  (aget jsonObj "title")
         ; author_id (.-author_id jsonObj)
          author_id (aget jsonObj "author_id" )
        ]
    (Book. id title author_id)))

(defn jsonToAuthor [jsonObj]
  (let [  id (aget jsonObj "id") first_name (aget jsonObj "firstName") last_name  (aget jsonObj "lastName")]
    (Author. id  first_name last_name  )))

(defn jsonToReview [jsonObj]
  (let [  id (aget jsonObj "id") 
          book_id (aget jsonObj "book_id") 
          body (aget jsonObj "body") stars  (aget jsonObj "stars")]
    (Review. id body stars book_id)))

(defn jsonToCategory [jsonObj]
  (let [id (aget jsonObj "id") title (aget jsonObj "title")]
    (Category. id title)))

(defn jsonToBookCategory [jsonObj]
  (let [id (aget jsonObj "id") 
        book_id (aget jsonObj "book_id") 
        category_id (aget jsonObj "category_id")]
    (BookCategory. id book_id category_id)))

(defn addBook
  ([id title author_id] 
    (swap! BookList conj (Book. id title author_id)))
  ([jsonObj] (swap! BookList conj (jsonToBook jsonObj)) )
)

(defn addAuthor
  ([id first_name last_name ]  
    (swap! AuthorList conj (Author. id first_name last_name )))
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
  (let [  report (by-id "report") report-details (by-id "report-details") ]
          (.removeChild report report-details)))

;this function looks up a string value
(defn get-list-item-by-str [list id]
  (let [  derefed-list (deref list)
          results (filter ( fn[n]  (= (str (:id n )) id)) derefed-list )]
          (first results)))

;this function returns the first related item
; this function looks up a numeric value
(defn get-list-item-by-id [list id]
  (let [  derefed-list (deref list)
          results (filter ( fn[n]  (=  (:id n ) id)) derefed-list )]
      (first results))
)

;This function is dedicated to negotiating 
;the relationship between Clojure's vector and 
;the generated javascript data structure. The generated js data structure
;includes a "tail" property. "tail" is actually a js array. 
;we extract the js array and perform a js splice. The result is, we've
;removed a item from the "list", In clojure list is a vector of atoms. 
;In js the list is an array of js objects.
;---
;Note! When resetting the compiler option to advance, I needed to tweak 
;this routine and a few others.
;What works is a similar. Extract the array, perform a js/splice, 
;then reset the vector to the spliced array values
(defn remove-item [vector-list item-id]
  (let[   array-instance (into-array (deref vector-list))
          item (get-list-item-by-id vector-list item-id)
          item-index (.indexOf array-instance item)
          end-split (+ item-index 1)]
          (.splice array-instance item-index end-split)
          (reset! vector-list (vec array-instance))))

(defn filter-list-by-field [list field_name field_value]
  (let [  derefed-list (deref list)
          results (filter ( fn[n]  (=  ( field_name  n ) field_value)) derefed-list )]
          results))

(defn remove-dependent-items [list field_name field_value]
  (let [ items-to-delete (filter-list-by-field list (keyword field_name) field_value) ]
       (doseq [item items-to-delete]
          (remove-item list (:id item)))) )


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

(defn not-null [obj] 
  (cond
    (= obj nil) false
    (= obj "undefined") false
    :else true
  ))

(defn elem-children [elem]
  (let [  hasChildren (.hasOwnProperty elem "children")  ]
          (cond 
            (= hasChildren true) true
            (= hasChildren "undefined") false
            (= hasChildren nil) false    
          )))

(defn elem-has-children [elem]
  (if  (not-null elem) (elem-children elem)
    false ))

(defn empty-elem [elem]
  (let [  child-list (aget elem "children")
          child-seq (array-seq child-list)
          cnt (-count child-seq)
          index-vals (range cnt)]
          (doseq [index index-vals]
            (do
                (if (elem-has-children (-nth child-seq index))
                  (empty-elem (-nth child-seq index))
                )
                (gdom/removeNode  (-nth child-seq index))
                (gobject/clear  (-nth child-seq index))
            )
          )))

(defn clear-menu-list []
  (let [  report-details (by-id "report-details")
          entity-menu-details (by-id "entity-menu-details")  ]
          (empty-elem report-details)
          (gdom/removeChildren  report-details)
          (empty-elem entity-menu-details)
          (gdom/removeChildren entity-menu-details)))

;we want an option to specify the li element as the recepient
;of the attr-key value set (in addition to the child). That way complex menu rows
;can be captured as click transmitters
(defn render-menu-link ([elem attr-key attr-value]
    ( let [ li (.createElement js/document "li") a-link (.createElement js/document "a")]
            (addAttribute a-link "href" "#")
            (addAttribute a-link attr-key attr-value)
            (addAttribute li attr-key attr-value)
            (append a-link elem)
            (append li a-link)
            li)))

(defn render-menu-item 
  ([child-elem ]
    (let [ li (.createElement js/document "li") a-link (.createElement js/document "a") ]
      (addAttribute a-link "href" "#")
      (append a-link child-elem)
      (append li a-link)
      li))
  
  ([child-elem  attr-key attr-value]
    (let [ li (.createElement js/document "li") a-link (.createElement js/document "a") ]
      (addAttribute a-link "href" "#")
      (addAttribute a-link attr-key attr-value)
      (append a-link child-elem)
      (append li a-link)
      li))
  
  ([href text attr-key attr-value]
    (let [  li (.createElement js/document "li") a-link (.createElement js/document "a")]
      (addAttribute a-link "href" href)
      (addAttribute a-link attr-key attr-value)
      (aset  a-link  "textContent" text)
      (append li a-link)
      li))
  
  ([href text attr-key attr-value data-attr data-id]
    (let [  li (.createElement js/document "li") a-link (.createElement js/document "a")]
      (addAttribute a-link "href" href)
      (addAttribute a-link attr-key attr-value)
      (addAttribute a-link data-attr data-id)
      (aset  a-link  "textContent" text)
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

(defn render-entity-menu 
  ( [entity]
  (let [ entity-menu-details (by-id "entity-menu-details")
         add-link (render-menu-item "#" "Add"  "id" (format "add-%s-menu-item" entity) )
      ]
      (append entity-menu-details add-link)
  ))

  ( [entity entity-id]
  (let [  entity-menu-details (by-id "entity-menu-details")
          data-attr (format "data-%s-id" entity)
          mod-link (render-menu-item "#" "Modify"  "id" (format "modify-%s-menu-item" entity) data-attr entity-id )
          del-link (render-menu-item "#" "Delete"  "id" (format "delete-%s-menu-item" entity) data-attr entity-id) ]
          (append entity-menu-details mod-link)
          (append entity-menu-details del-link) )))

;; BookCategories is a relationship, not an entity
;; The links for a relationship contain different requirements than an enrity.
;; We use the same html container, but we fill the child elements with some custom attributes
;; An example of an "anchor entity" would be a Book. We want to add a BookCategory to a specific book (
;; E.G. assign "Be Cool" as a "Comedy"). We would need the id for "Be Cool" just to render the "add new book category" dialog.
(defn render-relationship-menu
   ( [relationship anchor-entity anchor-entity-id]
  (let [  entity-menu-details (by-id "entity-menu-details")
          add-link (render-menu-item "#" "Add"  "id" (format "add-%s-menu-item" relationship) 
            (format "data-%s-id" anchor-entity) anchor-entity-id)]
          (append entity-menu-details add-link) ))

  ( [entity entity-id]
  (let [  entity-menu-details (by-id "entity-menu-details")
          data-attr (format "data-%s-id" entity)
          mod-link (render-menu-item "#" "Modify"  "id" (format "modify-%s-menu-item" entity) data-attr entity-id )
          del-link (render-menu-item "#" "Delete"  "id" (format "delete-%s-menu-item" entity) data-attr entity-id) ]
          (append entity-menu-details mod-link)
          (append entity-menu-details del-link) )))

(defn table-elem [] 
    (let [  table-elem (create-elem "table") ]
            (addAttribute table-elem "class" "review pure-table")
            table-elem))

(defn message-table-row 
  ([message]
  (let [ tr (create-elem "tr") td (create-elem "td") ]
         (aset td "textContent" message)
         (append tr td)
         tr ))

  ([message colspan]
  (let [  tr (create-elem "tr") td (create-elem "td") ]
          (addAttribute td "colspan" colspan)
          (aset  td "textContent" message)
          (append tr td)
          tr )))

(defn review-details-td [review-id]
    (let[ details-td (create-elem "td")  button (create-elem "button") ] 
          (addAttribute details-td "align" "right")
          (addAttribute button "class" "pure-button pure-button-c small-button")
          (addAttribute button "data-review-id" review-id)
          (aset  button "textContent" "Details" )
          (append details-td button)
          details-td ))

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
  (let [  tr (create-elem "tr") td (create-elem "td") ]
          (aset td "textContent" book-title )
          (addAttribute td "colspan" col-span)
          (append tr td)
          tr  )  )

(defn author-row [last_name first_name author-id]
  (let[ tr (create-elem "tr") label-td (create-elem "td")
        name-td (create-elem "td" )]
        (aset  name-td "textContent" (format-name last_name first_name))
        (aset  label-td "textContent" "Author:")
        (addAttribute name-td "data-author-id" author-id)
        (append tr label-td)
        (append tr  name-td)
        tr))

(defn review-header [ book-title num-stars review-id]
  (let [  table (table-elem) title-row (book-tr book-title 3)
          rating-row (review-row num-stars review-id)]
          (append table title-row)
          (append table rating-row) 
          table))


(defn no-data-message-row [table  message ]
  (let [  childNodes (aget table "childNodes") num_nodes (aget  childNodes "length")]
          (if (= 0 num_nodes)
            (append table (message-table-row message)))))

(defn no-data-message [fragment label-text message ]
  (let[ childNodes (aget fragment "childNodes" ) num_nodes(aget childNodes "length") ]
        (if (= 0 num_nodes)
          (append fragment  (render-menu-item (labeled-row label-text message ))))))

(defn render-book-list[]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document)]
        (set-menu-header "Books")
        (clear-menu-list)
        (render-entity-menu "book")
        (doseq [b (deref BookList)]
          (append fragment (render-menu-item "#" (:title b) "data-book-id"  (:id b) )))
        (append report-details fragment)))

(defn render-book-details [book]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document)
        category-count-elem (labeled-row "# Categories:" (count  (filter-list-by-field BookCategoryList (keyword "book_id") (:id book)) ))
        review-count-elem (labeled-row "# Reviews:" (count  (filter-list-by-field ReviewList (keyword "book_id") (:id book)) ))
        author (get-list-item-by-id AuthorList (:author_id book))
        author-elem (labeled-row "Author:"  (format-name  (:last_name author) (:first_name author)) )  ]
        (set-menu-header (:title book))
        (clear-menu-list)
        (render-entity-menu "book" (:id book))
        (append fragment (render-menu-link author-elem "data-author-id" (:author_id book) ))
        (append fragment (render-menu-link category-count-elem "data-categories-for-book-id" (:id book) ))
        (append fragment (render-menu-item review-count-elem "data-reviews-for-book-id" (:id book) ))
        (append report-details fragment) ))

(defn render-author-list 
  "Displays an Add (author) link in the top menu.
   Dsplays a list of author names in the main contain area.
   Author names are formatted as last_name, first_name
  "
  []
  (let[ report-details  (by-id "report-details") 
        fragment (.createDocumentFragment js/document)]
        (clear-menu-list)
        (render-entity-menu "author")
        (set-menu-header "Authors")
        (doseq [author (deref AuthorList)]
          (append fragment (render-menu-item "#" (format-name (:last_name author) (:first_name author) )
            "data-author-id"  (:id author) )) )
        (append report-details fragment)))

(defn render-author-details [author]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document)
        book-count-elem (labeled-row "# Books:" (:book_count author))]
        (clear-menu-list)
        (render-entity-menu "author" (:id author))
        (set-menu-header (format-name (:last_name author) (:first_name author) ))
        (append fragment (render-menu-item book-count-elem "reviews-by-book" (:id author) ))
        (doseq [b (deref BookList)]
          (if (= (:author_id b) (:id author) )
            (append fragment (render-menu-item "#" (:title b) "data-book-id"  (:id b) ))  ) )
        (append report-details fragment)))

(defn render-review-details [review]
  (let [  report-details  (by-id "report-details")
          table (table-elem)
          book (get-list-item-by-id BookList (:book_id review) )
          author (get-list-item-by-id AuthorList (:author_id book))
          author-tr (author-row (:last_name author) (:first_name author) (:id author) )
          review-tr (review-row (:stars review))
          body-tr (message-table-row (:body review) 2)
          book (get-list-item-by-id BookList (:book_id review))  ]
          (append table author-tr)
          (append table review-tr)
          (append table body-tr)
          (clear-menu-list)
          (render-entity-menu "review" (:id review))
          (set-menu-header (:title book) )
          (append report-details table) ))

;; View of all book reviews. Each book review is displayed in a summary form. If no book reviews
;; are found, displays a "New Reviews Found". Displays "Add" link (to add new review).
(defn render-review-list []
  (let[ report-details  (by-id "report-details") 
        fragment (.createDocumentFragment js/document)
        table (create-elem "table")]
        (clear-menu-list)
        (render-entity-menu "review")
        (set-menu-header "Reviews")
        (doseq [review (deref ReviewList)]
          (append fragment (book-tr (:title (get-list-item-by-id BookList (:book_id review))) 3 ))
          (append fragment (review-row (:stars review) (:id review))))
        (append table fragment)
        (append report-details table)
        (no-data-message table "No Reviews Found." "")
        (append report-details fragment)))

;; View of all categories. If not categories are founs displays "No Categories Found".
;; Displays "Add" link (to add new category)
(defn render-category-list []
  (let[ report-details  (by-id "report-details") 
        fragment (.createDocumentFragment js/document)]
        (clear-menu-list)
        (render-entity-menu "category")
        (set-menu-header "Categories")
        (doseq [category (deref CategoryList)]
          (append fragment (render-menu-item  (:title  category) "data-category-id" (:id category))))
        (no-data-message fragment "No Categories Found." "")
        (append report-details fragment)))

;; View of a single category. Displays link to "Display [CATEGORY] Books", where [CATEGORY] is current category value.
;; We added this link so that we could also add two header links "Modify" and "Delete". That allows the user
;; to modify or delete the current category.
(defn render-category-details [category]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document) id (:id category)]
        (append fragment
          (render-menu-item (format "Display %s Books" (:title category)) "data-books-for-category-id" (:id category)))
        (clear-menu-list)
        (render-entity-menu "category" (:id category))
        (set-menu-header (format "Category - %s"  (:title category)  ))
        (append report-details fragment)) )

;; View of book titles categorized by the value represented in the function parameter "category".
;; If no books found for this category, displays "No books found for this category".
(defn render-books-for-category [category] 
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document) id (:id category)]
        (doseq [i (deref BookCategoryList)]
          (if  (= (:category_id i) id)
            (do
             (doseq [b (deref BookList)]
                (if (= (:book_id i) (:id b) )
                  (append fragment (render-menu-item (:title b) "data-book-id" (:id b)))
                ) ))))
        (clear-menu-list)
        (set-menu-header (format "%s - %s"  (:title category) "Books" ))
        (no-data-message fragment "No books found for this category." "")
        (append report-details fragment)))

;; View of categories assigned to a particular book. If no categories found, displays "No Categories Found for this Book".
;; Function iterates through 2 related lists (BookCategories and Categories). BookCategories tells us the category id. The
;; Category List tells us the category name. Each book can be categorized zero to many times. 
;; Parameter: book - book value
(defn render-categories-for-book [book]
  (let[ report-details  (by-id "report-details")
        fragment (.createDocumentFragment js/document) id (:id book)]
      (doseq [book-category (deref BookCategoryList)]
        (if  (= (:book_id book-category) id)
          (do
             (doseq [category (deref CategoryList)]
                (if (= (:category_id book-category) (:id category) )
                  (append fragment (render-menu-item (:title category) "data-bookCategory-id" (:id book-category)))
                )))) )
      (clear-menu-list)
      (render-relationship-menu "bookCategory" "book" (:id book))
      (set-menu-header (format "Categories for: %s" (:title book) ) "data-book-id" (:id book)  )
      (no-data-message fragment "No Categoried Found for this Book." "")
      (append report-details fragment)))

(defn render-book-category-details [book-category]
  (let[ report-details (by-id "report-details")
        form (create-form)
        book (get-list-item-by-id BookList (:book_id book-category))
        book-group (create-read-only-control-group (:title book) "title" "Book Title")
        category (get-list-item-by-id CategoryList (:category_id book-category))
        category-group (create-read-only-control-group (:title category) "category" "Category") ]
        (clear-menu-list)
        (render-relationship-menu "bookCategory"  (:id book-category))
        (set-menu-header "Book Category"  )
        (append form book-group)
        (append form category-group) 
        (append report-details form) ) )

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

(defn create-error-box []
  (let [  entity-menu-details (by-id "entity-menu-details")
          message-div (create-elem "li" "class" "error-box") ]
          (addAttribute message-div "id" "error-message")
          (append entity-menu-details message-div)))

(defn render-error-response [message]
  (let [  message-div 
            (if (not-null (by-id "error-message")) (by-id "error-message") (create-error-box) )]
          (aset (.-firstChild message-div)   "innerText" message)))

(defn render-http-exception [status] 
  (let [ message (format "Network Exception - HTTP code %d" status)]
  (render-error-response message)))

(defn create-option-elem 
    ( [field-value field-name]
    (let [ op (create-elem "option")]
     (addAttribute op "value" field-value)
     (append op field-name)
     op 
    ))

    ( [field-value field-name select-value]
    (let [ op (create-elem "option")]
     (addAttribute op "value" field-value)
     (append op field-name)
     (if (= field-value select-value)
        (addAttribute op "selected" "selected")
     )
     op 
    ))
)

(defn author-select
  ( []
  (let [select-ctrl (create-elem "select") ]
   (doseq [author (deref AuthorList)]
    (append select-ctrl (create-option-elem (:id author) (format-name (:first_name author) (:last_name author) )))    
   )
   (addAttribute select-ctrl "name" "author-select")
   (addAttribute select-ctrl "id" "author-select-id")
   select-ctrl
  )) 

  ( [select-id]
  (let [select-ctrl (create-elem "select") ]
   (doseq [author (deref AuthorList)]
    (append select-ctrl (create-option-elem (:id author) (format-name (:first_name author) (:last_name author) ) select-id ))    
   )
   (addAttribute select-ctrl "name" "author-select")
   (addAttribute select-ctrl "id" "author-select-id")
   select-ctrl
  ))  
)


(defn book-select
  ( []
  (let [select-ctrl (create-elem "select") ]
   (doseq [book (deref BookList)]
    (append select-ctrl (create-option-elem (:id book)  (:title book)  ))
   )
   (addAttribute select-ctrl "name" "book-select")
   (addAttribute select-ctrl "id" "book-select-id")
   select-ctrl
  )) 

  ( [select-id]
  (let [select-ctrl (create-elem "select") ]
   (doseq [book (deref BookList)]
    (append select-ctrl (create-option-elem (:id book) (:title book)   select-id ))
   )
   (addAttribute select-ctrl "name" "book-select")
   (addAttribute select-ctrl "id" "book-select-id")
   select-ctrl
  ))  
)


(defn stars-select
  ( []
  (let [select-ctrl (create-elem "select") ]
    (append select-ctrl (create-option-elem  0  ""  ) )
    (append select-ctrl (create-option-elem  1  "★ " ))
    (append select-ctrl (create-option-elem  2  "★ ★" ))
    (append select-ctrl (create-option-elem  3  "★ ★ ★ " ))
    (append select-ctrl (create-option-elem  4  "★ ★ ★ ★ " ))
    (append select-ctrl (create-option-elem  5  "★ ★ ★ ★ ★ " ))

   (addAttribute select-ctrl "name" "stars-select")
   (addAttribute select-ctrl "id" "stars-select-id")
   select-ctrl
  )) 

  ( [select-val]
  (let [select-ctrl (create-elem "select") ]
       
    (append select-ctrl (create-option-elem  0  ""  select-val))
    (append select-ctrl (create-option-elem  1  "★ " select-val))
    (append select-ctrl (create-option-elem  2  "★ ★" select-val))
    (append select-ctrl (create-option-elem  3  "★ ★ ★ " select-val))
    (append select-ctrl (create-option-elem  4  "★ ★ ★ ★ " select-val))
    (append select-ctrl (create-option-elem  5  "★ ★ ★ ★ ★ " select-val))
    
  (addAttribute select-ctrl "name" "stars-select")
   (addAttribute select-ctrl "id" "stars-select-id")
   select-ctrl
  )) 

  ( [select-val read-ony]
    (let [select-ctrl (stars-select select-val) ]
        (addAttribute select-ctrl` "disabled" "true")  
        (addAttribute select-ctrl "style" "color:blue;")
      select-ctrl
  ))  
)

(defn category-select
  ( []
  (let [select-ctrl (create-elem "select") ]
   (doseq [category (deref CategoryList)]
    (append select-ctrl (create-option-elem (:id category)  (:title category)  ))
   )
   (addAttribute select-ctrl "name" "category-select")
   (addAttribute select-ctrl "id" "category-select-id")
   select-ctrl
  )) 

  ( [select-id]
  (let [select-ctrl (create-elem "select") ]
   (doseq [category (deref CategoryList)]
    (append select-ctrl (create-option-elem (:id category) (:title category)   select-id ))
   )
   (addAttribute select-ctrl "name" "category-select")
   (addAttribute select-ctrl "id" "category-select-id")
   select-ctrl
  ))  
)

(defn render-delete-book [book-id] 
  (let [  book (get-list-item-by-str BookList book-id)
          author (get-list-item-by-id AuthorList (:author_id book))
          report-details (by-id "report-details")
          form (create-form)
          title-group (create-read-only-control-group (:title book) "title" "Book Title")
          author-group (create-read-only-control-group  (format-name (:first_name author) (:last_name author))  "author" "Author")
          id-elem (create-input-elem "hidden" (:id book) "book-id")
          button (create-command-button "Confirm Delete" "data-submit" "del-book" "del-book")]
        (clear-menu-list) 
        (set-menu-header "Delete Book") 
        (append form id-elem)
        (append form title-group)
        (append form author-group)
        (append form button)
        (append report-details form) ))

(defn render-modify-book [book-id ]
  (let [ report-details (by-id "report-details") 
        form (create-form)
        book (get-list-item-by-str BookList book-id)
        author-select (author-select (:author_id book))
        title-group (create-input-control-group (:title book) "title" "Book Title")
        id-elem (create-input-elem "hidden" (:id book) "book-id")
        button (create-command-button "Submit" "data-submit" "mod-book" "mod-book")
        ]
      (clear-menu-list) 
      (set-menu-header "Modify Book") 
      (append form title-group)
      (append form id-elem)
      (append form author-select)
      (append form button)
      (append report-details form)))


(defn render-add-new-book [] 
  (let [ report-details (by-id "report-details") 
        form (create-form) author-select (author-select)
        title-group (create-input-control-group "" "title" "Book Title")
        button (create-command-button "Submit" "data-submit" "add-book" "add-book") ]
      (clear-menu-list) 
      (set-menu-header "Add New Book") 
      (append form title-group)
      (append form author-select)
      (append form button)
      (append report-details form) ))

(defn render-add-new-author [] 
  (let [ report-details (by-id "report-details") 
        form (create-form) 
        first-name-group (create-input-control-group "" "first_name" "First Name")
        last-name-group (create-input-control-group "" "last_name" "Last Name")
        button (create-command-button "Submit" "data-submit" "add-author" "add-author") ]
      (clear-menu-list) 
      (set-menu-header "Add New Author") 
      (append form first-name-group)
      (append form last-name-group)
      (append form button)
      (append report-details form) ))

(defn render-modify-author [author-id ]
  (let [ report-details (by-id "report-details") 
        form (create-form)
        author (get-list-item-by-str AuthorList author-id)
        first-name-group (create-input-control-group (:first_name author) "first_name" "First Name")
        last-name-group (create-input-control-group (:last_name author) "last_name" "Last Name")
        id-elem (create-input-elem "hidden" (:id author) "author-id")
        button (create-command-button "Submit" "data-submit" "mod-author" "mod-author")
        ]
      (clear-menu-list) 
      (set-menu-header "Modify Author") 
      (append form first-name-group)
      (append form last-name-group)
      (append form id-elem)
      (append form button)
      (append report-details form)))

(defn render-delete-author [author-id] 
  (let [  author (get-list-item-by-str AuthorList author-id)
          report-details (by-id "report-details")
          form (create-form)
          first-name-group (create-read-only-control-group (:first_name author) "first_name" "First Name")
          last-name-group (create-read-only-control-group (:last_name author) "last_name" "Last Name")
          id-elem (create-input-elem "hidden" (:id author) "author-id")
          button (create-command-button "Confirm Delete" "data-submit" "del-author" "del-author")]
        (clear-menu-list) 
        (set-menu-header "Delete Author") 
        (append form id-elem)
        (append form first-name-group)
        (append form last-name-group)
        (append form button)
        (append report-details form) ))


(defn render-add-new-category [] 
  (let [ report-details (by-id "report-details") 
        form (create-form) 
        title-group (create-input-control-group "" "title" "Category Title")
        button (create-command-button "Submit" "data-submit" "add-category" "add-category") ]
      (clear-menu-list) 
      (set-menu-header "Add New Category") 
      (append form title-group)
      (append form button)
      (append report-details form) ))

(defn render-modify-category [category-id ]
  (let [ report-details (by-id "report-details") 
        form (create-form)
        category (get-list-item-by-str CategoryList category-id)
        title-group (create-input-control-group (:title category) "title" "Category Title")
        id-elem (create-input-elem "hidden" (:id category) "category-id")
        button (create-command-button "Submit" "data-submit" "mod-category" "mod-category") ]
      (clear-menu-list) 
      (set-menu-header "Modify Category") 
      (append form title-group)
      (append form id-elem)
      (append form button)
      (append report-details form)))


(defn render-delete-category [category-id] 
  (let [  category (get-list-item-by-str CategoryList category-id)
          report-details (by-id "report-details")
          form (create-form)
          title-group (create-read-only-control-group (:title category) "title" "Category Title")
          id-elem (create-input-elem "hidden" (:id category) "category-id")
          button (create-command-button "Confirm Delete" "data-submit" "del-category" "del-category")]
        (clear-menu-list) 
        (set-menu-header "Delete Category") 
        (append form id-elem)
        (append form title-group)
        (append form button)
        (append report-details form) ))

(defn render-add-new-review [] 
  (let [ report-details (by-id "report-details") 
        form (create-form) 
        book-group (create-form-control-group  (create-label-elem "book-select-id" "Book Title") (book-select))
        stars-group (create-form-control-group (create-label-elem "stars-select-id" "Rating") (stars-select))
        body-group (create-form-control-group (create-label-elem "body" "Review Details") (create-elem "textarea" "id" "body"))
        button (create-command-button "Submit" "data-submit" "add-review" "add-review") ]
      (clear-menu-list) 
      (set-menu-header "Add New Review") 
      (append form book-group)
      (append form stars-group)
      (append form body-group)
      (append form button)
      (append report-details form) ))

(defn render-modify-review [review-id] 
  (let [ report-details (by-id "report-details") 
        review (get-list-item-by-str ReviewList review-id)
        id-elem (create-input-elem "hidden" (:id review) "review-id")
        form (create-form)
        book-group (create-form-control-group  (create-label-elem "book-select-id" "Book Title") 
              (book-select (:book_id review)) )
        stars-group (create-form-control-group (create-label-elem "stars-select-id" "Rating")
           (stars-select (:stars review)))
        body-group (create-form-control-group (create-label-elem "body" "Review Details")
         (create-elem "textarea" "id" "body" "value" (:body review) ) )
        button (create-command-button "Submit" "data-submit" "mod-review" "mod-review") ]
      (clear-menu-list) 
      (set-menu-header "Add New Review") 
      (append form id-elem)
      (append form book-group)
      (append form stars-group)
      (append form body-group)
      (append form button)
      (append report-details form) ))

(defn render-delete-review [review-id] 
  (let [  review (get-list-item-by-str ReviewList review-id)
          book (get-list-item-by-id BookList (:book_id review))
          report-details (by-id "report-details")
          form (create-form)
          book-group (create-read-only-control-group (:title book) "title" "Book Title")
          stars-group (create-form-control-group (create-label-elem "stars-select-id" "Rating")
          (stars-select (:stars review) true))
          body-group (create-form-control-group (create-label-elem "body" "Review Details")
          (create-read-only-control "textarea" "value" (:body review)) )
          id-elem (create-input-elem "hidden" (:id review) "review-id")
          button (create-command-button "Confirm Delete" "data-submit" "del-review" "del-review")]
          (clear-menu-list) 
          (set-menu-header "Delete Review") 
          (append form id-elem)
          (append form book-group)
          (append form stars-group)
          (append form body-group)
          (append form button)
          (append report-details form) ))

(defn render-add-new-book-category [book-id] 
  (let [  report-details (by-id "report-details") 
          form (create-form) 
          book (get-list-item-by-str BookList book-id)
          book-id-elem (create-input-elem "hidden" (:id book) "book-id")
          book-group (create-read-only-control-group (:title book) "title" "Book Title")
          category-group (create-form-control-group (create-label-elem "category-select-id" "Category")
           (category-select))
          button (create-command-button "Submit" "data-submit" "add-bookCategory" "add-bookCategory") ]
      (clear-menu-list) 
      (set-menu-header "Add New Book Category") 
      (append form book-id-elem)
      (append form book-group)
      (append form category-group)
      (append form button)
      (append report-details form) ))

(defn render-modify-book-category [book-category-id] 
  (let [  report-details (by-id "report-details") 
          form (create-form) 
          book-category (get-list-item-by-str BookCategoryList book-category-id)
          id-elem (create-input-elem "hidden" (:id book-category) "book-category-id")
          book (get-list-item-by-id BookList (:book_id book-category))
          book-id-elem (create-input-elem "hidden" (:id book) "book-id")
          book-group (create-read-only-control-group (:title book) "title" "Book Title")
          category-group (create-form-control-group (create-label-elem "category-select-id" "Category")
            (category-select (:category_id book-category))) 
          button (create-command-button "Submit" "data-submit" "mod-bookCategory" "mod-bookCategory") ]
      (clear-menu-list) 
      (set-menu-header "Modify Book Category") 
      (append form id-elem)
      (append form book-id-elem)
      (append form book-group)
      (append form category-group)
      (append form button)
      (append report-details form) ))

(defn render-delete-book-category [book-category-id] 
  (let [  book-category (get-list-item-by-str BookCategoryList book-category-id)
          book (get-list-item-by-id BookList (:book_id book-category))
          category (get-list-item-by-id CategoryList (:category_id book-category))
          report-details (by-id "report-details")
          form (create-form)
          book-group (create-read-only-control-group (:title book) "book" "Book Title")
          category-group (create-read-only-control-group (:title category) "category" "Category")
          id-elem (create-input-elem "hidden" (:id book-category) "book-category-id")
          button (create-command-button "Confirm Delete" "data-submit" "del-bookCategory" "del-bookCategory")]
        (clear-menu-list) 
        (set-menu-header "Delete Book Category") 
        (append form id-elem)
        (append form book-group)
        (append form category-group)
        (append form button)
        (append report-details form) ))

; All the modify functions needed tweaking when I changed the compiler
; option to "advanced". Calles to aset need to be replaced with the 
; pure js calls (set! (. -JSPROP jsobject))
(defn modifyBook [book]
  (let [  book-to-modify (get-list-item-by-id BookList (:id book))  ]
          (set!  (.-title book-to-modify) (:title book))
          (set! (.-author_id book-to-modify) (:author_id book))))

(defn modifyAuthor [author]
  (let [  author-to-modify (get-list-item-by-id AuthorList (:id author))  ]
          (set! (.-first_name  author-to-modify) (:first_name author))
          (set! (.-last_name  author-to-modify) (:last_name author)) ) )

(defn modifyCategory [category] 
  (let [  category-to-modify (get-list-item-by-id CategoryList (:id category))  ]
          (set! (.-title  category-to-modify)  (:title category))))

; Note. With js compile set to advance. Had to replace aset with .-JSFUNC 
(defn modifyReview [review] 
  (let [  review-to-modify (get-list-item-by-id ReviewList (:id review))  ]
          (set! (.-book_id review-to-modify)   (:book_id  review))
          (set! (.-stars review-to-modify) (:stars  review))
          (set! (.-body review-to-modify)  (:body  review))))

; Note! For the modify functions (and JS compile set to advance) , 
; I had to replace aset with the native js call
; Perhaps due to the "stringification" of remote json data? Not sure. But 
(defn modifyBookCategory [book-category] 
  (let [  book-category-to-modify (get-list-item-by-id BookCategoryList (:id book-category))]
          (set! (.-category_id book-category-to-modify)  (:category_id book-category))
          (set! (.-book_id  book-category-to-modify) (:book_id book-category))  ))

(defn deleteAuthor [author]
  (let [ dep-books (filter-list-by-field BookList (keyword "author_id")  (:id author) )  ]  
        (doseq
          [book dep-books] 
          (remove-dependent-items ReviewList "book_id" (:id book))
          (remove-dependent-items BookCategoryList "book_id" (:id book))
        )
        ;; we can't simultaneously use book in the iterator and remove it.
        ;; thus we remobe any books outside of the book iterator
        (remove-dependent-items BookList "author_id" (:id author) )
        (remove-item AuthorList (:id author ))
  ))

(defn deleteBook [book]
  (remove-dependent-items ReviewList "book_id" (:id book))
  (remove-dependent-items BookCategoryList "book_id" (:id book))
  (remove-item BookList (:id book )))

(defn deleteCategory [category]
  (remove-dependent-items BookCategoryList "category_id" (:id category))
  (remove-item CategoryList (:id category )))

(defn deleteReview [review]
  (remove-item ReviewList (:id review )))

(defn deleteBookCategory [book-category]
  (remove-item BookCategoryList (:id book-category )))

(defn process-author-op [status action data]
    (cond
      (= action "ADD") (addAuthor data) 
      (= action "MOD") (modifyAuthor (jsonToAuthor data))
      (= action "DEL") (deleteAuthor (jsonToAuthor data)) )
    (render-author-list))

(defn process-book-op [status action data]
  (let [ book (jsonToBook data) ]
    (cond
      (= action "ADD")
            ; when swithed compiler option to advanced
            ; had to swith this call to send raw json 
            ; just like add author above   
            (addBook data)
      (= action "MOD")  (modifyBook book)
      (= action "DEL")  (deleteBook book)
    ) 
    (render-book-list) ) )

(defn process-category-op [status action data]
    (cond
      (= action "ADD") (addCategory data)
      (= action "MOD") (modifyCategory (jsonToCategory data))
      (= action "DEL")(deleteCategory (jsonToCategory data))
    )
    (render-category-list))

(defn process-review-op [status action data]
    (cond
      (= action "ADD") (addReview data)
      (= action "MOD") (modifyReview (jsonToReview data))
      (= action "DEL") (deleteReview (jsonToReview data))    
    )
    (render-review-list))

(defn process-bookCategory-op [status action data]
  (let [ bookCategory (jsonToBookCategory data)] 
    (cond
      (= action "ADD")  (addBookCategory data)
      (= action "MOD")  (modifyBookCategory bookCategory)
      (= action "DEL")  (deleteBookCategory bookCategory))
    (render-categories-for-book (get-list-item-by-id BookList (:book_id bookCategory)) ) ))

(defn crud-response [json]
  (let [ entity (aget json "entity") status (aget json "status")
         action (aget json "action")  data (aget json "data") ]
      (cond
        (= entity "BOOK") (process-book-op status action data)
        (= entity "AUTHOR") (process-author-op status action data)
        (= entity "CATEGORY") (process-category-op status action data)
        (= entity "REVIEW") (process-review-op status action data)
        (= entity "BOOK-CATEGORY") (process-bookCategory-op status action data)
      )  ) )

;; Server reponse listener. This method listens for a server response. The web browser invokes this
;; method several times for each server response. This method inspects the "readyState" property each time the web browser
;; calls this method. One the "readyState" property cotains a value of 4, this method processes the response. 
;;
;; Checks the "status" property to see whether the response is a HTTP success message or a HTTP error message. 
;;
;; If the response is a HTTP error message, the render-http-exception method is invoked.
;;
;; If the response is a HTTP success message, the json body is then inspected. The json body indicatea whether the message in an 
;; appliction success message or an application error message. 
;;
;; If the message is an application error, the render-error-response method id called.
;;
;; If the message is an application success, then the crud-response method is called. 
(defn crud-response-handler [content]
  (let [  targ (aget content "currentTarget")
          readyState (aget targ "readyState")
          resp (aget targ "response")
          readyState-ok (= readyState 4) ]
          (if readyState-ok  
            (if (= (aget targ "status") 200 )
              (do
                (let [raw  (.parse js/JSON resp)]
                (cond
                  (.hasOwnProperty raw "entity")  (crud-response raw)
                  (.hasOwnProperty raw "EXCEPTION" ) (render-error-response (aget raw "EXCEPTION"))
                ) ) )
                (render-http-exception (aget targ "status"))  
              )
          ) ))

(defn submit-add-author []
  (let [  first-name-val (get-input-control-value "first_name") 
          last-name-val (get-input-control-value "last_name") 
          encoded-first-name  (js/encodeURIComponent first-name-val)  
          encoded-last-name  (js/encodeURIComponent last-name-val)  
          url (format "../rest/add/authors/%s/%s" encoded-first-name encoded-last-name) ]
      (do-ajax "POST" url crud-response-handler) ) )


(defn submit-mod-author []
  (let [  first-name-val (get-input-control-value "first_name") 
          last-name-val (get-input-control-value "last_name")
          author-id (get-input-control-value "author-id") 
          encoded-first-name  (js/encodeURIComponent first-name-val)  
          encoded-last-name  (js/encodeURIComponent last-name-val)  
          url (format "../rest/modify/authors/%d/%s/%s" author-id encoded-first-name encoded-last-name)]
      (do-ajax "PUT" url crud-response-handler) ) )


(defn submit-del-author []
  (let [  author-id (get-input-control-value "author-id") 
          url (format "../rest/delete/authors/%d" author-id) ]
          (do-ajax "DELETE" url crud-response-handler) ) )

(defn submit-add-book []
  (let[ title-elem (by-id "title") 
        author-elem (by-id "author-select-id")
        title-val (aget title-elem "value")
        author-id (aget author-elem "value")
        encoded-title  (js/encodeURIComponent title-val)  
        url (format "../rest/add/book/set/%d/%s" author-id encoded-title) ]
        (do-ajax "POST" url crud-response-handler) ) )

(defn submit-mod-book []
  (let[ title-elem (by-id "title") 
        author-elem (by-id "author-select-id")
        id-elem (by-id "book-id")
        title-val (aget title-elem "value")
        author-id (aget author-elem "value")
        book-id (aget id-elem "value")
        encoded-title  (js/encodeURIComponent title-val)  
        url (format "../rest/modify/book/set/%d/%d/%s" book-id author-id encoded-title) ]
        (do-ajax "PUT" url crud-response-handler) ) )

(defn submit-del-book []
  (let [  id-elem (by-id "book-id")
          book-id (aget id-elem "value")
          url (format "../rest/delete/book/%d" book-id )]
          (do-ajax "DELETE" url crud-response-handler) ))

(defn submit-add-category []
  (let [  title-val (get-input-control-value "title") 
          encoded-title  (js/encodeURIComponent title-val)  
          url (format "../rest/add/category/%s" encoded-title ) ]
          (do-ajax "POST" url crud-response-handler) ) )

(defn submit-mod-category []
  (let[ title-val (get-input-control-value "title") 
        category-id (get-input-control-value "category-id") 
        encoded-title  (js/encodeURIComponent title-val)  
        url (format "../rest/modify/category/%d/%s" category-id encoded-title) ]
        (do-ajax "PUT" url crud-response-handler) ) )

(defn submit-del-category []
  (let[ id-elem (by-id "category-id")
        category-id (aget id-elem "value")
        url (format "../rest/delete/category/%d" category-id ) ]
        (do-ajax "DELETE" url crud-response-handler)))

(defn submit-add-review []
  (let[ body-elem (by-id "body") 
        book-elem (by-id "book-select-id")
        stars-elem (by-id "stars-select-id")
        body-val (aget body-elem "value")
        stars-val (aget stars-elem "value")
        book-id (aget book-elem "value")
        encoded-body  (js/encodeURIComponent body-val)
        url (format "../rest/add/review/%s/%d/%d" encoded-body stars-val book-id) ]
        (do-ajax "POST" url crud-response-handler) ) )

(defn submit-mod-review []
  (let[ review-id (get-input-control-value "review-id") 
        body-elem (by-id "body") 
        book-elem (by-id "book-select-id")
        stars-elem (by-id "stars-select-id")
        body-val (aget body-elem "value")
        stars-val (aget stars-elem "value")
        book-id (aget book-elem "value")
        encoded-body  (js/encodeURIComponent body-val)
        url (format "../rest/modify/review/%d/%s/%d/%d" review-id encoded-body stars-val book-id) ]
        (do-ajax "PUT" url crud-response-handler) ) )

(defn submit-del-review []
  (let [  review-id (get-input-control-value "review-id") 
          url (format "../rest/delete/review/%d" review-id)]
          (do-ajax "DELETE" url crud-response-handler) ) )

(defn submit-add-bookCategory []
  (let [  book-id (get-input-control-value "book-id") 
          category-id (get-input-control-value "category-select-id") 
          url (format "../rest/add/book_category/%d/%d" book-id category-id ) ]
          (do-ajax "POST" url crud-response-handler) ) )

(defn submit-mod-bookCategory []
  (let [  book-category-id (get-input-control-value "book-category-id")
          book-id (get-input-control-value "book-id") 
          category-id (get-input-control-value "category-select-id") 
          url (format "../rest/modify/book_category/%d/%d/%d" book-category-id book-id category-id ) ]
          (do-ajax "PUT" url crud-response-handler) ) )

(defn submit-del-bookCategory []
  (let [  book-category-id (get-input-control-value "book-category-id") 
          url (format "../rest/delete/book_category/%d" book-category-id)]
          (do-ajax "DELETE" url crud-response-handler) ) )

(defn get-parent-dataset [elem]
  (let[ parent  (aget elem "parentElement")
        parent  (aget parent "parentElement")
        dataset (aget parent "dataset")]
        dataset))

(defn process-submit [command] 
  (cond
    (= command "add-book") (submit-add-book)
    (= command "mod-book") (submit-mod-book)
    (= command "del-book") (submit-del-book)

    (= command "add-author") (submit-add-author)
    (= command "mod-author") (submit-mod-author)
    (= command "del-author") (submit-del-author)

    (= command "add-category") (submit-add-category)
    (= command "mod-category") (submit-mod-category)
    (= command "del-category") (submit-del-category)

    (= command "add-review") (submit-add-review)
    (= command "mod-review") (submit-mod-review)
    (= command "del-review") (submit-del-review)

    (= command "add-bookCategory") (submit-add-bookCategory)
    (= command "mod-bookCategory") (submit-mod-bookCategory)
    (= command "del-bookCategory") (submit-del-bookCategory) ))

(defn list-click-listener [ event]
  (let [ target-elem (aget event "target") tag-name (aget target-elem "tagName")
          dataset (aget target-elem "dataset") parent-dataset (get-parent-dataset target-elem) ]
    (.preventDefault event)
    (cond
      (.hasOwnProperty dataset "bookId")
          (render-book-details  (get-list-item-by-str BookList (aget dataset "bookId")))
      (.hasOwnProperty dataset "authorId")
          (render-author-details (get-list-item-by-str AuthorList (aget dataset "authorId")))
      (.hasOwnProperty dataset "reviewId")
          (render-review-details (get-list-item-by-str ReviewList (aget dataset "reviewId")))
      (.hasOwnProperty dataset "categoryId")
          (render-category-details (get-list-item-by-str CategoryList (aget dataset "categoryId")))
      (.hasOwnProperty dataset "booksForCategoryId")
          (render-books-for-category (get-list-item-by-str CategoryList (aget dataset "booksForCategoryId")))
      (.hasOwnProperty dataset "bookcategoryId")
          (render-book-category-details (get-list-item-by-str BookCategoryList (aget dataset "bookcategoryId")))
      (.hasOwnProperty parent-dataset "categoriesForBookId")
          (render-categories-for-book (get-list-item-by-str BookList (aget parent-dataset "categoriesForBookId")))
      (.hasOwnProperty parent-dataset "reviewId")
          (render-review-details (get-list-item-by-str ReviewList (aget parent-dataset "reviewId")))
      (.hasOwnProperty parent-dataset "reviewsForBookId")
          (render-reviews-for-book (get-list-item-by-str BookList (aget parent-dataset "reviewsForBookId")))
      (.hasOwnProperty parent-dataset "authorId")
          (render-author-details (get-list-item-by-str AuthorList (aget parent-dataset "authorId")))
      (.hasOwnProperty dataset "submit") (process-submit (aget target-elem "id"))
    )  ))

(defn menu-listener [event]
  (let [ target-elem (. event -target) target-id (. target-elem -id)]
    (cond
      (= target-id "author-list-menu-item") (render-author-list)  
      (= target-id "book-list-menu-item") (render-book-list)
      (= target-id "category-list-menu-item") (render-category-list)
      (= target-id "review-list-menu-item") (render-review-list)
    )))

;; A dom string map looks like this {bookId: 2}
;; This function would return the entity name "book"
(defn get-dom-string-map-entity-name [dataset]
  (cond
      (.hasOwnProperty dataset "authorId") "author"
      (.hasOwnProperty dataset "bookId") "book"
      (.hasOwnProperty dataset "categoryId") "category"
      (.hasOwnProperty dataset "reviewId") "review"
    )  )

(defn entity-menu-listener [event]
  (let [  target-elem (. event -target) target-id (. target-elem -id)
          dataset (aget target-elem "dataset") ]
    (cond
      (= target-id "add-book-menu-item") (render-add-new-book)
      (= target-id "modify-book-menu-item") (render-modify-book (aget dataset "bookId") )
      (= target-id "delete-book-menu-item") (render-delete-book (aget dataset "bookId") )

      (= target-id "add-author-menu-item") (render-add-new-author)
      (= target-id "modify-author-menu-item") (render-modify-author (aget dataset "authorId") )
      (= target-id "delete-author-menu-item") (render-delete-author (aget dataset "authorId") )

      (= target-id "add-category-menu-item") (render-add-new-category)
      (= target-id "modify-category-menu-item") (render-modify-category (aget dataset "categoryId"))
      (= target-id "delete-category-menu-item") (render-delete-category (aget dataset "categoryId") )
  
      (= target-id "add-review-menu-item") (render-add-new-review)
      (= target-id "modify-review-menu-item") (render-modify-review (aget dataset "reviewId") )
      (= target-id "delete-review-menu-item") (render-delete-review (aget dataset "reviewId") )

      (= target-id "add-bookCategory-menu-item") 
          (if (= (get-dom-string-map-entity-name dataset)  "book")  (render-add-new-book-category (aget dataset "bookId"))   )

      (= target-id "modify-bookCategory-menu-item") (render-modify-book-category (aget dataset "bookcategoryId" ))
      (= target-id "delete-bookCategory-menu-item") (render-delete-book-category (aget dataset "bookcategoryId" ))
    )))

(defn import-books [raw-list]
  (doseq [b raw-list]
    (addBook b) )
  (render-book-list))

(defn import-list [raw-list add-function]
  (doseq [i raw-list]
    (add-function i)))

(defn add-router []
  (let [  report (by-id "report")
          menu (by-id "footer-menu")
          entity-menu-details (by-id "entity-menu-details")]
          (.addEventListener report "click" list-click-listener true)
          (.addEventListener menu "click" menu-listener)
          (.addEventListener entity-menu-details "click" entity-menu-listener) ))

(defn ajax-response-handler [content]
  (let [  targ (aget content "currentTarget")
          readyState (aget targ "readyState")
          resp (aget targ "response")
          readyState-ok (= readyState 4)]
          (if readyState-ok  
            (if (= (aget targ "status") 200 )
              (do
                (let [raw_list  (.parse js/JSON resp)] 
                (cond
                  (.hasOwnProperty raw_list "books")  (import-books (array-seq (aget raw_list "books")))
                  (.hasOwnProperty raw_list "authors") (import-list (array-seq (aget raw_list "authors")) addAuthor) 
                  (.hasOwnProperty raw_list "reviews") (import-list (array-seq (aget raw_list "reviews") ) addReview ) 
                  (.hasOwnProperty raw_list "categories") (import-list (array-seq (aget raw_list "categories") ) addCategory ) 
                  (.hasOwnProperty raw_list "book_categories") (import-list (array-seq (aget raw_list "book_categories") ) addBookCategory ) 
                ))) 
                (render-http-exception (aget targ "status"))  
              )
          )))

(defn doc-ready-handler []
  (let[ ready-state (. js/document -readyState)]
    (if (= "complete" ready-state)
      (do
        (add-router)
        (do-ajax "GET" "../rest/export/book/all" ajax-response-handler)
        (do-ajax "GET" "../rest/export/authors/all" ajax-response-handler)
        (do-ajax "GET" "../rest/export/review/all" ajax-response-handler)
        (do-ajax "GET" "../rest/export/category/all" ajax-response-handler)
        (do-ajax "GET" "../rest/export/book_category/all" ajax-response-handler)
      ))))

(defn on-doc-ready []
  (aset  js/document "onreadystatechange" doc-ready-handler ))

(on-doc-ready)

