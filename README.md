book-site-clojurescript
=======================

Sample Application - mobile client written in ClojureScript

Note! This the "Single Page" client implementation for the 
"Book Club Site" project. The dev-1.0 branch corresponds to
the Ruby on Rails server implementation located here:
(https://github.com/lorinpa/book-site-ror)

Also see [my blog article "Leveraging ClojureScript."] (http://public-action.org/mob/polyglot-leveraging-clojurescript.html)

For folks who are following my blog, I am tagging the dev-1.0 
branch as proj-v1 "Client for ROR server implementation"

Branch dev-2.0 is the client for the corresponding [book-site-jpa] (https://github.com/lorinpa/book-site-jpa) 
server implementation. The corresponding server is implemented in a stack of Java, Spring, Hibernate,
JPA, H2 database, and Jetty Web Server.


Feature branch "update-to-cljs-version-1.10.x" updates the client to the latest clojure (1.9.0), clojurescript (1.10.x) 
and Java (11) versions. In addition replaces the deprecated test utility with [doo] (https://github.com/bensu/doo).

Add test utility (doo) dependencies:
$> npm install karma karma-firefox-launcher --save-dev
$> npm install karma-cljs-test --save-dev

Run new doo utility test runner
$> lein doo once

Clean build

$> lein clean

Build the client app

$> lein cljsbuild once

  
