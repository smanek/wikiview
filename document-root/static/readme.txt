Wikiview - A Wikipedia Network Visualizer

Some of the more important third party libraries I'm using are:
html-template: A templating system, very similar to Perl's HTML Template.
clsql: Provides object relational mapping between CLOS objects and a SQL datastore
cl-ppcre: Perl Combpatible Regex engine
hunchentoot: A Common Lisp HTTP server

And a file-by-file code outline:
wikiview.asd: The package file that lets me easily load and compile my project with one command (require 'wikiview). This serves roughly the same purpose as a Makefile or a debian "dpkg" file.

global.lisp: Global configuration settings. For example, the SQL database's location and various aesthetic prefences.

misc.lisp: Just some generic useful functions I wrote, that don't really fit anywhere else. For exmaple, a 'while' macro and a list shuffling function

db-access.lisp: Some low level DB functions. It loads the data I got from wikipedia into intermediate classes. Also provides query functions for anything I need to lookup.

digraph.lisp: Loads the raw data from the SQL classes into an easy to view/query recursive datastructure (a directed graph) consisting of nodes and edges. Provides a greater level of abstraction, so only db-access has to directly know how the data is stored.

make-graph.lisp: Generates pretty SVG images for a given part of wikipedia (makes heavy use of the functions/classes in digraph.lisp, which in turn uses db-access.lisp).

read-csv.lisp: A finite state machine that can read arbitrary CSV file per RFC 4180. I use it to read the "popularity" file I generated from the SQL database of wikipedia pages. Incidentally, I used this command to the dump the relevant data:
mysql -uwikiview -pmwanza -h mysql.maneks.net wikidata -B -e "select page_title,page_counter from PAGE where page_counter > 10;" | sed 's/\t/","/g;s/^/"/;s/$/"/;s/\n//g' > popularity.csv

autocomplete.lisp: Builds a trie out of the article titles from wikipedia, and allows a user to complete/correct a title name using either simple tree-traveral (for completion) or a bayesian inference engine (for spelling correction).

control.lisp: A few simple convenience functions for starting the server. It builds caches of the templates, builds the autocompletion trie, establishes the connection with the SQL db, and brings up the hunchentoot http server.


Issues: The most vexing issue I encountered was that I had to host the data from wikipedia on a seperate service (dreamhost) from the server (vpslink) since vpslink had insufficient storage for wikipidia (~20GB), while dreamhost had insufficient ram for my site (~100MB). I ran some profiling, and it ends up that to generate my (relatively small graphs) takes an average of ~10 seconds. 9.5 seconds of that time are used talking to the SQL server, and less than .3 seconds are used doing acutal dataprocessing. I could probably cut down the processing time an order of magnitude without too much difficulty, but it didn't seem worthwhile (cut down mean run time from 10 seconds to 9.5 seconds and the expense of code readability and maintainability).