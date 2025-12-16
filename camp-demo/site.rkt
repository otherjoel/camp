#lang camp/site

title = "The River City Reader"
url = "https://rivercityreader.example.com"
founded = 1912-07-04
authors = ["Marian Paroo (marian@rivercitylibrary.ia)"]

sources = ".md.rkt"
static-folder = "static"
output-folder = "publish"

default-render = "(camp-demo/render render-page)"

[[collections]]
name = "blog"
source = "blog/*"
output-paths = "blog/[yyyy]/[MM]/*/"
render-with = "(camp-demo/render render-post)"
order = "descending"
sort-key = "date"
taxonomies = ["tags", "series"]

[[collections]]
name = "pages"
source = "pages/*"
output-paths = "*/"
render-with = "(camp-demo/render render-page)"
sort-key = "title"
order = "ascending"

[[feeds]]
filename = "feed.atom"
collections = ["blog"]
render-with = "(camp-demo/feeds feed-content)"
