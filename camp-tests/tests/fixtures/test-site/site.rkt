#lang camp/site

title = "Test Site"
url = "https://test.example.com"
founded = 2024-01-15
authors = ["Test Author (test@example.com)"]

sources = ".md.rkt"
static-folder = "static"
output-folder = "publish"

[[collections]]
name = "blog"
source = "blog/*"
output-paths = "blog/[yyyy]/[MM]/*/"
order = "descending"
sort-key = "date"
taxonomies = ["tags", "series"]

[[collections]]
name = "pages"
source = "pages/*"
output-paths = "*/"
sort-key = "title"
order = "ascending"

[[feeds]]
filename = "feed.atom"
collections = ["blog"]
render-with = '(test-site/render feed-content)'
