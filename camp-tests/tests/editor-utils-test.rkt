#lang racket/base

;; Tests for the in-app editor's pure helpers

(require rackunit
         rackunit/text-ui
         camp/app/private/editor-utils)

(define editor-utils-tests
  (test-suite
   "Editor helpers"

   (test-case "title shows filename and enclosing folder"
     (check-equal? (editor-title (string->path "/sites/camp-demo/site.rkt") #f)
                   "site.rkt — camp-demo")
     (check-equal? (editor-title (string->path "/sites/camp-demo/blog/first-post.md.rkt") #f)
                   "first-post.md.rkt — blog"))

   (test-case "dirty title gains a bullet"
     (check-equal? (editor-title (string->path "/sites/camp-demo/site.rkt") #t)
                   "• site.rkt — camp-demo"))

   (test-case "file at filesystem root has no folder suffix"
     (check-equal? (editor-title (string->path "/site.rkt") #f) "site.rkt"))

   (test-case "internal editor used only when no external editor is set"
     (check-true (use-internal-editor? ""))
     (check-false (use-internal-editor? "/usr/bin/vim"))
     (check-false (use-internal-editor? "/Applications/BBEdit.app")))

   (test-case "fill-paragraph wraps greedily at the given width"
     (check-equal? (fill-paragraph "aaa bbb ccc ddd" 7) "aaa bbb\nccc ddd")
     (check-equal? (fill-paragraph "aaa bbb ccc" 80) "aaa bbb ccc"))

   (test-case "fill-paragraph rejoins hard-wrapped lines and collapses spaces"
     (check-equal? (fill-paragraph "aaa\nbbb    ccc\n\tddd" 80) "aaa bbb ccc ddd"))

   (test-case "fill-paragraph carries the first line's indent to wrapped lines"
     (check-equal? (fill-paragraph "  aaa bbb ccc" 6) "  aaa\n  bbb\n  ccc"))

   (test-case "fill-paragraph never breaks a word longer than the width"
     (check-equal? (fill-paragraph "aa indivisible bb" 5) "aa\nindivisible\nbb"))

   (test-case "fill-paragraph leaves blank text alone"
     (check-equal? (fill-paragraph "" 10) "")
     (check-equal? (fill-paragraph "   " 10) "   "))

   (test-case "fill-unit wraps a bullet item with hanging indent"
     (check-equal? (fill-unit (vector "- aaa bbb ccc ddd") 0 9)
                   (list 0 0 "- aaa bbb\n  ccc ddd")))

   (test-case "fill-unit hangs ordered-list markers by their width"
     (check-equal? (fill-unit (vector "12. aaa bbb ccc") 0 11)
                   (list 0 0 "12. aaa bbb\n    ccc"))
     (check-equal? (fill-unit (vector "1) aaa bbb") 0 6)
                   (list 0 0 "1) aaa\n   bbb")))

   (test-case "fill-unit never merges adjacent list items"
     (check-equal? (fill-unit (vector "- aaa bbb ccc" "- second") 0 7)
                   (list 0 0 "- aaa\n  bbb\n  ccc"))
     (check-equal? (fill-unit (vector "- one" "- two aaa bbb" "- three") 1 7)
                   (list 1 1 "- two\n  aaa\n  bbb")))

   (test-case "fill-unit from a continuation line fills the whole owning item"
     (check-equal? (fill-unit (vector "- aaa bbb" "lazy ccc") 1 20)
                   (list 0 1 "- aaa bbb lazy ccc")))

   (test-case "fill-unit keeps a nested item's indent"
     (check-equal? (fill-unit (vector "  - aaa bbb ccc") 0 9)
                   (list 0 0 "  - aaa\n    bbb\n    ccc")))

   (test-case "fill-unit keeps blockquote prefixes on every line"
     (check-equal? (fill-unit (vector "> aaa bbb ccc") 0 7)
                   (list 0 0 "> aaa\n> bbb\n> ccc"))
     (check-equal? (fill-unit (vector "> - aaa bbb") 0 7)
                   (list 0 0 "> - aaa\n>   bbb")))

   (test-case "fill-unit on a plain paragraph matches fill-paragraph behavior"
     (check-equal? (fill-unit (vector "aaa bbb ccc ddd") 0 7)
                   (list 0 0 "aaa bbb\nccc ddd"))
     (check-equal? (fill-unit (vector "intro" "" "aaa bbb ccc") 2 80)
                   (list 2 2 "aaa bbb ccc")))

   (test-case "fill-unit refuses structurally unsafe lines"
     (check-false (fill-unit (vector "aaa" "" "bbb") 1 80))
     (check-false (fill-unit (vector "# Heading") 0 80))
     (check-false (fill-unit (vector "```racket") 0 80))
     (check-false (fill-unit (vector "```" "(code aaa bbb ccc ddd)" "```") 1 10))
     (check-false (fill-unit (vector "---") 0 80))
     (check-false (fill-unit (vector "===") 0 80))
     (check-false (fill-unit (vector "| a | b |") 0 80))
     (check-false (fill-unit (vector "#lang punct") 0 80)))

   (test-case "fill-unit refuses the top metadata block but not the body"
     (define doc (vector "#lang punct" "" "---" "title: aaa bbb ccc ddd" "---" "" "body text"))
     (check-false (fill-unit doc 3 10))
     (check-equal? (fill-unit doc 6 80) (list 6 6 "body text")))))

(module+ main
  (run-tests editor-utils-tests))

(module+ test
  (run-tests editor-utils-tests))
