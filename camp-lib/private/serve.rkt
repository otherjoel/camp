#lang racket/base

;; Static file server for Camp dev server

(require html-printer
         net/mime-type
         net/url
         racket/format
         racket/match
         racket/path
         racket/string
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in static-files: web-server/dispatchers/dispatch-files)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         web-server/dispatchers/dispatch
         web-server/dispatchers/filesystem-map
         web-server/http/request-structs
         web-server/http/response-structs
         web-server/http/response
         web-server/web-server
         "log.rkt"
         "output.rkt")

(provide start-server)

;; ============================================================================
;; Formatted HTML responses

(define (response/html5-xexpr xpr #:code [code 200] #:message [msg #"OK"])
  (response/full code msg (current-seconds) #"text/html; charset=utf-8" null
                 (list (string->bytes/utf-8 (xexpr->html5 xpr #:add-breaks? #t)))))

;; ============================================================================
;; Logging dispatcher

(define (zpad n)
  (cond
    [(= n 0) "00"]
    [(< n 10) (string-append "0" (number->string n))]
    [else (number->string n)]))

(define (simple-time)
  (define now (seconds->date (current-seconds)))
  (format "~a:~a:~a"
          (zpad (date-hour now))
          (zpad (date-minute now))
          (zpad (date-second now))))

(define (apache-log-datetime)
  (define now (seconds->date (current-seconds)))
  (match-define (date* sec min hr day month-num yr _wkday _yrday _dst tz-offset _ns _tzname) now)
  (define month
    (vector-ref #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")
                (sub1 month-num)))
  (format "~a/~a/~a:~a:~a:~a ~a~a~a"
          (zpad day) month yr (zpad hr) (zpad min) (zpad sec)
          (if (negative? tz-offset) "-" "+")
          (zpad (quotient (abs tz-offset) 3600))
          (zpad (/ (remainder (abs tz-offset) 3600) 60))))

(define (log-modern req resp)
  (define method (string-upcase (bytes->string/utf-8 (request-method req))))
  (define path (url->string (request-uri req)))
  (define code (response-code resp))
  (log-camp-info "~a ~a ~a ~a"
                 (simple-time)
                 (~a method #:min-width 4)
                 path
                 code))

(define (log-apache req resp)
  (log-camp-info
   "~a - - [~a] \"~a ~a HTTP/1.1\" ~a -"
   (request-client-ip req)
   (apache-log-datetime)
   (string-upcase (bytes->string/utf-8 (request-method req)))
   (url->string (request-uri req))
   (response-code resp)))

(define (log:make dispatcher #:format [log-format 'modern])
  (define log-fn (if (eq? log-format 'apache) log-apache log-modern))
  (lambda (conn req)
    (define original-handler (current-header-handler))  ; Capture before parameterize
    (with-handlers ([exn:dispatcher? (lambda (e) (next-dispatcher))])
      (parameterize ([current-header-handler
                      (lambda (resp)
                        (define new-resp (original-handler resp))
                        (log-fn req new-resp)
                        new-resp)])
        (dispatcher conn req)))))

;; ============================================================================
;; Directory listings

(define folder-icon
  (~a "data:image/svg+xml;base64,"
      "PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjwhRE9DVFl"
      "QRSBzdmcgUFVCTElDICItLy9XM0MvL0RURCBTVkcgMS4xLy9FTiIgImh0dHA6Ly93d3cudzMub3JnL0dyYX"
      "BoaWNzL1NWRy8xLjEvRFREL3N2ZzExLmR0ZCI+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwM"
      "DAvc3ZnIiBzdHlsZT0iZmlsbDogIzQxODNjNDsiIHZpZXdCb3g9IjAgMCAxNiAxNiIgd2lkdGg9IjE2IiBo"
      "ZWlnaHQ9IjE2IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBhdGggZD0iTTEuNzUgMUExLjc1IDEuNzUgMCAwIDA"
      "gMCAyLjc1djEwLjVDMCAxNC4yMTYuNzg0IDE1IDEuNzUgMTVoMTIuNUExLjc1IDEuNzUgMCAwIDAgMTYgMT"
      "MuMjV2LTguNUExLjc1IDEuNzUgMCAwIDAgMTQuMjUgM0g3LjVhLjI1LjI1IDAgMCAxLS4yLS4xbC0uOS0xL"
      "jJDNi4wNyAxLjI2IDUuNTUgMSA1IDFIMS43NVoiPjwvcGF0aD48L3N2Zz4="))

(define file-icon
  (~a "data:image/svg+xml;base64,"
      "PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iVVRGLTgiIHN0YW5kYWxvbmU9Im5vIj8+CjwhRE9DVFl"
      "QRSBzdmcgUFVCTElDICItLy9XM0MvL0RURCBTVkcgMS4xLy9FTiIgImh0dHA6Ly93d3cudzMub3JnL0dyYX"
      "BoaWNzL1NWRy8xLjEvRFREL3N2ZzExLmR0ZCI+CjxzdmcgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwM"
      "DAvc3ZnIiB2aWV3Qm94PSIwIDAgMTYgMTYiIHN0eWxlPSJmaWxsOmN1cnJlbnRjb2xvciIgd2lkdGg9IjE2"
      "IiBoZWlnaHQ9IjE2IiBhcmlhLWhpZGRlbj0idHJ1ZSI+PHBhdGggZD0iTTIgMS43NUMyIC43ODQgMi43ODQ"
      "gMCAzLjc1IDBoNi41ODZjLjQ2NCAwIC45MDkuMTg0IDEuMjM3LjUxM2wyLjkxNCAyLjkxNGMuMzI5LjMyOC"
      "41MTMuNzczLjUxMyAxLjIzN3Y5LjU4NkExLjc1IDEuNzUgMCAwIDEgMTMuMjUgMTZoLTkuNUExLjc1IDEuN"
      "zUgMCAwIDEgMiAxNC4yNVptMS43NS0uMjVhLjI1LjI1IDAgMCAwLS4yNS4yNXYxMi41YzAgLjEzOC4xMTIu"
      "MjUuMjUuMjVoOS41YS4yNS4yNSAwIDAgMCAuMjUtLjI1VjZoLTIuNzVBMS43NSAxLjc1IDAgMCAxIDkgNC4"
      "yNVYxLjVabTYuNzUuMDYyVjQuMjVjMCAuMTM4LjExMi4yNS4yNS4yNWgyLjY4OGwtLjAxMS0uMDEzLTIuOT"
      "E0LTIuOTE0LS4wMTMtLjAxMVoiPjwvcGF0aD48L3N2Zz4="))

(define css
  (~a "body { font-family: ui-monospace, monospace; }\n"
      "ul { line-height: 1.4; list-style-type: none; }\n"
      "li a {\n"
      "  padding-left: 20px;\n"
      "  text-decoration: none;\n"
      "  background-image: url(\"" file-icon "\");\n"
      "  background-repeat: no-repeat;\n"
      "  background-position: 0px center; }\n"
      "li.folder a { background-image: url(\"" folder-icon "\"); }\n"))

(define root-url
  (url #f #f #f #f #t null null #f))

(define up-url
  (url #f #f #f #f #f (list (path/param 'up null)) null #f))

(define (relative-path-url-to-root p)
  (define simple-path (simplify-path p))
  (define rel-path (find-relative-path (current-directory) simple-path))
  (cond
    [(equal? simple-path rel-path) root-url]
    [else
     (define pp
       (for/list ([d (in-list (explode-path rel-path))])
         (path/param (path->string d) null)))
     (url #f #f #f #f #t pp null #f)]))

(define (make-file-link class url text)
  `(li [[class ,class]]
       (a ([href ,(url->string url)])
          ,text)))

(define (files-list path)
  (for/list ([f (directory-list path #:build? #t)])
    (define name (path->string (file-name-from-path f)))
    (define u (relative-path-url-to-root f))
    (define class
      (if (directory-exists? f) "folder" "file"))
    (make-file-link class u name)))

(define (make-template-xexpr title-string body)
  `(html
    (head
     (title ,title-string)
     (style ,css))
    (body
     (h1 ,title-string)
     (hr)
     ,body)))

(define (directory-lister:make #:url->path url->path)
  (lift:make
   (lambda (req)
     (define-values (path pieces) (url->path (request-uri req)))
     (unless (directory-exists? path)
       (next-dispatcher))
     (define root-path?
       (match pieces [(list 'same ...) #t] [_ #f]))
     (define title-string
       (~a "Directory of "
           (url->string (request-uri req))))
     (response/html5-xexpr
      (make-template-xexpr title-string
                           `(ul ,@(if root-path?
                                      null
                                      (list (make-file-link "folder" up-url "..")))
                                ,@(files-list path)))))))

;; ============================================================================
;; Static file dispatcher helpers

(define (path->headers p)
  (cond
    [(string-suffix? (~a p) ".gz")
     (list (header #"Content-Encoding" #"gzip"))]
    [(or (string-suffix? (~a p) ".atom") (string-suffix? (~a p) ".xml"))
     (list (header #"Content-Type" #"application/xml")
           (header #"x-content-type-options" #"nosniff"))]
    [else '()]))

;; ============================================================================
;; 404 Not Found handler

(define (not-found req)
  (response/html5-xexpr
   #:code 404
   #:message #"Not Found"
   (make-template-xexpr "Error response"
                        '(div (p "Error code: 404")
                              (p "Message: File not found.")))))

;; ============================================================================
;; Main entry point

(define (start-server output-folder
                      #:port [port 8000]
                      #:watch? [watch? #t]
                      #:log-format [log-format 'modern])
  (define base-dir
    (if (path? output-folder)
        output-folder
        (string->path output-folder)))

  (unless (directory-exists? base-dir)
    (error 'start-server "output folder does not exist: ~a" base-dir))

  (define server-url (~a "http://localhost:" port))

  (displayln "")
  (displayln (~a "  " (bold "camp serve")))
  (displayln (~a "  " (dim "Serving") " " (path->string (simplify-path base-dir))))
  (displayln (~a "  " (dim "URL") "     " (cyan server-url)))
  (when watch?
    (displayln (~a "  " (dim "Watching for changes..."))))
  (displayln "")
  (displayln (~a "  " (dim "Press Ctrl+C to stop")))
  (displayln "")

  (define shutdown-server
    (parameterize ([current-directory base-dir])
      (define url->path/current-dir (make-url->path (current-directory)))
      (serve #:port port
             #:dispatch
             (log:make
              (sequencer:make
               (static-files:make
                #:url->path url->path/current-dir
                #:path->mime-type path-mime-type
                #:path->headers path->headers)
               (directory-lister:make #:url->path url->path/current-dir)
               (lift:make not-found))
              #:format log-format))))

  shutdown-server)
