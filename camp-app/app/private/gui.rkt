#lang racket/base

;; Shared GUI components: log textbox, dialogs, mixins

(require racket/gui
         camp/log
         (only-in camp/private/log use-color?)
         camp/private/ansi
         racket/class
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/string)

(use-color? #t)

(provide dragdrop-mix
         make-mix-close
         ?dialog
         mono-font
         log-msg
         log-output-textbox
         log-output-textbox%)

(define mono-font (make-object font% 12 "Menlo" 'modern))

;; ============================================================================
;; Logging helper

(define (log-msg fmt . args)
  (log-camp-info (apply format fmt args)))

;; ============================================================================
;; Log output textbox
;;
;; A styled terminal-like text display that shows log messages from the camp logger

(define monitor-thread
  (thread
   (λ ()
     (define listener (make-log-receiver camp-logger 'info))
     (let loop ()
       (sync/enable-break
        (handle-evt listener
                    (λ (vec)
                      (console-print (string-trim (vector-ref vec 1) "camp: " #:right? #f))
                      (loop))))))))

(define max-text-size 10240)
(define max-buffer-size 1000)

(define/obs @log-buffer '("Welcome to Camp!\n"))

(define (console-print str)
  (obs-update!
   @log-buffer
   (λ (buffer-lst)
     (define next-buffer (cons (string-append str "\n") buffer-lst))
     (if (> (length next-buffer) max-buffer-size)
         (take next-buffer max-buffer-size)
         next-buffer))))

(define log-output-textbox%
  (class* object% (view<%>)
    (init-field @buffer)
    (super-new)

    (define/public (dependencies)
      (list @buffer))

    (define/public (create parent)
      (define editor
        (new (class text%
               (super-new)
               (define allow-change? #f)
               (define/public (begin-allow-change)
                 (set! allow-change? #t))
               (define/public (end-allow-change)
                 (set! allow-change? #f))
               (define/augment (can-delete? _start _len)
                 allow-change?)
               (define/augment (can-insert? _start _len)
                 allow-change?))))

      (define s-l (send editor get-style-list))
      (define basic-style (send s-l find-named-style (send editor default-style-name)))
      (define mono-delta (make-object style-delta% 'change-size 11))
      (send mono-delta set-face "Menlo")
      (send mono-delta set-delta-background "Black")
      (send mono-delta set-delta-foreground "LightGray")

      (define mono-style
        (send s-l find-or-create-style basic-style mono-delta))
      (send editor change-style mono-style)

      ;; Style deltas for ANSI colors
      (define (make-fg-delta color-name)
        (define d (make-object style-delta%))
        (send d set-delta-foreground color-name)
        d)
      (define bold-delta
        (let ([d (make-object style-delta%)])
          (send d set-delta-foreground "White")
          (send d set-weight-on 'bold)
          d))
      (define style-deltas
        (hasheq 'red     (make-fg-delta "Red")
                'green   (make-fg-delta "Green")
                'yellow  (make-fg-delta "Yellow")
                'blue    (make-fg-delta "RoyalBlue")
                'magenta (make-fg-delta "Magenta")
                'cyan    (make-fg-delta "Cyan")
                'white   (make-fg-delta "White")
                'dim     (make-fg-delta "Gray")
                'bold    bold-delta
                'bright-green (make-fg-delta "LightGreen")
                'bright-cyan  (make-fg-delta "LightCyan")))

      (define default-delta (make-fg-delta "LightGray"))

      (define (insert-styled str)
        (for ([span (in-list (parse-ansi str))])
          (define text (car span))
          (define style (cdr span))
          (define start (send editor last-position))
          (send editor insert text)
          (define delta (if style (hash-ref style-deltas style default-delta) default-delta))
          (send editor change-style delta start (send editor last-position))))

      (send editor begin-allow-change)
      (insert-styled (string-join (reverse (obs-peek @buffer))))
      (send editor end-allow-change)

      (define canvas
        (new (context-mixin editor-canvas%)
             [parent parent]
             [editor editor]))
      (begin0 canvas
              (send canvas set-canvas-background (make-object color% 0 0 0))
              (send canvas set-context 'buffer (obs-peek @buffer))
              (send canvas set-context 'insert-styled insert-styled)))

    (define/public (update v dep val)
      (case/dep dep
                [@buffer
                 (define old (send v get-context 'buffer))
                 (define todo
                   (reverse
                    (for/list ([chunk (in-list val)])
                      #:break (and (not (null? old)) (eq? chunk (car old)))
                      chunk)))
                 (unless (null? todo)
                   (define editor (send v get-editor))
                   (define insert-styled (send v get-context 'insert-styled))
                   (send editor begin-edit-sequence)
                   (send editor begin-allow-change)
                   (send editor set-position (send editor last-position))
                   (for ([chunk (in-list todo)])
                     (insert-styled chunk))
                   (define last-pos (send editor last-position))
                   (send editor scroll-to-position last-pos #f 'same 'end)
                   (when (> last-pos max-text-size)
                     (send editor delete 0 (quotient max-text-size 2)))
                   (send editor end-allow-change)
                   (send editor end-edit-sequence)
                   (send v set-context 'buffer val))]))

    (define/public (destroy v)
      (send v clear-context))))

(define (log-output-textbox)
  (new log-output-textbox% [@buffer @log-buffer]))

;; ============================================================================
;; Closable dialogs

(define (make-mix-close)
  (define close-proc! void)
  (define (mix %)
    (class %
      (super-new)
      (set! close-proc!
            (lambda ()
              (send this show #f)))))
  (values (λ () (close-proc!)) mix))

(define (?dialog msg)
  (define-values (close! closing-mixin) (make-mix-close))
  (dialog
   #:title msg
   #:mixin closing-mixin
   (vpanel
    (text msg)
    (button "Close" close!))))

;; ============================================================================
;; Drag and drop mixin

(define (dragdrop-mix %)
  (class %
    (super-new)
    (send this accept-drop-files #t)
    (define/override (on-drop-file p)
      (log-msg "Dropped: ~a" (path->string p)))))
