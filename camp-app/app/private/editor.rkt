#lang racket/base

;; In-app editor: one syntax-highlighted window per source file.
;;
;; racket:text% colors any #lang whose reader supplies a 'color-lexer (punct,
;; camp/site, camp/book, racket…) by dispatching off the buffer's #lang line,
;; so no per-filetype configuration is needed here.

(require framework
         racket/class
         racket/gui
         racket/gui/easy
         racket/gui/easy/operator
         racket/list
         racket/path
         racket/string
         ;; must be a static require: parent-frame is a local member name,
         ;; which set-field! needs at compile time
         (only-in drracket-vim-tool/private/text vim-emulation-mixin parent-frame)
         camp/app/private/complete
         camp/app/private/editor-utils
         camp/app/private/gui
         (only-in camp/app/private/settings @vim-mode @fill-column @line-numbers?))

(provide open-editor!)

;; ============================================================================
;; Text class

(define camp-editor-keymap
  (let ([km (new keymap:aug-keymap%)])
    (send km add-function "camp:save"
          (λ (ed _evt) (send ed save-file #f 'text)))
    (send km add-function "camp:close-window"
          (λ (ed _evt) (send (send ed get-top-level-window) close-editor-window!)))
    (send km add-function "camp:find"
          (λ (ed _evt) (send (send ed get-top-level-window) focus-find-field!)))
    (send km add-function "camp:fill-paragraph"
          (λ (ed _evt) (send ed fill-paragraph!)))
    (send km map-function "d:s" "camp:save")
    (send km map-function "c:s" "camp:save")
    (send km map-function "d:w" "camp:close-window")
    (send km map-function "d:f" "camp:find")
    (send km map-function "d:j" "camp:fill-paragraph")
    km))

(define completion-idle-ms 2000)

(define (camp-editor-mixin %)
  (class %
    (init-field @dirty? on-save-cb)
    (inherit get-filename get-text compute-racket-amount-to-indent)
    (super-new)

    (define/public (refresh-completions)
      (define fn (get-filename))
      (when fn (request-completions! fn (get-text))))
    (define completion-timer
      (new timer% [notify-callback (λ () (refresh-completions))]))

    ;; Langs like punct publish an indenter via get-info; framework auto-wires
    ;; only the color lexer, so the indenter is looked up here
    (define lang-indenter #f)
    (define/public (refresh-lang-info!)
      (define info
        (with-handlers ([(λ (_) #t) (λ (_) #f)])
          (read-language (open-input-string (get-text)) (λ () #f))))
      (set! lang-indenter (and info (info 'drracket:indentation #f))))
    (define/augment (compute-amount-to-indent pos)
      (or (and lang-indenter (lang-indenter this pos))
          (compute-racket-amount-to-indent pos)))

    (define/override (set-modified mod?)
      (@dirty? . := . (and mod? #t))
      (super set-modified mod?))
    (define/augment (after-save-file success?)
      (when success?
        (refresh-completions)
        (refresh-lang-info!)
        (on-save-cb (get-filename)))
      (inner (void) after-save-file success?))
    (define/augment (after-insert start len)
      (send completion-timer start completion-idle-ms #t)
      (inner (void) after-insert start len))
    (define/augment (after-delete start len)
      (send completion-timer start completion-idle-ms #t)
      (inner (void) after-delete start len))
    (define/override (get-all-words)
      (define fn (get-filename))
      (remove-duplicates
       (append (if fn (completions-for fn) '())
               (buffer-words (get-text)))))
    (define/override (get-keymaps)
      (cons camp-editor-keymap (super get-keymaps)))

    ;; Hard-wrap the markdown fill unit (list item, quoted or plain
    ;; paragraph) around the caret
    (inherit get-start-position position-paragraph get-top-level-window
             paragraph-start-position paragraph-end-position
             delete insert set-position begin-edit-sequence end-edit-sequence)
    (define/private (status! msg)
      (send (get-top-level-window) set-vim-status-message msg))
    (define/public (fill-paragraph!)
      (define lines (list->vector (regexp-split #rx"\n" (get-text))))
      (define result (fill-unit lines
                                (position-paragraph (get-start-position))
                                (obs-peek @fill-column)))
      (cond
        [(not result)
         (status! "No wrappable paragraph here")]
        [else
         (define start (paragraph-start-position (first result)))
         (define end (paragraph-end-position (second result)))
         (define filled (third result))
         (cond
           [(equal? filled (get-text start end))
            (status! (format "Already wrapped at ~a columns" (obs-peek @fill-column)))]
           [else
            (status! "")
            (begin-edit-sequence)
            (delete start end)
            (insert filled start)
            (end-edit-sequence)])
         (set-position (+ start (string-length filled)))]))))

(define line-number-font
  (send the-font-list find-or-create-font 9 "Menlo" 'modern 'normal 'normal))
(define line-number-gap 8)

;; A small gutter drawn in the text's left padding: smaller dimmed numbers in
;; the standard style's color (so color schemes still work), full-strength on
;; the caret's line, and no number on soft-wrap continuation rows.
;; framework's text:line-numbers-mixin is not used because its drawing
;; methods are sealed inside its unit and cannot be overridden.
(define (camp-line-numbers-mixin %)
  (class %
    (inherit get-dc get-admin get-canvas last-line line-location line-paragraph
             get-visible-line-range position-paragraph
             get-start-position get-end-position
             set-padding invalidate-bitmap-cache in-edit-sequence? get-style-list)
    ;; fields precede super-new: augments below fire during superclass init
    (define show? #t)
    (define padding-left 0)
    (super-new)

    (define/public (show-line-numbers! on?)
      (set! show? on?)
      (setup-gutter!)
      (invalidate-bitmap-cache))
    (define/public (showing-line-numbers?) show?)

    (define/private (setup-gutter!)
      (define dc (get-dc))
      (define new-left
        (cond
          [(and show? dc)
           (define widest (number->string (max 100 (add1 (last-line)))))
           (define-values (w _h _b _s) (send dc get-text-extent widest line-number-font))
           (+ w line-number-gap)]
          [else 0]))
      (unless (= new-left padding-left)
        (set! padding-left new-left)
        (set-padding new-left 0 0 0)))

    (define/augment (after-insert start len)
      (inner (void) after-insert start len)
      (unless (in-edit-sequence?) (setup-gutter!)))
    (define/augment (after-delete start len)
      (inner (void) after-delete start len)
      (unless (in-edit-sequence?) (setup-gutter!)))
    (define/augment (after-edit-sequence)
      (setup-gutter!)
      (inner (void) after-edit-sequence))

    (define/override (on-paint before? dc left top right bottom dx dy draw-caret)
      (super on-paint before? dc left top right bottom dx dy draw-caret)
      (unless before?
        (when (and show? (zero? padding-left)) (setup-gutter!))
        (when (and show? (positive? padding-left))
          (draw-gutter dc dx dy top bottom))))

    (define/private (draw-gutter dc dx dy top bottom)
      (define saved-font (send dc get-font))
      (define saved-fg (send dc get-text-foreground))
      (define saved-alpha (send dc get-alpha))
      (define saved-mode (send dc get-text-mode))
      (send dc set-font line-number-font)
      (send dc set-text-mode 'transparent)
      (define sl (get-style-list))
      (define std-style (or (send sl find-named-style "Standard") (send sl basic-style)))
      (send dc set-text-foreground (send std-style get-foreground))
      ;; align number baselines with the text baseline
      (define-values (_sw std-h std-desc _se)
        (send dc get-text-extent "0" (send std-style get-font)))
      (define-values (_nw num-h num-desc _ne)
        (send dc get-text-extent "0" line-number-font))
      (define y-offset (- (- std-h std-desc) (- num-h num-desc)))
      (define view-left
        (let ([l (box 0)] [t (box 0)] [w (box 0)] [h (box 0)])
          (send (get-admin) get-view l t w h)
          (unbox l)))
      (define right-x (+ view-left dx padding-left (- line-number-gap)))
      ;; opaque background so text scrolled right doesn't show through
      (define bg (let ([cv (get-canvas)]) (and cv (send cv get-canvas-background))))
      (when bg
        (define saved-pen (send dc get-pen))
        (define saved-brush (send dc get-brush))
        (send dc set-pen bg 1 'transparent)
        (send dc set-brush bg 'solid)
        (send dc draw-rectangle (+ view-left dx) (+ dy top) padding-left (- bottom top))
        (send dc set-pen saved-pen)
        (send dc set-brush saved-brush))
      (define caret-para
        (let ([sp (get-start-position)])
          (and (= sp (get-end-position)) (position-paragraph sp))))
      (define-values (first-line last-vis-line)
        (let ([s (box 0)] [e (box 0)])
          (get-visible-line-range s e)
          (values (unbox s) (unbox e))))
      (for/fold ([prev-para #f]) ([line (in-range first-line (add1 last-vis-line))])
        (define para (line-paragraph line))
        (unless (equal? para prev-para)
          (define label (number->string (add1 para)))
          (define-values (w _h _b _s) (send dc get-text-extent label))
          (send dc set-alpha (if (equal? para caret-para) saved-alpha (* saved-alpha 0.45)))
          (send dc draw-text label (- right-x w) (+ dy (line-location line) y-offset)))
        para)
      (send dc set-alpha saved-alpha)
      (send dc set-text-foreground saved-fg)
      (send dc set-text-mode saved-mode)
      (send dc set-font saved-font))))

;; Vim's command mode matches raw key codes, so ⌘-shortcuts would otherwise be
;; parsed as vim commands (⌘S as `s`); give them to the camp keymap first
(define (command-keys-mixin %)
  (class %
    (super-new)
    (define/override (on-local-char e)
      (unless (and (send e get-meta-down)
                   (send camp-editor-keymap handle-key-event this e))
        (super on-local-char e)))))

(define camp-editor-text%
  (command-keys-mixin
   (vim-emulation-mixin
    (text:searching-mixin
     (camp-line-numbers-mixin
      (editor:autoload-mixin
       (camp-editor-mixin racket:text%)))))))

;; ============================================================================
;; Editor windows

(define open-editors (make-hash))

(define (open-editor! path #:on-save [on-save void])
  (define key (simple-form-path path))
  (cond
    [(hash-ref open-editors key #f)
     => (λ (r)
          (define frame (renderer-root r))
          (send frame show #t)
          (send frame focus))]
    [else (open-new-editor! key on-save)]))

(define (confirm-close! ed frame)
  (or (not (send ed is-modified?))
      (case (message-box/custom
             "Unsaved Changes"
             (format "Save changes to ~a?" (file-name-from-path (send ed get-filename)))
             "Save" "Discard" "Cancel"
             frame
             '(default=1 caution))
        [(1) (send ed save-file #f 'text)
             (not (send ed is-modified?))]
        [(2) #t]
        [else #f])))

(define (find-editor-canvas w)
  (cond
    [(is-a? w editor-canvas%) w]
    [(is-a? w area-container<%>)
     (for/or ([c (in-list (send w get-children))])
       (find-editor-canvas c))]
    [else #f]))

(define (open-new-editor! key on-save)
  (define/obs @dirty? #f)
  (define/obs @status "")
  (define/obs @find-text "")
  (define @title (obs-map @dirty? (λ (d) (editor-title key d))))
  (define ed (new camp-editor-text% [@dirty? @dirty?] [on-save-cb on-save]))
  (send ed load-file key 'text)
  (send ed refresh-completions)
  (send ed refresh-lang-info!)
  (send ed show-line-numbers! (obs-peek @line-numbers?))
  ;; wide enough to show a full wrap-column line plus gutter, insets, scrollbar
  (define window-width
    (let* ([sl (editor:get-standard-style-list)]
           [font (send (or (send sl find-named-style "Standard") (send sl basic-style))
                       get-font)]
           [dc (new bitmap-dc% [bitmap (make-bitmap 1 1)])])
      (define-values (cw _h _d _e) (send dc get-text-extent "0" font))
      (max 760 (inexact->exact (ceiling (+ (* cw (+ (obs-peek @fill-column) 4)) 110))))))
  (define r #f)
  (define find-field (box #f))

  (define (update-search! s)
    (send ed set-searching-state (and (non-empty-string? s) s) #f #f))
  (define (find-next!)
    (define str (obs-peek @find-text))
    (when (non-empty-string? str)
      (define hit
        (or (send ed find-string str 'forward (send ed get-end-position) 'eof #t #f)
            (send ed find-string str 'forward 0 'eof #t #f)))
      (when hit
        (send ed set-position hit (+ hit (string-length str))))))
  (define (focus-editor!)
    (define c (find-editor-canvas (renderer-root r)))
    (when c (send c focus)))

  (define (vim-sync on?)
    (queue-callback
     (λ ()
       (cond
         [on? (send ed on-initialization)]
         [else (send ed turn-off-vim-effects!)
               (@status . := . "")]))))
  (define (line-numbers-sync on?)
    (queue-callback (λ () (send ed show-line-numbers! on?))))

  (define (editor-window-mixin %)
    (class %
      (super-new)
      (define/public (vim?) (obs-peek @vim-mode))
      (define/public (set-vim-status-message s) (@status . := . s))
      ;; Stubs for the vim tool's tab/window ex-commands
      (define/public (next-tab) (void))
      (define/public (prev-tab) (void))
      (define/public (close-current-tab) (void))
      (define/public (open-in-new-tab _filename) (void))
      (define/public (move-current-tab-right) (void))
      (define/public (move-current-tab-left) (void))
      (define/public (get-definitions-canvas) (find-editor-canvas this))
      (define/public (get-interactions-canvas) (find-editor-canvas this))
      (define/public (focus-find-field!)
        (cond [(unbox find-field) => (λ (f) (send f focus))]))
      (define/public (close-editor-window!)
        (when (send this can-close?)
          (send this on-close)
          (send this show #f)))
      (define/augment (can-close?)
        (and (confirm-close! ed this) (inner #t can-close?)))
      ;; renderer-destroy is deferred: destroying the renderer from inside
      ;; its own frame's on-close re-enters gui-easy mid-teardown
      (define/augment (on-close)
        (hash-remove! open-editors key)
        (obs-unobserve! @vim-mode vim-sync)
        (obs-unobserve! @line-numbers? line-numbers-sync)
        (queue-callback (λ () (renderer-destroy r)) #f)
        (inner (void) on-close))))

  (set! r
        (render
         (window
          #:title @title
          #:size (list window-width 820)
          #:mixin editor-window-mixin
          (editor-canvas ed #:style '(auto-hscroll auto-vscroll) #:inset '(12 8))
          (hpanel #:stretch '(#t #f)
                  (input @find-text
                         (λ (action s)
                           (@find-text . := . s)
                           (case action
                             [(input) (update-search! s)]
                             [(return) (find-next!)]))
                         #:label "Find:"
                         #:stretch '(#f #f)
                         #:min-size '(240 #f)
                         #:mixin (λ (%)
                                   (class %
                                     (super-new)
                                     (set-box! find-field this)
                                     (define/override (on-subwindow-char rcv ev)
                                       (cond
                                         [(eq? (send ev get-key-code) 'escape)
                                          (@find-text . := . "")
                                          (update-search! "")
                                          (focus-editor!)
                                          #t]
                                         [else (super on-subwindow-char rcv ev)])))))
                  (spacer)
                  (text @status)))))
  (set-field! parent-frame ed (renderer-root r))
  (obs-observe! @vim-mode vim-sync)
  (obs-observe! @line-numbers? line-numbers-sync)
  (when (obs-peek @vim-mode) (send ed on-initialization))
  (hash-set! open-editors key r)
  (log-msg "Editing: ~a" (file-name-from-path key)))
