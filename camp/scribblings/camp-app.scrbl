#lang scribble/manual

@(require racket/runtime-path
          ;uncomment if you end up using #:style below
          ;scribble/core
          ;scribble/html-properties
          (for-label camp
                     punct/doc
                     racket/base))

@title[#:tag "gui-app"]{Camp Computer: the GUI client for Camp}

@define-runtime-path[app-window]{app.png}

@image[app-window #:scale 0.4 #; #:style #; (style #f (list (attributes '((style . "float: right;")))))]

Camp comes with @onscreen{Camp Computer}, a simple app that hides all the site scaffolding and removes
friction from common operations when you're ready to write. (The app is @emph{not} intended to be an
IDE for designing and developing the site itself.)

@margin-note{Camp Computer is tested on Mac OS; it should work on other platforms, but there may be
bugs.}

It must be installed separately with @tt{raco pkg install camp-app}. On Mac OS, if Camp is installed
in user scope, the GUI app will be copied to your user’s @filepath{Applications} folder automatically.

@section{User interface}

@bold{Page management:} You can double click on individual sources in the right-side pane to edit
them. (You can select your preferred editor by clicking @onscreen{File} menu →
@onscreen{Preferences…}.) You can also right-click a source file to get a context menu with
@onscreen{Edit}, @onscreen{Preview} and @onscreen{Delete} options. Previewing will start the project
server (if it is not already started) and open your browser to the localhost URL for that page.

The toolbar buttons are pretty straightforward:

@itemlist[
 
@item{@onscreen{New page}: Prompts you for metadata for the new page. If the current folder’s
@tech{collection} has any taxonomies defined, you can specify those as well.}
                                                            
 @item{@onscreen{Build site}: Same effect as running @secref["cli-build"].}
 
 @item{@onscreen{Start/Stop Preview}: Starts/stops the local server for previewing the site. While 
 the server is running, any changed files are rebuilt automatically. A new browser tab is opened to
 the localhost URL for the server every time the server is started.}
 
 @item{@onscreen{Publish}: Same effect as running @secref["cli-deploy"].}
 
 ]

@subsection{Site Management}

Camp Computer can manage multiple sites. Click @onscreen{Add site} to add an already-created site to
the site switcher.

To remove a site from the site switcher, click @onscreen{File} menu → @onscreen{Remove this site}.
This does not affect the site’s files, it only eliminates it as an option within the site selector.