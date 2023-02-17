(module snippets

    (snippets::load
     snippets::save
     snippets::export
     snippets::import
     snippets::load-dialog
     snippets::save-dialog
     snippets::manage-dialog)

  (import scheme bintracker-core)

  (define-constant *snippets-version* 1)

  ;; initalize snippets table in bt.db if it does not exist yet
  (define initialize-db
    (let ((initialized #f))
      (lambda ()
	(unless initialized
	  (when (null? (map-row identity btdb
				(string-append
  	  			 "SELECT name FROM sqlite_master "
  	  			 "WHERE type='table' AND name='snippets';")))
	    (execute btdb
		     (string-append "create table snippets "
  				    "(id CHAR(32) PRIMARY KEY, "
				    "name TEXT, contents BLOB, tags TEXT);")))
	  (set! initialized #t)))))

  (define (get-snippet id)
    (initialize-db)
    (let* ((q (prepare btdb "SELECT * FROM snippets WHERE id=?;"))
	   (res (map-row (lambda args args) q id)))
      (finalize! q)
      (and (not (null? res))
	   (car res))))

  (define (store-snippet id name content tags)
    (initialize-db)
    (let ((q (prepare btdb
		      (string-append
		       "INSERT INTO snippets (id, name, contents, tags) "
		       "VALUES (?, ?, ?, ?);"))))
      (if (get-snippet id)
	  (warning (string-append "snippet " id " already exists in database"))
	  (execute q id name content tags))
      (finalize! q)))

  (define (delete-snippets . ids)
    (initialize-db)
    (let ((s (prepare btdb "DELETE FROM snippets WHERE id=?;")))
      (for-each (lambda (id)
		  (execute s id))
		ids)
      (finalize! s)))

;;; Fetch a snippet from the database, where ID is the snippet's md5
;;; identifier.
  (define (snippets::load id)
    (and-let* ((snippet (get-snippet id)))
      (with-input-from-string (caddr snippet) deserialize)))

  ;;; Save a snippet to the database, where NAME is a string naming the snippet,
  ;;; CONTENT is any Scheme object that can be serialized, and TAGS is a comma-
  ;;; separated listing of tag names, passed as a string. Returns an md5 sum
  ;;; that uniquely identifies the snippet's database entry.
  (define (snippets::save name content tags)
    (let* ((content-str (->string content))
	   (tag-str (->string tags))
	   (md5 (string->md5sum
		 (string-append name (->string content) tag-str))))
      (store-snippet md5
		     name
		     (with-output-to-string (lambda () (serialize content)))
		     tag-str)
      md5))

  ;;; Exports the snippet with the md5 identifier ID to the file FILENAME.
  (define (snippets::export id filename)
    (and-let* ((snippet (cons 'bt-snippet
			      (cons *snippets-version* (get-snippet id)))))
      (with-output-to-file filename (lambda () (write snippet)))))

  ;;; Imports a snippet from the file FILENAME into the database.
  (define (snippets::import filename)
    (apply store-snippet
	   (with-input-from-file filename
	     (lambda ()
	       (let ((raw (read)))
		 (unless (eqv? 'bt-snippet (car raw))
		   (error 'snippets::import "Not a bt-snippet file"))
		 (unless (= *snippets-version* (cadr raw))
		   (error 'snippets::import
			  (string-append "Unsupported bt-snippet version "
					 (->string (cadr raw)))))
		 (cddr raw))))))

  (define (command->snippet-tags command)
    (let* ((bits (command-bits command))
	   (bitstr (string-append (number->string bits) "-bit")))
      (filter identity
	      (map (lambda (flag)
		     (case flag
		       ((is-note) "notes")
		       ((is-percussion) "percussion")
		       ((is-pcm-data) (string-append "pcm:" bitstr))
		       ((is-pwm-data) "pwm")
		       ((is-duty) (string-append "duty:" bitstr))
		       ((is-phase) (string-append "phase:" bitstr))
		       ((is-volume) (string-append "volume:" bitstr))
		       (else (and (> bits 0)
				  (not (command-keys command))
				  (string-append "data:" bitstr)))))
		   (filter (lambda (flag)
			     (string-prefix? "is-" (symbol->string flag)))
			   (command-flags command))))))

  (define (suggest-tags)
    (let ((mdef (current 'mdef))
	  (selection (ui-normalized-selection (current 'blockview))))
      (string-intersperse
       (list (string-append (->string (mdef-id mdef)))
	     (string-append (->string (target-platform-id (mdef-target mdef))))
	     ;; TODO handle modifier commands
	     (if (eqv? (cadr selection) (cadddr selection))
		 (string-intersperse
		  (command->snippet-tags
		   (mdef-get-inode-source-command (cadr selection) mdef))
		  ", ")
		 "data:misc"))
       ", ")))

  (define (suggest-name)
    (symbol->string (gensym (mdef-id (current 'mdef)))))

  (define (make-save-dialog-widget)
    (letrec
	((dialog
	  (make <ui-dialog>
	    'title "Save snippet..."
	    'children
	    `((na ,<ui-wrapper> setup
		  ((l0 label text: "Name:  ")
		   (name text bd: 1 highlightthickness: 0 height: 1)))
	      (tg ,<ui-wrapper> setup
		  ((l1 label text: "Tags:  ")
		   (taglist text bd: 1 highlightthickness: 0 height: 4
			    wrap: word))))
	    'traverse '(taglist)
	    'initializers
	    (make-hooks
	     `(configure-text-style
	       .
	       ,(lambda ()
		  (for-each (lambda (widget)
			      (stylize-text-widget (ui-ref dialog widget)))
			    '(taglist name))))
	     `(init
	       .
	       ,(lambda ()
		  ((ui-ref dialog 'name) 'insert 'end (suggest-name))
		  ((ui-ref dialog 'taglist) 'insert 'end (suggest-tags)))))
	    'finalizers
	    (make-hooks
	     `(f
	       .
	       ,(lambda a
		  (snippets::save
		   ((ui-ref dialog 'name) 'get "0.0" 'end)
		   (ui-selected-contents (current 'blockview))
		   ((ui-ref dialog 'taglist) 'get "0.0" 'end))))))))
      dialog))

  (define (snippets::save-dialog)
    (or (and-let* ((bv (current 'blockview))
		   (selection (ui-selection bv)))
	  (ui-show (make-save-dialog-widget))
	  #t)
	(tk/message-box title: "Error"
			message: "Error: Nothing selected"
			detail: (string-append "In order to save a snippet, "
					       "select some data first. ")
			type: 'ok)))

  (define (rank-snippets snippets)
    (let* ((mdef (current 'mdef))
	   (field-id (ui-blockview-get-current-field-id (current 'blockview)))
	   (want-tags
	    (cons (mdef-id mdef)
		  (cons (target-platform-id (mdef-target mdef))
			(command->snippet-tags
			 (mdef-get-inode-source-command field-id mdef))))))
      (sort (map (lambda (snippet)
		   (cons (length
			  (lset-intersection
			   string=?
			   want-tags
			   (map string-trim-both
				(string-split (cadddr snippet) ","))))
			 snippet))
		 snippets)
	    (lambda (x y) (>= (car x) (car y))))))

  (define (make-load-dialog-widget snippets)
    (letrec*
	((ranked-snippets (rank-snippets snippets))
	 (dialog
	  (make <ui-dialog>
	    'title "Load snippet..."
	    'children
	    `((lst ,<ui-wrapper> setup
		   ((selector treeview columns: (ID Name Tags)
			      selectmode: browse show: ()))
		   yscroll #t)
	      (pm ,<ui-wrapper> setup
		  ((lpm label text: "Paste Mode:                ")
		   (pm1 radiobutton text: " Replace")
		   (pm2 radiobutton text: " Porous/under")
		   (pm3 radiobutton text: " Porous/over"))))
	    'traverse '(selector pm1 pm2 pm3)
	    'initializers
	    (make-hooks
	     (cons 'setup-vars
		   (lambda ()
		     (let ((paste-mode (tk-var "pastemode")))
		       ((ui-ref dialog 'pm1)
			'configure variable: paste-mode value: "r")
		       ((ui-ref dialog 'pm2)
			'configure variable: paste-mode value: "u")
		       ((ui-ref dialog 'pm3)
			'configure variable: paste-mode value: "o")
		       (tk-set-var! "pastemode" "r"))))
	     (cons 'list-snippets
		   (lambda ()
		     (let ((selector (ui-ref dialog 'selector)))
		       (selector 'tag 'configure "bestMatch"
				 foreground: (colors 'text-3))
		       (selector 'tag 'configure "goodMatch"
				 foreground: (colors 'text))
		       (selector 'tag 'configure "noMatch"
				 foreground: (colors 'text-inactive))
		       (for-each (lambda (snippet)
				   (selector
				    'insert '{} 'end text: (cadr snippet)
				    values: (list (caddr snippet)
						  (fifth snippet))
				    tags: (cond
					   ((>= (car snippet) 3) "bestMatch")
					   ((> (car snippet) 0) "goodMatch")
					   (else "noMatch"))))
				 ranked-snippets))))
	     (cons 'select-first
		   (lambda ()
		     (focus-first-treeview-item (ui-ref dialog 'selector)))))
	    'finalizers
	    (make-hooks
	     (cons 'do-it
		   (lambda a
		     (let* ((selector (ui-ref dialog 'selector))
			    (id (selector 'item (selector 'focus) text:)))
		       (unless (string-null? id)
			 ((case (car (string->list (tk-get-var "pastemode")))
			    ((#\r) ui-paste)
			    ((#\o) ui-porous-paste-over)
			    ((#\u) ui-porous-paste-under))
			  (current 'blockview)
			  (snippets::load id))))))))))
      dialog))

  (define (snippets::load-dialog)
    (initialize-db)
    (and-let* ((_ (current 'blockview))
	       (snippets (map-row (lambda args args)
				  btdb
				  "SELECT * FROM snippets;"))
	       (_ (not (null? snippets))))
      (ui-show (make-load-dialog-widget snippets))))

  ;; Take a string listing one or more file names as returned by
  ;; tk/get-open-file, and separate it into a list of actual file names. This
  ;; exists mainly to handle filenames with spaces, which tk returns enclosed in
  ;; curly braces.
  (define (filenames->list filenames)
    (letrec ((chop-names
	      (lambda (s)
		(let ((names (string-trim s)))
		  (if (string-null? names)
		      '()
		      (let* ((name-with-spaces? (string-prefix? "{" names))
			     (head (if name-with-spaces?
				       (string-take
					names
					(+ 1 (string-index names #\})))
				       (car (string-split names))))
			     (tail (string-drop names
						(string-length head))))
			(cons (if name-with-spaces?
				  (string-delete (string->char-set "{}") head)
				  head)
			      (chop-names tail))))))))
      (chop-names filenames)))

  (define (import-files)
    (let ((filenames (tk/get-open-file
		      filetypes: '{{{Bintracker Snippets} {.bts}}
				   {{All Files} *}}
		      multiple: 'yes)))
      (unless (string-null? filenames)
	(handle-exceptions
	    exn
	    (report-exception exn (string-append "Failed to import snippet."))
	  (for-each snippets::import (filenames->list filenames))))))

  (define (delete-snippets-graphical selector)
    (and-let*
	((selection-raw (get-treeview-selection selector))
	 (selection (string-split selection-raw))
	 (selection-length (length selection))
	 (confirm
	  (string=? "yes"
		    (tk/message-box title: "Confirm deletion"
				    message: "Confirm deletion "
				    detail:
				    (string-append
				     "Really delete "
				     (number->string selection-length)
				     " snippet"
				     (if (= 1 selection-length) "? " "s? "))
				    type: 'yesno))))
      (apply delete-snippets
	     (map (lambda (item)
		    (wait-for-treeview-item selector item)
		    (selector 'item item text:))
		  selection))))

  (define (export-snippet-graphical selector)
    (let ((selection (get-treeview-selection selector)))
      (cond
       ((not selection)
	(tk/message-box title: "Nothing selected"
			message: "Nothing selected"
			detail: "Select a snippet from the list first. "
			type: 'ok))
       ((> (length (string-split selection)) 1)
	(and-let*
	    ((_ (string=? "yes"
			  (tk/message-box
			   title: "Confirm multi-export"
			   message: "Really export multiple snippets? "
			   detail: "File names will be chosen automatically. "
			   type: 'yesno)))
	     (directory (tk/choose-directory title: "Choose directory..."))
	     (_ (not (string-null? directory))))
	  (for-each (lambda (item)
		      (let ((id (begin (wait-for-treeview-item selector item)
				       (selector 'item item text:))))
			(snippets::export id (string-append
					      directory "/" id ".bts"))))
		    (string-split selection))))
       (else
	(let ((export-id (begin
			   (wait-for-treeview-item selector selection)
			   (selector 'item selection text:)))
	      (filename (tk/get-save-file
			 filetypes: '(((Bintracker Snippets) (.bts)))
			 defaultextension: '.bts)))
	  (unless (string-null? filename)
	    (snippets::export export-id filename)))))))

  (define (make-manage-dialog-widget)
    (letrec*
	((get-snippets (lambda ()
			 (map-row (lambda args args)
				  btdb
				  "SELECT * FROM snippets;")))
	 (fill-selector (lambda (selector snippets)
			  (for-each (lambda (snippet)
				      (selector
				       'insert '{} 'end text: (car snippet)
				       values: (list (cadr snippet)
						     (cadddr snippet))))
				    snippets)))
	 (get-item-list (lambda (selector)
			  (string-split
  			   (string-delete
  			    (string->char-set "{}\"")
  			    (->string (selector 'children '{}))))))
	 (clear-selector (lambda (selector)
			   (selector 'delete (selector 'children '{}))))
	 (dialog
	  (make <ui-dialog>
	    'title "Manage snippets..."
	    'children
	    `((buttons ,<ui-wrapper> setup
		       ((imp button text: "Import..."
			     command:
			     ,(lambda ()
				(let ((selector (ui-ref dialog 'selector)))
				  (import-files)
				  (clear-selector selector)
				  (fill-selector selector (get-snippets)))))
			(exp button text: "Export..."
			     command:
			     ,(lambda ()
				(export-snippet-graphical (ui-ref dialog
								  'selector))))
			(del button text: "Delete"
			     command:
			     ,(lambda ()
				(let ((selector (ui-ref dialog 'selector)))
				  (delete-snippets-graphical selector)
				  (clear-selector selector)
				  (fill-selector selector (get-snippets)))))))
	      (lst ,<ui-wrapper> setup
		   ((selector treeview columns: (ID Name Tags) show: (tree)))
		   yscroll #t))
	    'traverse '(imp exp del selector)
	    'initializers
	    (make-hooks
	     (cons 'list-snippets
		   (lambda ()
		     (fill-selector (ui-ref dialog 'selector)
				    (get-snippets))))))))
      dialog))

  (define (snippets::manage-dialog)
    (initialize-db)
    (ui-show (make-manage-dialog-widget)))

  ) ;; end module snippets
