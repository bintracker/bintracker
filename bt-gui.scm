
;; This file is part of Bintracker NG.
;; Copyright (c) utz/irrlicht project 2019
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker GUI abstractions
;; -----------------------------------------------------------------------------


(module bt-gui
    *

  (import scheme (chicken.base)
	  srfi-1 srfi-13
	  defstruct matchable simple-exceptions pstk
	  bt-state bt-types mdal)

  ;; TODO
  ;; In theory, a lot of stuff can be moved into here from core
  ;;
  ;; Menu should be modularized (eg. variable list of menus instead of
  ;; hard-coded, + provide 'add-menu config hook

  ;;; Deduces the "rowheight" setting of `ttk::treeview`. This assumes that
  ;;; the Treeview style has already been configured to use
  ;;; `(settings 'font-mono)` with `(settings 'font-size)`.
  ;;; This is necessary because Tk's `style lookup` command is broken, producing
  ;;; no result ca. 50% of the time.
  (define (get-treeview-rowheight)
    (+ 4 (string->number
	  (tk-eval (string-append "font metrics {-family \""
				  (settings 'font-mono) "\" -size "
				  (number->string (settings 'font-size))
				  "} -linespace")))))

    ;;; Determine how many characters are needed to print values of a given
  ;;; command.
  ;; TODO results should be cached
  (define (value-display-size command-config)
    (match (md:command-type command-config)
      ;; FIXME this is incorrect for negative numbers
      ((or 'int 'uint) (inexact->exact
			(ceiling
			 (/ (log (expt 2 (md:command-bits command-config)))
			    (log (settings 'number-base))))))
      ((or 'key 'ukey) (if (memq 'is_note (md:command-flags command-config))
			   3 (apply max
				    (map (o string-length car)
					 (hash-table-keys
					  (md:command-keys command-config))))))
      ('reference (if (>= 16 (settings 'number-base))
		      2 3))
      ('trigger 1)
      ('string 1)))

  (define (normalize-note-name name)
    (if (string=? "rest" name)
	"==="
	(if (string-contains "#" name)
	    name
	    (let ((name-string-list (string->list name)))
	      (list->string (append (list (car name-string-list) #\-)
				    (cdr name-string-list)))))))

  ;;; Transform an ifield value from MDAL format to tracker display format.
  ;;; Replaces empty values with dots, changes numbers depending on number
  ;;; format setting, and turns everything into a string.
  (define (normalize-field-value val field-id)
    (let* ((command-config (md:config-get-inode-source-command
  			    field-id (current-config))))
      (if (null? val)
	  (list->string (make-list (value-display-size command-config)
  	  			   #\.))
  	  (match (md:command-type command-config)
	    ((or 'int 'uint 'reference)
	     (string-pad (number->string val (settings 'number-base))
			 (value-display-size command-config)
			 #\0))
	    ((or 'key 'ukey) (if (memq 'is_note
				       (md:command-flags command-config))
				 (normalize-note-name val)
				 val))
	    ('trigger "x")
	    ('string val)))))


  (defstruct metatree-state
    cursor-x cursor-y)

  ;;; Abstraction of a metatree tk widget, ie. a speadsheet
  (defstruct metatree
    parent group-id type packframe canvas meta-header column-headers
    block-ids column-ids columns rownums xscroll yscroll mtstate)

  (define (metatree-column-set-tags col)
    (col 'tag 'configure 'active-cell
	 ;; TODO
	 background: (colors 'cursor))
    (col 'tag 'configure 'rowhl-minor
    	 background: (colors 'row-highlight-minor))
    (col 'tag 'configure 'rowhl-major
    	 background: (colors 'row-highlight-major)))

  ;;; {{type}} - either 'block (show an igroup's blocks) or 'order (show iorder)
  (define (init-metatree parent type group-id)
    (let* ((packframe (parent 'create-widget 'frame))
	   (canvas (packframe 'create-widget 'canvas
			      scrollregion: "0 0 1000 1000" bg: (colors 'row)
			      bd: 0 highlightthickness: 0))
	   (block-ids
	    (and (eq? type 'block)
		 (remove (lambda (id)
			   (eq? id (symbol-append group-id '_ORDER)))
			 (md:config-get-subnode-type-ids
			  group-id (current-config) 'block))))
	   (column-ids (if (eq? type 'block)
			   (flatten
			    (map (lambda (block-id)
				   (md:config-get-subnode-ids
				    block-id
				    (md:config-itree (current-config))))
				 block-ids))
			   (md:config-get-subnode-ids
			    (symbol-append group-id '_ORDER)
			    (md:config-itree (current-config)))))
	   (rownums (packframe 'create-widget 'treeview selectmode: 'none))
	   (columns (map (lambda (id)
		       (let ((tree (canvas 'create-widget 'treeview
					   selectmode: 'none)))
			 (tree 'heading "#0"
			       text: (if (eq? type 'block)
					  (symbol->string id)
					  (string-drop (symbol->string id) 2)))
			 (tree 'column "#0" width: 80)
			 ;; FIXME this is ignored - maybe only works for col 1ff
			 (tree 'column "#0" anchor: 'center)
			 (metatree-column-set-tags tree)
			 tree))
		     column-ids)))
      (metatree-column-set-tags rownums)
      (make-metatree
       parent: parent group-id: group-id type: type packframe: packframe
       canvas: canvas
       column-headers: (map (lambda (id)
			      (canvas 'create-widget 'label
				      text: (symbol->string id)))
			    column-ids)
       block-ids: block-ids column-ids: column-ids
       columns: columns rownums: rownums
       xscroll: (parent 'create-widget 'scrollbar orient: 'horizontal
			command: `(,canvas xview))
       yscroll: (packframe 'create-widget 'scrollbar orient: 'vertical
			   command:
			   (lambda args
			     (for-each (lambda (column)
					 (column 'yview 'moveto (cadr args)))
			   	       columns)
			     (rownums 'yview 'moveto (cadr args))))
       mtstate: (make-metatree-state cursor-x: 0 cursor-y: 0))))

  ;;; Pack the given metatree-widget. This only sets up the structure, but does
  ;;; not add any data. You most likely do not want to call this procedure
  ;;; directly, but rather invoke it through `update-order-view` or
  ;;; `update-blocks-view`.
  (define (show-metatree mt)
    (letrec ((canvas (metatree-canvas mt))
	     (tree-rowheight (get-treeview-rowheight))
	     (pack-columns
	      (lambda (columns xpos)
		(if (null? columns)
		    '()
		    (begin
		      (canvas 'create 'window (list xpos 0)
			      anchor: 'nw window: (car columns))
		      (tk/bind (car columns) '<Down> (lambda ()
						       (move-cursor mt 'down)))
		      (tk/bind (car columns) '<Up> (lambda ()
						     (move-cursor mt 'up)))
		      (tk/bind (car columns) '<Left> (lambda ()
						       (move-cursor mt 'left)))
		      (tk/bind (car columns) '<Right>
			       (lambda () (move-cursor mt 'right)))
		      ((car columns) 'configure height: 32)
		      (pack-columns (cdr columns) (+ xpos 80)))))))
      (tk/pack (metatree-xscroll mt) expand: 0 fill: 'x side: 'bottom)
      (tk/pack (metatree-packframe mt) expand: 1 fill: 'both)
      ((metatree-rownums mt) 'column "#0" width: 80)
      (tk/pack (metatree-yscroll mt) fill: 'y side: 'right)
      (tk/pack (metatree-rownums mt) fill: 'y side: 'left)
      (tk/pack canvas expand: 1 fill: 'both side: 'left)
      (pack-columns (metatree-columns mt) 0)
      (canvas 'configure xscrollcommand: (list (metatree-xscroll mt) 'set))
      (tk/bind (metatree-packframe mt) '<Configure>
      	       `(,(lambda (h)
      		    (for-each (lambda (column)
				(column 'configure height:
					(quotient h tree-rowheight)
					yscrollcommand:
					`(,(metatree-yscroll mt) set)))
      			      (metatree-columns mt)))
      		 %h))
      (canvas 'configure scrollregion:
	      (list 0 0 (* 80 (length (metatree-columns mt)))
		    1000))))

  (define (update-row-numbers metatree len padding highlight)
    (for-each (lambda (row)
		((metatree-rownums metatree) 'insert '{} 'end
		 text: (string-pad (number->string row
						   (app-settings-number-base
						    *bintracker-settings*))
				   padding #\0)
		 tags: (if highlight
			   (cond ((= 0 (modulo row 8)) "rowhl-major")
				 ((= 0 (modulo row 4)) "rowhl-minor")
				 (else ""))
			   "")))
	      (iota len)))

  (define (tree-item-list tree)
    (map string->symbol (string-split (tree 'children '{}))))

  (define (nth-tree-item tree index)
    (list-ref (tree-item-list tree) index))

  (define (tree-length tree)
    (length (tree-item-list tree)))

  (define (metatree-length mt)
    (length (tree-item-list (car (metatree-columns mt)))))

  (define (cursor-do mt method)
    (for-each (lambda (tree index)
		(when (= index (metatree-state-cursor-x (metatree-mtstate mt)))
		  (tree 'tag method "cursor-x" (tree-item-list tree))
		  (tree 'tag method "active-cell"
			(nth-tree-item tree (metatree-state-cursor-y
					   (metatree-mtstate mt)))))
		(tree 'tag method "cursor-y"
		      (nth-tree-item tree (metatree-state-cursor-y
					   (metatree-mtstate mt)))))
	      (metatree-columns mt)
	      (iota (length (metatree-column-ids mt))))
    ((metatree-rownums mt) 'tag method "cursor-y"
     (nth-tree-item (metatree-rownums mt)
		    (metatree-state-cursor-y (metatree-mtstate mt)))))

  (define (show-cursor mt)
    (cursor-do mt 'add))

  (define (delete-cursor mt)
    (cursor-do mt 'remove))

  (define (move-cursor mt direction)
    (let ((current-xpos (metatree-state-cursor-x (metatree-mtstate mt)))
	  (current-ypos (metatree-state-cursor-y (metatree-mtstate mt))))
      (delete-cursor mt)
      (match direction
	('up (metatree-state-cursor-y-set!
	      (metatree-mtstate mt)
	      (sub1 (if (= current-ypos 0)
			(metatree-length mt)
			current-ypos))))
	('down (metatree-state-cursor-y-set!
		(metatree-mtstate mt)
		(if (>= (+ 1 current-ypos) (metatree-length mt))
		    0 (add1 current-ypos))))
	('left (metatree-state-cursor-x-set!
		(metatree-mtstate mt)
		(sub1 (if (= current-xpos 0)
			  (length (metatree-columns mt))
			  current-xpos))))
	('right (metatree-state-cursor-x-set!
		 (metatree-mtstate mt)
		 (if (>= (+ 1 current-xpos) (length (metatree-columns mt)))
		     0
		     (add1 current-xpos)))))
      ;; TODO disable focus, see
      ;; https://stackoverflow.com/questions/4299432/in-tkinter-how-do-i-remove-focus-from-a-widget
      (show-cursor mt)))

  (define (update-order-view metatree parent-node-instance-path)
    (letrec ((fill-empty-values
	      (lambda (vals previous)
		(if (null? vals)
		    '()
		    (let ((next-val (if (null? (car vals))
					previous (car vals))))
		      (cons next-val (fill-empty-values (cdr vals)
							next-val))))))
	     (block-values (md:mod-get-block-instance-values
	  		    ((md:node-instance-path
	  		      (string-append parent-node-instance-path "/"
	  				     (symbol->string
	  				      (metatree-group-id metatree))
	  				     "_ORDER/0"))
	  		     (md:mod-global-node (current-mod))))))
      (for-each (lambda (column values field-id)
		  (for-each (lambda (value)
			      (column 'insert '{} 'end
				      text: (normalize-field-value value
								   field-id)))
			    values))
		(metatree-columns metatree)
		(map (lambda (fields) (fill-empty-values fields '()))
		     block-values)
		(md:config-get-subnode-ids
		 (symbol-append (metatree-group-id metatree)
				'_ORDER)
		 (md:config-itree (current-config))))
      (update-row-numbers metatree (length (car block-values))
			  3 #f)))

  (define (update-blocks-view metatree parent-node-instance-path order-pos)
    (let* ((parent-group-instance
	    ((md:node-instance-path parent-node-instance-path)
	     (md:mod-global-node (current-mod))))
	   (block-instance-ids
	    (list-ref (md:mod-get-order-values (metatree-group-id metatree)
					       parent-group-instance
					       (current-config))
		      order-pos))
	   (block-values
	    (concatenate
	     (map (lambda (block-id instance-id)
		    (md:mod-get-block-instance-values
		     ((md:node-instance-path
		       (string-append parent-node-instance-path "/"
				      (symbol->string block-id) "/"
				      (number->string instance-id)))
		      (md:mod-global-node (current-mod)))))
		  (metatree-block-ids metatree)
		  block-instance-ids))))
      (for-each
       (lambda (column values field-id)
	 (for-each (lambda (value rownum)
		     (column 'insert '{} 'end text:
			     (normalize-field-value value field-id)
			     tags: (cond ((= 0 (modulo rownum 8)) "rowhl-major")
					 ((= 0 (modulo rownum 4)) "rowhl-minor")
					 (else ""))))
		   values (iota (length values))))
       (metatree-columns metatree)
       block-values (metatree-column-ids metatree))
      (update-row-numbers metatree (length (car block-values))
			  4 #t)
      (show-cursor metatree)
      (tk/focus (car (metatree-columns metatree)))
      ))

  ) ;; end module bt-gui
