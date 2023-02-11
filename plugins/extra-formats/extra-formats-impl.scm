(module extra-formats

    (extra-formats::zx-tap-basic-loader
     extra-formats::zx-tap
     extra-formats::mmod->zx-tap
     extra-formats::zx-tap-dialog
     extra-formats::dragon-cas
     extra-formats::mmod->dragon-cas
     extra-formats::dragon-cas-dialog)

  (import scheme (chicken base) (chicken port) (chicken io)
	  (chicken bitwise) (chicken condition)
	  srfi-1 srfi-13 coops pstk
	  bt-state bt-types bt-gui mdal schemta)

  (define (int->ascii n #!optional (endianness 'little-endian))
    (int->bytes n 2 endianness))

  (define (show-export-dialog export-proc title filetypes default-extension
			      #!key (features '()))
    (letrec
  	((dialog-widget
  	  (make <ui-dialog>
  	    'title title
  	    'children
  	    (remove
  	     null?
  	     (list `(org-entry
  		     ,<ui-wrapper> setup
  		     ((l1 label text: "Origin address: ")
  		      (org entry
  			   bg: ,(colors 'row-highlight-minor)
  			   fg: ,(colors 'text)
  			   bd: 0
  			   highlightthickness: 0
  			   insertborderwidth: 1
  			   font: ,(list family: (settings 'font-mono)
  					size: (settings 'font-size)
  					weight: 'bold))))
  		   (if (memv 'loader features)
  		       `(loader-cb ,<ui-wrapper> setup
  				   ((l2 label text: "Include loader: ")
  				    (loader checkbutton)))
  		       '())
		   (if (memv 'text features)
		       `(txt-lbl ,<ui-wrapper> setup
				 ((l3 label text: "On-screen text:")))
		       '())
  		   (if (memv 'text features)
  		       `(txt-entry ,<ui-wrapper> setup
  				   ((info-text text takefocus: 0 bd: 0
  					       highlightthickness: 0 height: 8
  					       wrap: word))
  				   yscroll #t)
  		       '())))
  	    'traverse (remove null?
  			      `(org
  				,(if (memv 'loader features) 'loader '())
  				,(if (memv 'text features) 'info-text '())))
  	    'initializers
  	    (make-hooks
  	     `(configure-text-style
  	       .
  	       ,(lambda ()
  		  (when (memv 'loader features)
  		    ((ui-ref dialog-widget 'info-text) 'configure
  		     bg: (colors 'background) fg: (colors 'text)
		     blockcursor: 'yes insertbackground: (colors 'text)
  		     font: (list family: (settings 'font-mono)
  				 size: (settings 'font-size))))))
  	     `(init
  	       .
  	       ,(lambda a
  		  (let ((use-loader (tk-var "useloader"))
  			(mod (current 'mmod)))
		    ((ui-ref dialog-widget 'org) 'insert 'end
		     (number->string (mdef-default-origin (current 'mdef))))
		    (when (memv 'loader features)
		      ((ui-ref dialog-widget 'loader)
		       'configure variable: use-loader)
		      (tk-set-var! "useloader" 1))
  		    (when (memv 'text features)
  		      ((ui-ref dialog-widget 'info-text) 'insert 'end
  		       (string-append
  			"\n "
  			(cddr ((node-path "0/TITLE/0")
			       (mmod-global-node mod)))
  			"\n by "
  			(cddr ((node-path "0/AUTHOR/0")
			       (mmod-global-node mod))))))))))
	    'finalizers
	    (make-hooks
	     `(export-tap
	       .
	       ,(lambda a
		  (handle-exceptions
		      exn
		      (report-exception exn "Something went wrong")
		    (let* ((org
			    (string->number ((ui-ref dialog-widget 'org) 'get)))
			   (use-loader
			    (and (memv 'loader features)
				 (= 1 (string->number
				       (->string (tk-get-var "useloader"))))))
			   (loader-text
			    (and (memv 'text features)
				 ((ui-ref dialog-widget 'info-text)
				  'get "1.0" "end-1c")))
			   (filename
			    (tk/get-save-file*
  			     filetypes: filetypes
  			     defaultextension: default-extension)))
		      (unless (string-null? filename)
			(export-proc filename (current 'mmod)
				     origin: org loader: use-loader
				     text: loader-text))))))))))
      (ui-show dialog-widget)))

  (define (zx-tap-block-header type name data-length #!key param1 param2)
    (let ((raw-data
  	   (append '(#x00) ; flag byte (0 = header block)
  		   `(,(case type
  			((program) 0)
  			((num-array) 1)
  			((char-array 2))
  			((code) 3)
  			(else (error 'extra-formats/zx-tap-block-header
  				     (string-append "Unknown header type "
  						    (->string type))))))
  		   (map char->integer
  			(string->list (string-pad-right name 10 #\space)))
  		   (int->ascii data-length)
  		   (int->ascii (or param1 #x8000))
  		   (int->ascii (or param2 (if (eqv? type 'program)
  					      data-length
  					      #x8000))))))
      (append '(#x13 #x00) ; header block size, always 19 bytes
  	      raw-data
  	      (list (apply bitwise-xor raw-data)))))

  (define (zx-basic-line line-number tokens)
    (append (int->ascii line-number 'big-endian)
  	    (int->ascii (+ 1 (length tokens)))
  	    tokens
  	    (list #xd))) ; newline

  ;;; Create a autostarting, tokenized BASIC loader, which loads machine code
  ;;; at ORIGIN, and optionally prints TEXT to the screen. Returns a list of
  ;;; char.
  (define (extra-formats::zx-tap-basic-loader origin #!optional text)
    (let* ((int->chars
  	    (lambda (n)
  	      (map char->integer (string->list (number->string n)))))
  	   (basic-tokens
  	    (append (list (append '(#xe7 #xc3 #xa7 #x3a ; BORDER NOT PI:
  					 #xda #xc3 #xa7 #x3a ; PAPER NOT PI:
  					 #xd9 #xb0 #x22 #x37 #x22 #x3a
  					; INK VAL "7":
  					 #xfd #xb0 #x22) ; CLEAR VAL "
  				  (int->chars (sub1 origin))
  				  '(#x22)) ; "
  			  '(#xef #x22 #x22 #xaf) ; LOAD ""CODE
  			  '(#xfb)) ; CLS
  		    (if text
  			(map (lambda (line)
  			       (append '(#xf5 #x22) ; PRINT "
  				       (map char->integer (string->list line))
  				       '(#x22 #x3a))) ; ":
  			     (string-split text "\n"))
  			'())
  		    ;; RANDOMIZE USR VAL "nnnnn"
  		    (list (append '(#xf9 #xc0 #xb0 #x22)
  				  (int->chars origin)
  				  '(#x22)))))
  	   (basic-bin (cons #xff ; flag byte
  			    (flatten (map zx-basic-line
  					  (iota (length basic-tokens) 10 10)
  					  basic-tokens)))))
      (map integer->char
  	   (append (zx-tap-block-header 'program "" (sub1 (length basic-bin))
  					param1: 10)
  		   (int->ascii (+ 1 (length basic-bin)))
  		   basic-bin
  		   (list (apply bitwise-xor basic-bin))))))

  ;;; Create the contents of a .tap file from the binary BIN, to be loaded at
  ;;; ORIGIN. BIN must be a list of char. If LOADER is `#t`, then an
  ;;; auto-running Basic loader is prepended, which optionally prints TEXT to
  ;;; the screen.
  (define (extra-formats::zx-tap bin origin #!optional loader text)
    (append (if loader
  		(extra-formats::zx-tap-basic-loader origin text)
  		'())
  	    (map integer->char
  		 (zx-tap-block-header 'code "" (length bin) param1: origin))
  	    (map integer->char
		 (int->bytes (+ 2 (length bin)) 2 'little-endian))
	    (cons (integer->char #xff) bin)
	    (list (integer->char (apply bitwise-xor
  					(cons #xff (map char->integer bin)))))))

  ;;; Compile the MDAL module MOD and write it to a ZX Spectrum .tap file.
  ;;; ORIGIN may name a memory address at which to compile the mod. If LOADER is
  ;;; `#t`, and auto-running Basic loader is included, which optionally prints
  ;;; TEXT to the screen.
  (define (extra-formats::mmod->zx-tap filename mod
  				       #!key (origin (mdef-default-origin
  						      (mmod-mdef mod)))
  				       (loader #t)
  				       text)
    (let ((res (list->string (extra-formats::zx-tap
			      (mod->bin mod origin) origin loader text))))
      (with-output-to-file filename (lambda () (write-string res)) #:binary)))

  ;;; Graphical interface for ZX Spectrum .tap export.
  (define (extra-formats::zx-tap-dialog)
    (when (and (current 'mmod)
  	       (memv (string->symbol
  		      (target-platform-id (mdef-target (current 'mdef))))
  		     '(spectrum16 spectrum48 spectrum128)))
      (show-export-dialog extra-formats::mmod->zx-tap
  			  "ZX Spectrum .tap"
			  '(((ZX Spectrum .tap) (.tap)))
			  '.tap
  			  features: '(loader text))))


  (define (extra-formats::dragon-cas bin origin #!optional loader text)
    ;; https://worldofdragon.org/index.php?title=Tape%5CDisk_Preservation#CAS_File_Format
    ;; TODO check bin2cas https://www.6809.org.uk/dragon/#castools
    (letrec* ((leader (integer->char #x55))
	      (trailer (integer->char #x55))
	      (sync (integer->char #x3c))
	      (blocktype:namefile #\null)
	      (blocktype:data (integer->char 1))
	      (blocktype:eof (integer->char #xff))
	      (file-id:basic #\null)
	      ;; (file-id:data (integer->char 1))
	      (file-id:binary (integer->char 2))
	      (ascii:binary #\null)
	      ;; (ascii:ascii (integer->char #xff))
	      (gap:continuous #\null)
	      ;; (gap:blocks (integer->char #xff))
	      (namefile-block
	       (append (list leader sync blocktype:namefile)
		       (list (integer->char 15)) ; block length
		       (string->list "CODE    ")
		       (list file-id:binary ascii:binary gap:continuous)
		       (map integer->char (int->ascii origin 'big-endian))
		       (map integer->char (int->ascii origin 'big-endian))))
	      (checksum
	       (lambda (block)
		 (list
		  (integer->char
		   (bitwise-and #xff
				(apply + (map char->integer (cddr block))))))))
	      (make-datablock
	       (lambda (data)
		 (let ((raw (append (list leader sync blocktype:data
					  (integer->char (length data)))
				    data)))
		   (append raw (checksum raw) (list trailer)))))
	      (chop-bin
	       (lambda (bin)
		 (cond
		  ((null? bin) '())
		  ((< (length bin) 255) (make-datablock bin))
		  (else (cons (make-datablock (take bin 255))
			      (chop-bin (drop bin 255))))))))
      (append (make-list 128 leader)
	      namefile-block
	      (checksum namefile-block)
	      (list trailer)
	      (make-list 128 leader)
	      (flatten (chop-bin bin))
	      (list leader sync blocktype:eof #\null (integer->char #xff)
		    trailer))))

  (define (extra-formats::mmod->dragon-cas filename mod
					   #!key (origin (mdef-default-origin
							  (mmod-mdef mod)))
					   (loader #t)
					   text)
    (let ((res (list->string (extra-formats::dragon-cas
			      (mod->bin mod origin) origin loader text))))
      (with-output-to-file filename (lambda () (write-string res)) #:binary)))

  ;; Graphical interface for Dragon/CoCo .cas export.
  (define (extra-formats::dragon-cas-dialog)
    (when (and (current 'mmod)
  	       (memv (string->symbol
  		      (target-platform-id (mdef-target (current 'mdef))))
  		     '(dragon32 dragon64 coco3 mc10)))
      (show-export-dialog extra-formats::mmod->dragon-cas
  			  "Dragon/Tandy .cas"
			  '(((Dragon/Tandy .cas) (.cas)))
			  '.cas)))
  ) ;; end module extra-formats
