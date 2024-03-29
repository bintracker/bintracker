;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker Database Interface
;; -----------------------------------------------------------------------------

;;; The Bintracker Database holds information concerning an existing
;;; installation of Bintracker. It is an SQLite3 database, managed through the
;;; [sqlite3](https://wiki.call-cc.org/eggref/5/sqlite3) extension.
;;; If Bintracker does not find `bt.db` on application startup, it creates and
;;; populates a fresh database.
;;;
;;; ## Default Tables
;;;
;;; ### mdefs
;;;
;;; The `mdefs` table contains information about the installed MDAL
;;; definitions. It contains the following columns:
;;;
;;; - `id` - definition name
;;; - `version` - engine version
;;; - `hash` - MD5 hash of the .mdef file
;;; - `platform` - target platform
;;; - `description` - configuration description
;;;
;;; The `mdefs` table is automatically updated during startup of Bintracker.
(module bt-db
    *

  (cond-expand
    (windows (import scheme (chicken base) (chicken sort) (chicken condition)
		     (only (chicken file posix) directory?)
		     (only (chicken string) string-split)
		     (only shell capture)
		     srfi-1 srfi-13 sqlite3 simple-md5 mdal))
    (else (import scheme (chicken base) (chicken sort) (chicken condition)
		  (only (chicken file) directory)
		  (only (chicken file posix) directory?)
		  srfi-1 srfi-13 sqlite3 simple-md5 mdal)))

  (define btdb #f)

  ;;; Open the Bintracker database, and point the symbol `btdb` to it.
  (define (btdb-init!)
    (unless btdb
      (set! btdb (open-database "bt.db"))
      (btdb-update!)))

  ;;; Close the Bintracker database.
  (define (btdb-close!)
    (when btdb
      (finalize! btdb)
      (set! btdb #f)))

  (define mdal-mdef-dir "mdef/")

  (define (get-mdef-dir-subdirs)
    (filter (lambda (file)
	      (directory? (string-append mdal-mdef-dir file)))
	    (cond-expand
	      (windows (string-split (capture "dir /b mdef") "\n"))
	      (else (directory mdal-mdef-dir)))))

  ;; (define (bt-db-get-mdef-hash mdef-id)
  ;;   '())

  ;; (define (bt-db-get-mdef-info mdef-id)
  ;;   '())

  (define (btdb-get-mdef-feature mdef-id feature)
    (let ((res (map-row identity btdb
			(string-append
			 "SELECT "
			 (symbol->string feature)
			 " FROM mdefs WHERE id='"
			 (symbol->string mdef-id)
			 "'"))))
      (if (null? res) "" (car res))))

  (define (btdb-get-mdef-description mdef-id)
    (btdb-get-mdef-feature mdef-id 'description))

  (define (btdb-get-mdef-platform mdef-id)
    (btdb-get-mdef-feature mdef-id 'platform))

  ;;; Returns the list of available MDAL definitions. The returned list has
  ;;; the form `(MDEF-ID, ENGINE-VERSION, TARGET-PLATFORM, DESCRIPTION)`.
  (define (btdb-list-mdefs #!optional (platform 'any))
    (sort (map-row (lambda args args) btdb
		   (string-append
		    "SELECT id, version, platform, description FROM mdefs"
		    (if (eqv? platform 'any)
			""
			(string-append " WHERE platform='" (->string platform)
				       "'"))
		    ";"))
	  (lambda (s1 s2) (string-ci< (car s1) (car s2)))))

  (define (btdb-list-platforms)
    (map-row identity btdb "SELECT DISTINCT platform FROM mdefs"))

  ;;; Collect information on the MDAL definition named MDEF-ID into a
  ;;; list, which has the form `(VERSION HASH TARGET-PLATFORM DESCRIPTION)`.
  ;;; Returns `#f` if the mdef is invalid or not found in the MDAL definition
  ;;; directory.
  (define (gather-mdef-info mdef-id)
    (handle-exceptions
	exn
	(begin
	  (print "Ignoring broken MDEF "
		 mdef-id
		 " (broken by "
		 exn
		 " - "
		 (if ((condition-predicate 'mdal) exn)
		     ((condition-property-accessor 'mdal 'message) exn)
		     ((condition-property-accessor 'exn 'message) exn))
		 ")")
	  #f)
      (let ((mdef (file->mdef mdal-mdef-dir mdef-id)))
	(list (string-append
	       (number->string (engine-version-major
				(mdef-engine-version mdef)))
	       "." (number->string (engine-version-minor
				    (mdef-engine-version mdef))))
	      (file-md5sum (string-append mdal-mdef-dir mdef-id
  					  "/" mdef-id ".mdef"))
	      (target-platform-id (mdef-target mdef))
	      (or (mdef-description mdef) "")))))

  ;;; Add the MDAL definition named MDEF-ID to the Bintracker database. Returns
  ;;; `#t` on success, `#f` on failure.
  (define (btdb-add-mdef! mdef-id)
    (let ((info (gather-mdef-info mdef-id)))
      (and info
	   (let ((q (prepare btdb
			     (string-append
			      "INSERT INTO mdefs (id, version, hash, platform, "
			      "description) VALUES (?, ?, ?, ?, ?);"))))
	     (apply execute (cons q (cons mdef-id info)))
	     (finalize! q)
	     #t))))

  ;;; Remove the MDAL definition named MDEF-ID from the Bintracker
  ;;; database.
  (define (btdb-remove-mdef! mdef-id)
    (execute btdb (string-append "DELETE FROM mdefs WHERE id='"
				 mdef-id "';")))

  ;;; Update the MDAL definition named MDEF-ID in the Bintracker
  ;;; database. Returns `#t` on success, `#f` on failure.
  (define (btdb-update-mdef! mdef-id)
    (let ((info (gather-mdef-info mdef-id)))
      (and info
	   (let ((q (prepare btdb
			     (string-append "UPDATE mdefs SET version=?, "
					    "hash=?, platform=?, "
					    "description=? WHERE id=?;"))))
	     (apply execute (cons q (append info (list mdef-id))))
	     (finalize! q)
	     #t))))

  ;;; Scan the MDAL mdef directory, and update the Bintracker database
  ;;; accordingly. Definitions no longer found in the mdef directory are
  ;;; deleted from the database, newly found definitions are added, and
  ;;; entries for modified definitions are updated.
  (define (btdb-scan-mdal-mdefs!)
    (let ((mdef-dirs (get-mdef-dir-subdirs)))
      (for-each (lambda (db-entry)
      		  (print "removing mdef: " db-entry)
      		  (btdb-remove-mdef! db-entry))
      		(remove (lambda (db-entry)
      			  (member db-entry mdef-dirs))
      			(map-row identity btdb "SELECT id FROM mdefs;")))
      (let* ((current-mdefs (map-row identity btdb "SELECT id FROM mdefs;"))
	     (new-dirs (remove (lambda (dir)
				 (member dir current-mdefs))
			       mdef-dirs))
	     (infos (map gather-mdef-info new-dirs))
	     (ids+infos (filter-map (lambda (id info) (and info (cons id info)))
				    new-dirs
				    infos)))
	(unless (null? ids+infos)
	  (let ((q (prepare btdb
			    (string-append "INSERT INTO mdefs (id, version, "
					   "hash, platform, description) "
					   "VALUES (?, ?, ?, ?, ?);"))))
	    (for-each (lambda (id+info)
			(print "adding " (car id+info))
			(apply execute (cons q id+info)))
		      ids+infos)
	    (finalize! q)))
	(for-each (lambda (dir)
		    (print "found updated mdef: " dir)
		    (unless (btdb-update-mdef! dir)
		      (print "update broke mdef, removed it.")
		      (btdb-remove-mdef! dir)))
		  (remove
		   (lambda (dir)
		     (or (null? (map-row identity btdb
					 (string-append
					  "SELECT hash FROM mdefs WHERE id='"
					  dir
					  "';")))
			 (string=
			  (file-md5sum (string-append mdal-mdef-dir dir "/"
		  				      dir ".mdef"))
			  (first-result btdb
		  			(string-append
		  			 "SELECT hash FROM mdefs WHERE id='"
		  			 dir "';")))))
		   mdef-dirs)))))

  ;;; Update the Bintracker Database on first run of the application.
  ;;; Creates the `mdefs` table if necessary, then scans the MDAL mdef
  ;;; directory for new or modified definitions and adds them to the database
  ;;; as required.
  (define (btdb-update!)
    (when (null? (map-row identity btdb
			  (string-append
  	  		   "SELECT name FROM sqlite_master "
  	  		   "WHERE type='table' AND name='mdefs';")))
      (print "regenerating database")
      (execute btdb (string-append
  		     "create table mdefs "
  		     "(id TINYTEXT PRIMARY KEY,"
		     " version DECIMAL(2, 2), hash CHAR(32), "
  		     "platform TINYTEXT, description MEDIUMTEXT);")))
    (btdb-scan-mdal-mdefs!))

  ;; TODO table for last-used files, record for "favourite mdefs"

  ) ;; end module bt-db
