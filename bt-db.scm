;; This file is part of Bintracker.
;; Copyright (c) utz/irrlicht project 2019-2020
;; See LICENSE for license details.

;; -----------------------------------------------------------------------------
;;; # Bintracker Database Interface
;; -----------------------------------------------------------------------------

(module bt-db
    *

  (import scheme (chicken base) (only (chicken file) directory)
	  (only (chicken file posix) directory?)
	  srfi-1 srfi-13 sql-de-lite simple-md5 mdal)

  (define btdb #f)

  ;;; Open the Bintracker database, and point the symbol `btdb` to it.
  (define (btdb-init!)
    (unless btdb
      (set! btdb (open-database "bt.db"))
      (btdb-update!)))

  ;;; Close the Bintracker database.
  (define (btdb-close!)
    (unless (database-closed? btdb)
      (close-database btdb)
      (set! btdb #f)))

  (define mdal-config-dir "libmdal/unittests/config/")

  (define (get-config-dir-subdirs)
    (filter (lambda (file)
	      (directory? (string-append mdal-config-dir file)))
	    (directory mdal-config-dir)))

  ;; (define (bt-db-get-config-hash mdconf-id)
  ;;   '())

  ;; (define (bt-db-get-config-info mdconf-id)
  ;;   '())

  ;;; Returns the list of available MDAL configurations. The returned list has
  ;;; the form `(config-id, plugin-version, target-platform, description)`.
  (define (btdb-list-configs #!optional (platform 'any))
    (exec (sql btdb (string-append
		     "SELECT id, version, platform, description FROM configs"
		     (if (eqv? platform 'any)
			 ""
			 (string-append " WHERE platform='" (->string platform)
					"'"))
		     ";"))))

  ;;; Collect information on the MDAL configuration named `config-id` into a
  ;;; list, which has the form `(version hash target-platform description)`.
  ;;; Returns #f if the config is not found in the MDAL configuration directory.
  (define (gather-config-info mdconf-id)
    (let ((mdconf (file->config mdal-config-dir mdconf-id "libmdal/")))
      (and mdconf
	   (list (string-append
		  (number->string (plugin-version-major
				   (config-plugin-version mdconf)))
		  "." (number->string (plugin-version-minor
				       (config-plugin-version mdconf))))
		 (file-md5sum (string-append mdal-config-dir mdconf-id
  					     "/" mdconf-id ".mdconf"))
		 (target-platform-id (config-target mdconf))
		 (config-description mdconf)))))

  ;;; Add the MDAL configuration named `mdconf-id` to the Bintracker database.
  (define (btdb-add-config! mdconf-id)
    (let ((info (gather-config-info mdconf-id)))
      (when info
  	(exec (sql btdb (string-append
  			 "INSERT INTO configs (id, version, "
  			 "hash, platform, description) VALUES ('" mdconf-id
			 "', '" (car info)
  			 "', '" (cadr info)
			 "', '" (third info)
  			 "', '" (fourth info) "');"))))))

  ;;; Remove the MDAL configuration named `mdconf-id` from the Bintracker
  ;;; database.
  (define (btdb-remove-config! mdconf-id)
    (exec (sql btdb (string-append "DELETE FROM configs WHERE id='"
				   mdconf-id "';"))))

  ;;; Update the MDAL configuration named `mdconf-id` in the Bintracker
  ;;; database.
  (define (btdb-update-config! mdconf-id)
    (let ((info (gather-config-info mdconf-id)))
      (exec (sql btdb (string-append "UPDATE configs SET "
				     "version='" (car info)
				     "', hash='" (cadr info)
				     "', platform='" (third info)
				     "', description='" (fourth info)
				     "' WHERE id='" mdconf-id "';")))))

  ;;; Scan the MDAL config directory, and update the Bintracker database
  ;;; accordingly. Configurations no longer found in the config directory are
  ;;; deleted from the database, newly found configurations are added, and
  ;;; entries for modified configurations are updated.
  (define (btdb-scan-mdal-configs!)
    (let ((config-dirs (get-config-dir-subdirs)))
      (for-each (lambda (db-entry)
		  (display (string-append "removing config: " db-entry))
		  (newline)
		  (btdb-remove-config! db-entry))
		(remove (lambda (db-entry)
			  (member db-entry config-dirs))
			(exec (sql btdb "SELECT id FROM configs;"))))
      (let ((current-configs (exec (sql btdb "SELECT id FROM configs;"))))
	(for-each (lambda (dir)
		    (display (string-append "found new config: " dir))
		    (newline)
		    (btdb-add-config! dir))
		  (remove (lambda (dir)
			    (member dir current-configs))
			  config-dirs))
	(for-each (lambda (dir)
		    (display (string-append "found updated config: " dir))
		    (newline)
		    (btdb-update-config! dir))
		  (remove
		   (lambda (dir)
		     (string=
		      (file-md5sum (string-append mdal-config-dir dir "/"
						  dir ".mdconf"))
		      (car (exec (sql btdb
				      (string-append
				       "SELECT hash FROM configs WHERE id='"
				       dir "';"))))))
		   config-dirs)))))

  ;;; Update the Bintracker Database on first run of the application.
  ;;; Creates the `configs` table if necessary, then scans the MDAL config
  ;;; directory for new or modified configurations and adds them to the database
  ;;; as required.
  (define (btdb-update!)
    (when (null? (exec (sql btdb (string-append
  				  "SELECT name FROM sqlite_master "
  				  "WHERE type='table' AND name='configs';"))))
      (display "regenerating database")
      (newline)
      (exec (sql btdb (string-append
  		       "create table configs "
  		       "(id TINYTEXT, version DECIMAL(2, 2), hash CHAR(32), "
  		       "platform TINYTEXT, description MEDIUMTEXT);"))))
    (btdb-scan-mdal-configs!))

  ;; TODO table for last-used files, record for "favourite configs"

  ) ;; end module bt-db
