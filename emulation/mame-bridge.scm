(import scheme)
(cond-expand
  (chicken-4 (use posix))
  (chicken-5 (import chicken.process)))

;; TODO:
;;   - possibility to override mame executable name, rompath
;;   - possibility to start in debug mode
;;   - "-cart" arg name depends on system, so we need system configs
;;   - possibility to pass -wavwrite
(define (start-mame target-sys binary)
  (process "mame64" `(,target-sys "-rompath" "roms/" "-cart" ,binary)))
