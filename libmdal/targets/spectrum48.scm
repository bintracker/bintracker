(md:make-target
  "spectrum48"
  (eval (car (read-file "targets/cpu/z80.scm")))
  3500000
  (list (eval (car (read-file "targets/export/zxspectrum-tap.scm")))))
