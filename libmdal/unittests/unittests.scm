(load "mdal.scm")
; (define cfg (ssax:xml->sxml (open-input-file "config/Huby/Huby.mdconf") '()))
; (define cmd-node (car ((sxpath "mdalconfig/command") cfg)))
; (define my-cmd (md:xml-node->command cmd-node))
(define my-cfg (md:mdconf->config "config/Huby/Huby.mdconf"))

