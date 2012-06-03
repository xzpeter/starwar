;; using this to make linux binary. Before that, we have to configure asdf
;; correctly, so that ASD files can be found. 

(require "org.xzpeter.game.starwar")
(in-package :org.xzpeter.game.starwar)
(sb-ext:save-lisp-and-die "starwar" :toplevel #'main :executable t)
