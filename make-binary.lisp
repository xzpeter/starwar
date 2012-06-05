;; using this to make linux binary. Before that, we have to configure asdf
;; correctly, so that ASD files can be found. 

#+win32
(setq asdf:*central-registry*
      (append asdf:*central-registry*
	      '("Z:\\home\\xzpeter\\git-repo\\starwar\\"
		"Z:\\home\\xzpeter\\git-repo\\starwar\\lib\\")))

(require "org.xzpeter.game.starwar")
(sb-ext:save-lisp-and-die #+unix "starwar-linux"
			  #+win32 "starwar-win32.exe"
			  :toplevel #'org.xzpeter.game.starwar:main
			  :executable t)
