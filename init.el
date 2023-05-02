;;==============================================
;; All config is confined inside the conf.org
;; This file will load all the configuration embeded in orgfiles
;;===============================================
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
		 "lisp" (expand-file-name
			 "org" (expand-file-name
				"src" dotfiles-dir))))
		(org-contrib-dir (expand-file-name
				  "lisp"(expand-file-name
					 "contrib" (expand-file-name
						    ".." org-dir))))
		(load-path (append (list org-dir org-contrib-dir)
				   (or load-path nil))))
       (require 'org)
       (require 'ob-tangle))

  (mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(org-agenda-files
   '("/home/maximo/.org/tasks.org" "/home/maximo/.org/journal.org" "/home/maximo/.org/organizer.org" "/home/maximo/.emacs.d/config.org"))
 '(package-selected-packages
   '(mixed-pitch magit yaml-mode xref-js2 which-key web-mode use-package rjsx-mode rainbow-mode neotree material-theme lsp-ui lsp-ivy js2-refactor helm-lsp gotest go-projectile go-autocomplete flymake-python-pyflakes flymake-golangci flymake-go-staticcheck elpy dap-mode better-defaults)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
