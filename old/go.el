(use-package gotest
	     :ensure t)

(use-package go-eldoc
	     :ensure t)

(use-package go-guru
	     :ensure t)

(use-package neotree
  :ensure t
  )

(use-package go-autocomplete
	     :ensure t)

(use-package projectile
	     :ensure t)

(use-package go-projectile
	     :ensure t
	     :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
	      ("C-c C-p s" . projectile-switch-project)
	      ;;("C-c C-p a" . projectile-add-known-project)
	      ))


;;Error highlight 
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)


;; TAB width 4
(setq tab-width 4 indent-tabs-mode 1)
(defun my-go-mode-hook ()
      ; eldoc shows the signature of the function at point in the status bar.
      (go-eldoc-setup)
      ;; replace goto-definition with godef-jump THE SAME
      ;;(local-set-key (kbd "M-.") #'godef-jump)
      ;; Format using gofmt before save
      (add-hook 'before-save-hook 'gofmt-before-save)
      (add-hook 'before-save-hook #'lsp-organize-imports t t)
      (add-hook 'before-save-hook #'lsp-format-buffer)
      (auto-complete-mode 1)
      ; extra keybindings from https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el
      (let ((map go-mode-map))
        (define-key map (kbd "C-c p") 'go-test-current-project) ;; current package, really
        (define-key map (kbd "C-c f") 'go-test-current-file)
        (define-key map (kbd "C-c .") 'go-test-current-test)
        (define-key map (kbd "C-c b") 'go-run)
	;; Key bindings specific to go-mode
	(local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
	(local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
	(local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
	(local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
	(local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
	(local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg
	)) ;; go run file in wich you are
(add-hook 'go-mode-hook 'my-go-mode-hook)
;; Use projectile-test-project in place of "compile"; assign whatever key you want.
(global-set-key [f9] 'projectile-test-project)


; "projectile" recognizes git repos (etc) as "projects" and changes settings
; as you switch between them. 
(require 'go-projectile)
(go-projectile-tools-add-path)
(setq gofmt-command (concat go-projectile-tools-path "/bin/goimports"))


; gotest defines a better set of error regexps for go tests, but it only
; enables them when using its own functions. Add them globally for use in
(require 'compile)
(require 'gotest)
(dolist (elt go-test-compilation-error-regexp-alist-alist)
  (add-to-list 'compilation-error-regexp-alist-alist elt))
(defun prepend-go-compilation-regexps ()
  (dolist (elt (reverse go-test-compilation-error-regexp-alist))
    (add-to-list 'compilation-error-regexp-alist elt t)))
(add-hook 'go-mode-hook 'prepend-go-compilation-regexps)

 (with-eval-after-load 'go-mode
   (require 'go-autocomplete))


