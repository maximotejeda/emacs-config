;; REFERENCE https://wiki.crdb.io/wiki/spaces/CRDB/pages/73105658/Ben+s+Go+Emacs+Setup

;; install go-eldoc, projectile, go-projectile, gotest, compile, gocode

(use-package gotest
  :ensure t)

;; lsp configuration
;; Refference https://ianyepan.github.io/posts/emacs-ide/
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration)))
		  )
  :hook ((
	  typescript-mode
	   java-mode       ; eclipse-jdtls
           js-mode         ; ts-ls (tsserver wrapper)
           js-jsx-mode
	   web-mode
	  ) . lsp-deferred)
  :init
  (setq lsp-keep-workspace-alive t
        lsp-signature-doc-lines 5
        lsp-idle-delay 0.5
        lsp-prefer-capf t
        )
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (setq lsp-auto-guess-root t)
  (setq lsp-log-io nil)
  (setq lsp-restart 'auto-restart)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil)
  ;;(setq lsp-eldoc-hook t)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-semantic-tokens-enable nil)
  (setq lsp-enable-folding nil)
  (setq lsp-enable-imenu nil)
  (setq lsp-enable-snippet nil)
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-idle-delay 0.5))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)
	      ("C-c C-p s" . projectile-switch-project)))
;; addon projectile go
(use-package go-projectile
  :ensure t)

(use-package go-eldoc
  :ensure t)
;; required gocode for go-eldoc
;; go install github.com/nsf/gocode@latest

(setenv "PATH" (concat (getenv "PATH") path-separator "~/go/bin"))
(setq exec-path (append '("/usr/local/go/bin") exec-path))
(setq exec-path (append '("~/go/bin") exec-path))

;; Error highlight as you write
(add-hook 'after-init-hook #'global-flycheck-mode)


;; Company mode
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)

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
(defun my-go-mode-hook ()
      (setq tab-width 4 indent-tabs-mode 1)
      ; eldoc shows the signature of the function at point in the status bar.
      (go-eldoc-setup)
      ;; replace goto-definition with godef-jump THE SAME
      (local-set-key (kbd "M-.") #'godef-jump)
      ;; Format using gofmt before save
      (add-hook 'before-save-hook 'gofmt-before-save)
      (add-hook 'before-save-hook #'lsp-organize-imports t t)
      (add-hook 'before-save-hook #'lsp-format-buffer)

      ; extra keybindings from https://github.com/bbatsov/prelude/blob/master/modules/prelude-go.el
      (let ((map go-mode-map))
        (define-key map (kbd "C-c p") 'go-test-current-project) ;; current package, really
        (define-key map (kbd "C-c f") 'go-test-current-file)
        (define-key map (kbd "C-c .") 'go-test-current-test)
        (define-key map (kbd "C-c b") 'go-run))) ;; go run file in wich you are
(add-hook 'go-mode-hook 'my-go-mode-hook)
;; Use projectile-test-project in place of "compile"; assign whatever key you want.
(global-set-key [f9] 'projectile-test-project)


; "projectile" recognizes git repos (etc) as "projects" and changes settings
; as you switch between them. 
(projectile-global-mode 1)
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

; end .emacs additions
