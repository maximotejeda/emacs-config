(use-package lsp-dart
  :ensure t )

(use-package company
  :ensure t)


(use-package lsp-mode
  :ensure t
  :bind (:map lsp-mode-map
	      ("C-c r" . lsp-find-references)
	      ("C-c t" . lsp-find-definition)
	      ("C-c C-r" . lsp-rename)
	      )
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-modeline-diagnostics-enable t)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (js-mode . lsp)
	 (go-mode . lsp)
	 (web-mode . lsp)
	 (dart-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . lsp-treemacs))
  :commands lsp)


;; optionally
(use-package lsp-ui :ensure t :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :ensure t :commands helm-lsp-workspace-symbol)
;; if you are ivy user
(use-package lsp-ivy :ensure t :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs :ensure t :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode :ensure t)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
	     :ensure t
	     :config
	     (which-key-mode))
