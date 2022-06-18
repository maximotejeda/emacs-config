;; Refference https://lucidmanager.org/
;; Define and initialise package repo
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; use-package to simplifie the config file
(unless (package-installed-p 'use-package) ;; if not installed
  (package-refresh-contents)               ;; refresh contents
  (package-install 'use-package))          ;; install package
(require 'use-package)                     
(setq use-package-always-ensure 't)

;; User interface interaction
(setq inhibit-startup-message t)
(setq initial-scratch-message "Hello Maximo")
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq cursor-type 'bar)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Sensible line breaking
(add-hook 'text-mode-hook 'visual-line-mode)
;; Overwrite selected text
(delete-selection-mode t)

;; Scroll to the first and last line of the buffer
(setq scroll-error-top-bottom t)

;; Increase line spacing
(setq-default line-spacing 6)

;; Set default, fixed and variabel pitch fonts
;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (text-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 130)
  (set-face-attribute 'fixed-pitch nil :font "DejaVu Sans Mono")
  (set-face-attribute 'variable-pitch nil :font "DejaVu Sans"))

;; Required for proportional font
(use-package company-posframe
  :config
  (company-posframe-mode 1))

;; Theme
(use-package exotica-theme
  :config (load-theme 'exotica t))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(terraform-mode dockerfile-mode rjsx-mode gotest go-projectile flycheck lsp-ui lsp-mode go-mode yaml-mode rainbow-mode web-mode elpy org-superstar org-appear company-posframe mixed-pitch company which-key helm exotica-theme use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; helm config
;; Helm configuration
  (use-package helm
    :config
    (require 'helm-config)
    :init
    (helm-mode 1)
    :bind
    (("M-x"     . helm-M-x) ;; Evaluate functions
     ("C-x C-f" . helm-find-files) ;; Open or create files
     ("C-x b"   . helm-mini) ;; Select buffers
     ("C-x C-r" . helm-recentf) ;; Select recently saved files
     ("C-c i"   . helm-imenu) ;; Select document heading
     ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
     :map helm-map
     ("C-z" . helm-select-action)
     ("<tab>" . helm-execute-persistent-action)))


;; Which Key
;; help with keyboard shortcut
  (use-package which-key
    :config
    (which-key-mode)
    (setq which-key-idle-delay 0.5
          which-key-idle-secondary-delay 0.5)
    (which-key-setup-side-window-bottom))

;; company
 ;; Auto completion
  (use-package company
    :config
    (setq company-idle-delay 0
          company-minimum-prefix-length 4
          company-selection-wrap-around t))
  (global-company-mode)
