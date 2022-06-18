;; Org-Mode initial setup
  (use-package org
    :bind
    (("C-c l" . org-store-link)
     ("C-c a" . org-agenda)
     ("C-c c" . org-capture)))

  ;; Improve org mode looks
    (setq org-startup-indented t
          org-pretty-entities t
          org-hide-emphasis-markers t
          org-startup-with-inline-images t
          org-image-actual-width '(300))

;; Show hidden emphasis markers
(use-package org-appear
  :ensure t
    :hook (org-mode . org-appear-mode))

 ;; Nice bullets
(use-package org-superstar
  :ensure t
      :config
      (setq org-superstar-special-todo-items t)
      (add-hook 'org-mode-hook (lambda ()
                                 (org-superstar-mode 1))))

;; Increase size of LaTeX fragment previews
(plist-put org-format-latex-options :scale 2)
