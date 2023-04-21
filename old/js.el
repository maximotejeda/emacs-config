(use-package web-mode
  :ensure t
  :mode "\\.\\([jt]sx\\)\\'"
  :init
  
  )

(use-package rainbow-mode
  :ensure t
  :hook css-mode
  )

(use-package yaml-mode
  :ensure t
  :defer
  )


(use-package js2-mode
	     :ensure t
	     :init
	     (add-hook 'js-mode-hook 'js2-minor-mode)
	     (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
	     )

(use-package css-mode :ensure t)
;;(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
