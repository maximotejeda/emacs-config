(use-package web-mode
  :ensure t
  :mode "\\.\\([jt]sx\\)\\'")

(use-package rainbow-mode
  :ensure t
  :hook css-mode)

(use-package yaml-mode
  :ensure t
  :defer)


(use-package js2-mode
  :ensure t)
(use-package rjsx-mode)
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

;; For this to work we need to install lsp-mode
;; and in global install tsjs server for auto complete
;; npm install -g typescript-language-server
