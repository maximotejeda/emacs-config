(use-package elpy
	     :ensure t
	     :init
	     (advice-add 'python-mode :before 'elpy-enable)
	     (setq python-shell-interpreter "python3.11"
		   python-shell-interpreter-args "-i")
	     )
