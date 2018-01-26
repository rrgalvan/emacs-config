;; Elpy Mode "If you don’t want to configure anything yourself (or
;; can’t decide what you want), Elpy combines many helpful packages
;; for working with Python and sets everything up for you."

(use-package elpy
  :ensure t
  :defer 2
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-yasnippet)
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode))
    (elpy-enable)
    ;; jedi is great
    (setq elpy-rpc-backend "jedi")))

;; ;;,----------------------------------------------------------
;; ;;| Python
;; ;;|   Main python modes: (a) python (b) python-mode,
;; ;;|   see http://emacswiki.org/emacs/PythonProgrammingInEmacs
;; ;;`----------------------------------------------------------
;; (use-package python-mode
;;   :ensure t
;;   ;; :defer 1 ;; Wait for 1 seconds of idle time
;;   :config (progn
;;   	    (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;;   	    (add-to-list 'interpreter-mode-alist '("python" . python-mode))
;;   	    ;; (require 'ipython)
;;   	    ;; Other stuff
;;   	    (setq-default py-shell-name "ipython")
;;   	    (setq-default py-which-bufname "IPython")
;;   					; switch to the interpreter after executing code
;;   	    (setq py-shell-switch-buffers-on-execute-p t)
;;   	    (setq py-switch-buffers-on-execute-p t)
;;   					; don't split windows
;;   	    (setq py-split-windows-on-execute-p nil)
;;   	    )
;;    )

;; Jedi (python auto-completion)
;; (DISABLED BECAUSE OF ERRORS IN CURRENT VERSION)
;; (use-package jedi
;;   :ensure t
;;   :defer 1 ;; Wait for 1 seconds of idle time
;;   :config (progn
;; 	    (jedi:install-server)
;; 	    (add-hook 'python-mode-hook 'jedi:setup)
;; 	    (setq jedi:complete-on-dot t))
;;   )


(provide 'setup-python)
