;; -*- coding: utf-8 -*-
;;
;; Emacs Easy Config
;; Rafa R. Galvan <rafael(dot)rodriguez(at)uca.es>
;;
;; https://bitbucket.org/proyecto-ucaccar/emacs-easy-config

;; Start server (connect to it using emacsclient)
(load "server")
(unless (server-running-p) (server-start))

;;,-------------------
;;| Package management
;;`-------------------
(setq use-package-verbose t)
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)
(setq package-enable-at-startup nil)

;; In current emacs configuration we will employ 'use-package' for
;; install and config packages. See https://github.com/jwiegley/use-package

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;;,-------------------------------------------------------------------------
;;| Personalize basic emacs behaviour. See https://github.com/rrgalvan/emacs
;;`-------------------------------------------------------------------------

(when (display-graphic-p)
  ;; Set width of emacs windows (frame)
  (set-frame-width (selected-frame) 85)

  ;; display line numbers in margin. New in Emacs 23
  (global-linum-mode 1)
  ;; line numbers format (add a vertical line on the right)
(setq linum-format "%3d\u2502")

;; ;; set default color theme
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-gtk-ide)
;; (color-theme-rotor)
;; (color-theme-sanityinc-solarized-light)
;; (color-theme-solarized) // dark
;; (color-theme-arjen) // dark
)


;;(menu-bar-mode -1) ;; Disable menu bar
(tool-bar-mode -1) ;; Disable icons
;;(toggle-scroll-bar -1) ;; Disable scroll bar

;;,---------------------------------------------------------------------
;;| Color theme. To see all possibilities, write: M-x custom-themes
;;`---------------------------------------------------------------------
;; (load-theme 'misterioso)

(use-package panda-theme
  :ensure t
  :config
  (set-cursor-color "#f0f0f0")
  )

;;------------------------------------------------------------------------------
;; cursor blinking and changing colors
;; taken from http://stackoverflow.com/questions/4642835/how-to-change-the-cursor-color-on-emacs
;;------------------------------------------------------------------------------
; Using in Emacs 24.0

(setq blink-cursor-interval 0.5)
(setq blink-cursor-colors (list "#80c2e4" "#e4c040" "#92d88f" "#be369c" "#6785c5"  "#6a984a"))
  ;; "On each blink the cursor will cycle to the next color in this list.")
(setq blink-cursor-count 0)
(defun blink-cursor-timer-function ()
  "Zarza wrote this cyberpunk variant of timer `blink-cursor-timer'.
Warning: overwrites original version in `frame.el'.

This one changes the cursor color on each blink. Define colors in `blink-cursor-colors'."
  (when (not (internal-show-cursor-p))
    (when (>= blink-cursor-count (length blink-cursor-colors))
      (setq blink-cursor-count 0))
    (set-cursor-color (nth blink-cursor-count blink-cursor-colors))
    (setq blink-cursor-count (+ 1 blink-cursor-count))
    )
  (internal-show-cursor nil (not (internal-show-cursor-p)))
  )


;; No startup screen
(setq inhibit-startup-message t)

;;,-----------------------------------------------------------
;;| Directory for local lisp files (including subdirectoires)
;;`-----------------------------------------------------------
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(let ((default-directory site-lisp-dir)) (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path site-lisp-dir)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; In programming buffers, distinguish CamelCase sub-words
(add-hook 'prog-mode-hook 'subword-mode)

;; Scroll by one line at a time.
(setq scroll-step 1)

;; ;; Scroll
;; (global-set-key [S-down] 'scroll-up-line)
;; (global-set-key [S-up]  'scroll-down-line)

;; Make searches case insensitive.
(setq case-fold-search nil)

;; Turn on highlighting for search strings.
(setq search-highlight t)

;; turn on paren match highlighting
(show-paren-mode 1)

;;highlight entire body of bracket expression
;;(setq show-paren-style 'expression)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)

;; Ido for buffer switching
;; (require 'ido)
;; (ido-mode t)
;; (setq
;;  ido-case-fold  t                 ; be case-insensitive
;;  ido-confirm-unique-completion t ; wait for RET, even with unique completion
;;  confirm-nonexistent-file-or-buffer nil) ;; The confirmation in ido is rather annoying...

;; Remove trailing whitespace automatically
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Down-arrow at the end of a file doesn't add in a new line.
(setq next-line-add-newlines nil)

;; Silently ensure a file ends in a newline when it's saved.
(setq require-final-newline t)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Make script files executable automatically
;; http://www.masteringemacs.org/articles/2011/01/19/script-files-executable-automatically/
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; Include the size of the file in the mode line
(size-indication-mode nil)

;; Show which column I'm in in the mode line as well
(column-number-mode t)

;; 8 is wrong
(setq tab-width 4)

;; Make the cursor a thin bar, not a block (but I still like it blinking)
;;(setq cursor-type 'bar)

;; M-x compile is tedious
(global-set-key (kbd "C-c c") 'compile)

(when window-system
  ;; Highlight marked text - only works under X.
  (transient-mark-mode t)

  ;; Select font
  (if (find-font (font-spec :name "Dejavu Sans Mono"))
      (progn
	(set-fontset-font "fontset-default" 'unicode "Dejavu Sans Mono")
	))
  (if (find-font (font-spec :name "Inconsolata"))
      (progn
	(set-face-font 'default "Inconsolata-15")
	))
  )

;; I don't want to type in "yes" or "no" - I want y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;;,----------------------------------
;;| 'Standard' cut, copy, paste, undo
;;`----------------------------------
(cua-mode 1)
(setq cua-prefix-override-inhibit-delay 0.5)

;;,-------------
;;| Recent files
;;`-------------

;; ;; First idea: Let ido show recently closed buffers
;; (setq ido-use-virtual-buffers t)

;; Second idea: Use recentf and integrate it with icicles
(require 'recentf)
(setq recentf-max-saved-items 150
      recentf-max-menu-items 25)
(recentf-mode)

;; ;;,--------
;; ;;| Icicles
;; ;;`--------
;; (use-package icicles
;;   :defer t
;;   :ensure t
;;   :init
;;    (progn
;;      (icy-mode 1)
;;      (setq icicle-buffer-include-recent-files-nflag 20)
;;      )
;;    :bind (
;; 	  ("C-x b" . icicle-buffer)
;;      )
;;    )

(use-package ido
  :defer t
  :ensure t
)

;;,-------------------------------------------------
;;| Helm (auto-incremental completion and selection)
;;`-------------------------------------------------
(use-package helm
  :ensure helm
  :diminish helm-mode
  :defer t
  :init
  (progn
    (helm-mode)
    ;; Let <tab> completion in helm :-|
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; (define-key helm-map (kbd "C-z") 'helm-select-action))
    )
    :bind (
	   ("C-x b" . helm-mini))
    )

;; (defun ido-recentf-open ()
;;   "Use `ido-completing-read' to \\[find-file] a recent file"
;;   (interactive)
;;   (if (find-file (ido-completing-read "Find recent file: " recentf-list))
;;       (message "Opening file...")
;;     (message "Aborting")))

;; (global-set-key (kbd "C-x C-r") 'ido-recentf-open)


;;,----------------------------------------------
;;| Install manually 'dash package (to avoid bug)
;;`----------------------------------------------
(setq my-packages '(dash))
(when (not package-archive-contents) (package-refresh-contents))
(dolist (p my-packages) (when (not (package-installed-p p))
			  (package-install p)))

;;,------------------------------------------------
;;| Automatically recompile Emacs Lisp source files
;;`------------------------------------------------
(use-package auto-compile
  :defer t
  :ensure t)
;; :init (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;;,-------------
;;| Autocomplete
;;`-------------

(use-package auto-complete
  :ensure t
  :config
  (require 'auto-complete-config)
  (global-auto-complete-mode t)
  ;; Autocomplete in all buffers (except minibuffer)
  (defun auto-complete-mode-maybe ()
    "No maybe for you. Only AC!"
    (unless (minibufferp (current-buffer))
      (auto-complete-mode 1)))
  (ac-flyspell-workaround) ;; Fixes a known bug of delay due to flyspell
  )

;;,-------------
;;| Fortran mode
;;`-------------

(add-to-list 'auto-mode-alist '("\\.f\\'" . f90-mode))
(font-lock-add-keywords 'f90-mode
			;; '(("%" . font-lock-keyword-face)))
			'(("%" . font-lock-preprocessor-face)))

;;,------
;;| gtags
;;`------

;; (setq load-path (cons "/usr/local/share/gtags/gtags.el" load-path))
;; (autoload 'gtags-mode "gtags-mode" "Loading GNU Global")
					;(autoload 'gtags-mode "gtags" "" t)
;; (add-hook 'f90-mode-hook '(lambda ()
;;           (gtags-mode t)
;;           (setq gtags-global-command "/usr/local/bin/gtags")
;;           (setq gtags-suggested-key-mapping t)))

(use-package ggtags
  :ensure t
  :defer t)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode)) ;; Use c++ mode for .h files
(add-hook 'f90-mode-hook 'ggtags-mode)
(add-hook 'c-mode-common-hook (ggtags-mode 1))
;; (lambda ()
;;   (when (derived-mode-p 'c-mode 'c++-mode)
;;     (ggtags-mode 1))))

;;
;; Markdown
;;
(use-package markdown-mode
  :ensure t
  ;; :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; ;;,---------------------------------------------
;; ;;| smartparens for good handling of parentheses
;; ;;`---------------------------------------------
;; (use-package smartparens
;;   :ensure t
;;   :diminish smartparens-mode
;;   :config
;;   (progn
;;     (require 'smartparens-config)
;;     (smartparens-global-mode 1)

;; ;;;;;;;;;;;;;;;;;;
;;     ;; pair management

;;     (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
;;     (sp-local-pair 'web-mode "<" nil :when '(my/sp-web-mode-is-code-context))

;; ;;; markdown-mode, rst-mode, etc
;;     (sp-with-modes '(markdown-mode gfm-mode rst-mode)
;;       (sp-local-pair "*" "*" :bind "C-*")
;;       (sp-local-tag "2" "**" "**")
;;       (sp-local-tag "s" "```scheme" "```")
;;       (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))))

					;
;; (use-package imenu-anywhere
;;   :ensure t
;;   :bind (("C-c i" . imenu-anywhere)))

;;,----------------------------------------------------
;;| Rebox (fancy comment boxes, see rebox2.el for help)
;;`----------------------------------------------------

(use-package rebox2
 :defer 2 ;; Wait for 2 seconds idle
 :ensure t
 :config
 (progn
   (rebox-register-template
    98
    998
    '("??=============??"
      "?? box123456   ??"
      "??-------------??"))
   (rebox-register-template
    99
    999
    '("??,-----------"
      "??| box123456 "))
   (setq rebox-style-loop '(27 23 98 99 21))
   (setq rebox-min-fill-column 72)
   (global-set-key [(shift meta q)] 'rebox-dwim)
   (global-set-key [(meta /)] 'rebox-cycle)
   ;; Make rebox2 work for fortran90 comments (see https://github.com/lewang/rebox2/issues/11)
   (defvar rebox-language-character-alist
     '((3 . "/") (4 . "#") (5 . ";") (6 . "%") (7 . "!"))
     "List relating language to comment character, for generic languages.")
   ))
;; (defvar rebox-regexp-start
;;   ["^[ \t]*\\(/\\*\\|//+\\|#+\\|;+\\|%+\\)"
;;    "^"                                  ; 1
;;    "^[ \t]*/\\*"                        ; 2
;;    "^[ \t]*//+"                         ; 3
;;    "^[ \t]*#+"                          ; 4
;;    "^[ \t]*\;+"                         ; 5
;;    "^[ \t]*%+"                          ; 6
;;    "^[ \t]*!+"                          ; 7
;;    ])


;;,--------------------------------------------------------------------
;;| Fix bug with dead-keys in Ubuntu 13.10, 14.04, 14.10
;;`--------------------------------------------------------------------
(require 'iso-transl)


;;,-----------------------------------------------------
;;| Directory for store setup files (for specific modes)
;;`-----------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/setup"))

;;,------------------------
;;| FreeFem++ setup
;;`------------------------
(require 'setup-freefem)

;;,---------------------------------------------------------------
;;| LaTeX (using the package Auctex: $sudo apt-get install auctex)
;;`---------------------------------------------------------------

;; Compile to LaTeX
(setq TeX-PDF-mode t)
;; Turn on spell-checking in LaTeX
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; Activate RefTeX
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t) ;; Supply labels automatically

(use-package auto-complete-auctex
  :ensure t
  :init (require 'auto-complete-auctex)
  )

;; Math symbol menu and possibility of unicode auto-completion
(use-package ac-math
  :ensure t
  :init (require 'ac-math))


;; Okular

;; (setq TeX-view-program-list '(("Okular" "okular --unique %u")))
(setq TeX-view-program-selection '((output-pdf "Okular")))
(setq TeX-view-program-list
      '(("okular" "okular --unique %o#src:%n%(dir)./%b")))
(setq TeX-source-correlate-method 'synctex)

;; Direct/reverse search (through okular: $sudo apt-get install okular),
;; http://www.flannaghan.com/2013/01/31/synctex-f17
(add-hook 'LaTeX-mode-hook
	  (lambda()
	    (add-to-list 'TeX-expand-list
			 '("%(dir)"
			   (lambda () default-directory)))))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

;; (add-hook 'LaTeX-mode-hook '(lambda ()
;;                   (add-to-list 'TeX-expand-list
;;                        '("%u" Okular-make-url))))

;; (defun Okular-make-url () (concat
;;                "file://"
;;                (expand-file-name (funcall file (TeX-output-extension) t)
;;                          (file-name-directory (TeX-master-file)))
;;                "#src:"
;;                (TeX-current-line)
;;                (expand-file-name (TeX-master-directory))
;;                "./"
;;                (TeX-current-file-name-master-relative)))


;;,-------------------
;;| Choose color theme
;;`-------------------

;; (when (display-graphic-p)
;;   (when (>= emacs-major-version 24)
;;     (require 'color-theme-solarized)
;;     (set-frame-parameter nil 'background-mode 'dark)
;;     (set-frame-parameter nil 'background-mode 'dark)
;;     (setq solarized-high-contrast-mode-line t)
;;     (color-theme-solarized))
;;   )

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;; (load-theme 'zenburn t)
;; (load-theme 'hc-zenburn t)
;; (load-theme 'zerodark)

;;,-----
;;| Evil
;;`-----
(use-package evil
  :ensure evil
  :init
	(evil-mode 1))

;;,------
;;| Tramp
;;`------

(setq tramp-default-method "ssh") ;; Faster than scp, according to wiki

;;,------
;;| Magit
;;`------

(use-package magit
  :defer t
  :config (progn
	    (bind-key "C-x g" #'magit-status)))

;;,-----------------
;;| Google translate
;;`-----------------
(use-package google-translate
  :ensure google-translate
  :defer t
  :init
  (progn
    (global-set-key (kbd "\C-c tt") 'google-translate-at-point)
    (global-set-key (kbd "\C-c tT") 'google-translate-query-translate)
    (global-set-key (kbd "C-c tr") 'google-translate-at-point-reverse)
    (global-set-key (kbd "C-c tR") 'google-translate-query-translate-reverse)
    (setq google-translate-show-phonetic t)
    (setq google-translate-default-source-language "es")
    (setq google-translate-default-target-language "en")))

;;,-----------------
;; flycheck (on the fly check programming language syntax)
;;`-----------------
(use-package flycheck
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (setq flycheck-gfortran-language-standard "f2008ts"))

;;,---------------------------------------------------------------------
;;| Python
;;`---------------------------------------------------------------------
(require 'setup-python)

;;,-----------------------
;;| Emacs ipython notebook
;;`-----------------------
(use-package ein
  :config
  (require 'ein)
  )
;; (use-package ein
;;   :ensure t
;;   :commands (ein:notebooklist-open))

;;
;; Org mode
;;
(require 'setup-org-mode)

(provide 'init)
;;; init.el ends here

;;; gmsh
;; (use-package imenu-anywhere
;;   :ensure t
;;   :bind (("C-c i" . imenu-anywhere)))
(require 'gmsh)
(add-to-list 'auto-mode-alist '("\\.geo$" . gmsh-mode))

;; php
(add-to-list 'auto-mode-alist '("\\.php\\'" . html-mode)) ;; Use html mode for .php files
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0eccc893d77f889322d6299bec0f2263bffb6d3ecc79ccef76f1a2988859419e" default)))
 '(package-selected-packages
   (quote
    (panda-theme zeno-theme ein flycheck elpy google-translate evil ac-math auto-complete-auctex use-package rebox2 markdown-mode helm ggtags dash auto-complete auto-compile))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
