;; -*- coding: utf-8 -*-
;;
;; Emacs Easy Config
;; Rafa R. Galvan <rafael(dot)rodriguez(at)uca.es>
;;
;; https://bitbucket.org/proyecto-ucaccar/emacs-easy-config

;; Start server (connect to it using emacsclient)
(server-start)

;; Set width of emacs windows (frame)
(set-frame-width (selected-frame) 85)

;;
;; Personalize basic emacs behaviour. See https://github.com/rrgalvan/emacs
;; -------------------------------------------------------------------

;; No startup screen
(setq inhibit-startup-message t)

;; Directoy for local emacs lisp files:  ~/.emacs.d/site-lisp/
(add-to-list 'load-path (expand-file-name "~/.emacs.d/setup"))

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

;; display line numbers in margin. New in Emacs 23
(global-linum-mode 1)
;; line numbers format (add a vertical line on the right)
(setq linum-format "%3d\u2502")

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
  (set-fontset-font "fontset-default" 'unicode "Dejavu Sans Mono")
  (set-face-font 'default "Inconsolata-15")
)

;; 'Standard' cut, copy, paste, undo
;; -------------------------------------------------------------------
(cua-mode 1)

;; Recent files
;; -------------------------------------------------------------------
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;; I don't want to type in "yes" or "no" - I want y/n.
(fset 'yes-or-no-p 'y-or-n-p)

;;
;; Package management
;; -------------------------------------------------------------------
;;
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

;; Install manually 'dash package
(setq my-packages '(dash))
(when (not package-archive-contents) (package-refresh-contents))
(dolist (p my-packages) (when (not (package-installed-p p))
			  (package-install p)))

;; Automatically recompile Emacs Lisp source files
(use-package auto-compile
  :ensure t)
  ;; :init (auto-compile-on-load-mode))
(setq load-prefer-newer t)

;; smartparens for good handling of parentheses
;; -------------------------------------------------------------------
(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)

;;;;;;;;;;;;;;;;;;
    ;; pair management

    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-local-pair 'web-mode "<" nil :when '(my/sp-web-mode-is-code-context))

;;; markdown-mode
    (sp-with-modes '(markdown-mode gfm-mode rst-mode)
      (sp-local-pair "*" "*" :bind "C-*")
      (sp-local-tag "2" "**" "**")
      (sp-local-tag "s" "```scheme" "```")
      (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))))

;; Enhanced undo-redo
;; -------------------------------------------------------------------
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :init (global-undo-tree-mode))
(defalias 'redo 'undo-tree-redo)
(global-set-key [(control x)(control z)] 'redo)

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)))

;; ---------------------------------------------------------------- ;;
;; Rebox (fancy comment boxes, see rebox2.el for help)              ;;
;; ---------------------------------------------------------------- ;;
(use-package rebox2
  :ensure t )
(global-set-key [(shift meta q)] 'rebox-dwim)
(global-set-key [(meta /)] 'rebox-cycle)

;; Make rebox2 work for fortran90 comments (see https://github.com/lewang/rebox2/issues/11)
(defvar rebox-language-character-alist
  '((3 . "/") (4 . "#") (5 . ";") (6 . "%") (7 . "!"))
  "Alist relating language to comment character, for generic languages.")
(defvar rebox-regexp-start
  ["^[ \t]*\\(/\\*\\|//+\\|#+\\|;+\\|%+\\)"
   "^"                                  ; 1
   "^[ \t]*/\\*"                        ; 2
   "^[ \t]*//+"                         ; 3
   "^[ \t]*#+"                          ; 4
   "^[ \t]*\;+"                         ; 5
   "^[ \t]*%+"                          ; 6
   "^[ \t]*!+"                          ; 7
   ])

;; Fix bug with dead-keys in Ubuntu 13.10, 14.04, 14.10?
;; -------------------------------------------------------------------
(require 'iso-transl)
