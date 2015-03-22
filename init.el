;; -*- coding: utf-8 -*-
;;
;; Emacs Easy Config
;; Rafa R. Galvan <rafael(dot)rodriguez(at)uca.es>
;;
;; https://bitbucket.org/proyecto-ucaccar/emacs-easy-config

;; No startup screen
(setq inhibit-startup-message t)

;; Directoy for local emacs lisp files:  ~/.emacs.d/site-lisp/
(add-to-list 'load-path (expand-file-name "~/.emacs.d/setup"))

;; Recent files
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

;;
;; Package management
;; ------------------------------------------------------------
;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;; In current emacs configuration we will employ 'use-package' for
;; install and config packages. See https://github.com/jwiegley/use-package

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))


;; Fix bug with dead-keys in Ubuntu 13.10, 14.04, 14.10?
(require 'iso-transl)
