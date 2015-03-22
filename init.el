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

;; Fix bug with dead-keys in Ubuntu 13.10, 14.04, 14.10?
(require 'iso-transl)
