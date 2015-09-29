;;; org-babel-freefem++.el --- org-babel functions for freefem++ evaluation

;; Copyright (c) 2009, 2010, 2011 Eric S Fraga, Eric Schulte

;; Author: Eric S Fraga, Eric Schulte
;; Keywords: literate programming, reproducible research, freefem++
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-Babel support for evaluating freefem++ entries.
;;
;; This differs from most standard languages in that
;;
;; 1) there is no such thing as a "session" in freefem++
;;
;; 2) we are generally only going to return output from freefem++
;;
;; 3) we are adding the "cmdline" header argument
;;
;; 4) there are no variables

;;; Code:
(require 'ob)

(defvar org-babel-default-header-args:freefem++ '())

(defun org-babel-freefem++-expand (body params)
  "Expand a block of Freefem++ code according to its header arguments."
  body)

(defun org-babel-execute:freefem++ (body params)
  "Execute a block of Freefem++ entries with org-babel.  This function is
called by `org-babel-execute-src-block'."
  (message "executing Freefem++ source code block")
  (let* ((result-params (split-string (or (cdr (assoc :results params)) "")))
	 (cmdline (cdr (assoc :cmdline params)))
	 (in-file (org-babel-temp-file "freefem++-"))
;	 (cmd (format "maxima --very-quiet -r 'batchload(%S)$' %s"
	 (cmd (format "FreeFem++ %S"
		      in-file cmdline)))
    (with-temp-file in-file (insert body))
    (message cmd)
    ((lambda (raw) ;; " | grep -v batch | grep -v 'replaced' | sed '/^$/d' "
       (mapconcat
	#'identity
	(delq nil
	      (mapcar (lambda (line)
			(unless (or (string-match "batch" line)
				    (string-match "^rat: replaced .*$" line)
				    (= 0 (length line)))
			  line))
		      (split-string raw "[\r\n]"))) "\n"))
     (org-babel-eval cmd ""))))

(defun org-babel-prep-session:freefem++ (session params)
  (error "Freefem++ does not support sessions"))

(provide 'ob-freefem++)

;; arch-tag: d86c97ac-7eab-4349-8d8b-302dd09779a8

;;; org-babel-freefem++.el ends here
