;;
;; FreeFemm++
;;
(autoload 'freefem++-mode "freefem++-mode" "Major mode for editing FreeFem++ code." t)
(add-to-list 'auto-mode-alist '("\\.edp$" . freefem++-mode))
(add-to-list 'auto-mode-alist '("\\.idp$" . freefem++-mode))
(add-to-list 'auto-mode-alist '("\\.tdp$" . freefem++-mode))

;;(setq freefempp-program "/home/software/bin/FreeFem++")
;; (setq freefempp-program "FreeFem++-nw")
(setq freefempp-program "FreeFem++")
;; (setq freefempp-program "ff-mpirun -np 2")

(org-babel-do-load-languages
      'org-babel-load-languages
      '((freefem++ . t)))

(provide 'setup-freefem)
