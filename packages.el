;;; package.el --- gabesoft-solarized-theme layer for spacemacs

;;; Commentary:

;;; Code:

(setq gabesoft-solarized-theme-packages
      '((gabesoft-solarized-theme :location local)))

(setq gabesoft-solarized-theme-files '("gabesoft-solarized-common.el"
                                       "gabesoft-solarized-dark-theme.el"
                                       "gabesoft-solarized-light-theme.el"
                                       "gabesoft-solarized-init.el"))

(dolist (file gabesoft-solarized-theme-files)
  (load-file (concat user-emacs-directory
                     "layers/+themes/gabesoft-solarized-theme/"
                     file)))

(defun gabesoft-solarized-theme/init-gabesoft-solarized-theme nil)