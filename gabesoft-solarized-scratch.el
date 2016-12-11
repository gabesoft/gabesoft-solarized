(require 'gabesoft-solarized-light-theme)
(require 'gabesoft-solarized-dark-theme)

(defun gabesoft-solarized-enable-theme (theme)
  "Enable all variable and face settings defined by THEME."
  (enable-theme theme)
  (spaceline-compile)
  (set-font-camingo-small))

(defun gabesoft-solarized-enable-dark ()
  "Enable the `gabesoft-solarized-dark' theme."
  (interactive)
  (gabesoft-solarized-enable-theme 'gabesoft-solarized-dark))

(defun gabesoft-solarized-enable-light ()
  "Enable the `gabesoft-solarized-light' theme."
  (interactive)
  (gabesoft-solarized-enable-theme 'gabesoft-solarized-light))
