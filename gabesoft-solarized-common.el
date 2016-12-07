;;; gabesoft-solarized-common.el --- Theme

;; Copyright (C) 2016 , Gabriel Adomnicai

;; Author: Gabriel Adomnicai
;; Version: 0.1
;; Package-Requires: ((emacs "24"))
;; Created with ThemeCreator, https://github.com/mswift42/themecreator.


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;;; Code:

(defmacro gs-dyn-let (varlist fn setfaces setvars)
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defun gs-custom-colors-override () nil)

(defvar gs-solarized-colors
  '((:base03  . ("#002b36" . "#1c1c1c"))
    (:base02  . ("#073642" . "#262626"))
    (:base01  . ("#586e75" . "#4e4e4e"))
    (:base00  . ("#657b83" . "#585858"))
    (:base0   . ("#839496" . "#808080"))
    (:base1   . ("#93a1a1" . "#8a8a8a"))
    (:base2   . ("#eee8d5" . "#d7d7af"))
    (:base3   . ("#fdf6e3" . "#ffffd7"))
    (:yellow  . ("#b58900" . "#af8700"))
    (:orange  . ("#cb4b16" . "#d75f00"))
    (:red     . ("#dc322f" . "#d70000"))
    (:magenta . ("#d33682" . "#af005f"))
    (:violet  . ("#6c71c4" . "#5f5faf"))
    (:blue    . ("#268bd2" . "#0087ff"))
    (:cyan    . ("#2aa198" . "#00afaf"))
    (:green   . ("#859900" . "#5f8700")))
  "Solarized color values for gui and terminal.")

(defun gs-get-color-value (name)
  "Get the value for the color with NAME."
  (let ((item (assq name gs-solarized-colors)))
    (when item (cdr item))))

(defun gs-get-gui-color (name)
  "Get the gui value for the color with NAME."
  (car (gs-get-color-value name)))

(defun gs-get-term-color (name)
  "Get the terminal value for the color with NAME."
  (cdr (gs-get-color-value name)))

(defun gs-gui-color-p ()
  "Determine whether gui colors are supported."
  (or (display-graphic-p)
      (= (tty-display-color-cells) 16777216)))

(defun gs-get-color (name)
  "Get the value for the color NAME according to the current display capabilities."
  (if (gs-gui-color-p)
      (gs-get-gui-color name)
    (gs-get-term-color name)))

(defun gs-get-color-for-variant (variant name)
  "Get the value for the color with NAME according to VARIANT."
  (cond ((eq name :bg1) (if (eq variant 'dark) (gs-get-color :base03) (gs-get-color :base3)))
        ((eq name :bg2) (if (eq variant 'dark) (gs-get-color :base02) (gs-get-color :base2)))
        ((eq name :fg1) (if (eq variant 'dark) (gs-get-color :base1) (gs-get-color :base01)))
        ((eq name :fg2) (if (eq variant 'dark) (gs-get-color :base0) (gs-get-color :base00)))
        (t (gs-get-color name))))

(defun gs-create-gabesoft-solarized-theme (variant theme-name)
  (gs-dyn-let ((class '((class color) (min-colors 89)))
               (bg1      (gs-get-color-for-variant variant :bg1))
               (bg2      (gs-get-color-for-variant variant :bg2))
               (bg3      (gs-get-color-for-variant variant :bg1))
               (bg4      (gs-get-color-for-variant variant :bg2))
               (fg1      (gs-get-color-for-variant variant :fg1))
               (fg2      (gs-get-color-for-variant variant :fg2))
               (fg3      (gs-get-color-for-variant variant :fg1))
               (fg4      (gs-get-color-for-variant variant :fg2))

               (builtin  (gs-get-color-for-variant variant :cyan))
               (keyword  (gs-get-color-for-variant variant :blue))
               (const    (gs-get-color-for-variant variant :orange))
               (comment  (gs-get-color-for-variant variant :base00))
               (func     (gs-get-color-for-variant variant :violet))
               (str      (gs-get-color-for-variant variant :green))
               (type     (gs-get-color-for-variant variant :yellow))
               (var      (gs-get-color-for-variant variant :base1))
               (suc      (gs-get-color-for-variant variant :green))
               (err      (gs-get-color-for-variant variant :red))
               (warning  (gs-get-color-for-variant variant :red))
               (warning2 (gs-get-color-for-variant variant :magenta))

               (blue    (gs-get-color-for-variant variant :blue))
               (cyan    (gs-get-color-for-variant variant :cyan))
               (green   (gs-get-color-for-variant variant :green))
               (magenta (gs-get-color-for-variant variant :magenta))
               (orange  (gs-get-color-for-variant variant :orange))
               (red     (gs-get-color-for-variant variant :red))
               (violet  (gs-get-color-for-variant variant :violet))
               (yellow  (gs-get-color-for-variant variant :yellow)))
              gs-custom-colors-override

              (custom-theme-set-faces
               theme-name

               `(default ((,class (:background ,bg1 :foreground ,fg1))))
               `(default-italic ((,class (:italic t))))

               `(error ((,class (:foreground ,err))))
               `(warning ((,class (:foreground ,warning))))

               `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
               `(cursor ((,class (:background ,bg3))))
               `(ffap ((,class (:foreground ,fg4))))
               `(fringe ((,class (:background ,bg2 :foreground ,fg4))))
               `(header-line ((,class :background ,bg4)))
               `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
               `(hl-line ((,class (:background  ,bg2))))
               `(icompletep-determined ((,class :foreground ,builtin)))
               `(isearch ((,class (:bold t :foreground ,warning :background ,bg3))))
               `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))
               `(link ((,class (:foreground ,const :underline t))))
               `(link-visited ((,class (:foreground ,comment :underline t))))
               `(match ((,class (:background ,bg3 :foreground ,fg1))))
               `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
               `(page-break-lines ((,class (:foreground ,fg2))))
               `(region ((,class (:background ,fg1 :foreground ,bg1))))
               `(secondary-selection ((,class (:background ,bg3))))
               `(show-paren-match-face ((,class (:background ,warning))))
               `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
               `(success ((,class (:foreground ,suc))))
               `(tooltip ((,class (:background ,bg2 :foreground ,fg1 :bold nil :italic nil :underline nil))))
               `(trailing-whitespace ((,class :foreground nil :background ,warning)))
               `(vertical-border ((,class (:foreground ,fg3))))


               `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg1))))
               `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg1))))

               `(font-lock-builtin-face ((,class (:foreground ,builtin))))
               `(font-lock-comment-face ((,class (:foreground ,comment))))
               `(font-lock-constant-face ((,class (:foreground ,const))))
               `(font-lock-doc-face ((,class (:foreground ,comment))))
               `(font-lock-function-name-face ((,class (:foreground ,func ))))
               `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
               `(font-lock-negation-char-face ((,class (:foreground ,const))))
               `(font-lock-reference-face ((,class (:foreground ,const))))
               `(font-lock-string-face ((,class (:foreground ,str))))
               `(font-lock-type-face ((,class (:foreground ,type ))))
               `(font-lock-variable-name-face ((,class (:foreground ,var))))
               `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))

               `(mode-line ((,class (:box (:line-width 1 :color nil) :bold t :foreground ,fg4 :background ,bg2))))
               `(mode-line-buffer-id ((,class (:bold t :foreground ,func :background nil))))
               `(mode-line-emphasis ((,class (:foreground ,fg1))))
               `(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
               `(mode-line-inactive ((,class (:box (:line-width 1 :color nil :style pressed-button) :foreground ,var :background ,bg1 :weight normal))))

               `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
               `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
               `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
               `(org-agenda-done ((,class (:foreground ,bg4))))
               `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
               `(org-block ((,class (:foreground ,fg3))))
               `(org-code ((,class (:foreground ,fg2))))
               `(org-date ((,class (:underline t :foreground ,var) )))
               `(org-document-info-keyword ((,class (:foreground ,func))))
               `(org-done ((,class (:box (:line-width 1 :color ,bg3) :bold t :foreground ,bg4))))
               `(org-ellipsis ((,class (:foreground ,builtin))))
               `(org-footnote  ((,class (:underline t :foreground ,fg4))))
               `(org-hide ((,class (:foreground ,fg4))))
               `(org-level-1 ((,class (:bold t :foreground ,fg2 :height 1.1))))
               `(org-level-2 ((,class (:bold nil :foreground ,fg3))))
               `(org-level-3 ((,class (:bold t :foreground ,fg4))))
               `(org-level-4 ((,class (:bold nil :foreground ,bg4))))
               `(org-link ((,class (:underline t :foreground ,type ))))
               `(org-quote ((,class (:inherit org-block :slant italic))))
               `(org-scheduled ((,class (:foreground ,type))))
               `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
               `(org-sexp-date ((,class (:foreground ,fg4))))
               `(org-special-keyword ((,class (:foreground ,func))))
               `(org-todo ((,class (:box (:line-width 1 :color ,fg3) :foreground ,keyword :bold t))))
               `(org-verbatim ((,class (:foreground ,fg4))))
               `(org-verse ((,class (:inherit org-block :slant italic))))
               `(org-warning ((,class (:underline t :foreground ,warning))))

               `(font-latex-bold-face ((,class (:foreground ,type))))
               `(font-latex-italic-face ((,class (:foreground ,var :italic t))))
               `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
               `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
               `(font-latex-string-face ((,class (:foreground ,str))))

               `(ido-first-match ((,class (:foreground ,keyword :bold t))))
               `(ido-only-match ((,class (:foreground ,warning))))

               `(gnus-header-content ((,class (:foreground ,keyword))))
               `(gnus-header-from ((,class (:foreground ,var))))
               `(gnus-header-name ((,class (:foreground ,type))))
               `(gnus-header-subject ((,class (:foreground ,func :bold t))))

               `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
               `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
               `(mu4e-header-marks-face ((,class (:foreground ,type))))
               `(mu4e-view-url-number-face ((,class (:foreground ,type))))

               `(js2-external-variable ((,class (:foreground ,type  ))))
               `(js2-function-param ((,class (:foreground ,const))))
               `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
               `(js2-jsdoc-html-tag-name ((,class (:foreground ,var))))
               `(js2-jsdoc-value ((,class (:foreground ,str))))
               `(js2-private-function-call ((,class (:foreground ,const))))
               `(js2-private-member ((,class (:foreground ,fg3))))
               `(js3-error-face ((,class (:underline ,warning))))
               `(js3-external-variable-face ((,class (:foreground ,var))))
               `(js3-function-param-face ((,class (:foreground ,fg2))))
               `(js3-instance-member-face ((,class (:foreground ,const))))
               `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
               `(js3-warning-face ((,class (:underline ,keyword))))

               `(info-quoted-name ((,class (:foreground ,builtin))))
               `(info-string ((,class (:foreground ,str))))

               `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
               `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
               `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
               `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))

               `(rainbow-delimiters-depth-1-face ((,class :foreground ,fg1)))
               `(rainbow-delimiters-depth-2-face ((,class :foreground ,type)))
               `(rainbow-delimiters-depth-3-face ((,class :foreground ,var)))
               `(rainbow-delimiters-depth-4-face ((,class :foreground ,const)))
               `(rainbow-delimiters-depth-5-face ((,class :foreground ,keyword)))
               `(rainbow-delimiters-depth-6-face ((,class :foreground ,fg1)))
               `(rainbow-delimiters-depth-7-face ((,class :foreground ,type)))
               `(rainbow-delimiters-depth-8-face ((,class :foreground ,var)))
               `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))

               `(magit-branch ((,class (:foreground ,const :weight bold))))
               `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
               `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
               `(magit-diffstat-added   ((,class (:foreground ,type))))
               `(magit-diffstat-removed ((,class (:foreground ,var))))
               `(magit-hash ((,class (:foreground ,fg2))))
               `(magit-hunk-heading           ((,class (:background ,bg3))))
               `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
               `(magit-item-highlight ((,class :background ,bg3)))
               `(magit-log-author ((,class (:foreground ,fg3))))
               `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
               `(magit-process-ok ((,class (:foreground ,func :weight bold))))
               `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
               `(magit-section-highlight      ((,class (:background ,bg2))))

               `(term ((,class (:foreground ,fg1 :background ,bg1))))
               `(term-color-black ((,class (:foreground ,bg3 :background ,bg3))))
               `(term-color-blue ((,class (:foreground ,func :background ,func))))
               `(term-color-cyan ((,class (:foreground ,str :background ,str))))
               `(term-color-green ((,class (:foreground ,type :background ,bg3))))
               `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
               `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
               `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))
               `(term-color-yellow ((,class (:foreground ,var :background ,var))))

               `(helm-bookmark-w3m ((,class (:foreground ,type))))
               `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg1))))
               `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
               `(helm-buffer-saved-out ((,class (:foreground ,fg1 :background ,bg1))))
               `(helm-buffer-size ((,class (:foreground ,fg1 :background ,bg1))))
               `(helm-candidate-number ((,class (:foreground ,bg1 :background ,fg1))))
               `(helm-ff-directory ((,class (:foreground ,func :background ,bg1 :weight bold))))
               `(helm-ff-executable ((,class (:foreground ,var :background ,bg1 :weight normal))))
               `(helm-ff-file ((,class (:foreground ,fg1 :background ,bg1 :weight normal))))
               `(helm-ff-invalid-symlink ((,class (:foreground ,warning2 :background ,bg1 :weight bold))))
               `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
               `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg1 :weight bold))))
               `(helm-grep-cmd-line ((,class (:foreground ,fg1 :background ,bg1))))
               `(helm-grep-file ((,class (:foreground ,fg1 :background ,bg1))))
               `(helm-grep-finish ((,class (:foreground ,fg2 :background ,bg1))))
               `(helm-grep-lineno ((,class (:foreground ,fg1 :background ,bg1))))
               `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
               `(helm-grep-running ((,class (:foreground ,func :background ,bg1))))
               `(helm-header ((,class (:foreground ,fg2 :background ,bg1 :underline nil :box nil))))
               `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg1))))
               `(helm-selection ((,class (:background ,bg2 :underline nil))))
               `(helm-selection-line ((,class (:background ,bg2))))
               `(helm-separator ((,class (:foreground ,type :background ,bg1))))
               `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
               `(helm-source-header ((,class (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
               `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
               `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
               `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))

               `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
               `(company-preview ((,class (:background ,bg1 :foreground ,var))))
               `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
               `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
               `(company-scrollbar-bg ((,class (:background ,bg3))))
               `(company-scrollbar-fg ((,class (:foreground ,keyword))))
               `(company-template-field ((,class (:inherit region))))
               `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1 :bold t))))
               `(company-tooltip-common ((,class ( :foreground ,fg3))))
               `(company-tooltip-common-selection ((,class (:foreground ,str))))
               `(company-tooltip-mouse ((,class (:inherit highlight))))
               `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
               `(company-tooltop-annotation ((,class (:foreground ,const))))

               `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
               `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
               `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
               `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
               `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
               `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
               `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
               `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
               `(web-mode-keyword-face ((,class (:foreground ,keyword))))
               `(web-mode-string-face ((,class (:foreground ,str))))
               `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
               `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

               `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
               `(jde-java-font-lock-modifier-face ((t (:foreground ,fg2))))
               `(jde-java-font-lock-number-face ((t (:foreground ,var))))
               `(jde-java-font-lock-package-face ((t (:foreground ,var))))
               `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
               `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
               `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword)))))

              (custom-theme-set-variables
               theme-name
               `(ansi-color-names-vector [,bg4 ,red ,green ,yellow ,blue ,magenta ,cyan ,bg1]))

              ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'gabesoft-solarized-common)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; gabesoft-solarized-common.el ends here
