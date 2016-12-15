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
;;
;; An alternative version of Solarized

;;; Installation:
;;
;; Drop the `gabesoft-solarized-common.el' somewhere in your `load-path' and
;; the two themes in a folder that is on `custom-theme-load-path'
;; and enjoy!
;;
;;; Credits
;;
;; Bohidar Batsov created the original `solarized-theme' on which this
;; version is based
;;
;;; Code:

(defmacro gabesoft-solarized-dyn-let (varlist fn setfaces setvars)
  (list 'let (append varlist (funcall fn)) setfaces setvars))

(defun gabesoft-solarized-custom-colors-override () nil)

(defvar gabesoft-solarized-solarized-colors
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

(defun gabesoft-solarized-get-color-value (name)
  "Get the value for the color with NAME."
  (let ((item (assq name gabesoft-solarized-solarized-colors)))
    (when item (cdr item))))

(defun gabesoft-solarized-get-gui-color (name)
  "Get the gui value for the color with NAME."
  (car (gabesoft-solarized-get-color-value name)))

(defun gabesoft-solarized-get-term-color (name)
  "Get the terminal value for the color with NAME."
  (cdr (gabesoft-solarized-get-color-value name)))

(defun gabesoft-solarized-gui-color-p ()
  "Determine whether gui colors are supported."
  (or (display-graphic-p)
      (= (tty-display-color-cells) 16777216)))

(defun gabesoft-solarized-get-color (name)
  "Get the value for the color NAME according to the current display capabilities."
  (if (gabesoft-solarized-gui-color-p)
      (gabesoft-solarized-get-gui-color name)
    (gabesoft-solarized-get-term-color name)))

(defun gabesoft-solarized-get-color-for-variant (variant name)
  "Get the value for the color with NAME according to VARIANT."
  (cond ((eq name :bg-base) (if (eq variant 'dark) (gabesoft-solarized-get-color :base03) (gabesoft-solarized-get-color :base3)))
        ((eq name :bg-emph) (if (eq variant 'dark) (gabesoft-solarized-get-color :base02) (gabesoft-solarized-get-color :base2)))
        ((eq name :fg-base) (if (eq variant 'dark) (gabesoft-solarized-get-color :base0) (gabesoft-solarized-get-color :base00)))
        ((eq name :fg-emph) (if (eq variant 'dark) (gabesoft-solarized-get-color :base1) (gabesoft-solarized-get-color :base01)))

        ((eq name :bg-rev-base) (if (eq variant 'dark) (gabesoft-solarized-get-color :base00) (gabesoft-solarized-get-color :base0)))
        ((eq name :bg-rev-emph) (if (eq variant 'dark) (gabesoft-solarized-get-color :base01) (gabesoft-solarized-get-color :base1)))
        ((eq name :fg-rev-base) (if (eq variant 'dark) (gabesoft-solarized-get-color :base02) (gabesoft-solarized-get-color :base2)))
        ((eq name :fg-rev-emph) (if (eq variant 'dark) (gabesoft-solarized-get-color :base03) (gabesoft-solarized-get-color :base3)))

        (t (gabesoft-solarized-get-color name))))

(defun gabesoft-solarized-flip-variant (variant)
  "Revert the specified variant."
  (if (eq variant 'dark) 'light 'dark))


(defun gabesoft-solarized-create-theme (variant theme-name)
  "Create a theme based on VARIANT named THEME-NAME."
  (gabesoft-solarized-dyn-let
   ((class '((class color) (min-colors 89)))
    (bg-base      (gabesoft-solarized-get-color-for-variant variant :bg-base))
    (bg-emph      (gabesoft-solarized-get-color-for-variant variant :bg-emph))
    (bg-rev-base  (gabesoft-solarized-get-color-for-variant variant :bg-rev-base))
    (bg-rev-emph  (gabesoft-solarized-get-color-for-variant variant :bg-rev-emph))
    (fg-base      (gabesoft-solarized-get-color-for-variant variant :fg-base))
    (fg-emph      (gabesoft-solarized-get-color-for-variant variant :fg-emph))
    (fg-rev-base  (gabesoft-solarized-get-color-for-variant variant :fg-rev-base))
    (fg-rev-emph  (gabesoft-solarized-get-color-for-variant variant :fg-rev-emph))

    (builtin  (gabesoft-solarized-get-color-for-variant variant :green))
    (comment  (gabesoft-solarized-get-color-for-variant variant :bg-rev-emph))
    (const    (gabesoft-solarized-get-color-for-variant variant :cyan))
    (doc      (gabesoft-solarized-get-color-for-variant variant :bg-rev-base))
    (doc-bg   (gabesoft-solarized-get-color-for-variant variant :bg-emph))
    (err      (gabesoft-solarized-get-color-for-variant variant :red))
    (func     (gabesoft-solarized-get-color-for-variant variant :blue))
    (keyword  (gabesoft-solarized-get-color-for-variant variant :green))
    (mat      (gabesoft-solarized-get-color-for-variant variant :magenta))
    (str      (gabesoft-solarized-get-color-for-variant variant :cyan))
    (suc      (gabesoft-solarized-get-color-for-variant variant :green))
    (type     (gabesoft-solarized-get-color-for-variant variant :yellow))
    (var      (gabesoft-solarized-get-color-for-variant variant :blue))
    (war      (gabesoft-solarized-get-color-for-variant variant :orange))
    (war2     (gabesoft-solarized-get-color-for-variant variant :yellow))

    (blue    (gabesoft-solarized-get-color-for-variant variant :blue))
    (cyan    (gabesoft-solarized-get-color-for-variant variant :cyan))
    (green   (gabesoft-solarized-get-color-for-variant variant :green))
    (magenta (gabesoft-solarized-get-color-for-variant variant :magenta))
    (orange  (gabesoft-solarized-get-color-for-variant variant :orange))
    (red     (gabesoft-solarized-get-color-for-variant variant :red))
    (violet  (gabesoft-solarized-get-color-for-variant variant :violet))
    (yellow  (gabesoft-solarized-get-color-for-variant variant :yellow)))
   gabesoft-solarized-custom-colors-override

   (custom-theme-set-faces
    theme-name

    ;; basics
    `(default ((,class (:background ,bg-base :foreground ,fg-base))))
    `(default-italic ((,class (:italic t))))

    `(error ((,class (:foreground ,err))))
    `(warning ((,class (:foreground ,war))))
    `(success ((,class (:foreground ,suc))))

    `(highlight ((,class (:foreground ,yellow :background ,bg-emph :bold t))))
    `(isearch ((,class (:bold t :foreground ,mat :background ,bg-emph))))
    `(lazy-highlight ((,class (:foreground ,yellow :background ,bg-emph :bold t))))

    '(button ((t (:underline t))))
    `(cursor ((,class (:background ,bg-rev-base :background ,bg-base :inverse-video t))))
    `(custom-button ((,class :background ,bg-emph :foreground ,fg-base :box nil)))
    `(escape-glyph ((,class (:foreground ,violet))))
    `(ffap ((,class (:foreground ,fg-rev-emph))))
    `(fringe ((,class (:background ,bg-base :foreground ,comment))))
    `(hl-line ((,class (:background  ,bg-emph))))
    `(hl-line-face ((,class (:background  ,bg-emph))))
    `(icompletep-determined ((,class :foreground ,builtin)))
    `(link ((,class (:foreground ,comment :underline t :italic nil :bold t))))
    `(link-visited ((,class (:foreground ,comment :underline t :italic nil :bold nil))))
    `(match ((,class (:background ,bg-emph :foreground ,mat :weight bold))))
    `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
    `(page-break-lines ((,class (:foreground ,fg-emph))))
    `(region ((,class (:foreground ,bg-base :background ,fg-base))))
    `(secondary-selection ((,class (:foreground ,bg-rev-emph :background ,fg-rev-emph))))
    `(shadow ((,class (:foreground ,fg-emph :background ,bg-emph))))
    `(show-paren-match-face ((,class (:background ,mat))))
    `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
    `(tooltip ((,class (:background ,bg-base
                                    :foreground ,fg-base
                                    :bold nil
                                    :italic nil
                                    :underline nil
                                    :box (:line-width 4 :color ,bg-emph)))))
    `(trailing-whitespace ((,class :foreground nil :background ,war)))
    `(vertical-border ((,class (:foreground ,bg-emph))))

    `(eval-sexp-fu-flash ((,class (:background ,suc :foreground ,bg-base))))
    `(eval-sexp-fu-flash-error ((,class (:background ,err :foreground ,bg-base))))

    `(font-lock-builtin-face ((,class (:foreground ,builtin :bold nil :italic nil))))
    `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :background ,bg-base :slant ,'normal))))
    `(font-lock-comment-face ((,class (:foreground ,comment :background ,bg-base :italic nil :bold nil))))
    `(font-lock-constant-face ((,class (:foreground ,const :bold t))))
    `(font-lock-doc-face ((,class (:foreground ,doc :background ,doc-bg :italic nil :bold nil))))
    `(font-lock-function-name-face ((,class (:foreground ,func))))
    `(font-lock-keyword-face ((,class (:foreground ,keyword :bold t))))
    `(font-lock-negation-char-face ((,class (:foreground ,const))))
    `(font-lock-preprocessor-face ((,class (:foreground ,func))))
    `(font-lock-reference-face ((,class (:foreground ,const))))
    `(font-lock-string-face ((,class (:foreground ,str))))
    `(font-lock-type-face ((,class (:foreground ,type))))
    `(font-lock-variable-name-face ((,class (:foreground ,var :bold t))))
    `(font-lock-warning-face ((,class (:foreground ,err :background ,bg-base :bold t))))

    ;;ahs
    `(ahs-face ((,class (:background ,bg-emph :foreground ,yellow :bold t))))
    `(ahs-plugin-whole-buffer-face ((,class (:background ,fg-base :foreground ,bg-base :bold t))))

    ;; anzu
    `(anzu-mode-line ((,class (:foreground ,yellow :inherit bold))))

    ;; auto-complete
    `(ac-completion-face ((,class (:underline t :foreground ,keyword))))

    ;; avy
    `(avy-lead-face   ((,class (:background ,bg-base :foreground ,magenta))))
    `(avy-lead-face-0 ((,class (:background ,bg-base :foreground ,blue))))
    `(avy-lead-face-1 ((,class (:background ,bg-base :foreground ,yellow))))
    `(avy-lead-face-2 ((,class (:background ,bg-base :foreground ,green))))

    ;; cider
    `(cider-enlightened ((,class (:background nil :box nil :foreground ,yellow))))
    `(cider-enlightened-local ((,class (:foreground ,yellow))))
    `(cider-instrumented-face ((,class (:background nil :box nil :foreground ,red))))
    `(cider-result-overlay-face ((,class (:background nil :box nil :foreground ,blue))))
    `(cider-test-error-face ((,class (:background ,err :foreground ,bg-base))))
    `(cider-test-failure-face ((,class (:background ,err :foreground ,bg-base))))
    `(cider-test-success-face ((,class (:background ,suc :foreground ,bg-base))))
    `(cider-traced-face ((,class :box nil)))

    ;; company
    `(company-echo-common ((,class (:foreground ,bg-base :background ,fg-base))))
    `(company-preview ((,class (:foreground ,blue :background ,bg-base))))
    `(company-preview-common ((,class (:foreground ,fg-base :background ,bg-emph :underline nil))))
    `(company-preview-search ((,class (:foreground ,blue :background ,bg-base :underline nil))))
    `(company-scrollbar-bg ((,class (:background ,bg-base))))
    `(company-scrollbar-fg ((,class (:background ,bg-emph))))
    `(company-template-field ((,class (:inherit region :underline nil))))
    `(company-tooltip ((,class (:foreground ,fg-emph :background ,bg-base :bold t))))
    `(company-tooltip-annotation ((,class (:foreground ,magenta :underline nil))))
    `(company-tooltip-common ((,class (:foreground ,blue :underline nil))))
    `(company-tooltip-common-selection ((,class (:foreground ,yellow :underline nil))))
    `(company-tooltip-mouse ((,class (:inherit highlight :underline nil))))
    `(company-tooltip-search ((,class (:inherit match :underline nil))))
    `(company-tooltip-selection ((,class (:foreground ,fg-base :background ,bg-emph :underline nil))))
    `(company-tooltop-annotation ((,class (:foreground ,const))))

    ;; diff
    `(diff-added             ((,class :foreground ,green :inverse-video t)))
    `(diff-changed           ((,class :foreground ,yellow :inverse-video t)))
    `(diff-header            ((,class :background ,bg-emph :foreground ,func)))
    `(diff-indicator-added   ((,class :background nil :foreground ,green)))
    `(diff-indicator-changed ((,class :background nil :foreground ,keyword)))
    `(diff-indicator-removed ((,class :background nil :foreground ,red)))
    `(diff-refine-added      ((,class :background ,green :foreground ,bg-rev-emph)))
    `(diff-refine-changed    ((,class :background ,keyword :foreground ,bg-rev-emph)))
    `(diff-refine-removed    ((,class :background ,red :foreground ,bg-rev-emph)))
    `(diff-removed           ((,class :foreground ,red :inverse-video t)))

    ;; diff-hl
    `(diff-hl-change ((,class :background ,bg-emph :foreground ,blue)))
    `(diff-hl-delete ((,class :background ,bg-emph :foreground ,red)))
    `(diff-hl-insert ((,class :background ,bg-emph :foreground ,green)))

    ;; dired
    `(dired-directory ((,class (:foreground ,keyword :background ,bg-base :bold t))))
    `(dired-flagged ((,class (:foreground ,red))))
    `(dired-header ((,class (:foreground ,yellow :background ,bg-emph :bold t))))
    `(dired-ignored ((,class (:inherit shadow))))
    `(dired-mark ((,class (:foreground ,orange :inherit bold))))
    `(dired-marked ((,class (:foreground ,magenta :inherit bold))))
    `(dired-perm-write ((,class (:foreground ,fg-base :underline t))))
    `(dired-symlink ((,class (:foreground ,cyan :background ,bg-base :inherit bold))))
    `(dired-warning ((,class (:foreground ,war))))

    ;; ediff
    `(ediff-current-diff-A ((,class(:background ,bg-emph :foreground ,red))))
    `(ediff-current-diff-Ancestor ((,class(:background ,bg-emph :foreground ,cyan))))
    `(ediff-current-diff-B ((,class(:background ,bg-emph :foreground ,green))))
    `(ediff-current-diff-C ((,class(:background ,bg-emph :foreground ,blue))))
    `(ediff-even-diff-A ((,class(:background ,bg-rev-base))))
    `(ediff-even-diff-Ancestor ((,class(:background ,bg-rev-base))))
    `(ediff-even-diff-B ((,class(:background ,bg-rev-base))))
    `(ediff-even-diff-C ((,class(:background ,bg-rev-base))))
    `(ediff-fine-diff-A ((,class(:background nil :inherit bold))))
    `(ediff-fine-diff-Ancestor ((,class(:background nil :inherit bold))))
    `(ediff-fine-diff-B ((,class(:background nil :inherit bold))))
    `(ediff-fine-diff-C ((,class(:background nil :inherit bold))))
    `(ediff-odd-diff-A ((,class(:background ,bg-rev-emph))))
    `(ediff-odd-diff-Ancestor ((,class(:background ,bg-rev-emph))))
    `(ediff-odd-diff-B ((,class(:background ,bg-rev-emph))))
    `(ediff-odd-diff-C ((,class(:background ,bg-rev-emph))))

    ;; ein
    `(ein:cell-input-area((,class (:background ,bg-emph))))
    `(ein:cell-input-prompt ((,class (:foreground ,suc))))
    `(ein:cell-output-prompt ((,class (:foreground ,err))))
    `(ein:notification-tab-normal ((,class (:foreground ,keyword))))
    `(ein:notification-tab-selected ((,class (:foreground ,suc :inherit bold))))

    ;; eldoc
    `(eldoc-highlight-function-argument ((,class (:foreground ,yellow :inherit bold))))

    ;; elfeed
    `(elfeed-search-title-face ((,class (:foreground ,fg-emph))))
    `(elfeed-search-unread-title-face ((,class (:foreground ,fg-base))))
    `(elfeed-search-feed-face ((,class (:foreground ,blue))))
    `(elfeed-search-tag-face ((,class (:foreground ,func))))

    ;; enh-ruby
    `(enh-ruby-string-delimiter-face ((,class (:foreground ,str))))
    `(enh-ruby-op-face ((,class (:background ,bg-base :foreground ,fg-base))))

    ;; erc
    `(erc-input-face ((,class (:foreground ,func))))
    `(erc-my-nick-face ((,class (:foreground ,keyword))))
    `(erc-nick-default-face ((,class (:foreground ,keyword))))
    `(erc-nick-prefix-face ((,class (:foreground ,yellow))))
    `(erc-notice-face ((,class (:foreground ,str))))
    `(erc-prompt-face ((,class (:foreground ,yellow :inherit bold))))
    `(erc-timestamp-face ((,class (:foreground ,keyword))))

    ;; evil
    `(evil-ex-substitute-matches ((,class (:background ,bg-base :foreground ,red))))
    `(evil-ex-substitute-replacement ((,class (:background ,bg-base :foreground ,green))))

    ;; eshell
    `(eshell-ls-archive ((,class (:foreground ,red :inherit bold))))
    `(eshell-ls-backup ((,class (:inherit font-lock-comment-face))))
    `(eshell-ls-clutter ((,class (:inherit font-lock-comment-face))))
    `(eshell-ls-directory ((,class (:foreground ,keyword :inherit bold))))
    `(eshell-ls-executable ((,class (:foreground ,suc :inherit bold))))
    `(eshell-ls-missing ((,class (:inherit font-lock-warning-face))))
    `(eshell-ls-product ((,class (:inherit font-lock-doc-face))))
    `(eshell-ls-special ((,class (:foreground ,yellow :inherit bold))))
    `(eshell-ls-symlink ((,class (:foreground ,cyan :inherit bold))))
    `(eshell-ls-unreadable ((,class (:foreground ,fg-base))))
    `(eshell-prompt ((,class (:foreground ,keyword :inherit bold))))

    ;; flycheck
    `(flycheck-error ((,(append '((supports :underline (:style line))) class)
                       (:underline (:style line :color ,err)))
                      (,class (:foreground ,fg-base :background ,err :inherit bold :underline t))))
    `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
    `(flycheck-fringe-error ((,class (:foreground ,err :background ,bg-base :inherit bold))))
    `(flycheck-fringe-info ((,class (:foreground ,keyword :background ,bg-base :inherit bold))))
    `(flycheck-fringe-warning ((,class (:foreground ,war :background ,bg-base :inherit bold))))
    `(flycheck-info ((,(append '((supports :underline (:style line))) class)
                      (:underline (:style line :color ,keyword)))
                     (,class (:foreground ,fg-base :background ,keyword :inherit bold :underline t))))
    `(flycheck-warning ((,(append '((supports :underline (:style line))) class)
                         (:underline (:style line :color ,war)))
                        (,class (:foreground ,fg-base :background ,war :inherit bold :underline t))))

    ;; flyspell
    `(flyspell-duplicate
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,yellow) :inherit unspecified))
       (,class (:foreground ,yellow :weight bold :underline t))))
    `(flyspell-incorrect
      ((,(append '((supports :underline (:style line))) class)
        (:underline (:style line :color ,red) :inherit unspecified))
       (,class (:foreground ,red :weight bold :underline t))))

    ;; jabber
    `(jabber-activity-face ((,class (:inherit bold :foreground ,red))))
    `(jabber-activity-personal-face ((,class (:inherit bold :foreground ,blue))))
    `(jabber-chat-error ((,class (:inherit bold :foreground ,red))))
    `(jabber-chat-prompt-foreign ((,class (:inherit bold :foreground ,red))))
    `(jabber-chat-prompt-local ((,class (:inherit bold :foreground ,blue))))
    `(jabber-chat-prompt-system ((,class (:inherit bold :foreground ,green))))
    `(jabber-chat-text-foreign ((,class (:foreground ,fg-base))))
    `(jabber-chat-text-local ((,class (:foreground ,fg-base))))
    `(jabber-rare-time-face ((,class (:foreground ,green))))
    `(jabber-roster-user-away ((,class (:foreground ,yellow))))
    `(jabber-roster-user-chatty ((,class (:inherit bold :foreground ,green))))
    `(jabber-roster-user-dnd ((,class (:foreground ,red))))
    `(jabber-roster-user-error ((,class (:foreground ,err))))
    `(jabber-roster-user-offline ((,class (:foreground ,fg-base))))
    `(jabber-roster-user-online ((,class (:inherit bold :foreground ,green))))
    `(jabber-roster-user-xa ((,class (:foreground ,cyan))))

    ;; git-gutter
    `(git-gutter-fr:added ((,class (:foreground ,green :inherit bold))))
    `(git-gutter-fr:deleted ((,class (:foreground ,war :inherit bold))))
    `(git-gutter-fr:modified ((,class (:foreground ,keyword :inherit bold))))

    ;; git-timemachine
    `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,blue :inherit bold :background ,fg-emph))))

    ;; gnus
    `(gnus-emphasis-highlight-words ((,class (:background ,suc :foreground ,bg-base))))
    `(gnus-header-content ((,class (:foreground ,keyword))))
    `(gnus-header-from ((,class (:foreground ,var))))
    `(gnus-header-name ((,class (:foreground ,type))))
    `(gnus-header-subject ((,class (:foreground ,func :bold t))))
    `(gnus-summary-cancelled ((,class (:background ,war :foreground ,bg-base))))

    ;; guide-key
    `(guide-key/highlight-command-face ((,class (:foreground ,fg-base))))
    `(guide-key/key-face ((,class (:foreground ,keyword))))
    `(guide-key/prefix-command-face ((,class (:foreground ,keyword :inherit bold))))

    ;; haskell
    `(haskell-pragma-face ((,class (:foreground ,comment :bold t))))
    `(haskell-operator-face ((,class (:foreground ,blue :bold nil))))


    ;; header-line & mode-line
    `(header-line
      ((,class (:inverse-video unspecified
                               :overline ,bg-emph
                               :underline ,bg-emph
                               :foreground ,fg-base
                               :background ,bg-base
                               :bold t
                               :box (:line-width 2 :color ,bg-emph :style unspecified)))))
    `(mode-line
      ((,class (:inverse-video unspecified
                               :overline ,bg-emph
                               :underline ,bg-emph
                               :foreground ,bg-base
                               :background ,fg-emph
                               :box (:line-width 1 :color ,bg-emph :style unspecified)
                               ))))
    `(mode-line-buffer-id ((,class (:foreground unspecified :inherit bold))))
    `(mode-line-inactive
      ((,class (:inverse-video unspecified
                               :overline ,bg-emph
                               :underline ,bg-emph
                               :foreground ,fg-base
                               :background ,bg-emph
                               :box (:line-width 1 :color ,bg-emph :style unspecified)))))

    ;; helm
    `(helm-bookmark-directory ((,class (:inherit helm-ff-directory))))
    `(helm-bookmark-file ((,class (:foreground ,fg-base))))
    `(helm-bookmark-gnus ((,class (:foreground ,orange))))
    `(helm-bookmark-info ((,class (:foreground ,orange))))
    `(helm-bookmark-man ((,class (:foreground ,orange))))
    `(helm-bookmark-w3m ((,class (:foreground ,type))))
    `(helm-buffer-directory ((,class (:foreground ,fg-base :background ,bg-base))))
    `(helm-buffer-file ((,class (:foreground ,fg-base :background ,bg-base))))
    `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg-base))))
    `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg-base))))
    `(helm-buffer-saved-out ((,class (:foreground ,fg-base :background ,bg-base))))
    `(helm-buffer-size ((,class (:foreground ,fg-base :background ,bg-base))))
    `(helm-candidate-number ((,class (:foreground ,bg-base :background ,fg-base))))
    `(helm-ff-directory ((,class (:foreground ,func :background ,bg-base :weight bold))))
    `(helm-ff-dotted-directory ((,class (:foreground ,keyword :background ,bg-base :inherit bold))))
    `(helm-ff-dotted-symlink-directory ((,class (:foreground ,cyan :background ,bg-base :inherit bold))))
    `(helm-ff-executable ((,class (:foreground ,var :background ,bg-base :weight normal))))
    `(helm-ff-file ((,class (:foreground ,fg-base :background ,bg-base :weight normal))))
    `(helm-ff-invalid-symlink ((,class (:foreground ,war2 :background ,bg-base :weight bold))))
    `(helm-ff-prefix ((,class (:foreground ,bg-base :background ,keyword :weight normal))))
    `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg-base :weight bold))))
    `(helm-grep-cmd-line ((,class (:foreground ,fg-base :background ,bg-base))))
    `(helm-grep-file ((,class (:foreground ,fg-base :background ,bg-base))))
    `(helm-grep-finish ((,class (:foreground ,fg-emph :background ,bg-base))))
    `(helm-grep-lineno ((,class (:foreground ,type :background ,bg-base :inherit bold))))
    `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
    `(helm-grep-running ((,class (:foreground ,func :background ,bg-base))))
    `(helm-header ((,class (:inherit header-line))))
    `(helm-header-line-left-margin ((,class (:inherit header-line))))
    `(helm-match ((,class (:background ,bg-base :foreground ,mat))))
    `(helm-match-item ((,class (:background ,bg-emph :foreground ,yellow))))
    `(helm-moccur-buffer ((,class (:foreground ,func :background ,bg-base))))
    `(helm-selection ((,class (:background ,bg-emph :underline t))))
    `(helm-selection-line ((,class (:background ,bg-base :underline t))))
    `(helm-separator ((,class (:foreground ,type :background ,bg-base))))
    `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
    `(helm-source-header ((,class (:foreground ,yellow :background ,bg-base :underline nil :bold t :italic t))))
    `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg-base))))
    `(helm-time-zone-home ((,class (:foreground ,type :background ,bg-base))))
    `(helm-visible-mark ((,class (:foreground ,keyword :background ,bg-emph))))

    ;; helm-swoop
    `(helm-swoop-target-line-block-face ((,class (:foreground ,fg-base :background ,bg-emph))))
    `(helm-swoop-target-line-face ((,class (:background ,bg-emph))))
    `(helm-swoop-target-word-face ((,class (:foreground ,mat :background ,bg-emph))))

    ;; highlights
    `(hi-yellow ((,class (:foreground ,yellow :background ,bg-emph))))
    `(hi-green  ((,class (:foreground ,green :background ,bg-emph))))

    ;; highlight-indentation
    `(highlight-indentation-face ((,class (:background ,bg-emph))))

    ;; highlight-symbol
    `(highlight-symbol-face ((,class (:background ,bg-emph))))

    ;; hydra
    `(hydra-face-blue ((,class (:foreground ,blue))))
    `(hydra-face-red ((,class (:foreground ,red))))

    ;; info
    `(info-header-xref ((,class (:foreground ,func :underline t))))
    `(info-menu ((,class (:foreground ,suc))))
    `(info-node ((,class (:foreground ,func :inherit bold))))
    `(info-quoted-name ((,class (:foreground ,keyword))))
    `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
    `(info-string ((,class (:foreground ,str))))
    `(info-title-1 ((,class (:height 1.4 :inherit bold))))
    `(info-title-2 ((,class (:height 1.3 :inherit bold))))
    `(info-title-3 ((,class (:height 1.3))))
    `(info-title-4 ((,class (:height 1.2))))

    ;; ido
    `(ido-first-match ((,class (:foreground ,keyword :bold t))))
    `(ido-only-match ((,class (:foreground ,mat))))
    `(ido-subdir ((,class (:foreground ,keyword))))
    `(ido-vertical-match-face ((,class (:foreground ,yellow :underline nil))))

    ;; ivy
    `(ivy-current-match ((,class (:background ,bg-emph :inherit bold))))
    `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
    `(ivy-minibuffer-match-face-2 ((,class (:foreground ,blue :underline t))))
    `(ivy-minibuffer-match-face-3 ((,class (:foreground ,green :underline t))))
    `(ivy-minibuffer-match-face-4 ((,class (:foreground ,orange :underline t))))
    `(ivy-remote ((,class (:foreground ,cyan))))

    ;; jde
    `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
    `(jde-java-font-lock-modifier-face ((t (:foreground ,fg-emph))))
    `(jde-java-font-lock-number-face ((t (:foreground ,var))))
    `(jde-java-font-lock-package-face ((t (:foreground ,var))))
    `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
    `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
    `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))

    ;; js2
    `(js2-external-variable ((,class (:foreground ,type  ))))
    `(js2-function-param ((,class (:foreground ,const))))
    `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
    `(js2-jsdoc-html-tag-name ((,class (:foreground ,var))))
    `(js2-jsdoc-value ((,class (:background ,doc-bg))))
    `(js2-jsdoc-tag ((,class (:background ,doc-bg))))
    `(js2-jsdoc-type ((,class (:background ,doc-bg))))
    `(js2-private-function-call ((,class (:foreground ,const))))
    `(js2-private-member ((,class (:foreground ,fg-rev-base))))
    `(js3-error-face ((,class (:underline ,war))))
    `(js3-external-variable-face ((,class (:foreground ,var))))
    `(js3-function-param-face ((,class (:foreground ,fg-emph))))
    `(js3-instance-member-face ((,class (:foreground ,const))))
    `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
    `(js3-warning-face ((,class (:underline ,keyword))))

    ;; latex
    `(font-latex-bold-face ((,class (:foreground ,type))))
    `(font-latex-italic-face ((,class (:foreground ,var :italic t))))
    `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
    `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
    `(font-latex-string-face ((,class (:foreground ,str))))

    ;; linum-mode
    `(linum ((,class (:foreground ,fg-base :background ,bg-base))))

    ;; linum-relative
    `(linum-relative-current-face ((,class (:foreground ,yellow :bold t))))

    ;; magit
    `(magit-blame-culprit ((,class :background ,bg-emph :foreground ,yellow)))
    `(magit-blame-date    ((,class :background ,bg-emph :foreground ,green)))
    `(magit-blame-hash    ((,class :background ,bg-emph :foreground ,cyan)))
    `(magit-blame-header  ((,class :background ,bg-emph :foreground ,green)))
    `(magit-blame-heading ((,class :background ,bg-emph :foreground ,blue)))
    `(magit-blame-name    ((,class :background ,bg-emph :foreground ,yellow)))
    `(magit-blame-sha1    ((,class :background ,bg-emph :foreground ,violet)))
    `(magit-blame-subject ((,class :background ,bg-emph :foreground ,yellow)))
    `(magit-blame-summary ((,class :background ,bg-emph :foreground ,magenta)))
    `(magit-blame-time    ((,class :background ,bg-emph :foreground ,green)))
    `(magit-branch ((,class (:foreground ,const :weight bold))))
    `(magit-branch-current ((,class (:background ,bg-emph :foreground ,blue :inherit bold :box nil))))
    `(magit-branch-local ((,class (:background ,bg-emph :foreground ,blue :inherit bold))))
    `(magit-branch-remote ((,class (:background ,bg-emph :foreground ,cyan :inherit bold))))
    `(magit-diff-context-highlight ((,class (:background ,bg-emph :foreground ,fg-base))))
    `(magit-diff-file-header ((,class (:foreground ,fg-emph :background ,bg-rev-base))))
    `(magit-diff-file-heading ((,class (:background ,bg-emph :foreground ,comment))))
    `(magit-diff-file-heading-highlight ((,class (:background ,bg-emph :foreground ,comment))))
    `(magit-diff-hunk-header ((,class (:background ,bg-emph :foreground ,violet))))
    `(magit-diff-hunk-heading ((,class (:background ,bg-emph :foreground ,violet))))
    `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-emph :foreground ,violet))))
    `(magit-diffstat-added   ((,class (:foreground ,type))))
    `(magit-diffstat-removed ((,class (:foreground ,var))))
    `(magit-hash ((,class (:foreground ,var))))
    `(magit-hunk-heading           ((,class (:background ,bg-rev-base))))
    `(magit-hunk-heading-highlight ((,class (:background ,bg-rev-base))))
    `(magit-item-highlight ((,class :background ,bg-emph)))
    `(magit-log-author ((,class (:foreground ,orange :bold nil))))
    `(magit-log-head-label-head ((,class (:background ,yellow :foreground ,bg-base :inherit bold))))
    `(magit-log-head-label-local ((,class (:background ,green :foreground ,bg-base :inherit bold))))
    `(magit-log-head-label-remote ((,class (:background ,violet :foreground ,bg-base :inherit bold))))
    `(magit-log-head-label-tags ((,class (:background ,magenta :foreground ,bg-base :inherit bold))))
    `(magit-log-head-label-wip ((,class (:background ,cyan :foreground ,bg-base :inherit bold))))
    `(magit-log-sha1 ((,class (:foreground ,str))))
    `(magit-process-ng ((,class (:foreground ,war :weight bold))))
    `(magit-process-ok ((,class (:foreground ,func :inherit bold))))
    `(magit-section-heading        ((,class (:foreground ,keyword :inherit bold))))
    `(magit-section-highlight      ((,class (:background ,bg-emph))))
    `(magit-section-title ((,class (:background ,bg-base :foreground ,keyword :inherit bold))))

    ;; man
    `(Man-overstrike ((,class (:foreground ,yellow :inherit bold))))
    `(Man-reverse ((,class (:foreground ,green))))
    `(Man-underline ((,class (:foreground ,magenta :underline t))))

    ;; markdown
    `(markdown-header-face-1 ((,class (:inherit bold :foreground ,yellow :background ,bg-base))))
    `(markdown-header-face-2 ((,class (:inherit bold :foreground ,orange :background ,bg-base))))
    `(markdown-header-face-3 ((,class (:bold nil :foreground ,blue :background ,bg-base))))
    `(markdown-header-face-4 ((,class (:bold nil :foreground ,green :background ,bg-base))))
    `(markdown-header-face-5 ((,class (:bold nil :foreground ,magenta))))
    `(markdown-header-face-6 ((,class (:bold nil :foreground ,violet))))

    ;; mu4e
    `(mu4e-cited-1-face ((,class (:foreground ,fg-emph))))
    `(mu4e-cited-7-face ((,class (:foreground ,fg-rev-base))))
    `(mu4e-header-marks-face ((,class (:foreground ,type))))
    `(mu4e-view-url-number-face ((,class (:foreground ,type))))

    ;; org
    `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
    `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
    `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg-rev-emph))))
    `(org-agenda-done ((,class (:foreground ,bg-rev-emph))))
    `(org-agenda-structure ((,class (:weight bold :foreground ,fg-rev-base :box nil :background ,bg-rev-base))))
    `(org-block ((,class (:foreground ,fg-base :background ,bg-base))))
    `(org-block-begin-line ((,class (:foreground ,doc :background ,bg-emph :bold t :italic nil))))
    `(org-block-end-line ((,class (:foreground ,doc :background ,bg-emph :bold t :italic nil))))
    `(org-code ((,class (:foreground ,comment :background ,bg-emph :bold t))))
    `(org-date ((,class (:underline t :foreground ,var) )))
    `(org-document-info-keyword ((,class (:foreground ,func))))
    `(org-done ((,class (:box nil :bold t :foreground ,green))))
    `(org-headline-done ((,class (:foreground ,green))))
    `(org-ellipsis ((,class (:foreground ,builtin))))
    `(org-footnote  ((,class (:underline t :foreground ,fg-rev-emph))))
    `(org-hide ((,class (:foreground ,fg-rev-emph))))
    `(org-level-1 ((,class (:bold t :foreground ,blue :height 1.1))))
    `(org-level-2 ((,class (:bold t :foreground ,orange))))
    `(org-level-3 ((,class (:bold t :foreground ,cyan))))
    `(org-level-4 ((,class (:bold t :foreground ,magenta))))
    `(org-level-5 ((,class (:bold t :foreground ,yellow))))
    `(org-level-6 ((,class (:bold t :foreground ,red))))
    `(org-level-7 ((,class (:bold t :foreground ,green))))
    `(org-level-8 ((,class (:bold t :foreground ,violet))))
    `(org-link ((,class (:underline t :foreground ,type ))))
    `(org-quote ((,class (:inherit org-block :slant italic))))
    `(org-scheduled ((,class (:foreground ,type))))
    `(org-scheduled-today ((,class (:foreground ,func :weight bold :height 1.2))))
    `(org-sexp-date ((,class (:foreground ,fg-rev-emph))))
    `(org-special-keyword ((,class (:foreground ,func))))
    `(org-todo ((,class (:box nil :foreground ,keyword :bold t))))
    `(org-verbatim ((,class (:foreground ,keyword :background ,bg-emph))))
    `(org-verse ((,class (:inherit org-block :slant italic))))
    `(org-warning ((,class (:underline t :foreground ,war))))

    ;; powerline
    `(powerline-active1 ((,class (:foreground ,fg-rev-base :background ,bg-rev-base))))
    `(powerline-active2 ((,class (:foreground ,fg-rev-emph :background ,bg-rev-emph))))
    `(powerline-inactive1 ((,class (:foreground ,cyan :background ,bg-base))))
    `(powerline-inactive2 ((,class (:foreground ,cyan :background ,bg-emph))))

    ;; rainbow-blocks
    `(rainbow-blocks-depth-1-face ((,class (:foreground ,cyan))))
    `(rainbow-blocks-depth-2-face ((,class (:foreground ,yellow))))
    `(rainbow-blocks-depth-3-face ((,class (:foreground ,blue))))
    `(rainbow-blocks-depth-4-face ((,class (:foreground ,violet))))
    `(rainbow-blocks-depth-5-face ((,class (:foreground ,green))))
    `(rainbow-blocks-depth-6-face ((,class (:foreground ,yellow))))
    `(rainbow-blocks-depth-7-face ((,class (:foreground ,blue))))
    `(rainbow-blocks-depth-8-face ((,class (:foreground ,violet))))
    `(rainbow-blocks-depth-9-face ((,class (:foreground ,green))))
    `(rainbow-blocks-unmatched-face ((,class (:foreground ,red))))

    ;; rainbow-delimiters
    `(rainbow-delimiters-depth-1-face ((,class (:bold t :foreground ,blue))))
    `(rainbow-delimiters-depth-2-face ((,class (:bold t :foreground ,yellow))))
    `(rainbow-delimiters-depth-3-face ((,class (:foreground ,green :bold nil))))
    `(rainbow-delimiters-depth-4-face ((,class (:foreground ,fg-base :bold t))))
    `(rainbow-delimiters-depth-5-face ((,class (:foreground ,violet :bold nil))))
    `(rainbow-delimiters-depth-6-face ((,class (:foreground ,yellow :bold nil))))
    `(rainbow-delimiters-depth-7-face ((,class (:foreground ,blue :bold nil))))
    `(rainbow-delimiters-depth-8-face ((,class (:foreground ,cyan :bold t))))
    `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green))))
    `(rainbow-delimiters-depth-10-face ((,class (:foreground ,yellow))))
    `(rainbow-delimiters-depth-11-face ((,class (:foreground ,blue))))
    `(rainbow-delimiters-depth-12-face ((,class (:foreground ,violet))))
    `(rainbow-delimiters-unmatched-face ((,class (:foreground ,war :background ,bg-emph))))

    ;; undo-tree
    `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
    `(undo-tree-visualizer-default-face ((,class :foreground ,fg-emph)))
    `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
    `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))

    ;; shm
    `(shm-current-face ((,class (:background ,green))))
    `(shm-quarantine-face ((,class (:background ,red))))

    ;; show-paren
    `(show-paren-match ((,class (:background ,green))))
    `(show-paren-mismatch ((,class (:background ,red))))

    ;; smartparens
    `(sp-pair-overlay-face ((,class (:background ,bg-emph :foreground nil))))
    `(sp-show-pair-match-face ((,class (:foreground ,mat :inherit bold :underline t))))

    ;; speedbar
    `(speedbar-button-face ((,class (:inherit ,'default
                                              :foreground ,fg-emph))))
    `(speedbar-directory-face ((,class (:inherit ,'default :foreground ,blue))))
    `(speedbar-file-face ((,class (:inherit ,'default :foreground ,fg-base))))
    `(speedbar-highlight-face ((,class (:inherit ,'default :background ,bg-emph))))
    `(speedbar-selected-face ((,class (:inherit ,'default
                                                :foreground ,yellow :underline t))))
    `(speedbar-separator-face ((,class (:inherit ,'default
                                                 :background ,blue :foreground ,bg-base
                                                 :overline ,cyan))))
    `(speedbar-tag-face ((,class (:inherit ,'default :foreground ,green))))

    ;; spaceline
    `(spaceline-python-venv ((,class (:foreground ,fg-base))))
    `(spaceline-flycheck-error  ((,class (:foreground ,err))))
    `(spaceline-flycheck-info   ((,class (:foreground ,keyword))))
    `(spaceline-flycheck-warning((,class (:foreground ,war))))

    ;; spacemacs
    `(spacemacs-transient-state-title-face ((,class (:background nil :foreground ,fg-base :box nil :inherit bold))))

    ;; term
    `(term ((,class (:foreground ,fg-base :background ,bg-base))))
    `(term-color-black ((,class (:foreground ,bg-rev-base :background ,bg-rev-base))))
    `(term-color-blue ((,class (:foreground ,func :background ,func))))
    `(term-color-cyan ((,class (:foreground ,str :background ,str))))
    `(term-color-green ((,class (:foreground ,type :background ,bg-rev-base))))
    `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
    `(term-color-red ((,class (:foreground ,keyword :background ,bg-rev-base))))
    `(term-color-white ((,class (:foreground ,fg-emph :background ,fg-emph))))
    `(term-color-yellow ((,class (:foreground ,var :background ,var))))

    ;; web-mode
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

    )

   (custom-theme-set-variables
    theme-name
    `(ansi-color-names-vector [,bg-rev-emph ,red ,green ,yellow ,blue ,magenta ,cyan ,bg-base]))

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
