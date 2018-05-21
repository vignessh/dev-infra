(require 'package)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(dolist (archive '(("marmalade" . "http://marmalade-repo.org/packages/")
		   						("melpa-stable" . "http://stable.melpa.org/packages/")
       						("melpa" . "http://melpa.org/packages/")))
  (add-to-list 'package-archives archive :append))

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun my-packages-alist (archive packages)
  "take the archive name e.g. `melpa'
and a list of packages and create an associated list:
((package1 . archive) (package2 . archive) ...)"
  (let ((alist-of-packages ()))
    (dolist (package packages)
      (add-to-list 'alist-of-packages `(,package . ,archive)))
    alist-of-packages))

(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
	(my-packages-alist "melpa-stable"
			   '(clojure-mode cider))))

(defvar my-packages '(color-theme
                      textmate
                      expand-region
											paredit
                      clojure-mode
											clojure-mode-extra-font-locking
                      cider
											clj-refactor
											helm
											smex
											projectile
											crux
                      sass-mode
                      yaml-mode
											rainbow-delimiters
											company
											zenburn-theme
											json-mode
											json-reformat
											smartparens
											monokai-theme
											org
											adaptive-wrap)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Line numbers
(global-linum-mode t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Use y/n instead of full yes/no for confirmation messages
(fset 'yes-or-no-p 'y-or-n-p)

(global-company-mode)
(global-auto-revert-mode 1)

(require 'org)

;; Remove the markup characters, i.e., "/text/" becomes (italized) "text"
(setq org-hide-emphasis-markers t)

;; Turn on visual-line-mode for Org-mode only
;; Also install "adaptive-wrap" from elpa
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Configure org-mode with Cider

;; Configure Org-mode supported languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (sh . t)
   (dot . t)
   (emacs-lisp . t)))

;; Use cider as the Clojure execution backend
(setq org-babel-clojure-backend 'cider)
(require 'cider)

;; Useful keybindings when using Clojure from Org
(org-defkey org-mode-map "\C-x\C-e" 'cider-eval-last-sexp)
(org-defkey org-mode-map "\C-c\C-d" 'cider-doc)

;; Let's have pretty source code blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-support-shift-select 'always)

			;; Default theme
			(load-theme 'monokai t)

			;;
			;; Update the color of the company-mode context menu to fit the Monokai theme
			;; @source: https://github.com/search?q=deftheme+company-tooltip&type=Code
			;;
			(deftheme monokai-overrides)

			(let ((class '((class color) (min-colors 257)))
			      (terminal-class '((class color) (min-colors 89))))

			  (custom-theme-set-faces
			   'monokai-overrides

			   ;; Linum and mode-line improvements (only in sRGB).
			   `(linum
			     ((,class :foreground "#75715E"
			              :background "#49483E")))
			   `(mode-line-inactive
			     ((,class (:box (:line-width 1 :color "#2c2d26" :style nil)
			                    :background "#2c2d26"))))

			   ;; Custom region colouring.
			   `(region
			     ((,class :foreground "#75715E"
			              :background "#49483E")
			      (,terminal-class :foreground "#1B1E1C"
			                       :background "#8B8878")))

			   ;; Additional modes
			   ;; Company tweaks.
			   `(company-tooltip-common
			     ((t :foreground "#F8F8F0"
			         :background "#474747"
			         :underline t)))

			   `(company-template-field
			     ((t :inherit company-tooltip
			         :foreground "#C2A1FF")))

			   `(company-tooltip-selection
			     ((t :background "#349B8D"
			         :foreground "#BBF7EF")))

			   `(company-tooltip-common-selection
			     ((t :foreground "#F8F8F0"
			         :background "#474747"
			         :underline t)))

			   `(company-scrollbar-fg
			     ((t :background "#BBF7EF")))

			   `(company-tooltip-annotation
			     ((t :inherit company-tooltip
			         :foreground "#C2A1FF")))

			   ;; Popup menu tweaks.
			   `(popup-menu-face
			     ((t :foreground "#A1EFE4"
			         :background "#49483E")))

			   `(popup-menu-selection-face
			     ((t :background "#349B8D"
			         :foreground "#BBF7EF")))

			   ;; Circe
			   `(circe-prompt-face
			     ((t (:foreground "#C2A1FF" :weight bold))))

			   `(circe-server-face
			     ((t (:foreground "#75715E"))))

			   `(circe-highlight-nick-face
			     ((t (:foreground "#AE81FF" :weight bold))))

			   `(circe-my-message-face
			     ((t (:foreground "#E6DB74"))))

			   `(circe-originator-face
			     ((t (:weight bold))))))

;; Cljr customizations
(require 'clj-refactor)

(defun cljr-mode ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))

;; Clojure mode customizations
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'cljr-mode)

;; Rainbow delimiters
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; Show parenthesis mode
(show-paren-mode 1)

;; Disable the Emacs menu bar
(menu-bar-mode -1)
;; Textmate like fuzzy file locate and symbol lookup
;; Map to Super-t and Super-T. For the sake of Mac
;; terminal/iterm ssh user also mapped to Meta-t and
;; Meta-T
(textmate-mode)
(global-set-key (kbd "M-t") 'textmate-goto-file)
(global-set-key (kbd "M-T") 'textmate-goto-symbol)

;; Magit keybindings
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)

;; Turn off ido flex complete if the complete list is
;; exceed 2000. Emacs will freeze up otherwise.
(defvar af-ido-flex-fuzzy-limit (* 2000 5))
(defadvice ido-set-matches-1 (around my-ido-set-matches-1 activate)
  (let ((ido-enable-flex-matching (< (* (length (ad-get-arg 0)) (length ido-text))
                                     af-ido-flex-fuzzy-limit)))
    ad-do-it))



;; Provid IntelliJ C-W style incremental selection base on sexp.
(global-set-key (kbd "M-+") 'er/expand-region)

;; Robust version of duplicate a line
(require 'duplicate-line)
(global-set-key (kbd "C-c C-d") 'duplicate-line)
