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

(defvar my-packages '(
                      ;this has to be looked at as the literate starter kit server the purpose of below 3
                      literate-starter-kit
                      color-theme
                      textmate
                      expand-region
                      starter-kit-eshell
                      clojure-mode
                      cider
											helm
                      sass-mode
                      yaml-mode
											rainbow-delimiters
											company
											zenburn-theme
											json-mode
											json-reformat
											smartparens)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(global-company-mode)
(global-auto-revert-mode 1)

(load-theme 'zenburn t)

;; Cljr customizations
(require 'clj-refactor)

(defun cljr-mode ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1))

;; Clojure mode customizations
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'subword-mode)
(add-hook 'clojure-mode-hook #'smartparens-mode)
(add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'aggressive-indent-mode)
(add-hook 'clojure-mode-hook #'cljr-mode)

;; activate helm mode
(require 'helm-config)
(helm-mode 1)

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
