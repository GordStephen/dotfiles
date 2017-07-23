;;; .emacs --- Gord's Emacs config

;;; Commentary:
;; Featuring evil-mode keybindings from http://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html

;;; Code:

;; Disable GUI elements
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 0)

;; Package setup

;;;Load package manager and MELPA repository
(require 'package)
(add-to-list 'package-archives
    '("MELPA" . "https://melpa.org/packages/") t)
(package-initialize)

;;; Set up package autoloading - adapted from prelude by @bbatsov
(defun require-package (package)
    "Install PACKAGE unless already installed."
    (unless (package-installed-p package)
	(package-install package)))
(defun require-packages (packages)
    "Ensure PACKAGES are installed.
Missing packages are installed automatically."
    (package-refresh-contents)
    (mapc #'require-package packages))

;;; Declare required packages
(require-packages '(
    base16-theme
    telephone-line
    linum-relative
    ethan-wspace
    helm
    evil
    evil-surround
    evil-nerd-commenter
    flycheck
    markdown-mode
    julia-mode
    haskell-mode
    rust-mode
))

;; Configure general interface

;;; Load theme and set text size
(load-theme 'base16-default-dark t)
(set-face-attribute 'default nil :height 100)

;;; Enable line numbers
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(linum-relative-global-mode)

;;; Enable whitespace highlighting
(require 'ethan-wspace)
(setq mode-require-final-newline nil)
(global-ethan-wspace-mode 1)

;;; Enable show matching brackets
(show-paren-mode 1)

;;; Helm
(require 'helm-config)
(helm-mode 1)
(setq helm-autoresize-max-height 20)
(helm-autoresize-mode t)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-mini)
(setq helm-buffers-fuzzy-matching t
      helm-buffers-fuzzy-match    t)
(global-set-key (kbd "<f1>") 'helm-apropos)
(setq helm-apropos-fuzzy-match t)

;;; Dired
(require 'dired-x)
(setq dired-omit-files "^\\..*$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)
(global-set-key (kbd "<f2>") 'dired)

;;; Set directory for backups and autosaves
(setq temporary-file-directory "~/tmp/")
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;; Flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Configure specific modes

;;; General text modes
(add-hook 'text-mode-hook 'flyspell-mode)

;;; Org mode
(setq org-log-done t)

;;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; evil-mode

;;; Activate evil-mode
(require 'evil)
(evil-mode t)

;;; Activate surround vi operator
(require 'evil-surround)
(global-evil-surround-mode t)

;;; Activate comment vi operator
(evilnc-default-hotkeys)

;; telephone-line setup
(require 'telephone-line-config)
(require 'telephone-line-utils)

(telephone-line-defsegment* gs-telephone-line-buffer-segment ()
  `(""
    mode-line-mule-info
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    ,(telephone-line-raw mode-line-buffer-identification t)))

(defface gs-telephone-line-accent-active
  '((t (:foreground "#ffffff" :background "#545454" :inherit mode-line)))
  "Accent face for mode-line."
  :group 'telephone-line)

(defface gs-telephone-line-accent-inactive
  '((t (:foreground "#a8a8a8" :background "#383838" :inherit mode-line-inactive)))
  "Accent face for inactive mode-line."
  :group 'telephone-line)

(setq telephone-line-faces
      '((accent . (gs-telephone-line-accent-active . gs-telephone-line-accent-inactive))
        (nil . (mode-line . mode-line-inactive))))

(setq telephone-line-lhs
      '((accent . (gs-telephone-line-buffer-segment))
	(nil    . (telephone-line-vc-segment
		   telephone-line-process-segment))
	  ))
(setq telephone-line-rhs
      '((nil    . (telephone-line-misc-info-segment
		   telephone-line-major-mode-segment))
	(accent . (telephone-line-airline-position-segment))
	))

(telephone-line-mode t)

;;; M-x remap
(define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
(define-key evil-visual-state-map (kbd "SPC SPC") 'helm-M-x)

;;; Open splits in frames instead of windows
(evil-define-command evil-other-frame (&optional file)
  (interactive "<f>")
  (find-file-other-frame file))
(evil-ex-define-cmd "split" 'evil-other-frame)
(evil-ex-define-cmd "vsplit" 'evil-other-frame)

;;; Keybindings for ibuffer
(eval-after-load 'ibuffer
    '(progn
	(evil-set-initial-state 'ibuffer-mode 'normal)
	(evil-define-key 'normal ibuffer-mode-map
	    (kbd "j") 'evil-next-line
	    (kbd "k") 'evil-previous-line
	    (kbd "l") 'ibuffer-visit-buffer
	    (kbd "v") 'ibuffer-toggle-marks
	    (kbd "J") 'ibuffer-jump-to-buffer
	    (kbd "M-s a C-s") 'ibuffer-do-isearch
	    (kbd "M-s a M-C-s") 'ibuffer-do-isearch-regexp
	)
    )
)

;;; Keybindings for dired
(defun my-dired-up-directory ()
  "Take dired up one directory, but behave like dired-find-alternate-file."
  (interactive)
  (let ((old (current-buffer)))
    (dired-up-directory)
    (kill-buffer old)
    ))
(evil-define-key 'normal dired-mode-map "h" 'my-dired-up-directory)
(evil-define-key 'normal dired-mode-map "l" 'dired-find-alternate-file)
(evil-define-key 'normal dired-mode-map "o" 'dired-sort-toggle-or-edit)
(evil-define-key 'normal dired-mode-map "v" 'dired-toggle-marks)
(evil-define-key 'normal dired-mode-map "m" 'dired-mark)
(evil-define-key 'normal dired-mode-map "u" 'dired-unmark)
(evil-define-key 'normal dired-mode-map "U" 'dired-unmark-all-marks)
(evil-define-key 'normal dired-mode-map "c" 'dired-create-directory)
(evil-define-key 'normal dired-mode-map "n" 'evil-search-next)
(evil-define-key 'normal dired-mode-map "N" 'evil-search-previous)
(evil-define-key 'normal dired-mode-map "q" 'kill-this-buffer)

(global-visual-line-mode t)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ethan-wspace ethan-whitespace telephone-line rust-mode markdown-mode linum-relative julia-mode helm haskell-mode flycheck evil-surround evil-nerd-commenter evil elm-mode base16-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
