;; Featuring evil-mode keybindings from http://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html

;; Disable GUI elements
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Package setup

;;;Load package manager and MELPA repository
(require 'package)
(add-to-list
    'package-archives
    '("melpa" . "http://melpa.milkbox.net/packages/")
    t
)
(package-initialize)

;;; Set up package autoloading - adapted from prelude by @bbatsov
(defun require-package (package)
    "Install PACKAGE unless already installed."
    (unless (package-installed-p package)
	(package-install package)))
(defun require-packages (packages)
    "Ensure PACKAGES are installed.
Missing packages are installed automatically."
    (mapc #'require-package packages))

;;; Declare required packages
(require-packages '(
    base16-theme
    linum-relative
    helm
    evil
    evil-surround
    evil-nerd-commenter
    markdown-mode
    julia-mode
))

;; Configure general interface

;;; Load theme and set text size
(load-theme 'base16-default-dark t)
(set-face-attribute 'default nil :height 100)

;;; Enable line numbers
(require 'linum-relative)
(setq linum-relative-current-symbol "")
(linum-relative-global-mode)

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
(setq temporary-file-directory "~/tmp")
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

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

;;; M-x remap
(define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)
(define-key evil-visual-state-map (kbd "SPC SPC") 'helm-M-x)

;;; Window switching
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

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
  "Take dired up one directory, but behave like dired-find-alternate-file"
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

