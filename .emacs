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

;; Configure text editing

;;; Set directory for backups and autosaves
(setq temporary-file-directory "~/tmp")
(setq backup-directory-alist
          `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

;;; Activate evil-mode
(require 'evil)
(evil-mode t)

;;; Activate surround vi operator
(require 'evil-surround)
(global-evil-surround-mode t)

;;; Activate comment vi operator
(evilnc-default-hotkeys)

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

;; Configure dired
(require 'dired-x)
(setq dired-omit-files "^\\..*$")
(add-hook 'dired-mode-hook (lambda () (dired-omit-mode 1)))
(put 'dired-find-alternate-file 'disabled nil)
;; Keybindings from http://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html#dired
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
