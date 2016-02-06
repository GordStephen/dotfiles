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

;; Configure text editing

;;; Activate evil-mode
(require 'evil)
(evil-mode t)

;;; Activate surround vi operator
(require 'evil-surround)
(global-evil-surround-mode t)

;;; Activate comment vi operator
(evilnc-default-hotkeys)
