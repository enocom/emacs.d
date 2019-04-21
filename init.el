;; Use package to install external packages.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar installed-packages
  '(ace-window
    ag
    browse-kill-ring
    cider
    clojure-mode
    expand-region
    ido-completing-read+
    magit
    paredit
    projectile
    rainbow-delimiters
    smex
    yaml-mode))
;; Ensure emacs shells start with the same environment as regular shells on
;; macOS.
(if (eq system-type 'darwin)
    (add-to-list 'installed-packages 'exec-path-from-shell))
(dolist (p installed-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Move all customization information into its own file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Configuration is grouped by built-in packages and then external
;; packages. The groups are:
;; 1. *visual* configuration,
;; 2. *behavioral* configuration,
;; 3. *keyboard shortcut* configuration, and
;; 4. *external package* configuration.

; 1. Visual configuration:

;; increase font size for better readability.
(set-face-attribute 'default nil :height 140)
;; Configure initial frame size on start.
(setq initial-frame-alist '((top . 10)
                            (left . 10)
                            (width . 90)
                            (height . 50)))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
;; Use a dark theme to be easy on the eyes.
;; (load-theme 'tomorrow-night-eighties t)
;; Use a transparent titlebar.
;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; Turn off the menu bar.
(menu-bar-mode -1)
;; Turn off the toolbar.
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
;; Disable native scroll bars.
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
;; Disable the blinking curso.
(blink-cursor-mode 0)
;; Highlight matching paren.
(show-paren-mode 1)
;; Highlight current line.
(global-hl-line-mode 1)
;; Add line number to status bar.
(setq line-number-mode t)
;; Add column number to status bar.
(setq column-number-mode t)
;; Use emacs >= 26 global line numbers.
(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))
(global-set-key (kbd "C-c l") 'global-display-line-numbers-mode)

;; 2. Behavioral configuration:

;; Go straight to scratch buffer on startup.
(setq inhibit-startup-message t)
;; Set fill column to 80 characters.
(setq-default fill-column 80)
;; Make sure that text files are correctly formatted.
(setq require-final-newline 'ask)
;; Remove trailing whitespace on save.
(add-hook 'after-save-hook 'delete-trailing-whitespace)
;; Changes all yes/no questions to y/n.
(fset 'yes-or-no-p 'y-or-n-p)
;; Disable backup files.
(setq make-backup-files nil)
;; Disable lock files.
(setq create-lockfiles nil)
;; Disable the bell.
(setq ring-bell-function 'ignore)
;; Don't use hard tabs.
(setq-default indent-tabs-mode nil)
;; Configure kill-ring to integrate with copy/paste.
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t)

;; 3. Keyboard configuration:

;; Use ibuffer instead of default.
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Bind hippie expand to user space shortcut.
(global-set-key (kbd "C-c /") 'hippie-expand)
;; Lisp-friendly hippie expand.
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Save last position of point for visited buffers.
(require 'saveplace)
(setq-default save-place t)
;; Keep track of saved places in ~/.emacs.d/places.
(setq save-place-file (concat user-emacs-directory "places"))

;; Add a function to comment or uncomment a line.
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-c ;") 'toggle-comment-on-line)

;; Automatically load paredit when editing a lisp file.
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; 4. External package configuration:

;; Configure ace-window.
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(setq aw-dispatch-always t)

;; Configure browse-kill-ring.
(browse-kill-ring-default-keybindings)

;; Configure cider.
;; When there's a cider error, show its buffer.
(setq cider-show-error-buffer t)
;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")
;; Enable paredit in the REPL.
(add-hook 'cider-repl-mode-hook 'paredit-mode)
;; Don't open the repl buffer on connect.
(setq cider-repl-pop-to-buffer-on-connect nil)
;; When switching to the REPL, show it in the current window.
(setq cider-repl-display-in-current-window t)

;; Configure clojure-mode.
;; Enable paredit for Clojure
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
;; Enable Rainbow delimiters mode
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)
;; Following the Clojure style guide
(setq clojure-indent-style :align-arguments)
;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

;; Configure exec-path-from shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; Configure expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Configure ido-completing-read+.
(ido-mode t)
(ido-everywhere 1)
;; Allow partial matches
(setq ido-enable-flex-matching t)
;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)
(ido-ubiquitous-mode 1)
;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/workspace/"))
(setq projectile-globally-ignored-directories '("-/target"))

;; Configure smex.
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Configure vendored packages.
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'fill-column-marker)
(global-fcm-mode 1)

(defun today ()
  "Insert string for today's date, e.g., 2019 September 17."
  (interactive)
  (insert (format-time-string "%Y %B %e")))

;; Done.
