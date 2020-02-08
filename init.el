;;; init.el -- A minimal but useful config for Clojure.
;;;
;;; Commentary:
;;;
;;; Here is a well annotated Emacs configuration optimal for writing Clojure.
;;; Code:

;; Use package to install external packages.

(require 'package)
(add-to-list 'package-archives
             ;; Swap the following two lines to opt in to melpa stable.
             ;; (cons "melpa-stable" "https://stable.melpa.org/packages/" )
             (cons "melpa" "https://melpa.org/packages/" )
             t)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar installed-packages
  '(ace-window
    browse-kill-ring
    cider
    clojure-mode
    company
    company-lsp
    counsel ;; making ivy's implicit dependency explicit
    counsel-projectile
    doom-themes
    expand-region
    flycheck
    flycheck-clj-kondo
    ivy
    lsp-mode
    lsp-ui
    magit
    markdown-mode
    prettier-js
    projectile
    rainbow-delimiters
    smartparens
    solarized-theme
    swiper ;; making ivy's implicit dependency explicit
    typescript-mode
    yaml-mode
    zenburn-theme))
;; Ensure emacs shells start with the same environment as regular shells on
;; macOS.
(if (eq system-type 'darwin)
    (add-to-list 'installed-packages 'exec-path-from-shell))
(dolist (p installed-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Move all customization information into its own file.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :no-error)

;; Configuration is grouped by built-in packages and then external
;; packages.

; ** built-in configuration **
;; Use a dark theme to be easy on the eyes.
(load-theme 'doom-one)
;; prompt before killing emacs
(setq confirm-kill-emacs 'yes-or-no-p)
;; When running the Emacs in standalone mode:
(when (window-system)
  ;; Turn off the toolbar
  (tool-bar-mode -1)
  ;; Turn off the scrollbar
  (scroll-bar-mode -1)
  ;; Turn off the tool tips
  (tooltip-mode -1)
  ;; Turn off the small fringe around the frame.
  (fringe-mode -1)
  ;; increase font size for better readability.
  (set-face-attribute 'default nil
                      :height 150
                      :family "Monaco")
  ;; Fullscreen by default, as early as possible.
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  ;; Use a transparent menu bar.
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  ;; Assumes a dark colorscheme.
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; Removes icon from titlebar.
  (setq ns-use-proxy-icon nil)
  ;; Removes file name from titlebar.
  (setq frame-title-format nil))
;; Turn off the menu bar
(menu-bar-mode 0)
;; Disable the blinking cursor.
(blink-cursor-mode 0)
;; Highlight matching paren.
(show-paren-mode 1)
;; Highlight current line.
(global-hl-line-mode 1)
;; Add line number to status bar.
(setq line-number-mode t)
;; Add column number to status bar.
(setq column-number-mode t)
;; Add size of file to status bar.
(size-indication-mode t)
;; global line numbers.
(global-display-line-numbers-mode)
;; Go straight to scratch buffer on startup.
(setq inhibit-startup-message t)
;; Always select the help buffer on open.
(setq help-window-select t)
;; Set fill column to 80 characters.
(setq-default fill-column 80)
;; Make sure that text files are correctly formatted.
(setq require-final-newline t)
;; Remove trailing whitespace on save.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
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
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)
;; Configure kill-ring to integrate with copy/paste.
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)
;; Delete the selection with a keypress.
(delete-selection-mode t)
;; Revert buffers automatically when underlying files are changed externally.
(global-auto-revert-mode t)
;; Shows a list of buffers with ibuffer.
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; use better binding for goto-line
(global-set-key (kbd "M-g") 'goto-line)

;; ** external package configuration **

;; Configure ace-window.
(global-set-key (kbd "M-o") 'ace-window)
;; Use "home row" (e.g., a, s , d, f) to jump between windows.
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

;; Configure browse-kill-ring.
(browse-kill-ring-default-keybindings)

;; Configure cider.
;; When there's a cider error, show its buffer.
(setq cider-show-error-buffer t)
;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")
;; Enable smartparens in the REPL.
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
;; When switching to the REPL, show it in the current window.
(setq cider-repl-display-in-current-window t)
;; Disable the Cider help message.
(setq cider-repl-display-help-banner nil)
;; Just go to the symbol under the point; don't ask.
(setq cider-prompt-for-symbol nil)

;; Configure clojure-mode.
;; Enable smartparns for Clojure
(add-hook 'clojure-mode-hook 'smartparens-strict-mode)
;; Enable Rainbow delimiters mode
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)
;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))

;; Company (complete anything)
;; Enable in all buffers.
(add-hook 'after-init-hook 'global-company-mode)
;; Show numbers alongside the completion dialoge. Select with M-<number>.
(setq company-show-numbers t)
;; Stop downcasing auto-completion results
(setq company-dabbrev-downcase nil)
;; company-lsp
(setq company-lsp-cache-candidates t)
(require 'company-lsp)
(push 'company-lsp company-backends)

;; Enable counsel-projectile to get an Ivy-like interface in projectile.
(counsel-projectile-mode 1)

;; Configure exec-path-from shell.
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; Configure expand-region
(global-set-key (kbd "C-c n") 'er/expand-region)

;; Enable clj-kondo flychecking
(global-flycheck-mode)
(require 'flycheck-clj-kondo)

;; Configure Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-c C-r") 'ivy-resume)

;; lsp
(setq lsp-prefer-flymake nil)
(setq lsp-enable-on-type-formatting t)
(setq lsp-before-save-edits t)
;; lsp-ui
(add-hook 'lsp-mode-hook 'lsp-ui-mode)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)

;; prettier
;; npm install -g prettier
(add-hook 'typescript-mode-hook 'prettier-js-mode)

;; projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-project-search-path '("~/workspace/"))
(setq projectile-globally-ignored-directories '("-/target"))
(setq projectile-completion-system 'ivy)

;; smartparens
(require 'smartparens-config)
(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(global-set-key (kbd "C-c h") 'sp-backward-slurp-sexp)
(global-set-key (kbd "C-c j") 'sp-backward-barf-sexp)
(global-set-key (kbd "C-c k") 'sp-forward-barf-sexp)
(global-set-key (kbd "C-c l") 'sp-forward-slurp-sexp)
(global-set-key (kbd "C-c r") 'sp-splice-sexp-killing-backward)
(global-set-key (kbd "C-c s") 'sp-splice-sexp)
(global-set-key (kbd "C-c q") 'sp-indent-defun)
(global-set-key (kbd "C-M-f") 'sp-forward-sexp)
(global-set-key (kbd "C-M-b") 'sp-backward-sexp)

;; typescript-mode
;; Note: Must install `npm install -g typescript-language-server` and
;;       `npm install -g typescript`
(add-hook 'typescript-mode-hook 'lsp)


;; custom functions

(global-set-key (kbd "C-c w") 'toggle-truncate-lines)

;; setting indent levels as they bother me.
(setq js-indent-level 2)
(setq ts-indent-level 2)

(provide 'init)
;;; init.el ends here
