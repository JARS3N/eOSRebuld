(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; set visible bell
(setq visibile-bell t)


;;initialize package sources

(require 'package)


(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			; ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; initialize use-package on linux
(unless (package-installed-p 'use-package)
	(package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; set up common lisp
(setq inferior-lisp-program "sbcl")

;; setup for R/ess
;; https://www2.stat.duke.edu/courses/Spring09/sta244/computing/R-ESS.html
; automatically get the correct mode 
auto-mode-alist (append (list '("\\.c$" . c-mode)
			      '("\\.tex$" . latex-mode)
			      '("\\.S$" . S-mode)
			      '("\\.s$" . S-mode)
			      '("\\.R$" . R-mode)
			      '("\\.r$" . R-mode)
			      '("\\.html$" . html-mode)
                              '("\\.emacs" . emacs-lisp-mode)
	                )
		      auto-mode-alist)
(load "/usr/pkg/ess/lisp/ess-site")
;;(setq-default inferior-S+6-program-name "Splus")
(setq-default inferior-R-program-name "R")

;; these were causing the error page onload,conflict
;;(global-set-key (kbd "C-c") 'undefined)
;;(global-set-key (kbd "C-c") 'kill-ring-save)


(setq custom-safe-themes t)
(when (display-graphic-p)
  (load-theme 'cyberpunk t)
 )

(use-package command-log-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
(ivy-mode 1))


(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
:custom ((doom-modeline-height 15)))
