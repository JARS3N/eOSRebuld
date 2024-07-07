;;; Emacs Configuration

(setq user-emacs-directory "~/.emacs.d/")
;(load-file (expand-file-name "init.el" user-emacs-directory))
; server file location
(setq server-socket-dir (concat "/run/user/" (number-to-string (user-uid)) "/emacs/"))

;; Check if the server is already running
(require 'server)
(unless (server-running-p)
  (server-start))


;; Basic Settings
(setq inhibit-splash-screen t
      inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq visible-bell t)

;; Set Fonts
(set-face-attribute 'default nil :font "Fira Code-12")

;; Initialize package sources
(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; Packages

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure `use-package` is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load `use-package`
(eval-when-compile
  (require 'use-package))


;; Ensure `auto-package-update` is installed
(use-package auto-package-update
  :ensure t
  :config
  ;; Update every 7 days
  (setq auto-package-update-interval 7)
  ;; Delete old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not prompt before updating
  (setq auto-package-update-prompt-before-update t)
  ;; Show a message when updates have taken place
  (setq auto-package-update-hide-results nil)
  ;; Update at startup
  (auto-package-update-maybe))




;; Doom Themes
(use-package doom-themes
  :config
  (load-theme 'doom-laserwave t)  ; Load the doom-laserwave theme
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-neotree-file-icons 'simple)
  (doom-themes-org-config))

;; Applying customizations for Doom themes
(with-eval-after-load 'doom-themes
  (doom-themes-set-faces
   'doom-laserwave
   '(org-todo :foreground "#ff79c6" :weight 'bold)
   '(org-in-progress :foreground "#f1fa8c" :weight 'bold)
   '(org-waiting :foreground "#bd93f9" :weight 'bold)
   '(org-done :foreground "#50fa7b" :weight 'bold)
   '(org-tag :foreground "#ffb86c" :weight 'bold)))



;; Rainbow Delimiters
(use-package rainbow-delimiters
  :hook ((prog-mode ess-r-mode inferior-ess-r-mode lisp-mode slime-repl-mode) . rainbow-delimiters-mode))

(setq split-width-threshold 0)
(setq split-height-threshold nil)

;; Org Mode Configuration
(use-package org
  :config
  (setq org-directory "~/org/")
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-agenda-files (list org-directory))
  (setq org-log-done 'time)
  (setq org-src-fontify-natively t)
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
           "* %?\nEntered on %U\n  %i\n  %a"))))

;; Org Bullets
(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Custom faces for Org mode TODO states
(defface org-in-progress
  '((t (:foreground "#f1fa8c" :weight bold)))
  "Face for the IN-PROGRESS keyword in Org mode.")

(defface org-waiting
  '((t (:foreground "#bd93f9" :weight bold)))
  "Face for the WAITING keyword in Org mode.")

(defface org-todo
  '((t (:foreground "#ff79c6" :weight bold)))
  "Face for the TODO keyword in Org mode.")

(defface org-done
  '((t (:foreground "#50fa7b" :weight bold)))
  "Face for the DONE keyword in Org mode.")

(setq org-todo-keywords
      '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)")))

(setq org-todo-keyword-faces
      '(("TODO" . 'org-todo)
        ("IN-PROGRESS" . 'org-in-progress)
        ("WAITING" . 'org-waiting)
        ("DONE" . 'org-done)))

;; Set custom face for tags
(setq org-tag-faces
      '(("TAG" . 'org-tag)))



(defun my-org-mode-hook ()
  "Custom configurations for Org mode."
  (org-display-inline-images t t)
  (setq org-startup-with-inline-images t)
  (setq org-image-actual-width nil))
(add-hook 'org-mode-hook 'my-org-mode-hook)

;; Org launch telephone links
(org-link-set-parameters "tel" :follow (lambda (path) (browse-url (concat "tel:" path))))

;;;;;;;;;;;;;;;;;;;;;;;;  Language Support

;; Magit for Git integration
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

;; SLIME for Common Lisp
(use-package slime
  :ensure t
  :mode ("\\.lisp\\'" . lisp-mode)
  :config
  (setq inferior-lisp-program "sbcl"))

;; Python
(use-package python-mode
  :mode ("\\.py\\'" . python-mode))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; ESS for R
(add-hook 'ess-post-run-hook 'display-ess-r-plot)


;;Racket
; Install Geiser if not already installed
(unless (package-installed-p 'geiser)
  (package-refresh-contents)
  (package-install 'geiser))

(require 'geiser)
(setq geiser-active-implementations '(racket))
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . geiser-mode))

;; Install company-mode if not already installed
(unless (package-installed-p 'company)
  (package-refresh-contents)
  (package-install 'company))

;; Enable company-mode globally and in geiser-mode
(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'geiser-repl-mode-hook 'company-mode)

;; Optional: Install and configure racket-mode
(unless (package-installed-p 'racket-mode)
  (package-refresh-contents)
  (package-install 'racket-mode))

(require 'racket-mode)
(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))

;; END LANGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Ivy for fuzzy searching and completion
(use-package ivy
  :diminish ivy-mode
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
  (ivy-mode 1)
  (use-package counsel
    :bind (("M-x" . counsel-M-x)
           ("C-x C-f" . counsel-find-file)
           ("C-x b" . counsel-ibuffer)
           ("C-x C-r" . counsel-recentf)
           ("C-c g" . counsel-git)
           ("C-c j" . counsel-git-grep)
           ("C-c k" . counsel-rg)
           ("C-x l" . counsel-locate))
    :config
    (setq ivy-initial-inputs-alist nil)))  ;; Remove the ^ from counsel commands

;; Keybindings
(global-set-key (kbd "C-x C-b") 'ibuffer)  ;; Better buffer list
(global-set-key (kbd "C-c a") 'org-agenda) ;; Org agenda
(global-set-key (kbd "C-c c") 'org-capture) ;; Org capture
(global-set-key (kbd "C-c l") 'org-store-link) ;; Org store link
(global-set-key (kbd "M-o") 'other-window) ;; Easier window navigation
(global-set-key (kbd "C-c C-;") 'comment-region)
(global-set-key (kbd "C-c C-:") 'uncomment-region)
(global-set-key (kbd "C-c C-k") 'ess-rmarkdown)
(global-set-key (kbd "C-c d") 'my-org-cycle-date-format)
(global-set-key (kbd "C-c o") 'browse-url-at-point) ;; url link direct

;; Performance Tweaks
(setq gc-cons-threshold 100000000) ;; Increase garbage collection threshold
(setq read-process-output-max (* 1024 1024)) ;; Increase process output buffer size

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 100000000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Miscellaneous

;; SSH Mode
;; (use-package ssh
;;   :ensure t
;;   :config
;;   (add-hook 'ssh-mode-hook
;;             (lambda ()
;;               (setq ssh-directory-tracking-mode t)
;;               (shell-dirtrack-mode t)
;;               (setq dirtrackp nil))))

;; Tramp configuration
(require 'tramp)
(setq tramp-default-method "ssh")

;; Optional: Configure tramp to use SSH agent forwarding
(setq tramp-ssh-controlmaster-options
      (concat
       "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
       "-o ControlMaster=auto -o ControlPersist=yes"))


;; Doc view settings
(setq doc-view-resolution 300) ;; for better clarity

;; Org Mode Settings
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(setq org-fontify-whole-heading-line t)
(setq org-fontify-done-headline t)
(setq org-fontify-quote-and-verse-blocks t)

;; Custom Function: Run LLama Query
(defun run-llama-query (prompt)
  "Run a LLaMA query with PROMPT."
  (interactive "sEnter your query: ")
  (let ((output-buffer "*LLAMA Query Output*"))
    (with-output-to-temp-buffer output-buffer
      (shell-command (format "python c:users/arsenaul/PROJ/LLM/llama.py %s" prompt) output-buffer))))

