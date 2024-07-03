;; R related portion of emacs config

;;; START R

;; ESS for R
(use-package ess
  :ensure t
  :mode (("\\.R\\'" . R-mode)
         ("\\.r\\'" . R-mode))
  :init
  (require 'ess-site)
  :config
  ;; Customize ESS startup behavior
  (setq ess-ask-for-ess-directory nil) ;; Don't prompt for directory
  (setq ess-use-flymake nil) ;; Disable flymake to avoid issues
  (setq ess-auto-width 'window) ;; Adjust width to window
  (setq ess-tab-complete-in-script t) ;; Enable tab completion in scripts
  (setq inferior-R-program-name "R")
  ;; Ensure ESS buffers don't interfere with Projectile
  (add-hook 'ess-mode-hook
            (lambda ()
              (setq split-width-threshold 0)
              (setq split-height-threshold nil)))
  ;; Ensure that plots are rendered in the Emacs buffer
  (add-hook 'ess-R-post-run-hook
            (lambda ()
              (ess-execute-screen-options)
              (ess-eval-linewise "options(device='png', width=600, height=400)")
              (ess-eval-linewise "dev.control(displaylist='enable')")
              (ess-eval-linewise "par(mfrow=c(1,1))"))))

;; Polymode for R Markdown
(use-package polymode
  :ensure t)
(use-package poly-R
  :ensure t)
(use-package poly-markdown
  :ensure t)

;; Define polymode for R Markdown
(require 'polymode)
(require 'poly-R)
(require 'poly-markdown)

(defcustom pm-poly/markdown
  (pm-polymode-one "Rmarkdown"
                   :hostmode 'pm-host/markdown
                   :innermodes '(pm-inner/markdown
                                 pm-inner/r))
  "Polymode for Rmarkdown."
  :group 'polymodes
  :type 'object)

;; Ensure that ESS uses the correct display device for R plots
(setq inferior-ess-r-args "--no-save --no-restore")

;; Minibuffer configuration for better interaction with ESS
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)
(setq max-mini-window-height 0.3) ;; Limit minibuffer height

;; Display buffer configuration
(setq display-buffer-alist
      '(("\\*R\\*"
         (display-buffer-reuse-window display-buffer-pop-up-window)
         (reusable-frames . nil)
         (window-width . 0.5))
        ("\\*R Dired"
         (display-buffer-reuse-window display-buffer-pop-up-window)
         (reusable-frames . nil))
        ("\\*R"
         (display-buffer-reuse-window display-buffer-at-bottom)
         (window-height . 0.33)
         (reusable-frames . nil))))

;; Function to display plots in an Emacs buffer
(defun display-ess-r-plot ()
  "Display R plots within an Emacs buffer."
  (interactive)
  (setq ess-ask-for-ess-directory nil)
  (let ((buffer (get-buffer-create "*R-Plot*")))
    (with-current-buffer buffer
      (auto-image-file-mode t)
      (ess-plot-mode))
    (display-buffer buffer)))

;; ESS RStudio-like Keybindings
(with-eval-after-load 'ess-r-mode
  (define-key ess-r-mode-map (kbd "<C-return>") 'ess-eval-region-or-line-and-step)
  (define-key ess-r-mode-map (kbd "C-S-m") 'ess-insert-assign))

;;; END R
