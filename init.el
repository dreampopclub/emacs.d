(load "~/.emacs-env.el")

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; do ubuntu/home stuff
(when (eq emacs-env 'nell)
  ;; helm stuff

  (require 'helm)
  (require 'helm-config)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
  (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
  (global-set-key (kbd "C-x b")  'helm-mini)

  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))

  (setq helm-split-window-in-side-p           0 ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t)

  (helm-mode 1)

  ;; ;; set up for helm projectile file find
  ;; (global-set-key (kbd "C-x C-f")  'helm-find-files)
  ;; (global-set-key (kbd "C-x C-o")  'find-file)
  )

;; do macbook and work stuff
(when (eq emacs-env 'mbp)
  ;; Helpful keyboard tweak to make things easier on OS X.
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)

  ;;default open file dir
  (setq default-directory "~/src/m3/")

  ;;sass compile dirs
  ;;(setq scss-output-directory "../../maestro_activity_engine/activities_general/")
  (setq scss-output-directory "../maestro_activity_engine/")
  ;;(setq scss-output-directory "../../activities/")
  ;;(setq scss-output-directory "../")
  ;;(setq scss-compile-at-save t)

  ;;use projectile
  (global-set-key (kbd "C-x C-f") 'projectile-find-file)
  (global-set-key (kbd "C-x C-o") 'find-file)

  ;;Use this if you can't load with the automator script
  ;;Because the OS Gui app environment
  ;;does not have anything from .bashrc
  ;;(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  
  (load "~/.emacs.d/erc"))

;; No splash screen.
(setq inhibit-startup-message t)

;; Visual bell instead of beeping
(setq ring-bell-function 'ignore)

;;don't show toolbar
(tool-bar-mode -1)

;; Interactively do shit!
(require 'ido)
(ido-mode t)
;;(ido-ubiquitous-mode t)

;; Handy line-related thingsgX
(global-linum-mode t)
(column-number-mode t)
(global-hl-line-mode t)

;; Remember open buffers for next session.
;; (desktop-save-mode t)

;; Highlight matching parens, use rainbow delimiters.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;(global-rainbow-delimiters-mode t) this isn't working.
(show-paren-mode t)

;; Auto-completion is sick!
(require 'auto-complete)
(global-auto-complete-mode t)

;; Tabs and alignment
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
;(setq c-basic-offset 4)
;(setq c-basic-indent 2)
(setq tab-width 2)
(setq js-indent-level 2)
(setq css-indent-offset 2)
(setq js2-basic-offset 2)
;;(electric-indent-mode t)

;; Javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; start in css mode
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . css-mode))

;; Handy functions courtesy of whattheemacs.d
(defun open-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline))
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

;;keybindings
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-M-return>") 'open-line-above)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x g") 'goto-line)
;; No more minimizing Emacs by accident.
(global-unset-key (kbd "C-z"))

;;theme
(load-theme 'twilight-anti-bright t)

;;revert all buffers that are visiting a file
(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )

;;add newline at end of file
(setq require-final-newline t)

;;magit status
(global-set-key (kbd "C-c g") 'magit-status)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;ruby-block
;; (require 'ruby-block)
;;   (setq ruby-block-delay 0)
;;   (setq ruby-block-highlight-toggle t)
;;   (ruby-block-mode t)


;;icicles 
;;(require 'icicles)

;;Web-mode customizations
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;;ruby block
(require 'ruby-block)
(ruby-block-mode t)


(require 'yasnippet)
(yas-global-mode 1)
(yas-load-directory "~/.emacs.d/custom_snippets")
(add-hook 'term-mode-hook (lambda()
                            (setq yas-dont-activate t)))

;;save backups in /backups
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

;; obvs
;;(nyan-mode +1)


(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)
