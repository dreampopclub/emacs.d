(load "~/.emacs-env.el")

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.milkbox.net/packages/") t)
(package-initialize)

;; do ubuntu/home stuff


;; do macbook and work stuff
(when (eq emacs-env 'osx)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super)

  ;;use projectile
  (global-set-key (kbd "C-x C-f") 'projectile-find-file)
  (global-set-key (kbd "C-x C-o") 'find-file)
)

;; No splash screen.
(setq inhibit-startup-message t)

;; Visual bell instead of beeping
(setq ring-bell-function 'ignore)

;;don't show toolbar
;;(tool-bar-mode -1)

(require 'ido)
(ido-mode t)
;;(ido-ubiquitous-mode t)

;; Handy line-related things
(global-linum-mode t)
(column-number-mode t)
(global-hl-line-mode t)

;; Highlight matching parens, use rainbow delimiters.
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;(global-rainbow-delimiters-mode t) this isn't working.
(show-paren-mode t)

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

;; start erb in web mode
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

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
;;(load-theme 'twilight-anti-bright t)
(when (eq emacs-env 'work)
  ;;  (load-theme 'adwaita t)
  (load-theme 'cyberpunk t)
  ;;use projectile
  (global-set-key (kbd "C-x C-f") 'projectile-find-file)
  (global-set-key (kbd "C-x C-o") 'find-file)
  )

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
 '(column-number-mode t)
 '(grep-find-ignored-directories
   (quote
    ("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "ckeditor")))
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.min" "*.min.js")))
 '(package-selected-packages (quote (magit rainbow-delimiters auto-complete)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Nimbus Mono L" :foundry "urw" :slant normal :weight bold :height 113 :width normal)))))

;; cops busted this party
;;(add-hook 'ruby-mode-hook 'rubocop-mode)

;;rvm ruby crap
;;(add-hook 'ruby-mode-hook 'rvm-activate-corresponding-ruby)

;;activate ruby-test mode
;;(add-hook 'ruby-mode-hook 'ruby-test-mode)

;;turn on 80 char column marker
;;(column-marker-1 10)

;;Web-mode customizations
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

;;ruby block
;;(require 'ruby-block)
;;(ruby-block-mode t)
;;(setq ruby-block-highlight-toggle t)

;;(require 'yasnippet)
;;(yas-global-mode 1)
;;(yas-load-directory "~/.emacs.d/custom_snippets")
;;(add-hook 'term-mode-hook (lambda()
;;                           (setq yas-dont-activate t)))

;;save backups in /backups
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)


(defun lint-ruby-file ()
  ;;lint if ruby file
  (
   ;;when (string-match "\\.rb$" (buffer-file-name)) (rubocop-check-current-file)
   )
  )

;;after save, lint ruby files
(add-hook 'after-save-hook 'lint-ruby-file)

(defun change-theme (theme)
  "Disable all active themes and load THEME."
  (interactive
   (lexical-let ((themes (mapcar 'symbol-name (custom-available-themes))))
     (list (intern (completing-read "Load custom theme: " themes)))))
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme theme t))
