;; My Personal Details
(setq user-full-name "Mohammad Faisal")
(setq user-mail-address "faisalhmohd@gmail.com")

;; Package Management
(load "package")
(package-initialize)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

;; Setup Packages
(defvar faisal/packages '(autopair
                          auto-complete
                          projectile
                          web-mode
                          ido-vertical-mode
                          idle-highlight-mode
                          rjsx-mode
                          ag
                          yaml-mode
                          yasnippet
                          yasnippet-snippets
                          php-mode
			  magit
                          )
  "Default packages")

(defun faisal/packages-installed-p ()
  (cl-loop for pkg in faisal/packages
        when (not (package-installed-p pkg)) do (cl-return nil)
        finally (cl-return t)))

(unless (faisal/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg faisal/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;; Enable Yas Snipper Globally
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))
(yas-global-mode 1)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "C-t") 'yas-expand)

;; Splash Screen in Org Mode

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Require AG
(require 'ag)

;; Remove Menu bar

(menu-bar-mode -1)
(tool-bar-mode -1)

;; Marking text

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Enable mouse
(xterm-mouse-mode 1)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)

;; Remove tabs and set width to 2
(setq tab-width 2
      indent-tabs-mode nil)
(setq css-indent-offset 2)

;; Never backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Just type y and n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable PHP mode
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Key Bindings
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "C-l") 'comment-or-uncomment-region)
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Always show parentheses
(show-paren-mode t)

;; Use visual indicator instead of noises
(setq echo-keystrokes 0.1
      use-dialog-box nil)
;; Enable ido mode for navigating in filesystem
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Auto pair brackets on creation
(require 'autopair)

;; turn on autocomplete
(ac-config-default)

;; Highlight words
(add-hook 'prog-mode-hook (lambda () (idle-highlight-mode t)))

;; Add Web Mode

(setq web-mode-style-padding 2)
(setq web-mode-script-padding 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

;; YAML support
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; JS Mode
(defun js-custom ()
  "js-mode-hook"
  (setq js-indent-level 2))

(add-hook 'js-mode-hook 'js-custom)
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))

;; Load wombat theme
(load-theme 'wombat t)

;; Color Codes
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Projectile Mode
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)

;; New line at end of file
(setq require-final-newline t)

;; Display line numbers
(global-linum-mode t)
(setq linum-format "%4d\u2502 ")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (xclip use-package yasnippet-classic-snippets yasnippet php-mode aggressive-indent idle-highlight-mode gruvbox-theme spacemacs-theme rjsx-mode ido-vertical-mode auto-complete autopair))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; set maximum indentation for description lists
(setq org-list-description-max-indent 5)

;; prevent demoting heading also shifting text inside sections
(setq org-adapt-indentation nil)
(setq org-startup-folded nil)
(setq org-support-shift-select t)

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "~/todo.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n")))

;; set key for agenda
(global-set-key (kbd "C-c a") 'org-agenda)

;;capture todo items using C-c c t
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-agenda-files (quote ("~/todo.org")))

;; Add PHP Support
(autoload 'php-mode "php-mode" "Major mode for editing PHP code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; Turn off Bell completely
(setq ring-bell-function 'ignore)

;; Move current line up and down
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Add Copy to clipboard
(xclip-mode 1)

;; Enable Cua Mode
(cua-mode 1)

;; Auto reload file if changed on disk
(global-auto-revert-mode t)

;; Tramp to use ssh instead of scp by default
(setq tramp-default-method "ssh")

;; Add Magit status shortcut
(global-set-key (kbd "C-x g") 'magit-status)
