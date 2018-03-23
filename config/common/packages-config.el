;;; packages-config.el --- base configuration
;;; COMMENTARY:
;;; Code:

(require 'package)

(defun configure-repos ()
    "Set MELPA and ELPA repositories."
    (require 'package)
    (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                        (not (gnutls-available-p))))
           (proto (if no-ssl "http" "https")))
        ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
        (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
        ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
        (when (< emacs-major-version 24)
            ;; For important compatibility libraries like cl-lib
            (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
    (package-initialize))

(defun basic-setup ()
    "Configure builtin and other minimum of packages."

    (configure-repos)

    ;; Disable backup
    (setq make-backup-files nil)

    ;; Disable GUI
    (tooltip-mode -1)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (setq use-dialog-box nil)
    ;; (setq redisplay-dont-pause t)
    (setq ring-bell-function 'ignore)

    (use-package beacon
        :config
        ;; Set cursor
        (setq cursor-type 'bar)
        (beacon-mode 1))
    
    ;; Dired
    (use-package dired
        :init
        :config
        (setq dired-recursive-deletes 'top))

    ;; Imenu
    (use-package imenu
        :bind (("<f4>" . imenu))
        :init
        :config
        (setq imenu-autorescan t)
        (setq imenu-use-popup-menu t))

    ;; Display name of teh buffer
    (setq frame-title-format "GNU Emacs: %b")

    ;; Disable startup screen
    (setq inhibit-splash-screen   t)
    (setq inhibit-startup-message t)

    ;; Show-paren-mode settings
    (show-paren-mode t)
    (setq show-paren-style 'expression)

    ;; Electric-modes settings
    (electric-pair-mode 1)
    (electric-indent-mode 1)

    ;; Delete selection
    (delete-selection-mode t)

    ;; Regexp search
    (global-set-key (kbd "C-s") #'isearch-forward-regexp)
    (global-set-key (kbd "C-r") #'isearch-backward-regexp)
    (global-set-key (kbd "C-M-s") #'isearch-forward)
    (global-set-key (kbd "C-M-r") #'isearch-backward))

(provide 'packages-config)

;;; packages-config.el ends here
