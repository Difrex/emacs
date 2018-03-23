;;; common.el --- base configuration
;;; COMMENTARY:
;;; Code:

(require 'packages-config)
(require 'basic-settings)

(defun start-server-on-unix ()
    "If OS is Linux or MacOS start server."
    (defun system-is-linux()
        (string-equal system-type "gnu/linux"))

    (defun system-is-darwin()
        (string-equal system-type "darwin"))

    ;; Start Emacs as server. Linux and MacOS support
    (when (or (system-is-linux) (system-is-darwin))
        (require 'server)
        (message "Run as server.")
        (unless (server-running-p)
            (server-start))))

(defun use-package-setup ()
    "Configure MELPA and install use-package."
    ;; Repo configuration
    (configure-repos)

    ;; Install use-package
    (when (not (require 'use-package nil 'noerror))
        (and (message "Installing ")
             (package-install 'use-package)))

    ;; Verbosity
    (setq use-package-verbose t)
    ;; Always download packages
    (setq use-package-always-ensure t))

(defun common-setup ()
    "Setup base GNU Emacs configuration."
    (start-server-on-unix)
    (use-package-setup)
    (basic-setup)
    ;; Set indent to 4 space
    (configure-base 4))

(provide 'common)

;;; common.el ends here
