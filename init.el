;;; init.el -- main emacs config
;;; Commentary:
;;; Code:

;; Customizable parameters
(defcustom emacs-config-dir "~/.emacs.d/config"
    "Configs location."
    :group 'emacs
    :type 'string)

(defcustom emacs-enable-python t
    "Enable or disable python autocomplete."
    :group 'emacs
    :type 'boolean)

(defcustom emacs-enable-golang t
    "Enable or disable go-mode."
    :group 'emacs
    :type 'boolean)

(defcustom emacs-enable-erc nil
    "Enable or disable ERC chat."
    :group 'emacs
    :type 'boolean)

(defcustom emacs-enable-rust t
    "Enable or disable rust-mode."
    :group 'emacs
    :type 'boolean)

(defcustom emacs-enable-custom nil
    "Enable or disable custom settings."
    :group 'emacs
    :type 'boolean)

;; Basic GNU Emacs configuration settings
(add-to-list 'load-path (concat emacs-config-dir "/common"))
(require 'common)
(common-setup)

;; Custom settings
(when emacs-enable-custom
    (and
     (add-to-list 'load-path (concat emacs-config-dir "/custom"))
     (require 'custom-config)
     (configure-custom)))

(defun common-resetup ()
    "Reload common configuration."
    (interactive)
    (load-file (concat emacs-config-dir "/common/common.el"))
    (common-setup)
    (message "Common configuration reloaded."))

(defun custom-resetup ()
    "Reload custom configuration."
    (interactive)
    (load-file (concat emacs-config-dir "/custom/custom-config.el"))
    (configure-custom)
    (message "Custom configuration reloaded."))

;; Python mode settings
(when emacs-enable-python
    (and
     (add-to-list 'load-path (concat emacs-config-dir "/python"))
     (require 'python-config)
     (configure-python-packages)))

(defun python-resetup ()
    "Reload python configuration."
    (interactive)
    (load-file (concat emacs-config-dir "/python/python-config.el"))
    (configure-python-packages)
    (message "Python configuration reloaded."))

;; Go mode settings
(when emacs-enable-golang
    (and
     (add-to-list 'load-path (concat emacs-config-dir "/golang"))
     (require 'go-config)
     (configure-go-packages)))

(when emacs-enable-rust
    (and
     (add-to-list 'load-path (concat emacs-config-dir "/rust"))
     (require 'rust-config)
     (configure-rust)))

(defun golang-resetup ()
    "Reload go-mode configuration."
    (interactive)
    (load-file (concat emacs-config-dir "/golang/go-config.el"))
    (configure-go-packages)
    (message "Go configuration reloaded."))

(defun rust-resetup ()
    "Reload rust-mode configuration."
    (interactive)
    (load-file (concat emacs-config-dir "/rust/rust-config.el"))
    (configure-rust)
    (message "Rust configuration reloaded."))

;; ERC settings
(when emacs-enable-erc
    (and
     (add-to-list 'load-path (concat emacs-config-dir "/erc"))
     (require 'erc-config)
     (configure-erc)))

(defun erc-resetup ()
    "Reload ERC configuration."
    (interactive)
    (load-file (concat emacs-config-dir "/erc/erc-config.el"))
    (configure-erc)
    (message "ERC configuration reloaded."))

(defun load-secret-file (name)
    "Load file NAME from ~/.emacs.d/secret;
Also append .gpg for it."
    (load-file (concat emacs-config-dir "/../secrets/" name ".gpg")))

;; Enable rainbow
(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)

(line-number-mode nil)
(global-linum-mode nil)
(column-number-mode nil)

;; Close all buffers
(defun close-all-buffers ()
    "Close ALL open buffers."
    (interactive)
    (mapc 'kill-buffer (buffer-list)))

;; Copy and Cut whole lines
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
               (list (region-beginning) (region-end))
           (list (line-beginning-position) (line-beginning-position 2)))))

;; Diminish
(use-package diminish)
(eval-after-load "filladapt" '(diminish 'filladapt-mode))

(defun diminished-modes ()
    "Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor."
    (interactive)
    (let ((minor-modes minor-mode-alist)
          message)
        (while minor-modes
            (when (symbol-value (caar minor-modes))
                ;; This minor mode is active in this buffer
                (let* ((mode-pair (car minor-modes))
                       (mode (car mode-pair))
                       (minor-pair (or (assq mode diminished-mode-alist) mode-pair))
                       (minor-name (cadr minor-pair)))
                    (when (symbolp minor-name)
                        ;; This minor mode uses symbol indirection in the cdr
                        (let ((symbols-seen (list minor-name)))
                            (while (and (symbolp (callf symbol-value minor-name))
                                        (not (memq minor-name symbols-seen)))
                                (push minor-name symbols-seen))))
                    (push minor-name message)))
            (callf cdr minor-modes))
        ;; Handle :eval forms
        (setq message (mapconcat
                       (lambda (form)
                           (if (and (listp form) (eq (car form) :eval))
                                   (apply 'eval (cdr form))
                               form))
                       (nreverse message) ""))
        (when (= (string-to-char message) ?\ )
            (callf substring message 1))
        (message "%s" message)))

;; Hide modeline
(defvar-local hidden-mode-line-mode nil)

(define-minor-mode hidden-mode-line-mode
    "Minor mode to hide the mode-line in the current buffer."
    :init-value nil
    :global t
    :variable hidden-mode-line-mode
    :group 'editing-basics
    (if hidden-mode-line-mode
            (setq hide-mode-line mode-line-format
                  mode-line-format nil)
        (setq mode-line-format hide-mode-line
              hide-mode-line nil))
    (force-mode-line-update)
    ;; Apparently force-mode-line-update is not always enough to
    ;; redisplay the mode-line
    (redraw-display)
    (when (and (called-interactively-p 'interactive)
               hidden-mode-line-mode)
        (run-with-idle-timer
         0 nil 'message
         (concat "Hidden Mode Line Mode enabled.  "
                 "Use M-x hidden-mode-line-mode to make the mode-line appear."))))

;; If you want to hide the mode-line in every buffer by default
;; (add-hook 'after-change-major-mode-hook 'hidden-mode-line-mode)

;; Puppet
(use-package flymake-puppet)

;; Temporary maximize buffer
(defun toggle-maximize-buffer ()
    "Maximize buffer."
    (interactive)
    (if (= 1 (length (window-list)))
            (jump-to-register '_)
        (progn
            (window-configuration-to-register '_)
            (delete-other-windows))))
(global-set-key [(super shift return)] 'toggle-maximize-buffer)

;; Jabber
;; Notifications
(use-package jabber
    :bind (("C-<up>" . my-jabber-previous-input)
           ("C-<down>" . my-jabber-next-input))
    :init
    :config

    ;; Accounts
    (load-secret-file "jabber-accounts.el")

    (defvar my-jabber-input-history '() "Variable that holds input history")
    (make-variable-buffer-local 'my-jabber-input-history)

    (defvar my-jabber-input-history-position 0 "Current position in input history")
    (make-variable-buffer-local 'my-jabber-input-history-position)

    (defvar my-jabber-input-history-current nil "Current input value")
    (make-variable-buffer-local 'my-jabber-input-history-current)

    (defvar libnotify-program "/usr/bin/notify-send")
  
    (defun notify-send (title message)
        (start-process "notify" " notify"
                       libnotify-program "--icon=im-jabber" "--expire-time=4000" title message))

    (defun libnotify-jabber-notify (from buf text proposed-alert)
        "(jabber.el hook) Notify of new Jabber chat messages via libnotify."
        (when (or jabber-message-alert-same-buffer
                  (not (memq (selected-window) (get-buffer-window-list buf))))
            (if (jabber-muc-sender-p from)
                    (notify-send (format "(PM) %s"
                                         (jabber-jid-displayname (jabber-jid-user from)))
                                 (format "%s: %s" (jabber-jid-resource from) text)))
            (notify-send (format "%s" (jabber-jid-displayname from))
                         text)))

    (defun my-jabber-previous-input ()
        (interactive)
        (let (current-input (pos my-jabber-input-history-position) (len (length my-jabber-input-history)))
            (if (= pos 0)
                    (message "%s" "No previous input")
                (setq current-input (delete-and-extract-region jabber-point-insert (point-max)))
                (when (= pos len) ; running first time, save current input
                    (setq my-jabber-input-history-current current-input))
                (decf my-jabber-input-history-position)
                (insert (nth my-jabber-input-history-position my-jabber-input-history)))))

    (defun my-jabber-next-input ()
        (interactive)
        (let ((pos my-jabber-input-history-position) (len (length my-jabber-input-history)))
            (cond
             ((= pos (1- len)) ; pointing at the last element, insert saved input
              (incf my-jabber-input-history-position)
              (delete-region jabber-point-insert (point-max))
              (insert my-jabber-input-history-current)
              (setq my-jabber-input-history-current nil))
             ((= pos len)                              ; pointing beyound last element, notify user
              (message "%s" "No next input"))
             (t                                ; insert next history item
              (incf my-jabber-input-history-position)
              (delete-region jabber-point-insert (point-max))
              (insert (nth my-jabber-input-history-position my-jabber-input-history))))))

    (defun my-jabber-input-history-choose ()
        (interactive)
        (let ((choice (ido-completing-read "Select history item: " (reverse my-jabber-input-history))))
            (delete-region jabber-point-insert (point-max))
            (insert choice)))
    
    ;; Hooks
    (defun disable-linum-and-enable-visual-line-hook ()
        "Disable lium-mode and anable vial-line-mode for jabber chat."
        (linum-mode 0)
        (visual-line-mode))

    (defun my-jabber-input-history-hook (body id)
        (add-to-list 'my-jabber-input-history body t)
        (setq my-jabber-input-history-position (length my-jabber-input-history)))

    (add-hook 'jabber-alert-message-hooks 'libnotify-jabber-notify)
    (add-hook 'jabber-chat-mode-hook 'disable-linum-and-enable-visual-line-hook)
    (add-hook 'jabber-chat-send-hooks 'my-jabber-input-history-hook)
    
    (jabber-connect-all))


;; IDEC
(add-to-list 'load-path "~/projects/idecel")
(require 'idec-mode)

;; Update all packages
(auto-package-update-now)

(add-hook 'minimap-mode-hook 'hidden-mode-line-mode-hook)

;; custom
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file)

;;; init.el ends here
