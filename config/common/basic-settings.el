;;; basic-settings.el --- provide basic configuration
;;; Commentary:
;; Configure basic parameters like linum and filesize
;;; Code:


;; Projectile + Neotree
(defun configure-projectile ()
    "Projectile+Neotree configuration."
    (use-package projectile)
    (use-package neotree
        :bind (("M-e" . neotree-project-dir))
        :init
        :config
        (setq neo-smart-open t)
        (setq projectile-switch-project-action 'neotree-projectile-action)

        (defun neotree-project-dir ()
            "Open NeoTree using the git root."
            (interactive)
            (let ((project-dir (projectile-project-root))
                  (file-name (buffer-file-name)))
                (neotree-toggle)
                (if project-dir
                        (if (neo-global--window-exists-p)
                                (progn
                                    (neotree-dir project-dir)
                                    (neotree-find file-name)))
                    (message "Could not find git project root."))))

        ;; (global-set-key "\M-e" 'neotree-project-dir)
        (projectile-mode)
        (setq projectile-indexing-method 'native)))

;; Buffers selectin and ibuffer settings
(defun configure-ibuffer ()
    "Make awesome menu and bind it to F2;
Integrate projectile with ibuffer."
    (use-package bs)
    (use-package ibuffer
        :bind (("<f2>" . ibuffer)
               ("<f5>" . buffer-menu)))

    ;; (defalias 'list-buffers 'ibuffer-list-buffers)
    (use-package ibuffer-projectile
        :config
        (add-hook 'ibuffer-hook
                  (lambda ()
                      (ibuffer-projectile-set-filter-groups)
                      (unless (eq ibuffer-sorting-mode 'alphabetic)
                          (ibuffer-do-sort-by-alphabetic))))))

;; Linum plugin
(defun configure-linum (&optional enabled)
    "Make linum ENABLED."
    (line-number-mode nil)
    (global-linum-mode nil)
    (column-number-mode nil)
    (setq linum-format " %d "))

;; Comment block
(defun comment-dwim-line (&optional arg)
    "Replacement for 'comment-dwim' ARG."
    (interactive "*P")
    (comment-normalize-vars)
    (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
            (comment-or-uncomment-region (line-beginning-position) (line-end-position))
        (comment-dwim arg)))

;; HELM
(defun helm-configure ()
    "Configure HELM."
    (use-package helm
        :bind (("M-x" . helm-M-x)
               ("C-x r b" . helm-filtered-bookmarks)
               ("C-x C-f" . helm-find-files)
               ("M-i" . helm-swoop)
               ("M-I" . helm-swoop-back-to-last-point)
               ("C-c M-i" . helm-multi-swoop)
               ("C-x M-i" . helm-multi-swoop-all))
        :init
        ;; HELM TAB completion
        ;; (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
        :config
        (use-package helm-swoop)

        ;; Helm fuzy match
        (setq helm-mode-fuzzy-match t)
        (setq helm-M-x-fuzzy-match t)

        ;; When doing isearch, hand the word over to helm-swoop
        ;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
        ;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

        ;; Save buffer when helm-multi-swoop-edit complete
        (setq helm-multi-swoop-edit-save t)

        ;; If this value is t, split window inside the current window
        (setq helm-swoop-split-with-multiple-windows nil)

        ;; Split direction. 'split-window-vertically or 'split-window-horizontally
        (setq helm-swoop-split-direction 'split-window-vertically)

        ;; If nil, you can slightly boost invoke speed in exchange for text color
        (setq helm-swoop-speed-or-color nil)

        ;; Go to the opposite side of line from the end or beginning of line
        (setq helm-swoop-move-to-line-cycle t)

        ;; Optional face for line numbers
        ;; Face name is `helm-swoop-line-number-face`
        (setq helm-swoop-use-line-number-face t)

        (helm-mode 1)))

(defun configure-base (&optional indent)
    "Set base configuration, configure INDENT."
    (configure-projectile)
    (configure-ibuffer)
    (configure-linum)
    (helm-configure)

    ;; Short messages
    (defalias 'yes-or-no-p 'y-or-n-p)

    ;; Gnupg
    (use-package epg
        :config
        (epa-file-enable))

    ;; Check syntax on the fly
    (use-package flycheck
        :config
        (add-hook 'after-init-hook #'global-flycheck-mode))

    ;; Multiple cursors
    (use-package multiple-cursors
        :bind (("C-S-c C-S-c" . mc/edit-lines)
               ("C->" . mc/mark-next-like-this)
               ("C-<" . mc/mark-previous-like-this)
               ("C-C C-<" . mc/mark-all-like-this)))
    
    (use-package markdown-mode)

    (global-set-key "\M-;" 'comment-dwim-line)

    (setenv "PATH" (concat (getenv "PATH") ":~/.local/bin"))
    
    ;; Scrolling
    (setq scroll-step               1)
    (setq scroll-margin            10)
    (setq scroll-conservatively 10000)
    
    ;; Fringe settings
    (fringe-mode '(4 . 0))
    (setq-default indicate-empty-lines t)
    (setq-default indicate-buffer-boundaries 'left)
    ;; (global-fringe-mode t)

    ;; Display file size in mode-line
    (setq display-time-24hr-format t)
    (display-time-mode             t)
    (size-indication-mode          t)

    ;; Indent settings
    (setq-default indent-tabs-mode nil)
    (setq-default tab-width indent)
    (setq-default c-basic-offset indent)
    (setq-default standart-indent indent)
    (setq-default lisp-body-indent indent)
    (global-set-key (kbd "RET") 'newline-and-indent)

    ;; Clipboard settings
    (setq x-select-enable-clipboard t)

    ;; EOF newlines
    (setq require-final-newline t)
    (setq next-line-add-newlines nil)

    ;; Highlight search results
    (setq search-highlight t)
    (setq query-replace-highlight t)

    ;; Undo & Redo
    (global-unset-key "\C-z")
    (global-set-key "\C-z" 'advertised-undo))

(provide 'basic-settings)

;;; basic-settings.el ends here
