(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(require 'use-package)

;; Don't prompt when there is a process attached to a buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
kill-buffer-query-functions))

;; Don't prompt for active process with shells
(add-hook 'shell-mode-hook
          (lambda ()
            (set-process-query-on-exit-flag
             (get-buffer-process (current-buffer))
nil)))


(require 'helm-ls-git)
(global-set-key (kbd "C-<f6>") 'helm-ls-git-ls)
(global-set-key (kbd "C-x M-f") 'helm-browse-project)

(use-package multiple-cursors
  :init
  (use-package phi-search
    :init
    ;; credit to @jonebird for the following
    ;; Allow isearch functionality with multipl-cursors
    (add-hook 'multiple-cursors-mode-enabled-hook
              (lambda ()
                (interactive)
                (global-set-key (kbd "C-s") 'phi-search)
                (global-set-key (kbd "C-r") 'phi-search-backward)))

    (add-hook 'multiple-cursors-mode-disabled-hook
              (lambda ()
                (interactive)
                (global-set-key (kbd "C-s") 'isearch-forward)
                (global-set-key (kbd "C-r") 'isearch-backward)))))
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(load-theme 'darcula t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(global-auto-revert-mode t)
 '(ido-mode t nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(org-modern add-node-modules-path phi-search phi-search-mc lsp-mode use-package multiple-cursors helm-ls-git darcula-theme))
 '(scroll-bar-mode nil)
 '(show-paren-mode 1)
 '(tab-width 4)
 '(tool-bar-mode nil))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(electric-indent-mode -1)

(setq column-number-mode t)

;; org mode improvements
(with-eval-after-load 'org (global-org-modern-mode))

(custom-set-faces
 '(org-level-1 ((t (:foreground "#e06c75" :weight bold :height 1.3))))
 '(org-level-2 ((t (:foreground "#98c379" :weight bold :height 1.2))))
 '(org-level-3 ((t (:foreground "#61afef" :weight bold :height 1.15))))
 '(org-level-4 ((t (:foreground "#c678dd" :weight bold :height 1.1)))))


;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))
(put 'upcase-region 'disabled nil)
