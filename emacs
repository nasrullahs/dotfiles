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


(require 'helm-config)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (darcula)))
 '(custom-safe-themes
   (quote
    ("41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" "3d5720f488f2ed54dd4e40e9252da2912110948366a16aef503f3e9e7dfe4915" "7bc31a546e510e6bde482ebca992e293a54cb075a0cbfb384bf2bf5357d4dee3" "fad38808e844f1423c68a1888db75adf6586390f5295a03823fa1f4959046f81" "c697b65591ba1fdda42fae093563867a95046466285459bd4e686dc95a819310" default)))
 '(global-auto-revert-mode t)
 '(ido-mode t nil (ido))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (add-node-modules-path prettier-js phi-search phi-search-mc tide typescript-mode web-mode rjsx-mode ggtags flycheck-irony irony lsp-mode use-package multiple-cursors helm-ls-git google-c-style flycheck darcula-theme)))
 '(scroll-bar-mode nil)
 '(show-paren-mode 1)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(electric-indent-mode -1)

(setq column-number-mode t)

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

(use-package irony
  :commands irony-mode
  :init (dolist (hook '(c-mode-hook c++-mode-hook))
          (add-hook hook #'irony-mode))
  :config (progn
            (add-hook 'irony-mode-hook
                      (lambda ()
                        (irony-cdb-autosetup-compile-options)))
            ;; For some reason Irony is not detecting C++11, so force it
            (setq-default irony-additional-clang-options '("-std=c++11")))
:ensure t)

;; (use-package flycheck-irony
;;   :commands flycheck-irony-setup
;;   :ensure t)

;; (use-package flycheck
;;   :commands flycheck-mode
;;   :init (dolist (hook '(c-mode-hook c++-mode-hook js2-mode-hook))
;;           ;; Javascript requires JSHint
;;           (add-hook hook #'flycheck-mode))
;;   :config (progn
;;             (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
;;             (setq-default flycheck-clang-language-standard "c++11"
;;                           flycheck-gcc-language-standard "c++11"))
;; :ensure t)

(use-package google-c-style
  ;; Not available on MELPA Stable, loaded from plugins
  :commands google-set-c-style
  :init (add-hook 'c-mode-common-hook
                  (lambda ()
                    (google-set-c-style)
(c-set-offset 'inextern-lang 0))))

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))


(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

(require 'flycheck)
(use-package web-mode
  :ensure t
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.js\\'" . web-mode)
         ("\\.css\\'" . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-block-padding 2
        web-mode-comment-style 2

        web-mode-enable-css-colorization t
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-quoting nil
        web-mode-enable-comment-keywords t
        web-mode-enable-current-element-highlight t
        web-mode-enable-auto-indentation nil
        )
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package typescript-mode
  :ensure t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode #'subword-mode))

(use-package tide
  :init
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)))

(require 'prettier-js)
(eval-after-load 'web-mode
    '(progn
       (add-hook 'web-mode-hook #'add-node-modules-path)
       (add-hook 'web-mode-hook #'prettier-js-mode)))
