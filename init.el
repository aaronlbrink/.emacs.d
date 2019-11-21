(server-start)
(prefer-coding-system 'utf-8)

;; What does this line do?
(setq-default indent-tabs-mode nil)

;; make a .emacs-saves folder for auto-saved stuff
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs-saves/" t)))


(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(custom-safe-themes
   '("a77ced882e25028e994d168a612c763a4feb8c4ab67c5ff48688654d0264370c" "04589c18c2087cd6f12c01807eed0bdaa63983787025c209b89c779c61c3a4c4" default))
 '(fringe-mode 6 nil (fringe))
 '(linum-format 'dynamic)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(idle-org-agenda auto-org-md org-plus-contrib monokai-theme sudoku tramp speed-type soothe-theme org-bullets a js2-mode auctex w3m web-mode tide projectile ace-window grandshell-theme alect-themes alect-theme lua-mode boon-qwerty counsel smex ivy lsp-ui flycheck-inline flycheck ido-vertical-mode company-lsp company lsp-mode cherry-blossom-theme editorconfig use-package)))
(require 'package)
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))




(setq use-package-always-ensure t)

(use-package direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)

(use-package popwin)
(popwin-mode 1)

(push '(direx:direx-mode :position left :width 37 :dedicated t )
      popwin:special-display-config)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure org-plus-contrib
  :bind ("M-q" . 'toggle-truncate-lines)
  :config
  (progn
    ;; config stuff
    (setq org-return-follows-link t)
    (setq org-agenda-files '("~/org"))
    (setq org-agenda-custom-commands
          ' (
             ("H" "Office and Home Lists"
              ((agenda)
               (tags-todo "OFFICE")
               (tags-todo "HOME")
               (tags-todo "COMPUTER")
               (tags-todo "MOVIE")
               (tags-todo "READING")))
             ("D" "Daily Action List"
              (
               (agenda "" ((org-agenda-ndays 1)
                           (org-agenda-sorting-strategy
                            (quote ((agenda time-up priority-down tag-up) )))
                           (org-deadline-warning-days 0)
                           ))))
             )
        )
;;  (setq org-agenda-custom-commands
;;          '(("h" "Daily habits"
;;             ((agenda ""))
;;          ((org-agenda-show-log t)
  ;;            (org-agenda-ndays 7)
 ;;             (org-agenda-log-mode-items '(state))
;;              (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp ":DAILY:"))))
            ;; other commands here
;;            ))
    (setq org-refile-targets ' (("work.org" :maxlevel . 4)
                                ("main.org" :maxlevel . 2)
                                ("gtd.org"  :maxlevel . 1)
                                ("someday.org" :level . 2)))
    )
  :hook (
         (after-change-major-mode . (lambda() (electric-indent-mode -1)))
         )
  )

;; org-capture templates
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/gtd.org" "Tasks")
         "* TODO %?\n  %i\n  %a \nAdded: %U")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")))

;; org-capture keybindings (to avoid the template menu when capturing
(define-key global-map "\C-ct"
  (lambda () (interactive) (org-capture nil "t")))
(define-key global-map "\C-cj"
  (lambda () (interactive) (org-capture nil "j")))

;; org-mode requires these (see https://orgmode.org/manual/Activation.html)
;; taxking these lines out will cause things like C-c l to return undefined command
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key "\C-ca" 'org-agenda)


;; Org Specific Packages
;; https://github.com/jamcha-aa/auto-org-md
(use-package auto-org-md)

(use-package idle-org-agenda
  :after org-agenda
  :ensure t
  :config (idle-org-agenda-mode))

;
(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package company
  :config
  (global-company-mode 1))

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package evil
  :config
  (evil-mode 1))

(use-package flycheck-inline
  :config
  (with-eval-after-load 'flycheck (global-flycheck-inline-mode)))

(use-package smex)

(use-package ivy
  :config
  (ivy-mode 1))

(use-package counsel
  :bind*
  (("M-x"     . counsel-M-x)       ; M-x use counsel
   ("C-x C-f" . counsel-find-file)) ; C-x C-f use counsel-find-file
  )

(use-package lsp-mode
  :hook
  ((prog-major-mode . lsp-prog-major-mode-enable)
    (lsp-after-open-hook . lsp-enable-imenu)
    (prog-mode . lsp))
  :config
  (setq
    lsp-inhibit-message nil
    lsp-highlight-symbol nil
    lsp-enable-snippet nil))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable nil
      lsp-ui-flycheck-enable t)
  :hook
  (lsp-mode . lsp-ui-mode))

(use-package company-lsp)
  
(use-package projectile
  :config
  (projectile-mode 1)
  :ensure t
  :bind-keymap (("C-c p" . projectile-command-map))
)
(setq projectile-project-search-path '("/home/aaron/ldev/personal" "/home/aaron/ldev/brinkdevelopmentllc/kyros/"))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-mode))

;; TypeScript configuration
(use-package tide)
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode 1)
  (tide-hl-identifier-mode +1))

(use-package web-mode
  :config
  (with-eval-after-load 'tide
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
    (add-hook 'web-mode-hook
      (lambda ()
        (when (string-equal "tsx" (file-name-extension buffer-file-name))
          (setup-tide-mode))))
    ;; enable typescript-tslint checker
    (flycheck-add-mode 'typescript-tslint 'web-mode)))

;; C++ configuration
;; (defun c++-setup ()
;;  (c-set-offset 'innamespace 0)
;;  (c-set-offset 'inclass '++)
;;  (c-set-offset 'access-label '-)
;;  (setq flycheck-clang-include-path
;;               (list
;;                 (expand-file-name "include"
;;                   (projectile-project-root)))))
;;
;;(add-hook 'c++-mode-hook 'c++-setup)
;; (when (string-equal system-type "windows-nt")
;;   (let* (
;;         (mypaths
;;          '(
;;             "C:/msys64/usr/bin"
;;             "C:/msys64/mingw64/bin"
;;            ))
;;         )
;;     (setenv "PATH" (mapconcat 'identity mypaths ";"))
;;     (setq exec-path (append mypaths (list "." exec-directory)))))

;; Theme
(if (display-graphic-p)
  (progn
    (use-package cherry-blossom-theme)
    (load-theme 'cherry-blossom t))
  (progn
    (use-package monokai-theme)
    (load-theme 'monokai t)))

(set-cursor-color "#aaaaaa")
(set-background-color "#282828")
;; (set-face-attribute 'fringe nil :background "#000000")
(set-face-foreground 'vertical-border (face-background 'fringe))

;; font
(add-to-list 'default-frame-alist '(font . "Source Code Pro-10" ))
;; (set-default-font "Source Code Pro-11")
(set-face-attribute 'default t :font "Source Code Pro-10")


;; Set company theme since cherry blossom doesn't
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
    ;; `(mode-line-inactive ((t (:background "#000000"))))
    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Other aesthetic customization
(set-face-attribute 'default nil :height 90)
(prefer-coding-system 'utf-8)
;;(setq inhibit-startup-message t)
(setq initial-scratch-message nil)
;; Disable the awful bell in windows
(setq ring-bell-function 'ignore)

;; I don't like auto saving
;; (setq auto-save-default nil)
;; Remove unnecessary things
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#199919991999"))))
 '(company-scrollbar-fg ((t (:background "#0ccc0ccc0ccc"))))
 '(company-tooltip ((t (:inherit default :background "#222222"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
 '(mode-line-inactive ((t (:background "#000000")))))
(provide 'init)

;;; init.el ends here
