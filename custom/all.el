;;; all.el -- all of my configuration (including org)

;; Author: Aaron Brink <aaronlbrink@gmail.com>
;; Version: 1.0
;; Keywords: emacs-config
;; URL: https://gitlab.com/aaronlbrink

;;; Commentary:
;; I still have no idea about some of this stuff.

;;; Code:
(provide 'all)

;;
;; --- Start Customizing Individual Packages ---
;;

;; Simple directory explorer
;; Like VSCode's left panel which shows collapsable directories
;; https://github.com/emacsorphanage/direx
(use-package direx)
(global-set-key (kbd "C-x C-j") 'direx:jump-to-directory)
;; uh, next line exists above..what's the difference between jump-to-directory and
;; jump-to-directory-other-window?
;; (global-set-key (kbd "C-x C-j") 'direx:jump-to-directory-other-window)


;; Popup window manager
;; Allows you to close what should be temporary buffers such as *Completion* with
;; C-g at anytime.
;; https://github.com/emacsorphanage/popwin
(use-package popwin)
(popwin-mode 1)


(push '(direx:direx-mode :position left :width 37 :dedicated t )
      popwin:special-display-config)

;; Org mode
;; For notes, TODO lists, and document creation with a plain-text system
;; https://orgmode.org/
(use-package org
  :mode                            (("\\.org$" . org-mode))
  :ensure                          org-plus-contrib
  :bind                            ("M-q"      . 'toggle-truncate-lines)
  :config
  (progn
    ;; config stuff
    (setq org-return-follows-link  t)
    (setq org-agenda-files         '("~/org"))
    (setq org-agenda-custom-commands
          ' (
             ("H" "Office and Home Lists"
              ((agenda)
               (tags-todo          "OFFICE")
               (tags-todo "HOME")
               (tags-todo "COMPUTER")
               (tags-todo "MOVIE")
               (tags-todo "READING")))
             ("D" "Daily Action List"
              (
               (agenda             "" ((org-agenda-ndays 1)
                                       (org-agenda-sorting-strategy
                                        (quote ((agenda time-up priority-down tag-up) )))
                                       (org-deadline-warning-days 0)
                                       ))))
             )
          )
    (setq org-refile-targets ' (("work.org" :maxlevel . 4)
                                ("main.org" :maxlevel . 2)
                                ("gtd.org"  :maxlevel . 1)
                                ("someday.org" :level . 1)))
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
         "* %?\nEntered on %U\n  %i\n  %a")
        ("s" "Todo" entry (file+headline "~/org/someday.org" "Someday")
         "* %?\n  %i\n  %a \nAdded: %U")
)
      )


;; org-capture keybindings (these are speedy! no need to use the org template menu to
;; add a capture/note)
(define-key global-map "\C-ct"
  (lambda () (interactive) (org-capture nil "t")))
(define-key global-map "\C-cj"
  (lambda () (interactive) (org-capture nil "j")))
(define-key global-map "\C-cs"
  (lambda () (interactive) (org-capture nil "s")))


;; org-mode requires these (see https://orgmode.org/manual/Activation.html)
;; taking these lines out will cause things like C-c l to return undefined command
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key "\C-ca" 'org-agenda)

(setq org-list-allow-alphabetical t)


;; Org Specific Packages

;; Autocreate Markdown files from org
;; If enabled (M-x auto-org-md-mode), a markdown file will automatically be created
;; when saving an org file.
;; https://github.com/jamcha-aa/auto-org-md
(use-package auto-org-md)


;; Show org-agenda view after timeout
;; Idling in an org-file result in the agenda/your todos being displayed
;; https://github.com/enisozgen/idle-org-agenda
(use-package idle-org-agenda
  :after org-agenda
  :ensure t
  :config (idle-org-agenda-mode))


;; Support .editorconfig files in projects
;; Helps ensure everyone's editor comes to agreement on things like the number
;; of spaces to indent lines, etc.
;; https://editorconfig.org/
;; https://github.com/editorconfig/editorconfig-emacs
(use-package editorconfig
  :config
  (editorconfig-mode 1))


;; COMPlete ANYthing
;; Text completion framework
;; M-n and M-p to select completions, C-s and C-r and C-o to search through completions
;; https://company-mode.github.io/
(use-package company
  :config
  (global-company-mode 1))


;; Syntax checking
;; Checks for syntax errors while you type and highlights errors. Also works with Emac's
;; error buffer, which can be viewed to see all errors.
;; M-g n go to next error
;; Language support: https://www.flycheck.org/en/latest/languages.html#flycheck-languages
;; https://www.flycheck.org/en/latest/index.html
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  (progn (setq-default flycheck-emacs-lisp-load-path 'inherit))
  )

;; Display fly-check messages inline with code
(use-package flycheck-inline
  :config
  (with-eval-after-load 'flycheck (global-flycheck-inline-mode)))


;; M-x enhancement
;; Shows recent and frequently used M-x commands
;; https://github.com/nonsequitur/smex
(use-package smex)


;; Simple tools for minibuffer completion
;; Ivy includes 3 subparts: Ivy itself (list completion), Counsel (Ivy-enhanced versions
;; of common Emacs commands, and Swiper (Ivy-enhanced alternative to isearch)
;; https://github.com/abo-abo/swiper (includes all 3)
(use-package ivy
  :config
  (ivy-mode 1)
  :bind* (
     ("\C-s" . swiper)
     ("M-x"     . counsel-M-x)       ; M-x use counsel
     ("C-x C-f" . counsel-find-file)) ; C-x C-f use counsel-find-file
  )


;; Client for Language Server Protocol
;; Adds support for LSP and trys to provide an IDE-like experience with optional integration
;; ability with the popular company, flycheck, and projectile packages.
;; https://github.com/emacs-lsp/lsp-mode
(use-package lsp-mode
  :hook
  ((prog-major-mode . lsp-prog-major-mode-enable)
    (lsp-after-open-hook . lsp-enable-imenu)
    (prog-mode . lsp))
  :config
  (setq
    lsp-enable-snippet nil))

;; LSP UI stuff. See how to configure useful things:
;; https://emacs-lsp.github.io/lsp-ui/
(use-package lsp-ui
  :config
  (setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable nil
      lsp-ui-flycheck-enable t)
  :hook
  (lsp-mode . lsp-ui-mode))


;; Company completion backend for lsp-mode
;; Provides some things like using "." to trigger completion.
;; I need to invest sometime to understand how the LSP framework actually works to really
;; understand the necessity of this package.
;; https://github.com/tigersoldier/company-lsp
(use-package company-lsp)
  

;; Project interaction library
;; Provides navigation and management features on the project-level. If a directory has
;; a .git directory, then it's a project. Somethings you can do: find files in project,
;; visit project in dired, jump to recently visited files in project,
;; https://github.com/bbatsov/projectile
(use-package projectile
  :config
  (projectile-mode 1)
  :ensure t
  :bind-keymap (("C-c p" . projectile-command-map))
)
;;(setq projectile-project-search-path '("/home/aaron/ldev/personal" "/home/aaron/ldev/brinkdevelopmentllc/kyros/"))


;; Web Development Stuff
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


;;
;; --- Theming ---
;;

(load-theme 'misterioso t)

;; Custom Theme ~ab
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(fringe-mode 6 nil (fringe))
 '(linum-format 'dynamic)
 '(package-archives
   '(("gnu" . "http://elpa.gnu.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-selected-packages
   '(misterioso idle-org-agenda auto-org-md org-plus-contrib monokai-theme sudoku tramp speed-type soothe-theme org-bullets a js2-mode auctex w3m web-mode tide projectile ace-window grandshell-theme alect-themes alect-theme lua-mode boon-qwerty counsel smex ivy lsp-ui flycheck-inline flycheck ido-vertical-mode company-lsp company lsp-mode cherry-blossom-theme editorconfig use-package)))

;; Custom Theme, Typefaces
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :family "Source Code Pro")))))


(set-cursor-color "#aaaaaa")
;;(set-background-color "#282828")
(set-face-foreground 'vertical-border (face-background 'fringe))

(set-face-attribute 'default nil :height 20)

;; For Mac OS X
    (when (eq system-type 'darwin)
      (set-face-attribute 'default nil :family "Source Code Pro")

      ;; default font size (point * 10)
      ;;
      ;; WARNING!  Depending on the default font,
      ;; if the size is not supported very well, the frame will be clipped
      ;; so that the beginning of the buffer may not be visible correctly.
      (set-face-attribute 'default nil :height 145)
)

;; The theme for company (a plugin) does not get set by Custom.
(require 'color)
(let ((bg (face-attribute 'default :background)))
  (custom-set-faces
    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

;; Don't show welcome screen
(setq initial-scratch-message "Hi, Aaron!")

(provide 'all)
;;; all.el ends here
