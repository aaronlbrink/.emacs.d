;;; server.el -- selected packages and config for editing on servers

;; Author: Aaron Brink <aaronlbrink@gmail.com>
;; Version: 1.0
;; Keywords: emacs-config
;; URL: https://gitlab.com/aaronlbrink

;;; Commentary:
;; I still have no idea about some of this stuff.

;;; Code:
(provide 'server)

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

;;
;; --- Theming ---
;;

;; See all.el to implement theming on server.


;; Don't show welcome screen
(setq initial-scratch-message "Hi, Aaron!")

(provide 'server)
;;; all.el ends here
