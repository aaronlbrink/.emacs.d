;;; init.el -- aaron's emacs config

;; Author: Aaron Brink <aaronlbrink@gmail.com>
;; Version: 1.0
;; Keywords: emacs-config
;; URL: https://gitlab.com/aaronlbrink

;;; Commentary:
;; I still have no idea about some of this stuff.

;;; Code:
(prefer-coding-system 'utf-8)


;; TODO: Some package-specific configuration exists outside of their (use-package ...)
;;       should/can some of these configurations be placed inside their (use-package )?

;;
;; --- Emacs-specific Customizations (excluding theming) ---
;;
;; Use spaces instead of tabs by default
(setq-default indent-tabs-mode nil)

;; Save auto-save files and auto-backup files to a custom directory
(setq auto-save-file-name-transforms
     `((".*" "~/.emacs-saves/" t)))
(setq backup-directory-alist
     '(("." . "~/.emacs-saves/")))


;;
;; --- Initialize packages and package archives ---
;;

;; Run package's auto-loads
(package-initialize)
;; Boilerplate for packages, I think?
(require 'package)
;; Use ELPA and MELPA, the Emacs Lisp Package Archive, and Milkypostman's Archive respectively
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
    ("melpa" . "https://melpa.org/packages/")
    ("org" . "https://orgmode.org/elpa/")))

;; Ensure we have use-package package installed
;; https://github.com/jwiegley/use-package
;; use-package is a macro which allows isolating package configuration in the config.
;; Syntax like :mode, :bind, :ensure, :hoook, etc, come from this package, so use the
;; documentation at the link above when editing a package configuration below
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)


(add-to-list 'load-path "~/.emacs.d/custom/")
;; (require 'all)
(require 'server) ;; use this instead of require all for a nice server config

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(nginx-mode web-mode use-package tide smex projectile popwin org-plus-contrib monokai-theme lsp-ui idle-org-agenda flycheck-inline evil editorconfig direx counsel company-lsp cherry-blossom-theme auto-org-md)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
