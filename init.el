(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(tool-bar-mode -1)
(setq column-number-mode t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq size-indication-mode t)
(set-locale-environment "de_DE.utf-8")
(set-face-background 'default "black")
(set-face-foreground 'default "white")
(set-face-background 'cursor "green")
(set-face-foreground 'ediff-fine-diff-A "black")
(set-face-foreground 'ediff-fine-diff-Ancestor "black")
(set-face-foreground 'ediff-current-diff-C "black")
(set-face-foreground 'ediff-fine-diff-B "black")
(set-face-foreground 'ediff-fine-diff-C "black")
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("melpa" . "https://melpa.org/packages/")))
(setq use-package-always-ensure t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package gnu-elpa-keyring-update
  :config  (setq package-check-signature 'allow-unsigned))
(use-package which-key
  :config  (which-key-mode))
(use-package counsel
  :bind
  ("C-s" . swiper-isearch)
  ("C-x b" . counsel-switch-buffer)
  :config (counsel-mode))
(use-package ivy-rich
  :config (ivy-rich-mode))
(use-package helpful
  :commands
  (helpful-callable
   helpful-variable
   helpful-command
   helpful-key
   helpful-at-point)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ("C-h C-x d" . helpful-at-point))
(use-package projectile
  :config (projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map))
(use-package counsel-projectile
  :config (counsel-projectile-mode))
(use-package magit)
(use-package org
  :custom
  (org-agenda-files "~/.emacs.d/org-agenda-files")
  (org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)))
  :config
  (let ((filename org-agenda-files))
    (if (not (file-exists-p filename))
	(make-empty-file filename)))
  :hook (org-mode . visual-line-mode))
(use-package csv-mode)
(use-package yaml-mode)
