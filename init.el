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