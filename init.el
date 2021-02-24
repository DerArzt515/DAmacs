(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)
 
(setq visible-bell t)

(set-face-attribute 'default nil :font "JetBrains Mono Medium" :height 120)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Init use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-i-jump nil)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil));; Don't start searches with ^

(use-package ivy
  :diminish ;; hide from mode line
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-l" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-k" . ivy-previous-line)
	 ("C-l" . ivy-done)
	 ("C-d" . ivy-switch-buffer-kill)
	 :map ivy-reverse-i-search-map
	 ("C-k" . ivy-previous-line)
	 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))	 

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))
  ;; :ensure t
  ;; :config)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package all-the-icons)

;;ui
(column-number-mode)
(global-display-line-numbers-mode t)
;;Disable line num for some mode
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package afternoon-theme
  :ensure t)
(use-package flatland-theme
  :ensure t)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package doom-themes)

(use-package general
    :config
    (general-create-definer my/leader-keys
	:keymaps '(normal insert visual emacs)
	:prefix "SPC"
	:global-prefix "C-SPC")
    (general-create-definer my/lsp-keys
      :keymaps '(normal insert emacs)
      :prefix "SPC")

    (my/leader-keys
     "t" '(:ignore t :which-key "toggles")
     "tt" '(counsel-load-theme :which-key "choose theme")
     "f" '(:ignore t :which-key "files")
     "ff" '(counsel-find-file :which-key "find file")
     "fp" '((lambda () (interactive) (find-file "~/.emacs.d/init.el")) :which-key "open config")
     "fs" '(evil-save :which-key "save")
     "w" '(:ignroe t :which-key "window")
     "ws" '(evil-window-split :which-key "split -")
     "wv" '(split-window-right :which-key "split |")
     "wl" '(evil-window-right :which-key "go ->")
     "wh" '(evil-window-left :which-key "go <-")
     "wk" '(evil-window-up :which-key "go up")
     "wj" '(evil-window-down :which-key "go down")
     "b" '(:ignore t :which-key "buffers")
     "bb" '(counsel-switch-buffer :which-key "switch to buffer")
     "bp" '(previous-buffer :which-key "previous")
     "bn" '(next-buffer :which-key "next")
     "bk" 'kill-buffer
     "s" '(:ignore t :which-key "search")
     "ss" '(swiper :which-key "current buffer")
     "p" '(:keymap projectile-command-map :package projectile)
     "m" '((lambda ()(interactive) (general-simulate-key "C-,")) :which-key "minor mode")
     ":" 'counsel-M-x
     ))
 
(general-create-definer my-lisp/leader-keys
  :keymaps '(lisp)
  :prefix "m"
  :global-prefix "SPC-m")

(my-lisp/leader-keys
 "mc" '(check-parens :which-key "check-parens"))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :init
  (when (file-directory-p "~/repos/")
    (setq projectile-project-search-path '("~/repos/")))
  ;;(setq projectile-switch-project-action #'projectile-dired))
  )

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  
  (my/leader-keys
    "g" '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "status")))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)"))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package dash
  :ensure t)
(use-package f
  :ensure t)
(use-package s
  :ensure t)
(use-package emacsql
  :ensure t)
(use-package emacsql-sqlite3
  :ensure t)
(use-package org-roam
  :config
  (setq org-roam-directory "~/org/roam")
  (add-hook 'after-init-hook 'org-roam-mode)

  (my/leader-keys
    "n" '(:ignore t :which-key "notes")
    "nr" '(:ignore t :which-key "roam")
    "nrf" '(org-roam-find-file :which-key "find file")
    "nrI" '(org-roam-insert-immediate :which-key "insert Immediate")
    "nri" '(org-roam-insert :which-key "insert")
    "nrr" '(org-roam :which-key "roam")
    )
  )
  

;(use-package visual-fill-column
;  :hook (org-mode . efs/org-mode-visual-fill))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((emacs-lisp . t)
    (python . t)))

(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory ))))
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)
(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-,")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)

  :config
  ;; Set up Node debugging
  (require 'dap-node)
  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; Lang python
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

;;(use-package eglot
;;  :ensure t
;;  :hook ((python-mode . eglot-ensure))
;;  :config
;;  (add-to-list 'eglot-server-programs '(python-mode . ("pyright"))))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package company
  :after lsp-mode
  :hook ((lsp-mode . company-mode)
	 (prog-mode . company-mode))
  :bind (:map company-active-map
              ("TAB" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))



  
(use-package company-box
  :hook (company-mode . company-box-mode))


(load-theme 'doom-one t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(custom-safe-themes
   '("8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "eb122e1df607ee9364c2dfb118ae4715a49f1a9e070b9d2eb033f1cefd50a908" default))
 '(package-selected-packages
   '(python-mode evil-magit general doom-themes helpful ivy-rich which-key flatland-theme afternoon-theme rainbow-delimiters doom-modeline counsel consel ivy evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
