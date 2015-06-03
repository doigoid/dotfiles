;; use $PATH
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

;; this function is unavailable in Emacs 24, support legacy calls to this
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))

(defun doimports ()

  ;; commenting this out for a faster startup
  ;; (require 'magit-gh-pulls)
  ;; (require 'color-theme)
  ;; (require 'php-mode)
  ;; (require 'coffee-mode)
  (package-initialize)
  (require 'column-marker)
  (require 'magit)
  (require 'sass-mode)
  (require 'smart-tab)
  (require 'uniquify)
  (require 'python)
  (require 'git-gutter)
  (require 'pyflakes)
  (require 'pymacs)
  (require 'auto-complete)
  (require 'auto-complete-config)
)

(defun installimports ()
  (setq package-archives '(
    ("gnu" . "http://elpa.gnu.org/packages/")
    ("marmalade" . "http://marmalade-repo.org/packages/")
    ("melpa" . "http://melpa.milkbox.net/packages/")))
  (package-initialize)
  (package-refresh-contents)
  ;; external packages (installed via elpa)
  (when (not (require 'php-mode nil t))
    (package-install 'php-mode))
  (when (not (require 'coffee-mode nil t))
    (package-install 'coffee-mode))
  (when (not (require 'column-marker nil t))
    (package-install 'column-marker))
  (when (not (require 'magit nil t))
    (package-install 'magit))
  (when (not (require 'color-theme nil t))
    (package-install 'color-theme))
  (when (not (require 'sass-mode nil t))
    (package-install 'sass-mode))
  (when (not (require 'smart-tab nil t))
    (package-install 'smart-tab))
  (when (not (require 'uniquify nil t))
    (package-install 'uniquify))
  (when (not (require 'python nil t))
    (package-install 'python))
  (when (not (require 'git-gutter nil t))
    (package-install 'git-gutter))
  (when (not (require 'go-mode nil t))
    (package-install 'go-mode))
  (when (not (require 'pyflakes nil t))
    (package-install 'pyflakes))
  (when (not (require 'pymacs nil t))
    (package-install 'pymacs))
  (when (not (require 'auto-complete nil t))
    (package-install 'auto-complete))
  (when (not (require 'emacs-powerline nil t))
    (package-install 'emacs-powerline))
)

;; (if (featurep 'magit)
;;     (doimports)
;;     (installimports))

(doimports)

(ac-config-default)
(global-auto-complete-mode t)

(when (display-graphic-p)
  (set-exec-path-from-shell-PATH)
  (toggle-scroll-bar -1)
  (color-theme-initialize)
  (color-theme-charcoal-black))

;; window configuration
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq
 scroll-step 1 scroll-conservatively 10000
 line-number-mode t
 column-number-mode t
 vc-follow-symlinks t
 kill-whole-line t)

(fset 'yes-or-no-p 'y-or-n-p)
(setq ring-bell-function 'ignore)

;; buffer settings
(global-auto-revert-mode t)
(global-git-gutter-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(set-default 'fill-column 80)

;; xemacs specific
(when (display-graphic-p)
 ;;(color-theme-charcoal-black)
 (set-cursor-color "#66FF66"))

;; have ido ignore certain file and directory types
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-ignore-buffers
      '("\\` " "^\\*ESS\\*" "^\\*Messages\\*" "^\\*Help\\*" "^\\*Buffer"
		"^\\*.*Completions\\*$" "^\\*Ediff" "^\\*tramp" "^\\*cvs-"
		"_region_" " output\\*$" "^TAGS$" "^\*Ido")
      ido-ignore-directories
      '("\\`.svn/" "\\.git/" "\\`CVS/" "\\`\\.\\./" "\\`\\./")
      ido-ignore-files
      '("\\.pyc/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./"))
(define-key ido-file-dir-completion-map
  [remap set-mark-command]  'ido-restrict-to-matches)
(setq uniquify-buffer-name-style 'forward)

;; auto mode configurations
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq temporary-file-directory "~/.emacs.d/tmp/")

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info
(setq transient-mark-mode t)

;; smart-tabs-advice function
(defmacro smart-tabs-advice (function offset)
  `(progn
     (defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
       (cond
        (indent-tabs-mode
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let ((tab-width fill-column)
               (,offset fill-column)
               (wstart (window-start)))
           (unwind-protect
               (progn ad-do-it)
             (set-window-start (selected-window) wstart))))
        (t
         ad-do-it)))))

;; highlight column 80 on python files
(add-hook 'python-mode-hook
          (lambda () (interactive) (column-marker-1 80)))

;; Kills all them buffers except scratch
;; optained from http://www.chrislott.org/geek/emacs/dotemacs.html
(defun nuke-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (x) (kill-buffer x))
		  (buffer-list))
  (delete-other-windows))

(defun indent-or-expand (arg)
  "Either indent according to mode, or expand the word preceding point."
  (interactive "*P")
  (if (and
       (or (bobp) (= ?w (char-syntax (char-before))))
       (or (eobp) (not (= ?w (char-syntax (char-after))))))
      (dabbrev-expand arg)
    (indent-according-to-mode)))

(defun my-tab-fix ()
  (local-set-key [tab] 'indent-or-expand))

(add-hook 'c-mode-hook          'my-tab-fix)
(add-hook 'sh-mode-hook         'my-tab-fix)
(add-hook 'emacs-lisp-mode-hook 'my-tab-fix)
(add-hook 'python-mode-hook     'my-tab-fix)
(add-hook 'html-mode-hook       'my-tab-fix)
(add-hook 'js-mode-hook         'my-tab-fix)
(add-hook 'sass-mode-hook       'my-tab-fix)

(defvar maxframe-maximized-p nil "maxframe is in fullscreen mode")
(defun toggle-maxframe ()
  "Toggle maximized frame"
  (interactive)
  (setq maxframe-maximized-p (not maxframe-maximized-p))
  (cond (maxframe-maximized-p (maximize-frame))
        (t (restore-frame))))
(define-key global-map [(C-return)] 'ns-toggle-fullscreen)

;; key bindings
(global-set-key (kbd "C-x m") 'magit-status)
(global-set-key (kbd "C-x a") 'align-regexp)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C-x K") 'nuke-all-buffers)
(global-set-key (kbd "C-x E") (lambda () (interactive) (find-file "~/.emacs")))
(global-set-key (kbd "C-x P") (lambda () (interactive) (insert "import ipdb; ipdb.set_trace()")))
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-x \/") 'comment-region)
(global-set-key (kbd "C-x \\") 'uncomment-region)
(global-set-key (kbd "C-x +") 'text-scale-increase)
(global-set-key (kbd "C-x \-") 'text-scale-decrease)
(global-set-key (kbd "C-x 9") 'balance-windows)
(global-set-key (kbd "C-x z") 'zencoding-expand-line)
(global-set-key (kbd "C-x F") 'ns-open-file-using-panel)

;;(set-variable 'magit-emacsclient-executable "/usr/local/bin/emacs")

(defun magit-strip-orgin-from-branch-name
  " Force magit to use the branch name from the remote. "
  (remote branch)
  (concat "" branch))

(setq magit-default-tracking-name-function
      'magit-strip-orgin-from-branch-name)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; python specific configurations
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-force-py-shell-name-p t)

; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
(setq py-smart-indentation t)

;; (set-default-font "Inconsolata-14")
