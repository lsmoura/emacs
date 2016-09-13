(package-initialize)

;; Define constants
(defconst +home-dir+ "~")
(defconst +emacs-dir+ (concat +home-dir+ "/emacs"))
(defconst +emacs-lib-dir+ (concat +emacs-dir+ "/plugins"))
(defconst +emacs-snippets-dir+ (concat +emacs-dir+ "/snippets"))
(defconst +emacs-backup-dir+ (concat +emacs-dir+ "/backup"))

(custom-set-variables
 '(tool-bar-mode nil)
 '(menu-bar-mode nil))

(custom-set-faces
 )

(setq-default indent-tabs-mode nil) ;; Indent using only spaces.

;; Backups
; Reference: https://www.emacswiki.org/emacs/ForceBackups
(setq vc-make-backup-files t)
(setq version-control     t  ;; Use version numbers for backups.
      kept-new-versions   10 ;; Number of newest versions to keep.
      kept-old-versions   0  ;; Number of oldest versions to keep.
      delete-old-versions t  ;; Don't ask to delete excess backup versions.
      backup-by-copying   t) ;; Copy all files, don't rename them.

; Backup save location
(setq backup-directory-alist `(("" . ,(concat +emacs-backup-dir+ "/per-save"))))
(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist `(("" . ,(concat +emacs-backup-dir+ "/per-session"))))
	  (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; Org-mode
(add-to-list 'load-path (concat +emacs-lib-dir+ "/org-mode/lisp"))
(require 'org)

;; Move lines
(add-to-list 'load-path +emacs-lib-dir+)
(require 'move-line)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; Multiple-cursors
(add-to-list 'load-path (concat +emacs-lib-dir+ "/multiple-cursors.el"))
(require 'multiple-cursors)

; When you have an active region that spans multiple lines, the following will add a cursor to each line
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-next-word-like-this)

;; Javascript shaningans
(require 'comint);
;(defconst +node-cmd+ "node")
(defconst +node-cmd+ "/usr/local/bin/node")
(defun run-js ()
  "Execute the current buffer in node"
  (interactive)
  (pop-to-buffer (make-comint "run-js" +node-cmd+ nil "--interactive" buffer-file-name)))

;; Theme
(add-to-list 'custom-theme-load-path (concat +emacs-dir+ "/themes"))
(load-theme 'tangotango t)

;; auto-complete
(add-to-list 'load-path (concat +emacs-lib-dir+ "/auto-complete"))
(add-to-list 'load-path (concat +emacs-lib-dir+ "/popup-el"))
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat +emacs-lib-dir+ "/autocomplete/dict"))
(require 'auto-complete-config)
(ac-config-default)

;; yasnippet
(add-to-list 'load-path (concat +emacs-lib-dir+ "/yasnippet"))
(require 'yasnippet)
(setq yas-snippet-dirs '(+emacs-snippets-dir+))
(yas-global-mode 1)

;; js2-mode
(add-to-list 'load-path (concat +emacs-lib-dir+ "/js2-mode"))
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; ---
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
	      (0 (progn (compose-region (match-beginning 1)
					(match-end 1) "\u0192")
			nil)))))
