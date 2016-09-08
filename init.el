(package-initialize)

;; Define constants
(defconst +home-dir+ "~")
(defconst +emacs-dir+ (concat +home-dir+ "/emacs"))
(defconst +emacs-lib-dir+ (concat +emacs-dir+ "/plugins"))
(defconst +emacs-snippets-dir+ (concat +emacs-dir+ "/snippets"))

(custom-set-variables
 '(tool-bar-mode nil)
 '(menu-bar-mode nil))

(custom-set-faces
 )

(add-to-list 'load-path +emacs-lib-dir+)
(require 'move-line)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

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


;; ---
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *("
	      (0 (progn (compose-region (match-beginning 1)
					(match-end 1) "\u0192")
			nil)))))
