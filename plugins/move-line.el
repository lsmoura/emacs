(defun move-line (n)
  "Move the current line up or down by N lines. Adapted from https://www.emacswiki.org/emacs/MoveLine"
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)
    (indent-for-tab-command)))

(defun move-line-up (n)
  "Move the line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(provide 'move-line)
;;; end of move-line.el
