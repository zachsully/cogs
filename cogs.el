;; Cogs Mode
(define-derived-mode cogs-lang-mode fundamental-mode
  (setq font-lock-defaults
	'((("(-#.*#-)" . font-lock-preprocessor-face)
	   ("(-.*-)" . font-lock-comment-face))))
  (setq-local comment-start "(-")
  (setq-local comment-end   "-)")
  (setq mode-name "Cogs")
  (set-input-method 'TeX))

(autoload 'cogs-lang-mode "Cogs" nil t)
(add-to-list 'auto-mode-alist '("\\.cs$" . cogs-lang-mode))
