;;;###autoload
(define-minor-mode marc-mode
  "Support delightful pair programming with Marc."
  :lighter " marc"
  :global 't
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-h") 'delete-backward-char)
            (define-key map (kbd "C-m") 'paredit-newline)
            (define-key map (kbd "C-j") 'newline)
            map))

(provide 'marc-mode)
