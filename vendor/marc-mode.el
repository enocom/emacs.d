;;;###autoload
(define-minor-mode marc-mode
  "Support delightful pair programming with Marc."
  :lighter " marc"
  :global 't
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-h") 'delete-backward-char)
            map))

(provide 'marc-mode)
