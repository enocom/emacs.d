
;;; fill-column-marker.el --- A visual fill column indicator.

;; fill-column-marker combines column-marker and fill-column-indicator features
;; into a single package to provide a visual fill-column indicator both for
;; lines that cross the threshold and those that don't.

(defvar fill-column-marker-colour "dimgray"
  "The colour that is used for the column marker.")

(defun special-buffer-name-p (buffer-name)
  "A helper function to determine whether a buffer is a so-called
  \"special buffer\"; that is: a control buffer or one for which
  the content is not directly managed by the user."
  (let ((special-marker "*"))
    (string-prefix-p special-marker buffer-name)))

;; Configure fill-column-indicator for the thin vertical line that is drawn in
;; empty columns.

(require 'fill-column-indicator)
(setq fci-rule-color fill-column-marker-colour)

;; Configure a custom column-marker for the "character highlight" that is
;; applied to the columns that contain text that exceeds the fill-column width.

(require 'column-marker)
(defface fill-column-marker `((t (:background ,fill-column-marker-colour)))
  "Face used for the fill column marker.  Usually a background color."
  :group 'faces)

(defvar fill-column-marker-face 'fill-column-marker
    "Face used for a fill column marker.  Usually a background color.
Changing this directly affects only new markers.")

(column-marker-create fill-column-marker fill-column-marker-face)

;; Define our minor mode.

(define-minor-mode fcm-mode
  "A minor mode to toggle the fill column marker."
  :lighter nil
  (if fcm-mode
      (progn
        (fci-mode 1)
        (fill-column-marker fill-column))
    (progn
      (fci-mode 0)
      (let ((current-prefix-arg '-))
        (call-interactively 'fill-column-marker)))))

;; Define a global mode so that users don't have to enable it using major mode
;; hooks.  We do, however, not enable it for special buffers.

(define-globalized-minor-mode
  global-fcm-mode
  fcm-mode
  (lambda ()
    (when (not (or (special-buffer-name-p (buffer-name))
                   (string-prefix-p "magit" (buffer-name))))
        (fcm-mode 1))))

(provide 'fill-column-marker)