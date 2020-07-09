;;; features/base/functions.el -*- lexical-binding: t; -*-

;;;###autoload
(defun rocket/reset-input-method ()
  (interactive)
  (if current-input-method
      (progn
	(deactivate-input-method)
	(message "Input method reset"))
    (message "No current input method")))

;;;###autoload
(defun rocket/set-input-method (input-method &optional interactive)
  (interactive
   (let* ((default (or (car input-method-history) default-input-method)))
     (list (read-input-method-name
            (if default "Select input method (default %s): " "Select input method: ")
            default t)
           t)))
  (activate-input-method input-method)
  (setq default-input-method input-method)
  (when interactive
    (customize-mark-as-set 'default-input-method))
  default-input-method)
