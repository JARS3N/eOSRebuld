(defun print-condition-hook (condition hook)
  "Print this error message (condition) and abort the currenct operation."
  (declare (ignore hook))
  (princ condition)
  (clear-input)
  (abort))
*debugger-hook*
(setf *debugger-hook* #'print-condition-hook)
