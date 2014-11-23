; Attempt at writing function to create C declaration in header file for function under cursor

(defun generate-declaration ()
  (interactive)
  (let ((buffName (file-name-base))
	(buffH (concat buffName ".h")))
    
  (message "This buffer is called %s" buffName)))
