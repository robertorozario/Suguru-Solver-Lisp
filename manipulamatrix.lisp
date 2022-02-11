(defun insertList(l x v)
  (setf (nth x l) v)
  l)

(defun insertMatrix(m x y v)
  (setf (nth x m) (insertList (nth x m) y v))
  m)
  
