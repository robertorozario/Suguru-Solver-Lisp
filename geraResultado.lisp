(defun compMatrix(matriz)
  (if (null matriz)
    0
    (+ 1 (compMatrix (cdr matriz)))))

(defun insertList(l x v)
  (setf (nth x l) v)
  l)

(defun insertMatrix(m x y v)
  (setf (nth x m) (insertList (nth x m) y v))
  m)

(defun geraResultado (mp listaP x y)

    (if (null listaP)
        (return-from geraResultado mp)
        (if (and (= 0 (nth y (nth x mp))) (= y (- (compMatrix mp) 1)))
            (return-from geraResultado (geraResultado (insertMatrix mp x y (car listaP)) (cdr listaP) (+ x 1) 0))
            (if (= 0 (nth y (nth x mp)))
                 (return-from geraResultado (geraResultado (insertMatrix mp x y (car listaP)) (cdr listaP) x (+  y 1)))
                 (if (and (/= 0 (nth y (nth x mp))) (= y (- (compMatrix mp) 1)))
                    (return-from geraResultado (geraResultado mp listaP (+ x 1) 0))
                    (return-from geraResultado (geraResultado mp listaP x (+ y 1)))
                 )
            
            )
        )
    )
    
)


(print (geraResultado (list '(1 1 0 2 0 5 5)
          '(1 1 3 4 4 6 5)
          '(1 1 4 4 0 6 6)
          '(7 4 4 8 8 9 6)
          '(7 7 8 8 9 9 12)
          '(10 10 10 9 9 9 12)
          '(11 10 10 12 12 12 12)
       ) (list 9 3 6) 0 0))