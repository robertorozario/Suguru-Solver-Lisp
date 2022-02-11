(defun geraResultado (listA listB x y)
    (setf sublist (nth x listA))
    (if (null listB)
        listA
        (if ((and (= y (- 1 compMatrix listA)) (= 0 (nth y sublist))))   
            geraResultado (insertMatrix listA x y (car listB)) (cdr listB) (+ x 1) 0
            (if (= 0 (nth y sublist))
                geraResultado (insertMatrix listA x y (car listB)) (cdr listB) x (+ y 1)
                (if ((and (= y (- 1 compMatrix listA)) (/= 0 (nth y sublist))))
                    geraResultado listA listB (+ x 1) 0
                    geraResultado listA listB x (+ y 1)
                )
            )
        )
    )
)
#|      LINHAS 5 E 9 PRECISAM SER TESTADAS POIS GERA O SEGUINTE AVISO

     SYSTEM::%EXPAND-FORM:
          (AND (= Y (- 1 COMPMATRIX LISTA)) (= 0 (NTH Y SUBLIST))) should be a
          lambda expression

|#
