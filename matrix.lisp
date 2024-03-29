(setf mp(list '(3 0 0 3 1 2 0)
               '(0 6 0 4 0 0 3)
               '(2 0 0 0 3 2 0)
               '(0 5 0 0 0 0 0)
               '(3 0 4 0 0 4 0)
               '(0 0 1 5 1 0 1)
               '(1 0 0 2 0 3 4)
          )
)

(setf ma(list '(1 1 2 2 2 5 5)
          '(1 1 3 4 4 6 5)
          '(1 1 4 4 8 6 6)
          '(7 4 4 8 8 9 6)
          '(7 7 8 8 9 9 12)
          '(10 10 10 9 9 9 12)
          '(11 10 10 12 12 12 12)
       )
)

(defun counter(lista n)
    (if (null lista)
        0
        (if (= n (car lista))
            (+ (counter (cdr lista) n) 1)
            (counter (cdr lista) n)
        )
    )
)

(defun tamArea(lista n)
    (if (null lista)
        0
        (+ (counter (car lista) n) (tamArea (cdr lista) n))
    )
)
