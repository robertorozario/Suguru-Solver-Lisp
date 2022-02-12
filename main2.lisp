(defun counter(l x)
  (if (null l)
    0
    (if (= x (car l))
      (+ 1 (counter (cdr l) x))
      (+ 0 (counter (cdr l) x)))))

(defun tamArea (m x)
  (if (null m)
    0
    (+ (counter (car m) x) (tamArea (cdr m) x))))

(defun insertList(l x v)
  (setf (nth x l) v)
  l)

(defun insertMatrix(m x y v)
  (setf (nth x m) (insertList (nth x m) y v))
  m)

;Tamanho da Matriz (Já que é matriz quadrada, também da o tamanho das linhas)
(defun compMatrix(matriz)
  (if (null matriz)
    0
    (+ 1 (compMatrix (cdr matriz)))))

;Procura elementos iguais na mesma área
(defun comparaArea(mP mA x y a v)
  (if (and (and (and (= a (nth y (nth x mA))) (/= v (nth y (nth x mP)))) (= y (- (compMatrix mP) 1))) (= x (- (compMatrix mP) 1)))
    T
    (if (and (and (= a (nth y (nth x mA))) (/= v (nth y (nth x mP)))) (= y (- (compMatrix mP) 1)))
      (comparaArea mP mA (+ x 1) 0 a v)
      (if (and (= a (nth y (nth x mA))) (/= v (nth y (nth x mP))))
        (comparaArea mP mA x (+ y 1) a v)
        (if (and (and (/= a (nth y (nth x mA))) (= y (- (compMatrix mP) 1))) (= x (- (compMatrix mP) 1)))
          T
          (if (and (/= a (nth y (nth x mA))) (= y (- (compMatrix mP) 1)))
            (comparaArea mP mA (+ x 1) 0 a v)
            (if (/= a (nth y (nth x mA)))
              (comparaArea mP mA x (+ y 1) a v)
              NIL)))))))

;Procura elementos iguais na diagonal
(defun diagonal(m x y v)
  (if (and (= x 0) (= y 0))
    (/= v (nth (+ y 1) (nth (+ x 1) m)))
    (if (and (= x 0) (= y (- (compMatrix m) 1)))
      (/= v (nth (- y 1) (nth (+ x 1) m)))
      (if (and (= x (- (compMatrix m) 1)) (= y 0))
        (/= v (nth (+ y 1) (nth (- x 1) m)))
        (if (and (= x 0) (> y 0))
          (and (/= v (nth (+ y 1) (nth (+ x 1) m))) (/= v (nth (- y 1) (nth (+ x 1) m))))
          (if (and (> x 0) (= y 0))
            (and (/= v (nth (+ y 1) (nth (+ x 1) m))) (/= v (nth (+ y 1) (nth (- x 1) m))))
            (if (and (= x (- (compMatrix m) 1)) (= y (- (compMatrix m) 1)))
              (/= v (nth (- y 1) (nth (- x 1) m)))
              (if (and (= x (- (compMatrix m) 1)) (< y (- (compMatrix m) 1)))
                (and (/= v (nth (+ y 1) (nth (- x 1) m))) (/= v (nth (- y 1) (nth (- x 1) m))))
                (if (and (< x (- (compMatrix m) 1)) (= y (- (compMatrix m) 1)))
                  (and (/= v (nth (- y 1) (nth (+ x 1) m))) (/= v (nth (- y 1) (nth (- x 1) m))))
                  (and (/= v (nth (+ y 1) (nth (+ x 1) m))) (and (/= v (nth (+ y 1) (nth (- x 1) m))) (and (/= v (nth (- y 1) (nth (+ x 1) m))) (/= v (nth (- y 1) (nth (- x 1) m)))))))))))))))

;Procura elementos iguais na linha
(defun linha(m x y v)
  (if (= y 0)
    (/= v (nth (+ y 1) (nth x m)))
    (if (= y (- (compMatrix m) 1))
      (/= v (nth (- y 1) (nth x m)))
      (and (/= v (nth (+ y 1) (nth x m))) (/= v (nth (- y 1) (nth x m)))))))

;Procura elementos iguais na coluna
(defun coluna(m x y v)
  (if (= x 0)
    (/= v (nth y (nth (+ x 1) m)))
    (if (= x (- (compMatrix m) 1))
      (/= v (nth y (nth (- x 1) m)))
      (and (/= v (nth y (nth (+ x 1) m))) (/= v (nth y (nth (- x 1) m)))))))

;Procura elementos iguais ao redor do elemento
(defun comparaRedor(m x y v)
  (and (and (linha m x y v) (coluna m x y v)) (diagonal m x y v)))

;Junta todos os testes em uma função
(defun verify(mP mA v x y)
  (and (comparaRedor mP x y v) (comparaArea mP mA 0 0 (nth y (nth x mA)) v)))

(defun espacoembranco (m pos)
  (if (= (nth 0 pos) 0)
    (if (= (nth 1 pos) 0)
      (list (- 0 1) (- 0 1))
      (if (= (nth (- (nth 1 pos) 1) (nth 0 m)) 0)
        (list 0 (- (nth 1 pos) 1))
        (espacoembranco m (list 0 (- (nth 1 pos) 1)))))
    (if (= (nth 1 pos) 0)
      (if (= (nth (- (compMatrix m) 1) (nth (- (nth 0 pos) 1) m)) 0)
        (list (- (nth 0 pos) 1) (- (compMatrix m) 1))
        (espacoembranco m (list (- (nth 0 pos) 1) (- (compMatrix m) 1))))
      (if (= (nth (- (nth 1 pos) 1) (nth (nth 0 pos) m)) 0)
        (list (nth 0 pos) (- (nth 1 pos) 1))
        (espacoembranco m (list (nth 0 pos) (- (nth 1 pos) 1)))))))

(defun geraResultado (mp s pos)
  (if (null s)
    mp
    (if (and (= (nth 1 pos) (- (compMatrix mp) 1)) (= (nth (nth 1 pos) (nth (nth 0 pos) mp)) 0))
      (geraResultado (insertMatrix mp (nth 0 pos) (nth 1 pos) (car s)) (cdr s) (list (+ (nth 0 pos) 1) 0))
      (if (= (nth (nth 1 pos) (nth (nth 0 pos) mp)) 0)
        (geraResultado (insertMatrix mp (nth 0 pos) (nth 1 pos) (car s)) (cdr s) (list (nth 0 pos) (+ (nth 1 pos) 1)))
        (if (and (= (nth 1 pos) (- (compMatrix mp) 1)) (/= (nth (nth 1 pos) (nth (nth 0 pos) mp)) 0))
          (geraResultado mp s (list (+ (nth 0 pos) 1) 0))
          (geraResultado mp s (list (nth 0 pos) (+ (nth 1 pos) 1))))))))

(defun backtrack (v pos mp ma s)
  (if (or (< (nth 0 pos) 0) (< (nth 1 pos) 0))
    NIL
    (if (and (> (nth 0 pos) (- (compMatrix mp) 1)) (> (nth 1 pos) (- (compMatrix mp) 1)))
      (geraResultado mp (reverse s) (list 0 0))
      (if (/= (nth (nth 1 pos) (nth (nth 0 pos) mp)) 0)
        (if (< (+ (nth 1 pos) 1) (compMatrix mp))
          (backtrack 1 (list (nth 0 pos) (+ (nth 1 pos) 1)) mp ma s)
          (if (>= (+ (nth 0 pos) 1) (compMatrix mp))
            (backtrack 1 (list (+ (nth 0 pos) 1) (+ (nth 1 pos) 1)) mp ma s)
            (backtrack 1 (list (+ (nth 0 pos) 1) 0) mp ma s)))
        (if (> v (tamArea ma (nth (nth 1 pos) (nth (nth 0 pos) ma))))
          (backtrack (+ (car s) 1) (espacoembranco mp pos) mp ma (cdr s))
          (if (verify (geraResultado mp (reverse s) (list 0 0)) ma v (nth 0 pos) (nth 1 pos))
            (if (< (+ (nth 1 pos) 1) (compMatrix mp))
              (backtrack 1 (list (nth 0 pos) (+ (nth 1 pos) 1)) mp ma (cons v s))
              (if (>= (+ (nth 0 pos) 1) (compMatrix mp))
                (backtrack 1 (list (+ (nth 0 pos) 1) (+ (nth 1 pos) 1)) mp ma (cons v s))
                (backtrack 1 (list (+ (nth 0 pos) 1) 0) mp ma (cons v s))))
            (backtrack (+ v 1) pos mp ma s)))))))


(defun main()
  (write-line (write-to-string (backtrack 1 (list 0 0) (list '(3 0 0 3 1 2 0) '(0 6 0 4 0 0 3) '(2 0 0 0 3 2 0) '(0 5 0 0 0 0 0) '(3 0 4 0 0 4 0) '(0 0 1 5 1 0 1) '(1 0 0 2 0 3 4)) (list '(1 1 2 2 2 5 5) '(1 1 3 4 4 6 5) '(1 1 4 4 8 6 6) '(7 4 4 8 8 9 6) '(7 7 8 8 9 9 12) '(10 10 10 9 9 9 12) '(11 10 10 12 12 12 12)) ()))))
(main)
