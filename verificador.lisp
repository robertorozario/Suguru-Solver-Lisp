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
