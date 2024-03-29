;Contador para o tamArea
(defun counter(lista n)
    (if (null lista)
        0
        (if (= n (car lista))
            (+ (counter (cdr lista) n) 1)
            (counter (cdr lista) n)
        )
    )
)

;Mede o tamanho de uma área
(defun tamArea(lista n)
    (if (null lista)
        0
        (+ (counter (car lista) n) (tamArea (cdr lista) n))
    )
)

;Tamanho da Matriz (Já que é matriz quadrada, também da o tamanho das linhas)
(defun compMatrix(matriz)
  (if (null matriz)
    0
    (+ 1 (compMatrix (cdr matriz)))))

;Insere elemento em uma linha da matriz
(defun insertList(l x v)
  (setf (nth x l) v)
  l)

;Insere a Linha na matriz
(defun insertMatrix(m x y v)
  (setf (nth x m) (insertList (nth x m) y v))
  m)

#|
Procura o primeiro espaço não preenchido antes da posição dada, usada no backtrack
para voltar atrás quando todas as possibilidades de valor na posição atual foram
inválidas, se chegar a posição (0, 0), devolve uma posição negativa para sinalizar
que o tabuleiro não tem solução
|#
(defun espacoembranco(m x y)
    (if (and (= x 0) (= y 0))
        (return-from espacoembranco (list -1 -1))
        (if (= y 0)
            (if (= 0 (nth (- (compMatrix m) 1) (nth (- x 1)  m)))
                (return-from espacoembranco (list (- x 1) (- (compMatrix m) 1)))
                (return-from espacoembranco  (espacoembranco m (- x 1) (- (compMatrix m) 1)))
            )
            (if (= x 0)
                (if (= 0 (nth (- y 1) (nth 0 m)))
                    (return-from espacoembranco (list 0 (- y 1)))
                    (return-from espacoembranco  (espacoembranco m 0 (- y 1)))
                )
                (if (= 0 (nth (- y 1) (nth x m)))
                    (return-from espacoembranco (list x (- y 1)))
                    (return-from espacoembranco  (espacoembranco m x (- y 1)))
                )
            )
        )
    )
   
)

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

;Junta a pilha de resultados a matriz principal para dar o resultado final
(defun geraResultado (mp listaP x y)

    (if (or (> x (- (compMatrix mp) 1)) (null listaP))
        (return-from geraResultado mp)
        (if (and (>= y (- (compMatrix mp) 1)) (= 0 (nth y (nth x mp))))
            (return-from geraResultado (geraResultado (insertMatrix mp x y (car listaP)) (cdr listaP) (+ x 1) 0))
            (if (= 0 (nth y (nth x mp)))
                 (return-from geraResultado (geraResultado (insertMatrix mp x y (car listaP)) (cdr listaP) x (+  y 1)))
                 (if (and (>= y (- (compMatrix mp) 1)) (/= 0 (nth y (nth x mp))))
                    (return-from geraResultado (geraResultado mp listaP (+ x 1) 0))
                    (return-from geraResultado (geraResultado mp listaP x (+ y 1)))
                 )
            )
        )
    )
    
)

#|
Algoritmo de backtracking, faz uma sequência de testes que buscam encontrar os
valores corretos de cada espaço em branco e adiciona-los a uma pilha solução,
faz tentativa e erro em cada espaço em branco, se encontra um espaço que não pode
ser preenchido por nenhum valor, o resultado no topo da pilha solução é desempilhado
e o código volta a fazer testes na posição anterior usando o valor desempilhado + 1
como ponto de partida
|#
(defun backtrack (v x y mp ma s)
  (if (or (< x 0) (< y 0))
      (return-from backtrack 0)
      (if (and ( > x (- (compMatrix mp) 1))  (> y (- (compMatrix mp) 1)))
          (return-from backtrack (geraResultado mp (reverse s) 0 0))
          (if (/= 0 (nth y (nth x mp)))
            (if (<  (+ y 1) (compMatrix mp))
                (return-from backtrack (backtrack 1 x (+ y 1) mp ma s))
                (if (>= (+ x 1) (compMatrix mp))
                    (return-from backtrack (backtrack 1 (+ x 1) (+ y 1) mp ma s))
                    (return-from backtrack (backtrack 1 (+ x 1) 0 mp ma s))))
            (if (> v (tamArea ma (nth y (nth x ma))))
              (return-from backtrack (backtrack (+ (car s) 1) (car (espacoembranco mp x y)) (cdr (espacoembranco mp x y)) mp ma (cdr s)))
              (if (verify (geraResultado mp (reverse s) 0 0) ma v x y)
                (if ( < (+ y 1) (compMatrix mp))
                    (return-from backtrack (backtrack 1 x (+ y 1) mp ma (push v s)))
                    (if (>= (+ x 1) (compMatrix mp))
                      (return-from backtrack (backtrack 1 (+ x 1) (+ y 1) mp ma (push v s)))
                      (return-from backtrack (backtrack 1 (+ x 1) 0 mp ma (push v s)))
                    )
                )
                (return-from backtrack (backtrack (+ v 1) x y mp ma s))))))))

(setf s(list NIL))
(setf ma(list '(1 1 2 2 2 5 5)
            '(1 1 3 4 4 6 5)
            '(1 1 4 4 8 6 6)
            '(7 4 4 8 8 9 6)
            '(7 7 8 8 9 9 12)
            '(10 10 10 9 9 9 12)
            '(11 10 10 12 12 12 12)
        )
)
(setf mp(list '(3 0 0 3 1 2 0)
               '(0 6 0 4 0 0 3)
               '(2 0 0 0 3 2 0)
               '(0 5 0 0 0 0 0)
               '(3 0 4 0 0 4 0)
               '(0 0 1 5 1 0 1)
               '(1 0 0 2 0 3 4)
            )
)

(print (verify mp ma 6 0 2))
(print (backtrack 1 0 0 mp ma NIL))


