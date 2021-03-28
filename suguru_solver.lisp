(defstruct matriz
    regiao
    entrada
)

(setq matriz5
    (make-matriz
        :regiao (list '(0 1 2 2 2)
                      '(0 1 1 2 2)
                      '(3 1 4 4 4)
                      '(3 3 3 4 4)
                      '(3 5 5 5 5))
        :entrada (list '(0 0 0 0 0)
                       '(0 0 1 0 4)
                       '(0 0 0 0 0)
                       '(4 0 0 0 0)
                       '(0 0 0 0 0))
    )
)

(setq matriz6
    (make-matriz
        :regiao (list '(0 0 0 2 2 2)
                      '(1 1 0 0 2 2)
                      '(3 3 3 4 4 4)
                      '(7 3 6 4 4 5)
                      '(7 7 6 6 5 5)
                      '(7 7 6 6 5 5))
        :entrada (list '(0 5 0 0 0 0)
                       '(0 0 0 0 4 5)
                       '(0 0 0 0 0 0)
                       '(5 1 0 3 0 0)
                       '(0 0 0 0 0 2)
                       '(4 0 2 0 0 0))
    )
)

(setq matriz7
    (make-matriz
        :regiao (list '(0 0 1 1 2 2 2)
                      '(0 0 3 3 3 2 2)
                      '(0 4 4 3 5 6 6)
                      '(7 4 4 5 5 5 6)
                      '(7 7 4 8 5 9 6)
                      '(7 7 8 8 8 9 6)
                      '(10 10 10 8 9 9 9))
        :entrada (list '(1 0 0 0 0 1 0)
                       '(0 0 3 0 0 0 0)
                       '(0 0 0 0 5 0 0)
                       '(3 0 2 0 2 0 2)
                       '(2 0 0 0 0 0 0)
                       '(0 0 5 0 0 5 0)
                       '(1 0 0 0 1 0 0))
    )
)

(setq matriz8
    (make-matriz
        :regiao (list '(0 0 0 3 4 4 5 5)
                      '(0 2 2 3 4 4 5 5)
                      '(2 2 3 3 4 6 6 5)
                      '(2 7 7 3 8 8 6 6)
                      '(10 10 7 7 8 9 9 6)
                      '(11 10 10 7 8 13 9 9)
                      '(11 11 10 12 8 13 13 9)
                      '(11 11 12 12 12 12 13 13))
        :entrada (list '(0 0 0 3 0 0 2 0)
                       '(4 0 0 0 0 0 0 0)
                       '(0 2 0 0 0 0 0 0)
                       '(0 1 5 0 0 1 5 0)
                       '(0 2 0 0 0 0 0 0)
                       '(0 0 0 0 4 0 0 4)
                       '(0 0 0 0 0 3 0 0)
                       '(0 5 0 0 0 5 0 0 ))
    )
)

(defun setaValorNaLinhaDaMatriz(lista value y at)
    (if (null lista)
        (list '())
        (if (= at y)
            (cons value (cdr lista))
            (cons (car lista) (setaValorNaLinhaDaMatriz (cdr lista) value y (+ at 1)))
        )
    )
)

(defun buscaCoordenada(matriz_value value x y at)
    (if (null matriz_value)
        (list '())
        (if (= x at)
            (cons (setaValorNaLinhaDaMatriz (car matriz_value) value y 0) (cdr matriz_value))
            (cons (car matriz_value) (buscaCoordenada (cdr matriz_value) value x y (+ at 1)))
        )
    )
)

(defun retornaElementoDaLinha(lista y at)
    (if (null lista)
        -1
        (if (= y at)
            (car lista)
            (retornaElementoDaLinha (cdr lista) y (+ 1 at))
        )
    )
)

(defun retornaElementoMatriz(matriz_value x y at)
    (if (null matriz_value)
        -1
        (if (= x at)
            (retornaElementoDaLinha (car matriz_value) y 0)
            (retornaElementoMatriz (cdr matriz_value) x y (+ at 1))
        )
    )
)

(defun retornaPesoDaRegiao(matriz_value vetor_de_pesos x y)
    (if (null matriz_value)
        -1
        (if (null vetor_de_pesos)
            -1
            (retornaElementoDaLinha vetor_de_pesos (retornaElementoMatriz matriz_value x y 0) 0)
        )
    )
)

(defun retornaMaiorValorNaLinha(lista maximo)
    (if (null lista)
        maximo
        (if (> (car lista) maximo)
            (retornaMaiorValorNaLinha (cdr lista) (car lista))
            (retornaMaiorValorNaLinha (cdr lista) maximo)
        )
    )
)

(defun retornaMaiorValorNaMatriz(matriz_value maximo)
    (if (null matriz_value)
        maximo
        (if (> (retornaMaiorValorNaLinha (car matriz_value) 0) maximo)
            (retornaMaiorValorNaMatriz (cdr matriz_value) (retornaMaiorValorNaLinha (car matriz_value) 0))
            (retornaMaiorValorNaMatriz (cdr matriz_value) maximo)
        )
    )
)

(defun retornaQntdRegioes(matriz_value)
    (if (null matriz_value)
        0
        (+ 1 (retornaMaiorValorNaMatriz matriz_value 0))
    )
)

(defun retornaVezesNaLinha(lista value)
    (if (null lista)
        0
        (if (= value (car lista))
            (+ 1 (retornaVezesNaLinha (cdr lista) value))
            (retornaVezesNaLinha (cdr lista) value)
        )
    )
)

(defun retornaVezesNaMatriz(matriz_value value)
    (if (null matriz_value)
        0
        (+ (retornaVezesNaLinha (car matriz_value) value) (retornaVezesNaMatriz (cdr matriz_value) value))
    )
)

(defun defineVetorDePesos(matriz_value region_value qnt_regions)
    (if (= qnt_regions region_value)
        NIL
        (cons (retornaVezesNaMatriz matriz_value region_value) (defineVetorDePesos matriz_value (+ 1 region_value) qnt_regions))
    )
)

(defun retornaVetorDePesos(matriz_value)
    (if (null matriz_value)
        NIL
        (defineVetorDePesos matriz_value 0 (retornaQntdRegioes matriz_value))
    )
)

(defun retornaTamanhoLinha(lista)
    (if (null lista)
        0
        (+ 1 (retornaTamanhoLinha (cdr lista)))
    )
)

(defun retornaTamanhoMatriz(matriz_value)
    (if (null matriz_value)
        0
        (retornaTamanhoLinha (car matriz_value))
    )
)

(defun comparaElementos(matriz_value value x y)
    (if (< x 0)
        NIL
        (if (< y 0)
            NIL
            (if (= x (retornaTamanhoMatriz matriz_value))
                NIL
                (if (= y (retornaTamanhoMatriz matriz_value))
                    NIL
                    (if (= value (retornaElementoMatriz matriz_value x y 0))
                        T
                        NIL
                    )
                )
            )
        )
    )
)

(defun temVizinhos(matriz_value value x y)
    (or (comparaElementos matriz_value value x (- y 1))
    (or (comparaElementos matriz_value value x (+ y 1))
    (or (comparaElementos matriz_value value (- x 1) y)
    (or (comparaElementos matriz_value value (+ x 1) y)
    (or (comparaElementos matriz_value value (- x 1) (- y 1))
    (or (comparaElementos matriz_value value (- x 1) (+ y 1))
    (or (comparaElementos matriz_value value (+ x 1) (- y 1))
    (or (comparaElementos matriz_value value (+ x 1) (+ y 1))
    ))))))))
)

;Testado até aqui

(defun temMesmoValorNaRegiaoLinha(lista1 lista2 region_value data_value)
    (if (and (null lista1) (null lista2))
        NIL
        (if (and (= (car lista2) region_value) (= (car lista1) data_Value))
            T
            (temMesmoValorNaRegiaoLinha (cdr lista1) (cdr lista2) region_value data_value)
        )
    )
)

(defun temMesmoValorNaRegiaoMatriz(matriz1 matriz2 region_value data_value)
    (if (and (null matriz1) (null matriz2))
        NIL
        (if (temMesmoValorNaRegiaoLinha (car matriz1) (car matriz2) region_value data_value)
            T
            (temMesmoValorNaRegiaoMatriz (cdr matriz1) (cdr matriz2) region_value data_value)
        )
    )
)

(defun validaPonto(input_matriz region_matriz x y current_value)
    (if (= (retornaElementoMatriz input_matriz x y 0) 0)
        (if (or (temVizinhos input_matriz current_value x y) (temMesmoValorNaRegiaoMatriz input_matriz region_matriz (retornaElementoMatriz region_matriz x y 0) current_value))
            NIL
            T
        )
        NIL
    )
)

(defun comparaLinha(lista1 lista2)
    (if (and (null lista1) (null lista2))
        T
        (if (= (car lista1) (car lista2))
            (comparaLinha (cdr lista1) (cdr lista2))
            NIL
        )
    )
)

(defun comparaMatriz(matriz1 matriz2)
    (if (and (null matriz1) (null matriz2))
        T
        (if (and (not (null matriz1)) (null matriz2))
            NIL
            (if (and (null matriz1) (not (null matriz2)))
                NIL
                (if (comparaLinha (car matriz1) (car matriz1))
                    (comparaMatriz (cdr matriz1) (cdr matriz2))
                    NIL
                )
            )
        )
    )
)

(defun solverCompare(matriz_value input_matriz new_input_matriz region_matriz region_vector x y  current_value)
    (if (comparaMatriz new_input_matriz matriz_value)
        (solverSuguru input_matriz region_matriz region_vector x y (+ current_value 1))
        matriz_value
    )
)

(defun solverSuguruItemCompareX(input_matriz new_input_matriz region_matriz region_vector x y)
    (solverCompare (solverSuguru new_input_matriz region_matriz region_vector 0 (+ 1 y) 1) input_matriz new_input_matriz region_matriz region_vector x y current_value)
)

(defun solverSuguruItemCompareY(input_matriz new_input_matriz region_matriz region_vector x y)
    (solverCompare (solverSuguru new_input_matriz region_matriz region_vector (+ 1 x) y 1) input_matriz new_input_matriz region_matriz region_vector x y current_value)
)

(defun solverSuguruItem(input_matriz new_input_matriz region_matriz region_vector x y current_value)
    (if (and (= (+ 1 x) (retornaTamanhoMatriz input_matriz)) (= (+ 1 y) (retornaTamanhoMatriz input_matriz)))
        new_input_matriz
        (if (= (+ 1 x) (retornaTamanhoMatriz input_matriz))
            (solverSuguruItemCompareX input_matriz new_input_matriz region_vector x y current_value)
            (solverSuguruItemCompareY input_matriz new_input_matriz region_vector x y current_value)
        )
    )
)

(defun solverSuguruItemOtherwise(input_matriz region_matriz region_vector x y current_value)
    (if (and (= (+ 1 x) (retornaTamanhoMatriz input_matriz)) (= (+ 1 y) (retornaTamanhoMatriz input_matriz)))
        input_matriz
        (if (= (+ 1 x) (retornaTamanhoMatriz input_matriz))
            (solverSuguru input_matriz region_matriz region_vector 0 (+ 1 y) 1)
            (solverSuguru input_matriz region_matriz region_vector (+ 1 x) y 1)
        )
    )
)

(defun solverSuguru(input_matriz region_matriz region_vector x y current_value)
    (if (> current_value (retornaPesoDaRegiao region_matriz region_vector x y))
        input_matriz
        (if (validaPonto input_matriz region_matriz x y current_value)
            (solverSuguruItem input_matriz (buscaCoordenada input_matriz current_value x y 0) region_matriz region_vector x y current_value)
            (if (= 0 (retornaElementoMatriz input_matriz x y 0))
                (solverSuguru region_matriz region_vector x y (+ 1 current_value))
                (solverSuguruItemOtherwise input_matriz region_matriz region_vector x y current_value)
            )
        )
    )
)

(defun solver(matriz_value region_value)
    (solverSuguru matriz_value region_value (retornaVetorDePesos region_value) 0 0 1)
)

(defun main()
    ;(write-line (write-to-string (retornaElementoMatriz (matriz-entrada matriz8) 3 1 0)))
    ;(write-line (write-to-string (retornaPesoDaRegiao (matriz-regiao matriz8) '(2 3 4 5 6 7 8 9)
    ;1 0)))
    ;(write-line (write-to-string (retornaMaiorValorNaLinha '(0 1 2 3 5 3) 0)))
    ;(write-line (write-to-string (retornaMaiorValorNaMatriz (matriz-regiao matriz8) 0)))
    ;(write-line (write-to-string (retornaQntdRegioes (matriz-regiao matriz8))))
    ;(write-line (write-to-string (retornaVezesNaMatriz (matriz-regiao matriz8) 10)))
    ;(write-line (write-to-string (retornaVetorDePesos (matriz-regiao matriz8))))
    ;(write-line (write-to-string (retornaTamanhoMatriz (matriz-regiao matriz8))))
    ;(write-line (write-to-string (temVizinhos (matriz-entrada matriz8) 2 2 2)))
    (write-line (write-to-string (solver (matriz-entrada matriz8) (matriz-regiao matriz8))))
)

(main)
