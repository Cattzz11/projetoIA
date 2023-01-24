(defvar *corte-alfa* 0)
(defvar *corte-beta* 0)
(defvar *jogada-pc* nil) 
(defvar *nos-analisados* 0)
(defvar *tempo-despendido* 0)

;; algoritmo alfabeta com cortes alfa-beta
;;   entrada: nó raiz, profundidade máxima, jogador atual
;;   saída: melhor jogada
;;   se nó é folha ou profundidade máxima atingida
;;     retorne avaliação do nó
;;   senão
;;     se jogador atual é MAX
;;       melhor := -infinito
;;       para cada filho de nó
;;         melhor := max(melhor, alfabeta(filho, profundidade-1, MIN))
;;         se melhor >= beta
;;           retorne melhor
;;         alfa := max(alfa, melhor)
;;       retorne melhor
;;     senão
;;       melhor := infinito
;;       para cada filho de nó
;;         melhor := min(melhor, alfabeta(filho, profundidade-1, MAX))  
;;         se melhor <= alfa
;;           retorne melhor
;;         beta := min(beta, melhor)
;;       retorne melhor
;;     fim-se
;;   fim-se
;; fim-algoritmo

(defun alfa-beta (no profundidade-limite peca f-utilidade &optional (alfa -999999) (beta 999999) (tempo-inicial (get-universal-time))(tempo-maximo 5000))
"Função alfa-beta, com cortes. Função que se baseia no max-side e min-side dependendo da profundidade."
(let*(
(peca-a-jogar (if (= (get-no-profundidade no) 0) peca (troca-peca peca)))
(max-mix (verificar-profundidade-jogador no))
(caixas-jogador-1 (get-caixas-jogador-1 no))
(caixas-jogador-2 (get-caixas-jogador-2 no))
(tempo-actual (get-universal-time))
(tempo-gasto (- tempo-actual tempo-inicial))
(tempo-dispendido (setf tempo-despendido tempo-gasto))
(nos-analisados (setf nos-analisados (+ nos-analisados 1)))
)
(if (or (= profundidade-limite (get-no-profundidade no)))
(progn
(setf nos-analisados (+ nos-analisados 1))
(get-no-utilidade no))
(if (eq max-mix 'MAX)
(max-side (sucessores-alfabeta no (operadores) profundidade-limite peca f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo)
(min-side (sucessores-alfabeta no (operadores) profundidade-limite peca f-utilidade caixas-jogador-1 caixas-jogador-2) profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo)))))


(defun sucessores (no operadores peca profundidade-maxima funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(if (null operadores)
nil
(let* ((operador (first operadores))
(estado (get-no-estado no))
(numero-linhas (numero-linhas-tabuleiro estado))
(numero-colunas (numero-colunas-tabuleiro estado))
(lista-linhas-colunas-possiveis (if (eql operador 'inserir-arco-horizontal)
(reverse (lista-combinacoes (+ numero-linhas 1) numero-colunas))
(reverse (lista-combinacoes (+ numero-colunas 1) numero-linhas))))
(profundidade (get-no-profundidade no)))
(if (= profundidade profundidade-maxima)
nil
(append (sucessores-todas-possibilidades no operador peca lista-linhas-colunas-possiveis funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(sucessores no (rest operadores) peca profundidade-maxima funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2))))))

(defun sucessores-alfabeta (no operadores profundidade-limite peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(if (null operadores)
nil
(let* ((operador (first operadores))
(estado (get-no-estado no))
(numero-linhas (numero-linhas-tabuleiro estado))
(numero-colunas (numero-colunas-tabuleiro estado))
(lista-linhas-colunas-possiveis (if (eql operador 'inserir-arco-horizontal)
(reverse (lista-combinacoes (+ numero-linhas 1) numero-colunas))
(reverse (lista-combinacoes (+ numero-colunas 1) numero-linhas))))
(profundidade (get-no-profundidade no)))
(if (= profundidade profundidade-limite)
nil
(append (sucessores-todas-possibilidades-alfabeta no operador peca lista-linhas-colunas-possiveis funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(sucessores-alfabeta no (rest operadores) profundidade-limite peca funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2))))))

(defun sucessores-todas-possibilidades (no operador peca lista-linhas-colunas-possiveis funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(if (null lista-linhas-colunas-possiveis)
nil
(let* ((linha-coluna (first lista-linhas-colunas-possiveis))
(estado (get-no-estado no))
(novo-estado (funcall operador estado linha-coluna peca))
(novo-no (make-no novo-estado (get-no-profundidade no) (funcall funcao-utilidade novo-estado caixas-fechadas-j1 caixas-fechadas-j2))))
(append (list novo-no)
(sucessores-todas-possibilidades no operador peca (rest lista-linhas-colunas-possiveis) funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)))))

(defun sucessores-todas-possibilidades-alfabeta (no operador peca lista-linhas-colunas-possiveis funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)
(if (null lista-linhas-colunas-possiveis)
nil
(let* ((linha-coluna (first lista-linhas-colunas-possiveis))
(estado (get-no-estado no))
(novo-estado (funcall operador estado linha-coluna peca))
(novo-no (make-no novo-estado (get-no-profundidade no) (funcall funcao-utilidade novo-estado caixas-fechadas-j1 caixas-fechadas-j2))))
(append (list novo-no)
(sucessores-todas-possibilidades-alfabeta no operador peca (rest lista-linhas-colunas-possiveis) funcao-utilidade caixas-fechadas-j1 caixas-fechadas-j2)))))

(defun max-side (lista-no profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo)
(if (null lista-no)
nil
(let* ((no (first lista-no))
(estado (get-no-estado no))
(profundidade (get-no-profundidade no))
(utilidade (get-no-utilidade no))
(operadores (operadores-estado estado))
(sucessores (sucessores-alfabeta no operadores profundidade-limite peca-a-jogar f-utilidade (caixas-fechadas-j1 estado) (caixas-fechadas-j2 estado)))
(sucessores-ordenados (sort sucessores (lambda (no1 no2) (> (get-no-utilidade no1) (get-no-utilidade no2))))))
(if (>= (get-no-utilidade no) beta)
(cons no nil)
(let* ((maximo (max-side sucessores-ordenados profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))
(novo-alfa (max alfa (get-no-utilidade (first maximo)))))
(if (>= novo-alfa beta)
(cons no nil)
(append (list no)
(max-side (rest lista-no) profundidade-limite peca-a-jogar f-utilidade novo-alfa beta tempo-inicial tempo-maximo))))))))

(defun min-side (lista-no profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo)
(if (null lista-no)
nil
(let* ((no (first lista-no))
(estado (get-no-estado no))
(profundidade (get-no-profundidade no))
(utilidade (get-no-utilidade no))
(operadores (operadores-estado estado))
(sucessores (sucessores-alfabeta no operadores profundidade-limite peca-a-jogar f-utilidade (caixas-fechadas-j1 estado) (caixas-fechadas-j2 estado)))
(sucessores-ordenados (sort sucessores (lambda (no1 no2) (< (get-no-utilidade no1) (get-no-utilidade no2))))))
(if (<= (get-no-utilidade no) alfa)
(cons no nil)
(let* ((minimo (min-side sucessores-ordenados profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))
(novo-beta (min beta (get-no-utilidade (first minimo)))))
(if (<= novo-beta alfa)
(cons no nil)
(append (list no)
(min-side (rest lista-no) profundidade-limite peca-a-jogar f-utilidade alfa novo-beta tempo-inicial tempo-maximo))))))))


(defun troca-peca(peca)
      (if (equal peca 1) 2 1)
)

(defun alfabeta (estado profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo)
  (let* ((operadores (operadores-estado estado))
       (sucessores (sucessores-alfabeta (make-no estado 0 (funcall f-utilidade estado (caixas-fechadas-j1 estado) (caixas-fechadas-j2 estado))) operadores profundidade-limite peca-a-jogar f-utilidade (caixas-fechadas-j1 estado) (caixas-fechadas-j2 estado)))
       (sucessores-ordenados (sort sucessores (lambda (no1 no2) (> (get-no-utilidade no1) (get-no-utilidade no2))))))
    (if (>= (get-no-utilidade (first sucessores-ordenados)) beta)
      (first sucessores-ordenados)
      (let* ((maximo (max-side sucessores-ordenados profundidade-limite peca-a-jogar f-utilidade alfa beta tempo-inicial tempo-maximo))
           (novo-alfa (max alfa (get-no-utilidade (first maximo)))))
      (if (>= novo-alfa beta)
          (first maximo)
        (min-side (rest maximo) profundidade-limite peca-a-jogar f-utilidade novo-alfa beta tempo-inicial tempo-maximo))))))

(defun verifica-max-sucessor(alfa valor-utilidade sucessor)
      (if (>= valor-utilidade alfa)
            sucessor
      nil)
      )

(defun verifica-min-sucessor(beta valor-utilidade sucessor)
      (if (<= valor-utilidade beta)
            sucessor
      nil)
      )

(defun f-utilidade(no peca old-utilidade caixas-fechadas-j1 caixas-fechadas-j2 old-num-caixas-j1 old-num-caixas-j2)
      (let* ((estado (get-no-estado no))
            (num-caixas-j1 (caixas-fechadas-j1 estado))
            (num-caixas-j2 (caixas-fechadas-j2 estado))
            (utilidade (get-no-utilidade no))
            (profundidade (get-no-profundidade no))
            (valor-utilidade (if (equal peca 1)
                              (+ utilidade (* 10 (- num-caixas-j1 old-num-caixas-j1)) (* 10 (- num-caixas-j2 old-num-caixas-j2)))
                              (+ utilidade (* 10 (- num-caixas-j2 old-num-caixas-j2)) (* 10 (- num-caixas-j1 old-num-caixas-j1))))))
      (if (equal profundidade 0)
            valor-utilidade
      (if (equal peca 1)
            (if (>= valor-utilidade old-utilidade)
                  valor-utilidade
            (- valor-utilidade))
      (if (<= valor-utilidade old-utilidade)
            valor-utilidade
      (- valor-utilidade))))
      )
)

(defun check-prof-player(no)
      (if (equal (get-no-profundidade no) 0)
            (get-no-estado no)
      (check-prof-player (get-no-pai no)))
)

(defun check-caixa-fechou(no num-caixas-prev)
      (let* ((estado (get-no-estado no))
            (num-caixas-j1 (caixas-fechadas-j1 estado))
            (num-caixas-j2 (caixas-fechadas-j2 estado)))
      (if (equal (get-no-profundidade no) 0)
            (if (equal num-caixas-j1 num-caixas-prev)
                  nil
            (list (get-no-estado no) 1))
      (if (equal num-caixas-j1 num-caixas-prev)
            (check-caixa-fechou (get-no-pai no) num-caixas-prev)
      (list (get-no-estado no) 1)))
      )
)

(defun list-combinacoes(max-lines max-cols)
      (let* ((list-lines (list-combinacoes-lines max-lines max-cols))
            (list-cols (list-combinacoes-cols max-lines max-cols)))
      (append list-lines list-cols))
)

(defun list-combinacoes-lines(max-lines max-cols)
      (let* ((list-lines (list-combinacoes-lines-aux max-lines max-cols 1)))
      list-lines)
)

(defun list-combinacoes-lines-aux(max-lines max-cols linha)
      (if (<= linha max-lines)
            (append (list-combinacoes-lines-aux max-lines max-cols (+ linha 1)) (list (list linha 1)))
      nil)
)

(defun list-combinacoes-cols(max-lines max-cols)
      (let* ((list-cols (list-combinacoes-cols-aux max-lines max-cols 1)))
      list-cols)
)

(defun list-combinacoes-cols-aux(max-lines max-cols coluna)
      (if (<= coluna max-cols)
            (append (list-combinacoes-cols-aux max-lines max-cols (+ coluna 1)) (list (list 1 coluna)))
      nil)
)

(defun combinacoes-numero-lista (numero lista) "Devolve uma lista com várias listas compostas pelo elemento recebido e um elemento da lista recebida"
      (if (null lista)
            nil
      (append (list (list numero (first lista))) (combinacoes-numero-lista numero (rest lista))))
)

(defun criar-lista-numeros (tamanho &optional (valor-por-omissao 1)) "Devolve uma lista com o tamanho recebido como argumento e o valor dos elementos da lista é recebido se não por omissão têm todos o valor 1"
      (if (<= tamanho 0)
            nil
      (append (list valor-por-omissao) (criar-lista-numeros (- tamanho 1) valor-por-omissao)))
)

(defun funcao-utilidade (no peca old-utilidade caixas-fechadas-j1 caixas-fechadas-j2 old-numero-caixas-j1 old-numero-caixas-j2)
"Função utilidade que faz a verificação do vencedor atribuindo um valor para o resultado"
      (let* ((estado (get-no-estado no))
            (num-caixas-j1 (caixas-fechadas-j1 estado))
            (num-caixas-j2 (caixas-fechadas-j2 estado))
            (utilidade (get-no-utilidade no))
            (profundidade (get-no-profundidade no))
            (valor-utilidade (if (equal peca 1)
                              (+ utilidade (* 10 (- num-caixas-j1 old-numero-caixas-j1)) (* 10 (- num-caixas-j2 old-numero-caixas-j2)))
                              (+ utilidade (* 10 (- num-caixas-j2 old-numero-caixas-j2)) (* 10 (- num-caixas-j1 old-numero-caixas-j1))))))
      (if (equal profundidade 0)
            valor-utilidade
      (if (equal peca 1)
            (if (>= valor-utilidade old-utilidade)
                  valor-utilidade
            (- valor-utilidade))
      (if (<= valor-utilidade old-utilidade)
            valor-utilidade
      (- valor-utilidade))))
      )
)


