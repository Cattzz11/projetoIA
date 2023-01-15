(defun criar-no (estado &optional (prof 0) (util nil) (caixas-jogador-1 0)(caixas-jogador-2 0))
    (list estado prof util caixas-jogador-1 caixas-jogador-2)
)

(defun estado (no)
    (car no)
)

(defun profundidade (no)
  (car (cdr no))
)

(defun utilidade (no)
  (car (cdr (cdr no)))
)

(defun caixas-jogador-1 (no)
  (car (cdr (cdr (cdr no))))
)

(defun caixas-jogador-2 (no)
  (car (cdr (cdr (cdr (cdr no)))))
)

(defun put-arco-horizontal (linha coluna tabuleiro)
   (cond
   ((null tabuleiro) nil)
   ((put-arco-posicao-horizontal linha coluna (get-horizontais tabuleiro)))
   )
)

(defun get-horizontais(tab)
    (first tab)
)

(defun get-verticais(tab)
    (second tab)
)

(defun put-arco-posicao-horizontal (linha coluna lista)
"Insere um arco (representado pelo valor 1) numa lista que representa o conjunto de arcos horizontais ou verticais de um tabuleiro."
  (cond
   ((null lista) nil)
   ((= linha 1) (cons (substituir coluna (car lista)) (cdr lista)))
   (t (cons (car lista) (put-arco-posicao-horizontal (- linha 1) coluna (cdr lista))))
  )
)

(defun put-arco-vertical (linha coluna tabuleiro)
   (cond
   ((null tabuleiro) nil)
   ((put-arco-posicao-vertical linha coluna (get-verticais tabuleiro)))
   )
)


(defun put-arco-posicao-vertical (linha coluna lista)
"Insere um arco (representado pelo valor 1) numa lista que representa o conjunto de arcos horizontais ou verticais de um tabuleiro."
  (cond
   ((null lista) nil)
   ((= linha 1) (cons (substituir coluna (car lista)) (cdr lista)))
   (t (cons (car lista) (put-arco-posicao-vertical (- linha 1) coluna (cdr lista))))
  )
)

(defun get-num-colunas (tabuleiro)
  (get-num-listas (get-horizontais tabuleiro))
)

(defun get-num-linhas (tabuleiro)
  (get-num-listas (get-verticais tabuleiro))
)

(defun get-num-listas (lista) 
"Devolve o numero de listas existentes"
  (cond ((null lista) 0)
   (t (+ 1 (get-num-listas (cdr lista)))))
)

(defun caixas-fechadas-count (tabuleiro) "retorna o numero de caixas fechadas de um tabuleiro."
	(cond
		((null tabuleiro) nil)
		(t (contar-objetivo (caixas-fechadas-aux (get-horizontais tabuleiro)(get-cabecas-por-coluna tabuleiro))))
	)
)

(defun caixas-fechadas-aux (horizontais verticais) "retorna a lista completa das caixas verticais e horizontais de um tabuleiro"
	(cond
		((or (null horizontais)	(null verticais)) nil)
		(t (append (caixas-fechadas-aux2 (car horizontais) (cadr horizontais) (car verticais)) (caixas-fechadas-aux (cdr horizontais) (cdr verticais))))
	)
)
  

(defun caixas-fechadas-aux2 (linha1 linha2 coluna)"retorna uma lista com as caixas das duas linhas e da duas colunas."
	(cond
		((or (null linha1) (null linha2) (null coluna)) nil)
		(t (cons (list (car linha1)(car linha2)(car coluna)(cadr coluna)) (caixas-fechadas-aux2 (cdr linha1)(cdr linha2)(cdr coluna))))
	)
)

(defun get-cabecas-lista-aux (lista n)"Função que vai buscar as cabeças de uma linha vertical ou horizontal,escolhida pelo utilizador, e cria numa nova lista."
	(cond
		((null lista) nil)
		(t (cons (nth n (car lista)) (get-cabecas-lista-aux (cdr lista)n)))
	)
)


(defun get-cabecas-por-coluna (tabuleiro)"retorna as listas de cada cabeca de cada coluna" 
	(contador 0 (get-num-colunas tabuleiro) 'get-cabecas-lista-aux (get-verticais  tabuleiro)) 
)


(defun contador (i tamanhoMax funcao &rest argumentos) "contador que executa determinada ação ao longo de uma lista. Para quando indice for = tamanho da lista."
   (cond 
	((= i tamanhoMax) nil)
	(T (ecase funcao (get-cabecas-lista-aux (cons (get-cabecas-lista-aux (car argumentos) i) (contador (+ i 1) tamanhoMax funcao (car argumentos))))))
   )
)

(defun contar-objetivo (lista) "conta o numero de caixas fechadas de um tabuleiro"
    (cond
        ((null lista) 0)
        ((= (contar-zeros (car lista)) 0) (+ 1 (contar-objetivo (cdr lista))))
        (t (contar-objetivo (cdr lista)))
    )
)

(defun contar-zeros-lista (list)
  (+ (contar-zeros-lista-aux (car list)) (contar-zeros-lista-aux (car (cdr list))))
) 

(defun contar-zeros-lista-aux (list)
  (cond
   ((null list) 0)
   ((= 0 (find 0 (car list))) (+ (contar-zeros-lista-aux (cdr list)) (count-of 0 (car list))))
   (t (contar-zeros-lista-aux (cdr list)))
  )
)


(defun count-of (the-element list)
"conta o numero de ocorrencias, ou seja, colocamos um elemento e uma lista e conta quantas vezes esse elemento existe numa lista"
  ((lambda (f)
     (funcall f f list 0))
   (lambda (c tail count-so-far)
     (if (null tail)
         count-so-far
       (funcall c c (rest tail)
                (+ count-so-far (if (eql the-element (first tail)) 1 0))))))
)

;; função que troca uma peça de um jogador para a peça do outro jogador
(defun trocar-peca (peca)
  (cond
    ((= peca 1) 2)
    ((= peca 2) 1)
  )
)

;; função que cria uma lista com todos os operadores do problema
(defun criar-operadores ()
  (list 'put-arco-horizontal 'put-arco-vertical)
)

;; Recebe indices de linha e coluna e uma lista de arcos horizontais ou de arcos verticais e verifica se naquela posição o valor é 1, se for devolve 0, se for 0 devolve 1
(defun possivel-add-arco (coluna lista)
    (cond
        ((null lista) nil)
        ((= linha 1) (possivel-add-arco-aux (car lista)))
        (t (possivel-add-arco (- linha 1) (cdr lista)))
    )
)


(defun possivel-add-arco-aux (indice lista)
    (cond
        ((null lista) nil)
        ((= indice 1) (substituir 1 lista))
        (t (cons (car lista) (possivel-add-arco-aux (- indice 1) (cdr lista))))
    )
)
















