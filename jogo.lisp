(defvar *jogador1* 1)
(defvar *jogador2* 2)
(defvar *start-time* 0)

(defun comecar()
    (progn
        (let ((path (insert-path)))
            (load-ficheiros path)
        )
    )
)

(defun insert-path()
    (progn
        (format t "~%Insira o caminho para o ficheiro: ")
        (format nil (read-line))
        (if (file-exists-p (format nil (read-line)))
            (format nil (read-line))
            (progn
                (format t "~%O caminho inserido não existe. Insira um caminho válido: ")
                (insert-path)
            )
        )
    )
)

(defun load-ficheiros(path)
    (progn
        (compile-file (concatenate 'string path "\\alfabeta.lisp"))
        (compile-file (concatenate 'string path "\\puzzle.lisp"))
        (load (concatenate 'string path "\\alfabeta.lisp"))
        (load (concatenate 'string path "\\puzzle.lisp"))
        (init-menu path)
    )
)


(defun init-menu (path) "Apresenta o menu principal do programa na consola. Sendo possível iniciar uma procura ou sair do programa"
	(loop	
		(progn
			(format t "~%> ------------------------------------------------------")
            (format t "~%>|                 Dots and Boxes Lisp                 |")
        	(format t "~%>|                                                     |")
            (format t "~%>|               1 - Iniciar Jogo                      |")
            (format t "~%>|               3 - Sair                              |")
            (format t "~%>|                                                     |")
			(format t "~%> ------------------------------------------------------")
			(format t "~%> Opcao")
			(format t "~%> ")
			
			(let ((opcao (ler-teclado)))
				(cond
					((not (numberp opcao)) (init-menu ))		
					((and (<= opcao 2) (>= opcao 1)) (cond
														((= opcao 1) (start-play path))
														((= opcao 2) (progn (format t "PROGRAMA TERMINADO")) (return))
													)
					)
					(T (progn
							(format t "~%> Opcao Invalida!")
							(format t "~%> Opcoes Validas: [1, 2]")
							(format t "~%  ")
						)
					)
				)
			)
		)
	)
)

(defun start-play(path)
    (progn
				(format t "~%> ------------------------------------------------------")
				(format t "~%>|         Puzzle dos Pontos e das Caixas              |")
				(format t "~%>|                                                     |")
				(format t "~%>|            1. Computador vs Computador              |")
				(format t "~%>|            2. Humano vs Computador 			        |") 
				(format t "~%>|            3. Voltar atrás	                        |")		
				(format t "~%>|                                                     |")
				(format t "~%> ------------------------------------------------------")
				(format t "~%> Opcao")
				(format t "~%> ")	
				(let ((opcao (ler-teclado)))
					(cond
						((not (numberp opcao)) (menu-selecionar-jogo))		
						((and (<= opcao 3) (>= opcao 1)) (cond
															((= opcao 1) (do-play-pc-pc))
															((= opcao 2) (do-play-humano-pc path))
															((= opcao 3) (return))
														)
                        )
						(T (progn
								(format t "~%> Opcao Invalida!")
								(format t "~%> Opcoes Validas: [1, 2, 3]")
								(format t "~%  ")
							)
						)
                    )
				)
			)
)

(defun do-play-pc-pc ()
    (if (y-or-n-p "Pretende iniciar a jogada como Player 1? (y/n)")
        (play-pc-pc  (tab-init) *jogador1* 0 0)
        (play-pc-pc  (tab-init) *jogador2* 0 0)
    )
)

(defun do-play-humano-pc ()
    (let ((start-time 0))
        (if (y-or-n-p "Pretende iniciar a jogada como Player 1? (y/n)")
            (human-play (tab-init) *jogador1* 0 0)
            (pc-play (tab-init) *jogador2* 0 0)
        )
    )
)

(defun play-pc-pc (tab peca num-caixas-p1 num-caixas-p2)
	(let* (
		(play (le-jogada tab))
		(new-tab (do-play tab peca (first play) (second play) (third play)))
		(num-caixas-player (caixas-fechadas-count new-tab))
	)
	(cond
		((winner-p tab num-caixas-player peca num-caixas-p1 num-caixas-p2)
			(progn
				(format t "~%> O jogador ~a ganhou!")
			)
		)
		((tab-completo new-tab)
			(progn
				(format t "~%> Empate!")
			)
		)
		(and 
			(= peca *jogador1*)
			(> num-caixas-player num-caixas-p1))
		)
		(progn 
			(imprime-tabuleiro new-tab)
			(do-play-pc-pc new-tab peca num-caixas-p1 num-caixas-p2)
		)
		(T
			(progn
				(imprime-tabuleiro new-tab)
				(do-play-pc-pc new-tab (trocar-peca peca) num-caixas-player num-caixas-p2)
			)
		)
	)
)

(defun human-play (tab peca num-caixas-p1 num-caixas-p2 path)
	(let* (
		(play (le-jogada tab))
		(new-tab (do-play tab peca (first play) (second play) (third play)))
		(num-caixas-fechadas-tab-antigo (caixas-fechadas-count tab))
		(num-caixas-fechadas-tab (caixas-fechadas-count new-tab))
		(numero-c-p1 (+ num-caixas-p1 (- num-caixas-fechadas-tab num-caixas-fechadas-tab-antigo)))
	)
	(format t "~%> Caixas Player 1: ~a~%" num-caixas-p1)
	(format t "~%> Caixas Player 2: ~a~%" num-caixas-p2)
	(cond
		((winner-p new-tab num-caixas-fechadas-tab peca num-caixas-p1 num-caixas-p2)
			(progn
				(format t "~%> O jogador ~a ganhou!")
			)
		)
		((tab-completo new-tab)
			(progn
				(format t "~%> Empate!")
			)
		)
		(and 
			(= peca *jogador1*)
			(> num-caixas-fechadas-tab num-caixas-fechadas-tab-antigo))
		)
		(progn 
			(imprime-tabuleiro new-tab)
			(pc-play new-tab peca num-caixas-p1 num-caixas-p2 path)
		)
		(T
			(progn
				(pc-play new-tab (trocar-peca peca) num-caixas-p1 num-caixas-p2 path)
			)
		)
	)
)
	
(defun pc-play (tab peca num-caixas-p1 num-caixas-p2 path &optional(time-play (get-universal-time)))
	(let* (
		(value-alfa-beta (alfabeta (criar-no tab 0 0 num-caixas-p1 num-caixas-p2) 3 peca 'f-utilidade))
		(new-tab (get-estado pc-play))
		(num-caixas-fechadas-tab-antigo (caixas-fechadas-count tab))
		(num-caixas-fechadas-tab(caixas-fechadas new-tab))
		(numero-c-p2 (+ num-caixas-p2 (- num-caixas-fechadas-tab num-caixas-fechadas-tab-antigo)))
		(print-winner (winner-p new-tab num-caixas-fechadas-tab peca num-caixas-p1 numero-c-p2))
		(stats (estatisticas-log new-tab value-alfa-beta print-winner path))
	)
	(progn
		(cond 
			((winner-p new-tab num-caixas-fechadas-tab peca num-caixas-p1 numero-c-p2)
				(progn
					(format t "~%Ganhou!")
					(format t "~%Tempo demorado: ~a" *start-time*)
					stats)
			)
			((tab-completo new-tab)
				(progn
					(format t "~%Empate!")
					(format t "~%Tempo demorado: ~a" *start-time*)
					stats)
			)
			((and 
				(= peca *jogador2*)
				(> num-caixas-fechadas-tab num-caixas-fechadas-tab-antigo))
			)
			(pc-play new-tab peca num-caixas-p1 numero-c-p2 path time-play)
		)
		(T (progn
            (let ((new-time (+ *start-time* (- (get-universal-time) time-play))))
              (let ((*start-time* new-time))
                (format t "~%Tempo ~a:" *start-time*) (format t "segundos~%")
                (imprime-tabuleiro new-tab)
                (human-play new-tab (trocar-peca peca))
			  )
			)
		   )
		)
	)
)	)
	
(defun winner-p (tab new-num-caixas peca c-p1 c-p2)
	(let*
		(num-linhas (get-num-linhas tab))
		(num-colunas (get-num-colunas tab))
		(num-max-caixas (* num-linhas num-colunas))
		(num-caixas-vencer (cond ((evenp num-max-caixas) (+ (/ num-max-caixas 2) 1)) (T (/ (+ num-max-caixas 1) 2))))
		(resultado (>= new-num-caixas num-caixas-vencer))
	)
	(cond 
		(resultado (cond
			((and (= peca *jogador1*) (> c-p1 c-p2)) *jogador1*)
			((and (= peca *jogador2*) (> c-p2 c-p1)) *jogador2*)
		))
		(T nil)
	)
)

(defun le-jogada (tabuleiro) "Le uma jogada fazendo a verificacao da sua legalidade. A jogada lida (arco-horizontal ou arco-vertical) e a posicao na no tabuleiro (entre 1 e 8)"
	(let* (
		(jogada (read))
		(posicao (read))
		(legal (legal-p tabuleiro jogada posicao))
	)
		(cond
			(legal (list jogada posicao))
			(T (progn
				(format t "~%Jogada ilegal!~%")
				(le-jogada tabuleiro)
			))
		)
	)
)

(defun legal-p (tabuleiro jogada posicao)
	(let* (
		(num-linhas (get-num-linhas tabuleiro))
		(num-colunas (get-num-colunas tabuleiro))
		(num-max-caixas (* num-linhas num-colunas))
		(legal (cond
			((and (>= posicao 1) (<= posicao num-max-caixas))
				(cond
					((and (equal jogada 'get-horizontais) (<= posicao (- num-max-caixas num-colunas)))
						(T)
					)
					((and (equal jogada 'get-verticas) (<= posicao (- num-max-caixas 1)))
						(T)
					)
					(T nil)
				)
			)
			(T nil)
		))
	)
		legal
	)
)

(defun le-operador ()
"Lê o operador que o utilizador pretende executar"
(format t "%> ------------------------------------------------------")
(format t "%>| 					Tipo de Jogada 					   |")
(format t "%>| 													   |")
(format t "%>| 			1.colocacao de um arco horizontal 		   |")
(format t "%>| 			2.colocacao de um arco vertical 		   |")
(format t "%>| 													   |")
(format t "%> ------------------------------------------------------")
(format t "%> Opcao")
(format t "%> ")
(let ((operador-lido (read)))
(if (or (not (integerp operador-lido))
(< operador-lido 1) (> operador-lido 2))
(progn (format t "&Entrada inválida.")
(le-operador))
(if (= 1 operador-lido)
'put-arco-horizontal
'put-arco-vertical))))


(defun le-valor-y (valor) "Le a coordenada y que o utilizador pretende inserir o arco"
(format t "~&Valor de ~A [1 <= ~A <= 7]: " valor valor)
(let ((valor-lido (read)))
(cond ((or (not (integerp valor-lido))
(< valor-lido 1) (> valor-lido 8))
(format t "&Entrada inválida.")
(le-valor-y valor))
(T valor-lido))))

(defun le-valor-x (valor) "Le a coordenada y que o utilizador pretende inserir o arco"
(format t "~&Valor de ~A [1 <= ~A <= 8]: " valor valor)
(let ((valor-lido (read)))
(cond ((or (not (integerp valor-lido))
(< valor-lido 1) (> valor-lido 7))
(format t "&Entrada inválida.")
(le-valor-x valor))
(T valor-lido))))


(defun tab-init ()
	(
 (;arcos horizontais
 (0 0 0 0 0 0)
 (0 0 0 0 0 0)
 (0 0 0 0 0 0)
 (0 0 0 0 0 0)
 (0 0 0 0 0 0)
 (0 0 0 0 0 0)
 )
 (;arcos verticais
 (0 0 0 0 0)
 (0 0 0 0 0)
 (0 0 0 0 0)
 (0 0 0 0 0)
 (0 0 0 0 0)
 (0 0 0 0 0)
 (0 0 0 0 0)
 )
)
)

(defun tab-completo ()
(;estado
 (;tabuleiro figura 1
 (;arcos horizontais
 (1 2 1 1 0 2)
 (2 1 1 1 1 0)
 (0 2 1 1 2 0)
 (0 1 0 2 2 0)
 (1 2 0 0 0 0)
 (0 1 2 1 2 1)
 )
 (;arcos verticais
 (1 0 1 0 0)
 (2 1 1 2 2)
 (2 1 1 2 0)
 (1 2 2 1 1)
 (1 2 2 0 0)
 (0 1 2 1 2)
 (2 2 1 2 0)
 )
 )))

(defun do-play (tab peca operador x y)
	(funcall operador x y peca tab)
)

(defun imprime-tabuleiro (tabuleiro) "Imprime o tabuleiro, linha a linha"
	(let ((linhas (first tabuleiro)) (colunas (append (rodar (second tabuleiro)) '(NIL))))
		(mapcar #'(lambda (linha coluna) (progn (imprime-linha linha) 
												(imprime-coluna coluna)
												(imprime-coluna coluna))) linhas colunas)
	)
)

(defun converte-arco-horizontal (v)
  "Converte os inteiros dos arcos horizontais para os simbolos --- (jogador com peca 1) e ... (jogador com peca 2)"
  (cond ((= v 1) "---")
		((= v 2) "...")
		(T "   ")))

(defun converte-arco-vertical (v)
  "Converte os inteiros dos arcos verticais para os simbolos | (jogador com peca 1) e . (jogador com peca 2)"
  (cond ((= v 1) "|")
		((= v 2) ".")
		(T " ")))

(defun imprime-linha (linha)
  "Imprime uma linha do tabuleiro"
  (format t "~%")
  (mapcar #'(lambda (v) (format t "~A" (converte-arco-horizontal v))) linha)
  (format t "~%"))

(defun imprime-coluna (coluna)
  "Imprime uma coluna do tabuleiro"
  (mapcar #'(lambda (v) (format t "~A" (converte-arco-vertical v))) coluna)
  (format t "~%"))

(defun rodar (lista)
  "Roda uma lista de listas"
  (cond ((null (first lista)) nil)
		(T (cons (mapcar #'first lista) (rodar (mapcar #'rest lista))))))

(defun alisa (lista) "Retorna uma lista com todos os Átomos na lista principal"
	(cond ((null lista) nil)
		((atom (first lista)) (cons (first lista) (alisa (rest lista))))
		(T (alisa (append (first lista) (rest lista))))
	)
)

(defun tabuleiro-preenchido-p (lista)
  "Verifica se o tabuleiro está preenchido"
  (let* ((lista-alisada (alisa lista))
         (tamanho (length lista-alisada))
         (resultado (reduce #'+ (mapcar #'(lambda (n)
                                           (if (null n) 0 1))
                                       lista-alisada))))
    (if (= tamanho resultado)
        t
        nil)))

(defun ler-teclado () "Ler do teclado algo do utilizador"
	(read)
)

;; função que troca uma peça de um jogador para a peça do outro jogador
(defun trocar-peca (peca)
  (cond
    ((= peca 1) 2)
    ((= peca 2) 1)
  )
)

(defun estatisticas-log (tabuleiro alfabeta peca-vencedora caminho) "Função que escreve as estatisticas num ficheiro e imprime na consola."	
		(with-open-file (file (concatenate 'string caminho "\\log.dat")
							:direction :output
							:if-exists :append 
							:if-does-not-exist :create)
			;; Esta parte serÃ¡ escrita no ficheiro do tipo .DAT
			(format file "~%Vencedor: ~s ~%" peca-vencedora)
			(format file "~%Tabuleiro: ~s ~%" tabuleiro)
			(format file "~%Caixas Fechadas: ~s ~%" (caixas-fechadas-count tabuleiro))
			(format file "~%Cortes Alfa: ~s ~%" *corte-alfa*)
			(format file "~%Cortes Beta: ~s ~%" *corte-beta*)
			(format file "~%Nos analisados: ~s ~%" *nos-analisados* )
			(format file "~%Tempo Maximo ~s ~%" *tempo-despendido*)
			(format file "___________________________________________________~%~%~%")
		)	
		;;Esta parte serÃ¡ mostrada na consola
			(format t "~%Vencedor: ~s ~%" peca-vencedora)
			(format t "~%Tabuleiro: ~s ~%" tabuleiro)
			(format t "~%Caixas Fechadas: ~s ~%" (caixas-fechadas-count tabuleiro))		
)