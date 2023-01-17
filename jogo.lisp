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
			
			(let ((opcao (read-keyboard)))
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
				(let ((opcao (read-keyboard)))
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
            (humano-play (tab-init) *jogador1* 0 0)
            (pc-play (tab-init) *jogador2* 0 0)
        )
    )
)

(defun play-pc-pc (tab peca num-caixas-p1 num-caixas-p2)
	(let* (
		(play (read-play tab))
		(new-tab (do-play tab peca (first play) (second play) (third play)))
		(num-caixas-player (caixas-fechadas new-tab))
	)
	(cond
		((winner-p tab num-caixas-player peca num-caixas-p1 num-caixas-p2)
			(progn
				(format t "~%> O jogador ~a ganhou!")
				(play-again)
			)
		)
		((tabululeiro-full new-tab)
			(progn
				(format t "~%> Empate!")
			)
		)
		(and 
			(= peca *jogador1*)
			(> num-caixas-player num-caixas-p1))
		)
		(progn 
			(imprime-tab new-tab)
			(do-play-pc-pc new-tab peca num-caixas-p1 num-caixas-p2)
		)
		(T
			(progn
				(imprime-tab new-tab)
				(do-play-pc-pc new-tab (change-peca peca) num-caixas-player num-caixas-p2)
			)
		)
	)
)

(defun human-play (tab peca num-caixas-p1 num-caixas-p2 path)
	(let* (
		(play (read-play tab))
		(new-tab (do-play tab peca (first play) (second play) (third play)))
		(num-caixas-fechadas-tab-antigo (caixas-fechadas tab))
		(num-caixas-fechadas-tab(caixas-fechadas new-tab))
		(numero-c-p1 (+ num-caixas-p1 (- num-caixas-fechadas-tab num-caixas-fechadas-tab-antigo)))
	)
	(format t "~%> Caixas Player 1: ~a~%" num-caixas-p1)
	(format t "~%> Caixas Player 2: ~a~%" num-caixas-p2)
	(cond
		((winner-p new-tab num-caixas-fechadas-tab peca num-caixas-p1 num-caixas-p2)
			(progn
				(format t "~%> O jogador ~a ganhou!")
				(play-again)
			)
		)
		((tabululeiro-full new-tab)
			(progn
				(format t "~%> Empate!")
			)
		)
		(and 
			(= peca *jogador1*)
			(> num-caixas-fechadas-tab num-caixas-fechadas-tab-antigo))
		)
		(progn 
			(imprime-tab new-tab)
			(pc-play new-tab peca num-caixas-p1 num-caixas-p2 path)
		)
		(T
			(progn
				(pc-play new-tab (change-peca peca) num-caixas-p1 num-caixas-p2 path)
			)
		)
	)
)
	
(defun pc-play (tab peca num-caixas-p1 num-caixas-p2 path &optional(time-play (get-universal-time)))
	(let* (
		(value-alfa-beta (alfa-beta (criar-no tab 0 0 num-caixas-p1 num-caixas-p2) 3 peca 'utilidade-function))
		(new-tab (estado pc-play))
		(num-caixas-fechadas-tab-antigo (caixas-fechadas tab))
		(num-caixas-fechadas-tab(caixas-fechadas new-tab))
		(numero-c-p2 (+ num-caixas-p2 (- num-caixas-fechadas-tab num-caixas-fechadas-tab-antigo)))
		(print-winner (winner-p new-tab num-caixas-fechadas-tab peca num-caixas-p1 numero-c-p2))
		(stats (statslog new-tab value-alfa-beta print-winner path))
	)
	(progn
		(cond 
			((winner-p new-tab num-caixas-fechadas-tab peca num-caixas-p1 numero-c-p2)
				(progn
					(format t "~%Ganhou!")
					(format t "~%Tempo demorado: ~a" *start-time*)
					stats)
			)
			((tabululeiro-full new-tab)
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
                (imprime-tab new-tab)
                (human-play new-tab (change-peca peca))
			  )
			)
		   )
		)
	)
)	
	
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

