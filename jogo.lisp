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
														((= opcao 1) (start-play caminho))
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
															((= opcao 1) (fazer-uma-partida-pc-pc))
															((= opcao 2) (fazer-uma-partida-humano-pc caminho))
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
        (pc-pc-play (tab-init) *jogador1* 0 0)
        (pc-pc-play (tab-init) *jogador2* 0 0)
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

