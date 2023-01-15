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

(defun alfabeta (no prof-max peca utilidade &optional (alfa -100000) (beta 100000))
  (if (or (folha no) (= prof-max 0))
      (funcall utilidade no peca)
      (if (max? peca)
          (loop for filho in (filhos no peca)
                maximize (alfabeta filho (1- prof-max) (min peca) utilidade alfa beta)
                if (>= *resultado* beta) return *resultado*
                do (setq alfa (max alfa *resultado*)))
          (loop for filho in (filhos no peca)
                minimize (alfabeta filho (1- prof-max) (max peca) utilidade alfa beta)
                if (<= *resultado* alfa) return *resultado*
                do (setq beta (min beta *resultado*))))))