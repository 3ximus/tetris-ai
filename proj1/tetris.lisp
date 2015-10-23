;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;										 			 		;
;				Projeto IA 2015/2016 --- grupo 59	 		;
;					Andre Sobral   nº 69481			 		;
;					Rui Lourenco   nº 69701			 		;
;					Fabio Almeida  nº 76959			 		;
;										 			 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tipo Accao ;;;
(defun cria-accao (posicao peca)
  (cons posicao peca))

(defun accao-coluna (accao)
  (first accao))

(defun accao-peca (accao)
  (rest accao))

;;; Tipo Tabuleiro ;;;
(defstruct tabuleiro (linhas 18) (colunas 10))

(defun cria-tabuleiro ()
  (let* ((tabuleiro (make-tabuleiro))
         (linhas (tabuleiro-linhas tabuleiro))
         (colunas (tabuleiro-colunas tabuleiro)))
    (make-array '(linhas colunas))))

    