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
(defparameter *LINHAS* 18)
(defparameter *COLUNAS* 10)
(defstruct tabuleiro (data NIL))

(defun cria-tabuleiro ()
  (make-tabuleiro :data (make-array (list *LINHAS* *COLUNAS*))))

(defun copia-tabuleiro (tabuleiro)
  (copy-tabuleiro tabuleiro))
	
(defun tabuleiro-preenchido-p (tabuleiro x y)
  (not (equal (aref (tabuleiro-data tabuleiro) x y) NIL)))

(defun tabuleiro-altura-coluna (tabuleiro coluna)
  (let ((max 0))
    (do* ((n (- *LINHAS* 1) (- n 1))) ((or (not (= max 0)) (= n -1)))
      (if (tabuleiro-preenchido-p tabuleiro n coluna)
          (setf max (+ n 1)))
      max)))
  
(defun tabuleiro-linha-completa-p (tabuleiro linha)
  (dotimes (coluna *COLUNAS* t)
    (when (equal (tabuleiro-preenchido-p tabuleiro linha coluna) nil)
	(return nil))))
