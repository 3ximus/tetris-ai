;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;										 			 		;
;				Projeto IA 2015/2016 --- grupo 59	 		;
;					Andre Sobral   nº 69481			 		;
;					Rui Lourenco   nº 69701			 		;
;					Fabio Almeida  nº 76959			 		;
;										 			 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Tipo Accao ;;;
(defun cria-accao (coluna peca)
  (cons coluna peca))

(defun accao-coluna (accao)
  (first accao))

(defun accao-peca (accao)
  (rest accao))

;;; Tipo Tabuleiro ;;;
(defparameter *LINHAS*  18)
(defparameter *COLUNAS* 10)
(defstruct tabuleiro (data NIL))

(defun cria-tabuleiro ()
  (make-tabuleiro :data (make-array (list *LINHAS* *COLUNAS*))))

(defun copia-tabuleiro (tabuleiro)
  (copy-tabuleiro tabuleiro))

;;;
;;; Verifica se a posicao (linha coluna) esta preenchida.
;;;
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
  (not (equal (aref (tabuleiro-data tabuleiro) linha coluna) NIL)))

;;;
;;; Devolve a posicao mais alta da coluna que esteja preenchida.
;;;
(defun tabuleiro-altura-coluna (tabuleiro coluna)
  (let ((max 0))
    (do* ((n (- *LINHAS* 1) (- n 1))) ((or (not (= max 0)) (= n -1)))
      (if (tabuleiro-preenchido-p tabuleiro n coluna)
          (setf max (+ n 1))))
      max))
 
;;;
;;; Verifica se (linha) esta completamente preenchida.
;;; 
(defun tabuleiro-linha-completa-p (tabuleiro linha)
  (dotimes (coluna *COLUNAS* T)
    (when (equal (tabuleiro-preenchido-p tabuleiro linha coluna) NIL)
	(return NIL))))

;;;
;;; Preenche a posicao (linha coluna).
;;;
(defun tabuleiro-preenche! (tabuleiro linha coluna)
  (if (and (valida-linha linha) (valida-coluna coluna))
      (setf (aref (tabuleiro-data tabuleiro) linha coluna) T)))

;;;
;;; Remove (linha) de um tabuleiro.
;;; As linhas acima da linha removida descem uma posicao.
;;;
(defun tabuleiro-remove-linha! (tab linha)
  (do ((n linha (+ n 1))) ((= n (- *LINHAS* 1)))
    (dotimes (coluna *COLUNAS* T)
      (setf (aref (tabuleiro-data tab) n coluna) (aref (tabuleiro-data tab) (+ n 1) coluna))))
  (tabuleiro-linha-vazia tab (- *LINHAS* 1)))

;;;
;;; Coloca todas as posicoes da (linha) vazias.
;;;
(defun tabuleiro-linha-vazia (tab linha)
  (dotimes (coluna *COLUNAS* T)
      (setf (aref (tabuleiro-data tab) linha coluna) NIL)))

;;;
;;; Verifica se a linha do topo do tabuleiro esta preenchida
;;;
(defun tabuleiro-topo-preenchido-p (tab)
  (dotimes (coluna *COLUNAS* NIL)
    (if(= (tabuleiro-altura-coluna tab coluna) *LINHAS*)
      (return T))))

;;;     
;;; Verifica se 2 tabuleiros sao iguais
;;;     
(defun tabuleiros-iguais-p (tab1 tab2)
  (dotimes (linha *LINHAS* T)
    (dotimes (coluna *COLUNAS* T)
      (if (not (equal (aref (tabuleiro-data tab1) linha coluna) (aref (tabuleiro-data tab2) linha coluna)))
        (return-from tabuleiros-iguais-p NIL)))))

;;;
;;; Cria um array com o conteudo do tabuleiro
;;;
(defun tabuleiro->array (tabuleiro)
  (tabuleiro-data tabuleiro))

;;;
;;; Cria um tabuleiro com conteudo igual a (arrayOfContent)
;;;
(defun array->tabuleiro (arrayOfContent)
  (make-tabuleiro :data arrayOfContent))

;;;
;;; Funcoes auxiliares
;;;
(defun valida-linha (linha)
  (if (or(> linha (- *LINHAS* 1))(< linha 0))
      NIL
    T))

(defun valida-coluna (coluna)
  (if (or(> coluna (- *COLUNAS* 1))(< coluna 0))
      NIL
    T))
