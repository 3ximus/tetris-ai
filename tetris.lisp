;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;										 			 		;
;				Projeto IA 2015/2016 --- grupo 59	 		;
;					Andre Sobral   nº 69481			 		;
;					Rui Lourenco   nº 69701			 		;
;					Fabio Almeida  nº 76959			 		;
;										 			 		;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ------------ Tipo Accao ------------  ;;;
(defun cria-accao (coluna peca)
  (cons coluna peca))

(defun accao-coluna (accao)
  (first accao))

(defun accao-peca (accao)
  (rest accao))

;;; ------------------------  ;;;
;;;      Tipo Tabuleiro
;;; ------------------------  ;;;

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
      T)))

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
  (copy-array (tabuleiro-data tabuleiro)))

;;;
;;; Cria um tabuleiro com conteudo igual a (arrayOfContent)
;;;
(defun array->tabuleiro (arrayOfContent)
  (make-tabuleiro :data arrayOfContent))

;;;
;;; Funcoes auxiliares
;;;
(defun valida-linha (linha)
  "Testa se linha existe"
  (if (or(> linha (- *LINHAS* 1))(< linha 0))
      NIL
    T))

(defun valida-coluna (coluna)
  "Testa se coluna existe"
  (if (or(> coluna (- *COLUNAS* 1))(< coluna 0))
      NIL
    T))
;;;
;;; Faz a copia de um array
;;;
(defun copy-array (array &key
                         (element-type (array-element-type array))
                         (fill-pointer (and (array-has-fill-pointer-p array)(fill-pointer array)))
                         (adjustable (adjustable-array-p array)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions
                                :element-type element-type
                                :adjustable adjustable
                                :fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;;; ------------------------  ;;;
;;;      Tipo Estado
;;; ------------------------  ;;;
(defstruct estado (pontos 0) (pecas-por-colocar NIL) (pecas-colocadas NIL) (tabuleiro NIL))

; TODO



;;; ------------------------  ;;;
;;;      Tipo Problema
;;; ------------------------  ;;;

; TODO



;;; ------------------------  ;;;
;;;    Funcoes de Procura
;;; ------------------------  ;;;

;;;
;;; Recebe um estado e indica se este corresponde a uma solucao
;;;
(defun solucao (estado)
  (if (and (= estado-pecas-por-colocar NIL) (= (tabuleiro-topo-preenchido-p estado-tabuleiro) NIL))
    T NIL))

;;;
;;; Identifica peca basedo no identificador da peca e rotacao
;;; Verifica se jogada e valida e retorna o array da peca
;;;
(defun identifica-jogada (estado peca rotacao coluna)
  (cond 
    ;; peca i
    ((and (= peca 'i)(= rotacao 0)(jogada-valida(estado peca-i0 coluna))) (list coluna peca-i0))
    ((and (= peca 'i)(= rotacao 1)(jogada-valida(estado peca-i1 coluna))) (list coluna peca-i1))
    ;; peca l
    ((and (= peca 'l)(= rotacao 0)(jogada-valida(estado peca-l0 coluna))) (list coluna peca-l0))
    ((and (= peca 'l)(= rotacao 1)(jogada-valida(estado peca-l1 coluna))) (list coluna peca-l1))
    ((and (= peca 'l)(= rotacao 2)(jogada-valida(estado peca-l2 coluna))) (list coluna peca-l2))
    ((and (= peca 'l)(= rotacao 3)(jogada-valida(estado peca-l3 coluna))) (list coluna peca-l3))
    ;; peca j
    ((and (= peca 'j)(= rotacao 0)(jogada-valida(estado peca-j0 coluna))) (list coluna peca-j0))
    ((and (= peca 'j)(= rotacao 1)(jogada-valida(estado peca-j1 coluna))) (list coluna peca-j1))
    ((and (= peca 'j)(= rotacao 2)(jogada-valida(estado peca-j2 coluna))) (list coluna peca-j2))
    ((and (= peca 'j)(= rotacao 3)(jogada-valida(estado peca-j3 coluna))) (list coluna peca-j3))
    ;; peca o
    ((and (= peca 'o)(= rotacao 0)(jogada-valida(estado peca-o0 coluna))) (list coluna peca-o0))
    ;; peca s
    ((and (= peca 's)(= rotacao 0)(jogada-valida(estado peca-s0 coluna))) (list coluna peca-s0))
    ((and (= peca 's)(= rotacao 1)(jogada-valida(estado peca-s1 coluna))) (list coluna peca-s1))
    ;; peca z
    ((and (= peca 'z)(= rotacao 0)(jogada-valida(estado peca-z0 coluna))) (list coluna peca-z0))
    ((and (= peca 'z)(= rotacao 1)(jogada-valida(estado peca-z1 coluna))) (list coluna peca-z1))
    ;; peca t
    ((and (= peca 't)(= rotacao 0)(jogada-valida(estado peca-t0 coluna))) (list coluna peca-t0))
    ((and (= peca 't)(= rotacao 1)(jogada-valida(estado peca-t1 coluna))) (list coluna peca-t1))
    ((and (= peca 't)(= rotacao 2)(jogada-valida(estado peca-t2 coluna))) (list coluna peca-t2))
    ((and (= peca 't)(= rotacao 3)(jogada-valida(estado peca-t3 coluna))) (list coluna peca-t3))))

;;;
;;; Verifica se uma jogada é valida
;;;
(defun jogada-valida (estado peca-array coluna)
  (let ((linha-base (tabuleiro-altura-coluna estado-tabuleiro coluna)))
    (if (<= (array-dimension peca-array 1) (- *COLUNAS* coluna))
      T NIL)))

;;;
;;; Indica se uma accao e valida
;;;
(defun accoes (estado)
  (let ((lista-accoes NIL) (peca (first (estado-pecas-por-colocar estado))))
    (dotimes (coluna *COLUNAS*)
      (identifica-jogada estado peca rotacao coluna))))

;;;
;;; Recebe um estado e uma accao e aplica a accao a esse estado
;;;
(defun resultado (estado accao)
  )