;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;	                                                  ;
;       Projeto IA 2015/2016 --- grupo 59           ;
;         Andre Sobral   nº 69481                   ;
;         Rui Lourenco   nº 69701                   ;
;         Fabio Almeida  nº 76959                   ;
;                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (copia-array (tabuleiro-data tabuleiro)))

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
(defun copia-array (array)
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;;; ------------------------  ;;;
;;;      Tipo Estado
;;; ------------------------  ;;;
(defstruct estado (pontos 0) (pecas-por-colocar nil) (pecas-colocadas nil) (tabuleiro (cria-tabuleiro)))

;;;
;;; Devolve uma copia de um estado
;;;
(defun copia-estado (estado)
  (copy-estado estado))

;;;
;;; Verifica se 2 estados sao iguais
;;;
(defun estados-iguais-p (estado-incial estado-final)
  (if (and (= (estado-pontos estado-incial) (estado-pontos estado-final))
           (equal (estado-pecas-por-colocar estado-incial) (estado-pecas-por-colocar estado-final))
           (equal (estado-pecas-colocadas estado-incial) (estado-pecas-colocadas estado-final))
           (tabuleiros-iguais-p (estado-tabuleiro estado-incial) (estado-tabuleiro estado-final)))
    t
    nil))

;;;
;;; Verifica se este estado corresponde ao estado final de um jogo
;;; Isto é se ja nao existirem pecas por colocar ou o tabuleiro atingir o topo
;;;
(defun estado-final-p (estado)
  (if (or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
          (null (estado-pecas-por-colocar estado)))
    t
    nil))

;;; ------------------------  ;;;
;;;      Tipo Problema
;;; ------------------------  ;;;

(defstruct problema (estado-inicial (make-estado)) (solucao (solucao estado-inicial)) 
		    (accoes (accoes estado-inicial)) (resultado NIL) (custo-caminho NIL))



;;; ------------------------  ;;;
;;;    Funcoes de Procura
;;; ------------------------  ;;;

;;; -----------------------------------------------------------
;;; Recebe um estado e indica se este corresponde a uma solucao
;;; -----------------------------------------------------------
(defun solucao (estado)
  (if (and (equal estado-pecas-por-colocar NIL) (equal (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) NIL))
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
  (let ((linha-base (tabuleiro-altura-coluna (estado-tabuleiro estado) coluna)))
    (if (<= (array-dimension peca-array 1) (- (- *COLUNAS* 1) coluna))
      T NIL)))

;;; ------------------------------
;;; Indica se uma accao e valida
;;; ------------------------------
(defun accoes (estado)
  (let ((lista-accoes NIL) (peca (first (estado-pecas-por-colocar estado))) (max-rotacao 0))
    (cond ((or (= peca 'i)(= peca 's)(= peca 'z)) ((setf max-rotacao 1)))
      ((or (= peca 'l)(= peca 'j)(= peca 't)) ((setf max-rotacao 3))))
    (dotimes (rotacao max-rotacao)
      (dotimes (coluna (- *COLUNAS* 1)
        (identifica-jogada estado peca rotacao coluna))))))

;;;
;;; Descobre a base na coluna de uma peca
;;;
(defun base-peca-coluna (peca-array coluna)
  (dotimes (linha (- (array-dimension peca-array 0) 1))
    (if (aref peca-array linha coluna)) (return-from base-peca-coluna linha)))

;;;
;;; Desenha um peca num tabuleiro
;;;
(defun desenha-peca-tabuleiro (estado peca-array linha coluna)
  (dotimes (peca-linha (- (array-dimension peca-array 0) 1))
    (dotimes (peca-coluna (- (array-dimension peca-array 1) 1))
      (if (aref peca-array peca-linha peca-coluna) 
        (setf (aref (estado-tabuleiro estado) (+ linha peca-linha) (+ coluna peca-coluna)))))))

;;;
;;; Insere uma peca numa posicao
;;;
(defun insere-peca (estado peca-array coluna)
  (let ((linha-base 0)(linha-max 0)(linha-a-inserir 0))
    (dotimes (peca-coluna (- (array-dimension peca-array 1) 1))
      (setf linha-max (+ (tabuleiro-altura-coluna (estado-tabuleiro estado) (+ coluna peca-coluna)) 1))
      (setf linha-base (- linha-max (base-peca-coluna peca-array peca-coluna)))
      (if (> linha-base linha-a-inserir) (setf linha-a-inserir linha-base)))
    (desenha-peca-tabuleiro (estado peca-array linha-a-inserir coluna))))

;;;
;;; Calcula numero de pontos a somar segundo o numero de linhas dado
;;; 
(defun calcula-pontos (contador)
  (cond ((= contador 1) 100)
    ((= contador 2) 300)
    ((= contador 3) 500)
    ((= contador 4) 800)))

;;; -----------------------------------------------------------
;;; Recebe um estado e uma accao e aplica a accao a esse estado
;;; -----------------------------------------------------------
(defun resultado (estado accao)
  (let ((novo-estado (copia-estado estado))(cont 0))
    (insere-peca novo-estado (rest accao) (first accao))
    (if ((equal (tabuleiro-topo-preenchido-p (estado-tabuleiro estado)) NIL) novo-estado)
      (dotimes (linha (- *LINHAS* 1))
        (if (tabuleiro-linha-completa-p (estado-tabuleiro novo-estado) linha)
          (tabuleiro-remove-linha! (estado-tabuleiro novo-estado) linha)
          (- linha 1)(+ cont 1)))
      (setf (estado-pontos novo-estado) (+ (estado-pontos novo-estado) (calcula-pontos cont))))))


;;; ------------------------
;;; Devolve o valor de qualidade de um estado que corresponde ao valor negativo dos pontos
;;; ------------------------
(defun qualidade (estado)
  (- 0 (estado-pontos estado)))

;;; ------------------------
;;; Devolve o custo de oportunidade de todas as accoes tomadas ate ao momento
;;; Ou seja a diferenca entre as pontuacoes maximas possiveis por peca e os pontos obtidos ate ao momento
;;; ------------------------
(defun custo-oportunidade (estado)
  (let ((max-pontos 0))
    (dolist (peca (estado-pecas-colocadas estado))
      (cond ((= peca 'i) (incf max-pontos 800))
        ((= peca 'j) (incf max-pontos 500))
        ((= peca 'l) (incf max-pontos 500))
        ((= peca 's) (incf max-pontos 300))
        ((= peca 'z) (incf max-pontos 300))
        ((= peca 't) (incf max-pontos 300))
        ((= peca 'o) (incf max-pontos 300))))))


;;; ------------------------  ;;;
;;;         Procuras
;;; ------------------------  ;;;