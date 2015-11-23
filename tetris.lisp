;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                   ;
;       Projeto IA 2015/2016 --- grupo 59           ;
;         Andre Sobral   n 69481                    ;
;         Rui Lourenco   n 69701                    ;
;         Fabio Almeida  n 76959                    ;
;                                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(load "utils.fas")

;;; ======================== ;;;
;;;       Tipo Accao
;;; ======================== ;;;

;;; --------------------------------------------
;;; Cria accao
;;; --------------------------------------------
(defun cria-accao (coluna peca)
  "Cria accao a partir de uma coluna e um array de uma peca"
  (cons coluna peca))

;;; --------------------------------------------
;;; Coluna de uma accao
;;; --------------------------------------------
(defun accao-coluna (accao)
  "Devolve a coluna de uma accao"
  (first accao))

;;; --------------------------------------------
;;; Peca de uma accao
;;; --------------------------------------------
(defun accao-peca (accao)
  "Devolve a peca de uma accao"
  (rest accao))

;;; ======================== ;;;
;;;      Tipo Tabuleiro
;;; ======================== ;;;

;;; Parametros globais
(defparameter *LINHAS*  18)
(defparameter *COLUNAS* 10)

;;; Estrutura tabuleiro
(defstruct tabuleiro (data NIL))

;;; --------------------------------------------
;;; Cria tabuleiro
;;; --------------------------------------------
(defun cria-tabuleiro ()
  "Cria um tabuleiro vazio"
  (make-tabuleiro :data (make-array (list *LINHAS* *COLUNAS*))))

;;; --------------------------------------------
;;; Copia tabuleiro--
(defun copia-tabuleiro (tabuleiro)
  "Devolve copia de um tabuleiro"
  (make-tabuleiro :data (tabuleiro->array tabuleiro)))

;;; --------------------------------------------
;;; Verifica se a posicao (linha coluna) esta preenchida.
;;; --------------------------------------------
(defun tabuleiro-preenchido-p (tabuleiro linha coluna)
  (not (equal (aref (tabuleiro-data tabuleiro) linha coluna) NIL)))

;;; --------------------------------------------
;;; Devolve a posicao mais alta da coluna que esteja preenchida.
;;; --------------------------------------------
(defun tabuleiro-altura-coluna (tabuleiro coluna)
  (let ((max 0))
    (do* ((n (- *LINHAS* 1) (- n 1))) ((or (not (= max 0)) (= n -1)))
      (if (tabuleiro-preenchido-p tabuleiro n coluna)
          (setf max (+ n 1))))
      max))
 
;;; --------------------------------------------
;;; Verifica se (linha) esta completamente preenchida.
;;;  --------------------------------------------
(defun tabuleiro-linha-completa-p (tabuleiro linha)
  (dotimes (coluna *COLUNAS* T)
    (when (equal (tabuleiro-preenchido-p tabuleiro linha coluna) NIL)
	(return NIL))))

;;; --------------------------------------------
;;; Preenche a posicao (linha coluna).
;;; --------------------------------------------
(defun tabuleiro-preenche! (tabuleiro linha coluna)
  (if (and (valida-linha linha) (valida-coluna coluna))
      (setf (aref (tabuleiro-data tabuleiro) linha coluna) T)))

;;; --------------------------------------------
;;; Remove (linha) de um tabuleiro.
;;; As linhas acima da linha removida descem uma posicao.
;;; --------------------------------------------
(defun tabuleiro-remove-linha! (tab linha)
  (do ((n linha (+ n 1))) ((= n (- *LINHAS* 1)))
    (dotimes (coluna *COLUNAS* T)
      (setf (aref (tabuleiro-data tab) n coluna) (aref (tabuleiro-data tab) (+ n 1) coluna))))
  (tabuleiro-linha-vazia tab (- *LINHAS* 1)))

;;; --------------------------------------------
;;; Coloca todas as posicoes da (linha) vazias.
;;; --------------------------------------------
(defun tabuleiro-linha-vazia (tab linha)
  (dotimes (coluna *COLUNAS* T)
      (setf (aref (tabuleiro-data tab) linha coluna) NIL)))

;;; --------------------------------------------
;;; Verifica se a linha do topo do tabuleiro esta preenchida
;;; --------------------------------------------
(defun tabuleiro-topo-preenchido-p (tab)
  (dotimes (coluna *COLUNAS* NIL)
    (if (aref (tabuleiro-data tab) (- *LINHAS* 1) coluna)
      (return T))))

;;; --------------------------------------------
;;; Verifica se 2 tabuleiros sao iguais
;;; --------------------------------------------
(defun tabuleiros-iguais-p (tab1 tab2)
  (dotimes (linha *LINHAS* T)
    (dotimes (coluna *COLUNAS* T)
      (if (not (equal (aref (tabuleiro-data tab1) linha coluna) (aref (tabuleiro-data tab2) linha coluna)))
        (return-from tabuleiros-iguais-p NIL)))))

;;; --------------------------------------------
;;; Cria um array com o conteudo do tabuleiro
;;; --------------------------------------------
(defun tabuleiro->array (tabuleiro)
  (copia-array (tabuleiro-data tabuleiro)))

;;; --------------------------------------------
;;; Cria um tabuleiro com conteudo igual a (arrayOfContent)
;;; --------------------------------------------
(defun array->tabuleiro (arrayOfContent)
  (make-tabuleiro :data (copia-array arrayOfContent)))

;;; Funcoes auxiliares
(defun valida-linha (linha)
  "Testa se linha existe"
  (if (or(> linha (- *LINHAS* 1))(< linha 0))
      NIL
    T))

;;; Testa se coluna existe
(defun valida-coluna (coluna)
  "Testa se coluna existe"
  (if (or(> coluna (- *COLUNAS* 1))(< coluna 0))
      NIL
    T))

;;; Faz a copia de um array
(defun copia-array (array)
  "Copia um array"
  (let* ((dimensions (array-dimensions array))
         (new-array (make-array dimensions)))
    (dotimes (i (array-total-size array))
      (setf (row-major-aref new-array i)
            (row-major-aref array i)))
    new-array))

;;; ======================== ;;;
;;;      Tipo Estado
;;; ======================== ;;;

;;; Estrutura do tipo estado
(defstruct estado (pontos 0) (pecas-por-colocar nil) (pecas-colocadas nil) (tabuleiro (cria-tabuleiro)))

;;; -----------------------------------------------------------
;;; Copia um estado
;;; -----------------------------------------------------------
(defun copia-estado (estado)
  "Copia um estado"
  (make-estado :pontos (estado-pontos estado) 
               :pecas-por-colocar (copy-list(estado-pecas-por-colocar estado))
               :pecas-colocadas (copy-list (estado-pecas-colocadas estado)) 
               :tabuleiro (copia-tabuleiro (estado-tabuleiro estado))))

;;; -----------------------------------------------------------
;;; Verifica se 2 estados sao iguais
;;; -----------------------------------------------------------
(defun estados-iguais-p (estado-incial estado-final)
  "Verifica se 2 estados sao iguais"
  (if (and (= (estado-pontos estado-incial) (estado-pontos estado-final))
           (equal (estado-pecas-por-colocar estado-incial) (estado-pecas-por-colocar estado-final))
           (equal (estado-pecas-colocadas estado-incial) (estado-pecas-colocadas estado-final))
           (tabuleiros-iguais-p (estado-tabuleiro estado-incial) (estado-tabuleiro estado-final)))
    T
    NIL))

;;; -----------------------------------------------------------
;;; Verifica se estado e final
;;; -----------------------------------------------------------
(defun estado-final-p (estado)
  "Verifica se estado e um estado final de um jogo"
  (if (or (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))
          (null (estado-pecas-por-colocar estado)))
    T
    NIL))

;;; ======================== ;;;
;;;      Tipo Problema
;;; ======================== ;;;

;;; Estrutura do tipo problema
(defstruct problema (estado-inicial (make-estado)) (solucao NIL) 
		    (accoes NIL) (resultado NIL) (custo-caminho NIL))

;;; ======================== ;;;
;;;    Funcoes de Procura
;;; ======================== ;;;

;;; --------------------------------------------
;;; Verifica se estado e solucao
;;; - estado -> estado a verificar
;;; Devolve valor logico
;;; --------------------------------------------
(defun solucao (estado)
  "Indica se um estado corresponde a uma solucao"
  (if (and (null (estado-pecas-por-colocar estado)) (null (tabuleiro-topo-preenchido-p (estado-tabuleiro estado))))
    T NIL))

;;; Verifica jogada valida
;;; - peca-array -> array de uma peca
;;; - coluna -> coluna a inserir
;;; Devolve valor logico
(defun jogada-valida (peca-array coluna)
  "Verifica se uma jogada e valida"
  (if (<= (array-dimension peca-array 1) (- *COLUNAS* coluna))
    T 
    NIL))

;;; Identifica jogada
;;; - peca -> identificador de uma peca
;;; - rotacao -> rotacao a aplicar a uma peca
;;; - coluna -> coluna a inserir uma peca
;;; Retorna uma lista com a accao a executar
(defun identifica-accao (peca rotacao coluna)
  "Identifica accao a executar baseado na peca, rotacao da peca e coluna a inserir"
  (cond 
    ;; peca i
    ((and (equal peca 'i)(= rotacao 0)(jogada-valida peca-i0 coluna)) (list (cria-accao coluna peca-i0)))
    ((and (equal peca 'i)(= rotacao 1)(jogada-valida peca-i1 coluna)) (list (cria-accao coluna peca-i1)))
    ;; peca l
    ((and (equal peca 'l)(= rotacao 0)(jogada-valida peca-l0 coluna)) (list (cria-accao coluna peca-l0)))
    ((and (equal peca 'l)(= rotacao 1)(jogada-valida peca-l1 coluna)) (list (cria-accao coluna peca-l1)))
    ((and (equal peca 'l)(= rotacao 2)(jogada-valida peca-l2 coluna)) (list (cria-accao coluna peca-l2)))
    ((and (equal peca 'l)(= rotacao 3)(jogada-valida peca-l3 coluna)) (list (cria-accao coluna peca-l3)))
    ;; peca j
    ((and (equal peca 'j)(= rotacao 0)(jogada-valida peca-j0 coluna)) (list (cria-accao coluna peca-j0)))
    ((and (equal peca 'j)(= rotacao 1)(jogada-valida peca-j1 coluna)) (list (cria-accao coluna peca-j1)))
    ((and (equal peca 'j)(= rotacao 2)(jogada-valida peca-j2 coluna)) (list (cria-accao coluna peca-j2)))
    ((and (equal peca 'j)(= rotacao 3)(jogada-valida peca-j3 coluna)) (list (cria-accao coluna peca-j3)))
    ;; peca o
    ((and (equal peca 'o)(= rotacao 0)(jogada-valida peca-o0 coluna)) (list (cria-accao coluna peca-o0)))
    ;; peca s
    ((and (equal peca 's)(= rotacao 0)(jogada-valida peca-s0 coluna)) (list (cria-accao coluna peca-s0)))
    ((and (equal peca 's)(= rotacao 1)(jogada-valida peca-s1 coluna)) (list (cria-accao coluna peca-s1)))
    ;; peca z
    ((and (equal peca 'z)(= rotacao 0)(jogada-valida peca-z0 coluna)) (list (cria-accao coluna peca-z0)))
    ((and (equal peca 'z)(= rotacao 1)(jogada-valida peca-z1 coluna)) (list (cria-accao coluna peca-z1)))
    ;; peca t
    ((and (equal peca 't)(= rotacao 0)(jogada-valida peca-t0 coluna)) (list (cria-accao coluna peca-t0)))
    ((and (equal peca 't)(= rotacao 1)(jogada-valida peca-t1 coluna)) (list (cria-accao coluna peca-t1)))
    ((and (equal peca 't)(= rotacao 2)(jogada-valida peca-t2 coluna)) (list (cria-accao coluna peca-t2)))
    ((and (equal peca 't)(= rotacao 3)(jogada-valida peca-t3 coluna)) (list (cria-accao coluna peca-t3)))))

;;; --------------------------------------------
;;; Cria lista de accoes
;;; - estado -> estado a analizar
;;; Devolve uma lista de accoes possiveis
;;; --------------------------------------------
(defun accoes (estado)
  "Compoe a lista de accoes possiveis"
  (let ((lista-accoes NIL) (peca (first (estado-pecas-por-colocar estado))) (max-rotacao 0))
    ; define as rotacoes maximas possiveis de aplicar na peca (0 defualt)
    (cond ((or (equal peca 'i)(equal peca 's)(equal peca 'z)) (setf max-rotacao 1))
          ((or (equal peca 'l)(equal peca 'j)(equal peca 't)) (setf max-rotacao 3)))
    (dotimes (rotacao (+ max-rotacao 1) lista-accoes)
      ; analisa todas as colunas para cada difente rotacao
      (dotimes (coluna *COLUNAS*)
        ; cria a lista de accoes com todas as acceos possiveis
        (setf lista-accoes (append lista-accoes (identifica-accao peca rotacao coluna)))))))

;;; Desenha um peca num tabuleiro
;;; - tabuleiro -> tabuleiro onde desenhar
;;; - peca-array -> array da peca a desenhar
;;; - linha -> linha onde comecar desenhar
;;; - coluna -> coluna onde comecar a desenhar
;;; Nao devolve nada
(defun desenha-peca-tabuleiro (tabuleiro peca-array linha coluna)
  "Desenha uma peca numa certa posicao num tabuleiro"
 (dotimes (peca-linha (array-dimension peca-array 0))
   (dotimes (peca-coluna (array-dimension peca-array 1))
     (if (and (valida-linha (+ peca-linha linha)) (aref peca-array peca-linha peca-coluna))
      (progn
      ; insere a peca com segundo offset calculado
       (setf (aref (tabuleiro-data tabuleiro) (+ linha peca-linha) (+ coluna peca-coluna))
        (aref peca-array peca-linha peca-coluna))

       )))))


;;; Descobre base de uma coluna de uma peca
;;; - peca-array -> array de peca a analizar
;;; - coluna a analizar
;;; Devolve linha mais abaixo na coluna da peca
(defun base-peca-coluna (peca-array coluna)
  "Descobre base de uma coluna de uma peca"
 (dotimes (linha (array-dimension peca-array 0))
   (if (aref peca-array linha coluna)(return linha))))

;;; Insere uma peca num tabuleiro
;;; - tabuleiro -> tabuleiro onde inserir a peca
;;; - coluna -> coluna onde inserir a peca
;;; - peca-array -> array de peca a inserir
;;; Nao devolve nada
(defun insere-peca (tabuleiro coluna peca-array)
  "Insere uma peca numa posicao do tabuleiro"
  ; verifica se jogada e valida
  (if (jogada-valida peca-array coluna)
    (let ((linha-base 0)(linha-max 0)(linha-a-inserir 0))
      ; percorre as varias colunas de uma peca
      (dotimes (peca-coluna (array-dimension peca-array 1))
        ; define linha-max como sendo a linha maxima da coluna a analizar (coluna + coluna da peca)
        (setf linha-max (tabuleiro-altura-coluna tabuleiro (+ coluna peca-coluna)))
        ; define a linha-base como sendo a diferenca entre a linha-max e a linha mais abaixo da peca (na coluna a analizar)
        ; permite saber o deslocamento para baixo da peca (overlap de posicoes vazias na peca com posicoes preenchidas no tabuleiro)
        (setf linha-base (- linha-max (base-peca-coluna peca-array peca-coluna)))
        ; uma vez descoberta a linha-a-inserir esta nunca pode reduzir o seu valor (colunas anteriores nao o permitem)
        (if (> linha-base linha-a-inserir)
          ; assim se a nova linha-base for superior a peca tem de se deslocar para a nova linha-base
          (setf linha-a-inserir linha-base)))
      ; desenha peca na posicao descoberta
      (desenha-peca-tabuleiro tabuleiro peca-array linha-a-inserir coluna))))

;;; Calcula pontos
;;; - contador -> numero de linhas removidas
;;; Devolve o numero de pontos calculado
(defun calcula-pontos (contador)
  "Calcula pontos consoante numero de linhas removido"
  (cond ((= contador 0) 0)
    ((= contador 1) 100)
    ((= contador 2) 300)
    ((= contador 3) 500)
    ((= contador 4) 800)))

;;; -----------------------------------------------------------
;;; Aplica accao num estado
;;; - estado -> estado onde aplicar accao
;;; - accao -> accao a aplicar
;;; Devolve novo estado
;;; -----------------------------------------------------------
(defun resultado (estado-inicial accao)
  "Recebe um estado e uma accao e aplica a accao a esse estado"
  (let* ((novo-estado (copia-estado estado-inicial))(contador 0))
    ; insere a peca no tabuleiro do estado
    (insere-peca (estado-tabuleiro novo-estado) (accao-coluna accao) (accao-peca accao))
    ; atualiza as pecas colocadas do novo-estado
    (setf (estado-pecas-colocadas novo-estado) 
      (append (list (first (estado-pecas-por-colocar novo-estado))) (estado-pecas-colocadas novo-estado)))
    ; atualiza pecas-por-colocar no novo estado
    (setf (estado-pecas-por-colocar novo-estado) (rest (estado-pecas-por-colocar novo-estado)))
    ; se nao perder o jogo calcula pontos consoante linhas removidas
    (if (not (tabuleiro-topo-preenchido-p (estado-tabuleiro novo-estado))) (progn
      (dotimes (linha *LINHAS*)
        (if (tabuleiro-linha-completa-p (estado-tabuleiro novo-estado) linha) (progn
          (tabuleiro-remove-linha! (estado-tabuleiro novo-estado) linha)
          ; conta linha removida e ajusta iterador
          (decf linha)(incf contador))))
      (incf (estado-pontos novo-estado)(calcula-pontos contador))))
    novo-estado))


;;; -----------------------------------------------------------
;;; Calcula qualidade de um estado
;;; - estado
;;; Devolve qualidade de um estado
;;; -----------------------------------------------------------
(defun qualidade (estado)
  "Calcula a qualidade de um estado"
  (- 0 (estado-pontos estado)))

;;; -----------------------------------------------------------
;;; Calcula o Custo-Oportunidade
;;; - estado
;;; Devolve valor diferenca entre as pontuacoes maximas possiveis por peca e os pontos obtidos ate ao momento
;;; -----------------------------------------------------------
(defun custo-oportunidade (estado)
  "Calcula o custo oportunidade de todas as accoes tomadas"
  (let ((max-pontos 0))
    ; percorre lista de pecas
    (dolist (peca (estado-pecas-colocadas estado) max-pontos)
      ; por cada peca incrementa pontos maximos possiveis
      (cond ((equal peca 'i) (incf max-pontos 800))
        ((or (equal peca 'j)(equal peca 'l)) (incf max-pontos 500))
        ((or (equal peca 's)(equal peca 'z)(equal peca 't)(equal peca 'o)) (incf max-pontos 300))))
    ; calcula a diferenca entre pontos possiveis e obtidos
  (- max-pontos (estado-pontos estado))))


;;; ======================== ;;;
;;;         Procuras
;;; ======================== ;;;

;;; -----------------------------------------------------------
;;; Procura uma solucao para resolver o problema (Profundidade primeiro)
;;;  - problema
;;; Devolve uma sequencia de accoes em que (do inicio para o fim) representam uma solucao do problema 
;;; -----------------------------------------------------------
(defun procura-pp (problema)
	)

;;; -----------------------------------------------------------
;;; Procura com algoritmo A* para descobrir sequencia de accoes e maximizar os pontos 
;;;  - problema
;;;  - heuristica -> funcao que recebe um estado e devolve um numero que representa o custo-qualidade
;;;                  a partir desse estado ate ao melhor estado objectivo
;;; Devolve uma sequencia de accoes em que (do inicio para o fim) representam uma solucao do problema 
;;; -----------------------------------------------------------
(defun procura-A* (problema heuristica)
	)

;;; -----------------------------------------------------------
;;; Identifica a melhor procura possivel  
;;;  - tabuleiro 
;;;  - lista-pecas -> lista de pecas por colocar 
;;; Devolve uma sequencia de accoes em que (do inicio para o fim) representam uma solucao do problema 
;;; -----------------------------------------------------------
(defun procura-best (tabuleiro lista-pecas)
	)

