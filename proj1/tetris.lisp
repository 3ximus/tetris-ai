(defun cria-accao (posicao peca)
  (cons posicao peca))

(defun accao-coluna (accao)
  (first accao))

(defun accao-peca (accao)
  (rest accao))

(defstruct tabuleiro (linhas 18) (colunas 10))

(defun cria-tabuleiro ()
  (let* ((tabuleiro (make-tabuleiro))
         (linhas (tabuleiro-linhas tabuleiro))
         (colunas (tabuleiro-colunas tabuleiro)))
    (make-array '(linhas colunas))))

    