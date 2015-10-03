;Funcao maximo de 3 numeros
(defun max3 (x y z)
  (if (> x y z)
      x
    (if (> y z x)
        y
      z)))

