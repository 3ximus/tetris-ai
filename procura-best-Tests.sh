# Testes da procura-best
# Avalia tempo de execucao e qualidade do algoritmo para cada input
echo "Starting tests."
echo "Running test24..........."
clisp -i tetris.lisp -i utils.lisp < test24/input > test24/stats.txt
echo "Running test25..........."
clisp -i tetris.lisp -i utils.lisp < test25/input > test25/stats.txt
echo "diff test24 outputs"
diff test25/output test25/output.txt
echo "diff test25 outputs"
diff test25/output test25/output.txt
