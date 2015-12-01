# Testes da procura-best
# Avalia tempo de execucao e qualidade do algoritmo para cada input
echo "Starting tests."

echo "Running test01..........."
clisp -i tetris.lisp -i utils.lisp < test01/input > test01/stats.txt
echo "Running test02..........."
clisp -i tetris.lisp -i utils.lisp < test02/input > test02/stats.txt
echo "Running test03........... [ 90 pecas ]"
clisp -i tetris.lisp -i utils.lisp < test03/input > test03/stats.txt
echo "Running test24..........."
clisp -i tetris.lisp -i utils.lisp < test24/input > test24/stats.txt
echo "Running test25..........."
clisp -i tetris.lisp -i utils.lisp < test25/input > test25/stats.txt

echo "diff test01 outputs"
diff test01/output test01/output.txt
echo "diff test02 outputs"
diff test02/output test02/output.txt
echo "diff test03 outputs"
diff test03/output test03/output.txt
echo "diff test24 outputs"
diff test24/output test24/output.txt
echo "diff test25 outputs"
diff test25/output test25/output.txt

echo "Filtering stats.........."
tail -n +23 test01/stats.txt > test01/FilteredStats.txt # Test01
tail -n +23 test02/stats.txt > test02/FilteredStats.txt # Test02
tail -n +23 test03/stats.txt > test03/FilteredStats.txt # Test03
tail -n +23 test24/stats.txt > test24/FilteredStats.txt # Test24
tail -n +23 test25/stats.txt > test25/FilteredStats.txt # Test25

# Apaga ficheiros stats.txt de cada teste.
# Ficheiro stat.txt tem o output produzido pelo clisp
find . -name "stats.txt" -type f -delete