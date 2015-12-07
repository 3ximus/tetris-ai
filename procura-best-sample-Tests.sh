# procura-best-sample tests

echo "Starting sample tests."

echo "Running test01..........."
clisp -i tetris.lisp -i utils.lisp < tests/test01/input > tests/test01/stats.txt
echo "Running test02..........."
clisp -i tetris.lisp -i utils.lisp < tests/test02/input > tests/test02/stats.txt
echo "Running test03..........."
clisp -i tetris.lisp -i utils.lisp < tests/test03/input > tests/test03/stats.txt
echo "Running test04..........."
clisp -i tetris.lisp -i utils.lisp < tests/test04/input > tests/test04/stats.txt
echo "Running test05..........."
clisp -i tetris.lisp -i utils.lisp < tests/test05/input > tests/test05/stats.txt

echo "diff test01 outputs"
diff tests/test01/output tests/test01/output.txt
echo "diff test02 outputs"
diff tests/test02/output tests/test02/output.txt
echo "diff test03 outputs"
diff tests/test03/output tests/test03/output.txt
echo "diff test04 outputs"
diff tests/test04/output tests/test04/output.txt
echo "diff test05 outputs"
diff tests/test05/output tests/test05/output.txt

echo "Filtering stats.........."
tail -n +23 tests/test01/stats.txt > tests/test01/FilteredStats.txt # Test01
tail -n +23 tests/test02/stats.txt > tests/test02/FilteredStats.txt # Test02
tail -n +23 tests/test03/stats.txt > tests/test03/FilteredStats.txt # Test03
tail -n +23 tests/test04/stats.txt > tests/test04/FilteredStats.txt # Test04
tail -n +23 tests/test05/stats.txt > tests/test05/FilteredStats.txt # Test05

# Apaga ficheiros stats.txt de cada teste.
# Ficheiro stat.txt tem o output produzido pelo clisp
find . -name "stats.txt" -type f -delete