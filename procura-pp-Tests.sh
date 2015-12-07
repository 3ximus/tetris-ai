# Testes da procura-best
# Avalia tempo de execucao e qualidade do algoritmo para cada input
echo "Starting procura-pp tests."

echo "Running test01..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test01/input > procura-pp/stats/stats01.txt
echo "Running test02..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test02/input > procura-pp/stats/stats02.txt
echo "Running test03..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test03/input > procura-pp/stats/stats03.txt
echo "Running test04..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test04/input > procura-pp/stats/stats04.txt
echo "Running test05..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test05/input > procura-pp/stats/stats05.txt
echo "Running test06..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test06/input > procura-pp/stats/stats06.txt
echo "Running test07..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test07/input > procura-pp/stats/stats07.txt
echo "Running test08..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test08/input > procura-pp/stats/stats08.txt
echo "Running test09..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test09/input > procura-pp/stats/stats09.txt
echo "Running test10..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test10/input > procura-pp/stats/stats10.txt
echo "Running test11..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test11/input > procura-pp/stats/stats11.txt
echo "Running test12..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test12/input > procura-pp/stats/stats12.txt
echo "Running test13..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test13/input > procura-pp/stats/stats13.txt
echo "Running test14..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test14/input > procura-pp/stats/stats14.txt
echo "Running test15..........."
clisp -i tetris.lisp -i utils.lisp < procura-pp/tests/test15/input > procura-pp/stats/stats15.txt

echo "Filtering stats.........."
tail -n +23 procura-pp/stats/stats01.txt > procura-pp/stats/FilteredStats01.txt # Test01
tail -n +23 procura-pp/stats/stats02.txt > procura-pp/stats/FilteredStats02.txt # Test02
tail -n +23 procura-pp/stats/stats03.txt > procura-pp/stats/FilteredStats03.txt # Test03
tail -n +23 procura-pp/stats/stats04.txt > procura-pp/stats/FilteredStats04.txt # Test04
tail -n +23 procura-pp/stats/stats05.txt > procura-pp/stats/FilteredStats05.txt # Test05
tail -n +23 procura-pp/stats/stats06.txt > procura-pp/stats/FilteredStats06.txt # Test06
tail -n +23 procura-pp/stats/stats07.txt > procura-pp/stats/FilteredStats07.txt # Test07
tail -n +23 procura-pp/stats/stats08.txt > procura-pp/stats/FilteredStats08.txt # Test08
tail -n +23 procura-pp/stats/stats09.txt > procura-pp/stats/FilteredStats09.txt # Test09
tail -n +23 procura-pp/stats/stats10.txt > procura-pp/stats/FilteredStats10.txt # Test10
tail -n +23 procura-pp/stats/stats11.txt > procura-pp/stats/FilteredStats11.txt # Test11
tail -n +23 procura-pp/stats/stats12.txt > procura-pp/stats/FilteredStats12.txt # Test12
tail -n +23 procura-pp/stats/stats13.txt > procura-pp/stats/FilteredStats13.txt # Test13
tail -n +23 procura-pp/stats/stats14.txt > procura-pp/stats/FilteredStats14.txt # Test14
tail -n +23 procura-pp/stats/stats15.txt > procura-pp/stats/FilteredStats15.txt # Test15

# Apaga ficheiros stats.txt de cada teste.
# Ficheiro stat.txt tem o output produzido pelo clisp
find procura-pp/stats/ -name "stats*.txt" -type f -delete