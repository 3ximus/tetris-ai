# Testes da procura-best
# Avalia tempo de execucao e qualidade do algoritmo para cada input
echo "Starting procura-A-star tests."

echo "Running test01..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test01/input > procura-A-star/stats/stats01.txt
echo "Running test02..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test02/input > procura-A-star/stats/stats02.txt
echo "Running test03..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test03/input > procura-A-star/stats/stats03.txt
echo "Running test04..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test04/input > procura-A-star/stats/stats04.txt
echo "Running test05..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test05/input > procura-A-star/stats/stats05.txt
echo "Running test06..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test06/input > procura-A-star/stats/stats06.txt
echo "Running test07..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test07/input > procura-A-star/stats/stats07.txt
echo "Running test08..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test08/input > procura-A-star/stats/stats08.txt
echo "Running test09..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test09/input > procura-A-star/stats/stats09.txt
echo "Running test10..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test10/input > procura-A-star/stats/stats10.txt
echo "Running test11..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test11/input > procura-A-star/stats/stats11.txt
echo "Running test12..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test12/input > procura-A-star/stats/stats12.txt
echo "Running test13..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test13/input > procura-A-star/stats/stats13.txt
echo "Running test14..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test14/input > procura-A-star/stats/stats14.txt
echo "Running test15..........."
clisp -i tetris.lisp -i utils.lisp < procura-A-star/tests/test15/input > procura-A-star/stats/stats15.txt

echo "Filtering stats.........."
tail -n +23 procura-A-star/stats/stats01.txt > procura-A-star/stats/FilteredStats01.txt # Test01
tail -n +23 procura-A-star/stats/stats02.txt > procura-A-star/stats/FilteredStats02.txt # Test02
tail -n +23 procura-A-star/stats/stats03.txt > procura-A-star/stats/FilteredStats03.txt # Test03
tail -n +23 procura-A-star/stats/stats04.txt > procura-A-star/stats/FilteredStats04.txt # Test04
tail -n +23 procura-A-star/stats/stats05.txt > procura-A-star/stats/FilteredStats05.txt # Test05
tail -n +23 procura-A-star/stats/stats06.txt > procura-A-star/stats/FilteredStats06.txt # Test06
tail -n +23 procura-A-star/stats/stats07.txt > procura-A-star/stats/FilteredStats07.txt # Test07
tail -n +23 procura-A-star/stats/stats08.txt > procura-A-star/stats/FilteredStats08.txt # Test08
tail -n +23 procura-A-star/stats/stats09.txt > procura-A-star/stats/FilteredStats09.txt # Test09
tail -n +23 procura-A-star/stats/stats10.txt > procura-A-star/stats/FilteredStats10.txt # Test10
tail -n +23 procura-A-star/stats/stats11.txt > procura-A-star/stats/FilteredStats11.txt # Test11
tail -n +23 procura-A-star/stats/stats12.txt > procura-A-star/stats/FilteredStats12.txt # Test12
tail -n +23 procura-A-star/stats/stats13.txt > procura-A-star/stats/FilteredStats13.txt # Test13
tail -n +23 procura-A-star/stats/stats14.txt > procura-A-star/stats/FilteredStats14.txt # Test14
tail -n +23 procura-A-star/stats/stats15.txt > procura-A-star/stats/FilteredStats15.txt # Test15

# Apaga ficheiros stats.txt de cada teste.
# Ficheiro stat.txt tem o output produzido pelo clisp
find procura-A-star/stats/ -name "stats*.txt" -type f -delete