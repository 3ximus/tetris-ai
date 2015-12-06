# Testes da procura-best
# Avalia tempo de execucao e qualidade do algoritmo para cada input
echo "Starting tests."

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
echo "Running test06..........."
clisp -i tetris.lisp -i utils.lisp < tests/test06/input > tests/test06/stats.txt
echo "Running test07..........."
clisp -i tetris.lisp -i utils.lisp < tests/test07/input > tests/test07/stats.txt
echo "Running test08..........."
clisp -i tetris.lisp -i utils.lisp < tests/test08/input > tests/test08/stats.txt
echo "Running test09..........."
clisp -i tetris.lisp -i utils.lisp < tests/test09/input > tests/test09/stats.txt
echo "Running test10..........."
clisp -i tetris.lisp -i utils.lisp < tests/test10/input > tests/test10/stats.txt
echo "Running test11..........."
clisp -i tetris.lisp -i utils.lisp < tests/test11/input > tests/test11/stats.txt
echo "Running test12..........."
clisp -i tetris.lisp -i utils.lisp < tests/test12/input > tests/test12/stats.txt
echo "Running test13..........."
clisp -i tetris.lisp -i utils.lisp < tests/test13/input > tests/test13/stats.txt
echo "Running test14..........."
clisp -i tetris.lisp -i utils.lisp < tests/test14/input > tests/test14/stats.txt
echo "Running test15..........."
clisp -i tetris.lisp -i utils.lisp < tests/test15/input > tests/test15/stats.txt
echo "Running test16..........."
clisp -i tetris.lisp -i utils.lisp < tests/test16/input > tests/test16/stats.txt
echo "Running test17..........."
clisp -i tetris.lisp -i utils.lisp < tests/test17/input > tests/test17/stats.txt
echo "Running test18..........."
clisp -i tetris.lisp -i utils.lisp < tests/test18/input > tests/test18/stats.txt
echo "Running test19..........."
clisp -i tetris.lisp -i utils.lisp < tests/test19/input > tests/test19/stats.txt
echo "Running test20..........."
clisp -i tetris.lisp -i utils.lisp < tests/test20/input > tests/test20/stats.txt
echo "Running test21..........."
clisp -i tetris.lisp -i utils.lisp < tests/test21/input > tests/test21/stats.txt
echo "Running test22..........."
clisp -i tetris.lisp -i utils.lisp < tests/test22/input > tests/test22/stats.txt
echo "Running test23..........."
clisp -i tetris.lisp -i utils.lisp < tests/test23/input > tests/test23/stats.txt
echo "Running test24..........."
clisp -i tetris.lisp -i utils.lisp < tests/test24/input > tests/test24/stats.txt
echo "Running test25..........."
clisp -i tetris.lisp -i utils.lisp < tests/test25/input > tests/test25/stats.txt

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
echo "diff test06 outputs"
diff tests/test06/output tests/test06/output.txt
echo "diff test07 outputs"
diff tests/test07/output tests/test07/output.txt
echo "diff test08 outputs"
diff tests/test08/output tests/test08/output.txt
echo "diff test09 outputs"
diff tests/test09/output tests/test09/output.txt
echo "diff test10 outputs"
diff tests/test10/output tests/test10/output.txt
echo "diff test11 outputs"
diff tests/test11/output tests/test11/output.txt
echo "diff test12 outputs"
diff tests/test12/output tests/test12/output.txt
echo "diff test13 outputs"
diff tests/test13/output tests/test13/output.txt
echo "diff test14 outputs"
diff tests/test14/output tests/test14/output.txt
echo "diff test15 outputs"
diff tests/test15/output tests/test15/output.txt
echo "diff test16 outputs"
diff tests/test16/output tests/test16/output.txt
echo "diff test17 outputs"
diff tests/test17/output tests/test17/output.txt
echo "diff test18 outputs"
diff tests/test18/output tests/test18/output.txt
echo "diff test19 outputs"
diff tests/test19/output tests/test19/output.txt
echo "diff test20 outputs"
diff tests/test20/output tests/test20/output.txt
echo "diff test21 outputs"
diff tests/test21/output tests/test21/output.txt
echo "diff test22 outputs"
diff tests/test22/output tests/test22/output.txt
echo "diff test23 outputs"
diff tests/test23/output tests/test23/output.txt
echo "diff test24 outputs"
diff tests/test24/output tests/test24/output.txt
echo "diff test25 outputs"
diff tests/test25/output tests/test25/output.txt

echo "Filtering stats.........."
tail -n +23 tests/test01/stats.txt > tests/test01/FilteredStats.txt # Test01
tail -n +23 tests/test02/stats.txt > tests/test02/FilteredStats.txt # Test02
tail -n +23 tests/test03/stats.txt > tests/test03/FilteredStats.txt # Test03
tail -n +23 tests/test04/stats.txt > tests/test04/FilteredStats.txt # Test04
tail -n +23 tests/test05/stats.txt > tests/test05/FilteredStats.txt # Test05
tail -n +23 tests/test06/stats.txt > tests/test06/FilteredStats.txt # Test06
tail -n +23 tests/test07/stats.txt > tests/test07/FilteredStats.txt # Test07
tail -n +23 tests/test08/stats.txt > tests/test08/FilteredStats.txt # Test08
tail -n +23 tests/test09/stats.txt > tests/test09/FilteredStats.txt # Test09
tail -n +23 tests/test10/stats.txt > tests/test10/FilteredStats.txt # Test10
tail -n +23 tests/test11/stats.txt > tests/test11/FilteredStats.txt # Test11
tail -n +23 tests/test12/stats.txt > tests/test12/FilteredStats.txt # Test12
tail -n +23 tests/test13/stats.txt > tests/test13/FilteredStats.txt # Test13
tail -n +23 tests/test14/stats.txt > tests/test14/FilteredStats.txt # Test14
tail -n +23 tests/test15/stats.txt > tests/test15/FilteredStats.txt # Test15
tail -n +23 tests/test16/stats.txt > tests/test16/FilteredStats.txt # Test16
tail -n +23 tests/test17/stats.txt > tests/test17/FilteredStats.txt # Test17
tail -n +23 tests/test18/stats.txt > tests/test18/FilteredStats.txt # Test18
tail -n +23 tests/test19/stats.txt > tests/test19/FilteredStats.txt # Test19
tail -n +23 tests/test20/stats.txt > tests/test20/FilteredStats.txt # Test20
tail -n +23 tests/test21/stats.txt > tests/test21/FilteredStats.txt # Test21
tail -n +23 tests/test22/stats.txt > tests/test22/FilteredStats.txt # Test22
tail -n +23 tests/test23/stats.txt > tests/test23/FilteredStats.txt # Test23
tail -n +23 tests/test24/stats.txt > tests/test24/FilteredStats.txt # Test24
tail -n +23 tests/test25/stats.txt > tests/test25/FilteredStats.txt # Test25

# Apaga ficheiros stats.txt de cada teste.
# Ficheiro stat.txt tem o output produzido pelo clisp
find . -name "stats.txt" -type f -delete