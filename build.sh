#!/bin/sh
set -e
rm -rf tests
echo "-------------------------------------------"
echo "Making tests directory"
echo "-------------------------------------------"
mkdir tests
echo "Creating new test files in tests directory"
echo "-------------------------------------------"
echo "-------------------------------------------"
cd tests
printf "DEFAULT <- baz baz <- foo.o, bar.o: \"g++ foo.o bar.o -o baz\"
foo.o <- foo.cpp : \"g++ -c foo.cpp -o foo.o\"bar.o <- bar.cpp: \"g++ -c bar.cpp -o bar.o\"
existence <- existence.ml, a.txt : \"ocamlc -o existence existence.ml ; ./existence\"
a.txt, b.txt<- do.sh:\"chmod +x do.sh ; ./do.sh\"
do.sh<-REMODELFILE:\"echo 'touch a.txt; touch b.txt' > do.sh\"" > REMODELFILE
printf "#include <iostream>\nvoid foo() { std::cout << \"Hello, foo!\"; }" > foo.cpp
printf "#include <iostream>\nextern int foo (); void bar() { std::cout << \"Hello, bar!\"; } int main() { foo(); bar(); return 0; }" > bar.cpp
printf "output_string stdout (string_of_bool (Sys.file_exists \"a.txt\"))" > existence.ml
cd ..
echo "-------------------------------------------"
echo "Compiling the program"
ocamlbuild -libs unix src/remodel.native 1> ocamlbuild_log
mv remodel.native remodel
PATH=$PATH:`pwd`
cd tests
echo "-------------------------------------------"
printf "Test case 1 : run DEFAULT for the first time..."
remodel
# grab md5s of bar.o and bar.cpp - these should not change after running
# check whether this machine has md5 on it
bar1=`md5 -q bar.cpp`
foo1=`md5 -q foo.cpp`

remodel 2> test1
a=`wc -l test1 | awk '{print $1}'`
if [[ $a -gt 1 || !( -e bar.o )  || !( -e foo.o ) || !( -e baz ) ]]
	then echo "FAIL"
else echo "PASS"
fi

sleep 3

bar2=`md5 -q bar.o`
foo2=`md5 -q foo.o`
baz=`md5 -q baz`

output="Test case 2 : remove foo.cpp and run remodel on bar.o..."
rm foo.cpp
remodel bar.o 2> test2
a=`wc -l test2 | awk '{print $1}'`
if [[ $a == 0 && $bar1 == `md5 -q bar.cpp` && $bar2 == `md5 -q bar.o` && $foo2 == `md5 -q foo.o` && $baz == `md5 -q baz` ]]
	then echo $output"PASS"
else echo $output"FAIL"
fi

sleep 3

output="Test case 2.1 : run remodel on DEFAULT (PASS means build failed)..."
remodel 2> test2.1
a=`wc -l test2.1 | awk '{print $1}'`
if [[ $a -gt 0 ]]
	then echo $output"PASS"
else echo $output"FAIL"
fi

sleep 3

output="Test case 3 : rewrite foo.cpp and run remodel on DEFAULT..."
printf "#include <iostream>\nvoid foo() { std::cout << \"Yo, foo!\"; }" > foo.cpp
remodel 2> test3
a=`wc -l test3 | awk '{print $1}'`
if [[ $a == 0 && `md5 -q bar.cpp` == $bar1 && `md5 -q bar.o` == $bar2 && `md5 -q foo.cpp` != $foo1 && `md5 -q foo.o` != $foo2 && `md5 -q baz` != $baz ]]
	then echo $output"PASS"
else echo $output"FAIL"
fi

sleep 3

foo1=`md5 -q foo.cpp`
foo2=`md5 -q foo.o`
baz=`md5 -q baz`

output="Test case 4 : dual targets and a target with a command and no dependencies on existence.ml..."
remodel existence 2> test4 1> ".crap"
a=`wc -l test4 | awk '{print $1}'`
if [[ $a == 0 && -e do.sh && -e a.txt && -e b.txt ]]
	then echo $output"PASS"
else echo $output"FAIL"
fi