#!/bin/sh
set -e
rm -rf tests
mkdir tests

cd tests
printf "#include <iostream>\nvoid foo() { std::cout << \"Hello, foo!\"; }" > foo.cpp
printf "#include <iostream>\nextern int foo (); void bar() { std::cout << \"Hello, bar!\"; } int main() { foo(); bar(); return 0; }" > bar.cpp
printf "output_string stdout (string_of_bool (Sys.file_exists \"a.txt\"))" > existence.ml
cd ..

ocamlopt unix.cmxa -o remodel src/remodel.ml 
PATH=$PATH:`pwd`
cd tests
remodel
# grab md5s of bar.o and bar.cpp - these should not change after running
# check whether this machine has md5 on it
bar1=`md5 -q bar.cpp`
foo1=`md5 -q foo.cpp`

printf "Test case 1 : run DEFAULT for the first time..."
remodel 2> test1
a=`wc -l test1 | awk '{print $1}'`
if [[ $a -gt 1 || !( -e bar.o )  || !( -e foo.o ) || !( -e baz ) ]]
	then echo "FAIL"
else echo "PASS"
fi

bar2=`md5 -q bar.o`
foo2=`md5 -q foo.o`
baz=`md5 -q baz`

printf "Test case 2 : remove foo.cpp and run remodel on bar.o..."
rm foo.cpp
remodel bar.o 2> test2
a=`wc -l test2 | awk '{print $1}'`
if [[ $a -lt 2 && $bar1 == `md5 -q bar.cpp` && $bar2 == `md5 -q bar.o` && $foo2 == `md5 -q foo.o` && $baz == `md5 -q baz` ]]
	then echo "PASS"
else echo "FAIL"
fi

printf "Test case 3 : rewrite foo.cpp and run remodel on DEFAULT..."
printf "#include <iostream>\nvoid foo() { std::cout << \"Yo, foo!\"; }" > foo.cpp
remodel 2> test3
a=`wc -l test3 | awk '{print $1}'`
if [[ $a -lt 2 && `md5 -q bar.cpp` == $bar1 && `md5 -q bar.o` == $bar2 && `md5 -q foo.cpp` != $foo1 && `md5 -q foo.o` != $foo2 && `md5 -q baz` != $baz ]]
	then echo "PASS"
else echo "FAIL"
fi

foo1=`md5 -q foo.cpp`
foo2=`md5 -q foo.o`
baz=`md5 -q baz`

printf "Test case 4 : dual targets and a target with a command and no dependencies on existence.ml..."
remodel existence 2> test4 1> out4
a=`wc -l test4 | awk '{print $1}'`
if [[ $a -lt 2 && -e do.sh && -e a.txt && -e b.txt ]]
	then echo "PASS"
else echo "FAIL"
fi