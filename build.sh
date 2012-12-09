# make examples
cd tests
rm bar* foo* baz
printf "#include <iostream>\nvoid foo() { std::cout << \"Hello, foo!\"; }" > foo.cpp
printf "#include <iostream>\nextern int foo (); void bar() { std::cout << \"Hello, bar!\"; } int main() { foo(); bar(); return 0; }" > bar.cpp
cd ..
ocamlopt unix.cmxa -o remodel src/remodel.ml 
PATH=$PATH:`pwd`
cd tests
remodel
# grab md5s of bar.o and bar.cpp - these should not change after running
# check whether this machine has md5 on it
bar1=`md5 -q bar.cpp`
bar2=`md5 -q bar.o`
foo1=`md5 -q foo.cpp`
foo2=`md5 -q foo.o`
baz=`md5 -q baz`

printf "Test case 1 : run DEFAULT for the first time..."
remodel 2> test1
a=`wc -l test1 | awk '{print $1}'`
if [[ $a -gt 1 ]]
	then echo "FAIL"
else echo "PASS"
fi

printf "Test case 2 : remove foo.cpp and run remodel on bar.o..."
rm foo.cpp
remodel bar.o 2> test2
a=`wc -l test1 | awk '{print $1}'`
if [[ $a -lt 2 && $bar1 == `md5 -q bar.cpp` && $bar2 == `md5 -q bar.o` && $foo2 == `md5 -q foo.o` && $baz == `md5 -q baz` ]]
	then echo "PASS"
else echo "FAIL"
fi

printf "Test case 3 : restore foo.cpp and run remodel on default..."
printf "#include <iostream>\nvoid foo() { std::cout << \"Hello, foo!\"; }" > foo.cpp
remodel
a=`wc -l test1 | awk '{print $1}'`
if [[ $a -lt 2 && `md5 -q bar.cpp` == $bar1 && `md5 -q bar.o` == $bar2 && `md5 -q foo.cpp` != $foo1 && `md5 -q foo.o` != $foo2 && `md5 -q baz` == $baz ]]
	then echo "PASS"
else echo "FAIL"
fi
