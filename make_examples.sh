# make examples
touch baz foo.o bar.o foo.cpp bar.cpp
cd tests
printf "#include <iostream>\nvoid foo() { std::cout << \"Hello, foo!\"; }" > foo.cpp
printf "#include <iostream>\nextern int foo (); void bar() { std::cout << \"Hello, bar!\"; } int main() { foo(); bar(); return 0; }" > bar.cpp