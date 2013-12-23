#!/bin/bash
#set -e
PATH=$PATH:`pwd`

remodel=$1
remodelfile=$2
if [[ ! $remodel || ! $remodelfile ]] ; then
  echo "ERROR : Please provide arguments for the remodel executable and the name of the remodel file"
  exit -1
fi

rm -rf tests
echo "-------------------------------------------"
echo "Making tests directory"
echo "-------------------------------------------"
mkdir tests
echo "Creating tests directory"
echo "-------------------------------------------"

cd tests
#################### create files ####################
function make_files {
    echo "Creating test files"
    printf "DEFAULT <- baz baz <- foo.o, bar.o: \"g++ foo.o bar.o -o baz\"
foo.o <- foo.cpp : \"g++ -c foo.cpp -o foo.o\"bar.o <- bar.cpp: \"g++ -c bar.cpp -o bar.o\"
existence <- existence.ml, a.txt : \"ocamlc -o existence existence.ml ; ./existence\"
a.txt, b.txt<- do.sh:\"chmod +x do.sh ; ./do.sh\"
do.sh<-REMODELFILE:\"echo 'touch a.txt; touch b.txt' > do.sh\"" > emmas_remodel
    
    printf "DEFAULT <- baz 
baz <- foo.o, bar.o: \"g++ foo.o bar.o -o baz\"
foo.o <- foo.cpp : \"g++ -c foo.cpp -o foo.o\"
bar.o <- bar.cpp: \"g++ -c bar.cpp -o bar.o\"
existence <- existence.ml, a.txt : \"ocamlc -o existence existence.ml ; ./existence\"
a.txt, b.txt<- do.sh:\"chmod +x do.sh ; ./do.sh\"
do.sh<-REMODELFILE:\"echo 'touch a.txt; touch b.txt' > do.sh\"" > linebreak_delimited_remodel
    
    printf "#include <iostream>\nvoid foo() { std::cout << \"Hello, foo!\"; }" > foo.cpp
    
    printf "#include <iostream>\nextern int foo (); void bar() { std::cout << \"Hello, bar!\"; } int main() { foo(); bar(); return 0; }" > bar.cpp
    
    printf "output_string stdout (string_of_bool (Sys.file_exists \"a.txt\"))" > existence.ml
    echo "-------------------------------------------"
}

function remove_source_files {
    rm emmas_remodel
    rm linebreak_delimiter_remodel
    rm foo.cpp
    rm bar.cpp
    rm existence.ml
}

function rmfn {
    if [[ -e $1 ]]
    then rm $1
    fi
}

function remove_generated_files {
    rmfn do.sh
    rmfn existence.cmo
    rmfn foo.o
    rmfn test2.1
    rmfn a.txt
    rmfn bar.o
    rmfn existence
    rmfn test1
    rmfn test3
    rmfn b.txt
    rmfn baz
    rmfn existence.cmi
    rmfn test2
    rmfn test4
}

#################### Test Case 1 ####################
function test_case_1 {
    
    rmf=$1
    output="Test case 1 : run DEFAULT for the first time..."
    mv $rmf $remodelfile
    echo `$remodel 1> test1.out 2> test1.err`
    # grab md5s of bar.o and bar.cpp - these should not change after running
    # check whether this machine has md5 on it
    bar1=`md5 -q bar.cpp`
    foo1=`md5 -q foo.cpp`
    
    a=`wc -l test1.err | awk '{print $1}'`

    if [[ $a -gt 1 ]] ; then
	echo $output"FAIL -- error in program (see test1.err)" 
    elif [[ !( -e bar.o ) ]] ; then 
	echo $output"FAIL -- did not create bar.o" 
    elif [[ !( -e foo.o ) ]] ; then 
	echo $output"FAIL -- did not create foo.o"
    elif [[ !( -e baz ) ]] ; then 
	echo $output"FAIL -- did not create baz"
    else echo $output"PASS"
    fi

    mv $remodelfile $rmf

}

make_files
test_case_1 emmas_remodel
remove_generated_files
test_case_1 linebreak_delimited_remodel

sleep 3

bar2=`md5 -q bar.o`
foo2=`md5 -q foo.o`
baz=`md5 -q baz`


#################### Test Case 2 #################### 
function test_case_2 {

    rmf=$1
    mv $rmf $remodelfile

    output="Test case 2 : remove foo.cpp and run remodel on bar.o..."
    if [[ -e foo.cpp ]] ; then 
	rm foo.cpp
    fi

    echo `$remodel bar.o 1> test2.out 2> test2.err`
    a=`wc -l test2.err | awk '{print $1}'`

    if [[ !( $a == 0 ) ]] ; then 
	echo $output"FAIL -- executable had errors"
    elif [[ !( $bar1 == `md5 -q bar.cpp` ) ]] ; then
	echo $output"FAIL -- bar.cpp has changed"
    elif [[ !( $bar2 == `md5 -q bar.o` ) ]] ; then
	echo $output"FAIL -- bar.o has been re-compiled"
    elif [[ !( $foo2 == `md5 -q foo.o` ) ]] ; then
	echo $output"FAIL -- foo.o has been re-compiled"
    elif [[ !( $baz == `md5 -q baz`) ]] ; then 
	echo $output"FAIL -- baz has been rebuilt"
    else echo $output"PASS"
    fi

    mv $remodelfile $rmf

}

test_case_2 emmas_remodel
test_case_2 linebreak_delimited_remodel
sleep 3


#################### Test Case 2.1 #################### 
function test_case_2_1 {

    rmf=$1
    mv $rmf $remodelfile

    output="Test case 2.1 : run remodel on DEFAULT (PASS means build failed)..."
    echo `$remodel 1> test2.1.out 2> test2.1.err`
    a=`wc -l test2.1.err | awk '{print $1}'`
    if [[ $a -gt 0 ]]
    then echo $output"PASS"
    else echo $output"FAIL -- program did not notice that foo.cpp was removed; this should have generated errors"
    fi

    mv $remodelfile $rmf

}

test_case_2_1 emmas_remodel
test_case_2_1 linebreak_delimited_remodel
sleep 3

#################### Test Case 3 #################### 
function test_case_3 {

    rmf=$1
    mv $rmf $remodelfile

    output="Test case 3 : rewrite foo.cpp and run remodel on DEFAULT..."
    printf "#include <iostream>\nvoid foo() { std::cout << \"Yo, foo!\"; }" > foo.cpp

    echo `remodel 2> test3.err 1> test3.out`
    a=`wc -l test3.err | awk '{print $1}'`

    if [[ !( $a == 0 ) ]] ; then
	echo $output"FAIL -- executable generated errors"
    elif [[ !( `md5 -q bar.cpp` == $bar1 ) ]] ; then
	echo $output"FAIL -- bar.cpp hash changed"
    elif [[ !( `md5 -q bar.o` == $bar2 ) ]] ; then
	echo $output"FAIL -- recompiled bar.o"
    elif [[ `md5 -q foo.cpp` == $foo1 ]] ; then
	echo $output"FAIL -- foo.cpp not changed"
    elif [[ `md5 -q foo.o` == $foo2 ]] ; then
	echo $output"FAIL -- foo.o not recompiled" 
    elif [[ `md5 -q baz` == $baz ]] ; then
	echo $output"FAIL -- baz not recompiled"
    else echo $output"PASS"
    fi

    mv $remodelfile $rmf

}

test_case_3 emmas_remodel
test_case_3 linebreak_delimited_remodel
sleep 3

foo1=`md5 -q foo.cpp`
foo2=`md5 -q foo.o`
baz=`md5 -q baz`

#################### Test Case 4 #################### 
function test_case_4 {

    rmf=$1
    mv $rmf $remodelfile

    output="Test case 4 : dual targets and a target with a command and no dependencies on existence.ml..."
    echo `$remodel existence 2> test4.err 1> test4.out`
    a=`wc -l test4.err | awk '{print $1}'`
    if [[ !( $a == 0 ) ]] ; then 
	echo $output"FAIL -- executable generated errors"
    elif [[ !( -e do.sh ) ]] ; then 
	echo $output"FAIL -- do.sh not created"
    elif [[ !( -e a.txt ) ]] ; then 
	echo $output"FAIL -- a.txt not created"
    elif [[ !( -e b.txt ) ]] ; then
	echo $output"FAIL -- b.txt not created"
    else echo $output"PASS"
    fi

    mv $remodelfile $rmf
}

test_case_4 emmas_remodel
test_case_4 linebreak_delimited_remodel

#################### Test Case 5 #################### 
function test_case_5 {
    
    rmf=$1
    mv $rmf $remodelfile
    
    output="Test case 5 : change remodelfile..."
    echo "do.sh <- sturff : \" sleep 5 ; touch sturff \" " >> $remodelfile
    oldtime=`ls -mtime do.sh | awk '{print $1}'`
    aoldtime=`ls -mtime a.txt | awk '{print $1}'`

    echo `$remodel 2> test5.err 1> test5.out`
    a=`wc -l test5.err | awk '{print $1}'`

    # should re-generate do.sh, but none of its children because the md5 hash should be the same
    if [[ $oldtime==`ls -mtime do.sh | awk '{print $8}'` ]] ; then
	echo $output"FAIL -- did not regenerate do.sh"
    elif [[ ! ( $aoldtime == `ls -mtime a.txt | awk '{print $1}'` ) ]] ; then
	echo $output"FAIL -- should not be regenerating a.txt."
    else echo $output"PASS"
    fi

    mv $remodelfile $rmf
}

test_case_5 emmas_remodel
test_case_5 linebreak_delimited_remodel
