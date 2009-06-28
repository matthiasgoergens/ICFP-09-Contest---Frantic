#!/bin/sh

./run_task1_controller.sh ../task/bin1.obf 1001 > 1001.input
cat 1001.input | ./run_java.sh ../task/bin1.obf 1001 > 1001.output
./createSub.sh 1001.input 1001.output ../submission/1001-auto.osf
./run_submission_printer.sh ../submission/1001-auto.osf > ../submission/1001-auto.txt

./run_task1_controller.sh ../task/bin1.obf 1002 > 1002.input
cat 1002.input | ./run_java.sh ../task/bin1.obf 1002 > 1002.output
./createSub.sh 1002.input 1002.output ../submission/1002-auto.osf
./run_submission_printer.sh ../submission/1002-auto.osf > ../submission/1002-auto.txt

./run_task1_controller.sh ../task/bin1.obf 1003 > 1003.input
cat 1003.input | ./run_java.sh ../task/bin1.obf 1003 > 1003.output
./createSub.sh 1003.input 1003.output ../submission/1003-auto.osf
./run_submission_printer.sh ../submission/1003-auto.osf > ../submission/1003-auto.txt

./run_task1_controller.sh ../task/bin1.obf 1004 > 1004.input
cat 1004.input | ./run_java.sh ../task/bin1.obf 1004 > 1004.output
./createSub.sh 1004.input 1004.output ../submission/1004-auto.osf
./run_submission_printer.sh ../submission/1004-auto.osf > ../submission/1004-auto.txt
