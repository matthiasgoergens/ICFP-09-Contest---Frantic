#!/usr/bin/env sh
../HaskellVM/Analyze full ../task/bin1.obf > bin1.dot
dot bin1.dot -Tps -O
../HaskellVM/Analyze full ../task/bin2.obf > bin2.dot
dot bin2.dot -Tps -O
../HaskellVM/Analyze full ../task/bin3.obf > bin3.dot
dot bin3.dot -Tps -O