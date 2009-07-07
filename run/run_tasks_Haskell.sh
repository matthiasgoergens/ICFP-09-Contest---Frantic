#!/bin/sh


for T in 1001 1002 1003 1004; do
    ./run_controller.sh ../task/bin1.obf $T with +RTS -M1504m -RTS > $T.output 
    tail $T.output
    ./createSub.sh $T.0.input $T.output $T.osf  
done

for T in 2001 2003 2004; do
    ./run_controller.sh ../task/bin2.obf $T with +RTS -M1504m -RTS > $T.output 
    tail $T.output
    ./createSub.sh $T.0.input $T.output $T.osf  
done

for T in 3001; do
    ./run_controller.sh ../task/bin3.obf $T with +RTS -M1504m -RTS > $T.output 
    tail $T.output
    ./createSub.sh $T.0.input $T.output $T.osf  
done
