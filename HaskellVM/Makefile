
all: VM createsub

VM:
	ghc -O6 --make Main.hs -o vm


createsub:
	ghc -O6 --make CreateSub.hs -o createSub

output2CSV:
	ghc -O6 --make Output2CSV.hs -o output2csv

VM2:
	ghc -O6 --make VM2.hs -o vm2 

clean:
	rm vm
	rm -f *.o
	rm -f *.hi

run1:
	./vm ../task/bin1.obf