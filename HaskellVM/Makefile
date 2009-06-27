

all:
	ghc -O6 --make Main.hs -o vm


createsub:
	ghc -O6 --make CreateSub.hs -o createSub

VM2:
	ghc -O6 --make VM2

clean:
	rm vm
	rm -f *.o
	rm -f *.hi

run1:
	./vm ../task/bin1.obf