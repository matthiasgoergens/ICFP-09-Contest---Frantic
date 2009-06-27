
all: VM createsub

VM:
	ghc -O6 --make Main.hs -o vm


createsub:
	ghc -O6 --make CreateSub.hs -o createSub

VM2:
	ghc -O6 --make vm2

clean:
	rm vm
	rm -f *.o
	rm -f *.hi

run1:
	./vm ../task/bin1.obf