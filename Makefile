main: 
	ghc --make 2048

clean:
	rm *.o || true
	rm *.hi || true
	rm 2048 || true
