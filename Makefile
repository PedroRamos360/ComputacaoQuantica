all: teste

teste:
	ghc -o teste teste.hs
	.\teste.exe

clean:
	del *.exe *.o *.hi
