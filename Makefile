all: teleportation

teste:
	ghc -o teste teste.hs
	.\teste.exe

teleportation:
	ghc -o tel teleportation.hs
	.\tel.exe
	

clean:
	del *.exe *.o *.hi
