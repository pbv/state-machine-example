
DLL= libqueue.so
SRC= queue.c
OBJ= queue.o

$(DLL) : $(SRC)
	gcc -pthread -fpic -c $(SRC)
	gcc -pthread -rdynamic -shared -o $(DLL) $(OBJ)

