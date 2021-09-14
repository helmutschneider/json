OUT_BIN = app

all:
	gcc -std=c17 -Wall -Wextra -pedantic node.c parser.c main.c -o ${OUT_BIN}

clean:
	rm -rf ${OUT_BIN}

run: clean all
	./${OUT_BIN}
