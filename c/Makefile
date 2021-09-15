OUT_BIN = ./target/app
TEST_BIN = ./target/tests
GCC_BIN = gcc -std=c17 -Wall -Wextra -pedantic

all:
	${GCC_BIN} node.c parser.c main.c -o ${OUT_BIN}

clean:
	rm -rf ${OUT_BIN} ${TEST_BIN} && mkdir -p target/

run: clean all
	./${OUT_BIN}

tests: clean
	${GCC_BIN} node.c parser.c tests.c -o ${TEST_BIN} && ./${TEST_BIN}
