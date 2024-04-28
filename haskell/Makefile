OUT_DIR=build
CC=ghc -O0 -Wall -outputdir "${OUT_DIR}"

all: clean
	mkdir -p "${OUT_DIR}" && ${CC} -o "${OUT_DIR}/app" Main.hs && build/app
clean:
	rm -rf "${OUT_DIR}"
