OUT_BINARY = build/json

all: clean
	mkdir -p build/ && fpc json.pas -o${OUT_BINARY} -S2
clean:
	rm -rf ./build
tests: all
	${OUT_BINARY}
