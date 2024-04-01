CC=gnatmake -D build
COMPILE_ARGS=-cargs -g -O0
BIND_ARGS=-bargs -Es
all: clean
	$(CC) src/main.adb -o build/app $(COMPILE_ARGS) $(BIND_ARGS)
tests: clean
	$(CC) src/tests.adb -o build/tests $(COMPILE_ARGS) $(BIND_ARGS) && build/tests
clean:
	rm -rf build && mkdir -p build 
