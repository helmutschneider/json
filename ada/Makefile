CC=gnatmake
all: clean
	$(CC) src/main.adb -O2 -D build -o build/app && build/app
tests: clean
	$(CC) src/tests.adb -D build -o build/tests && build/tests
clean:
	rm -rf build && mkdir -p build
