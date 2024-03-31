CC=gnatmake
all: clean
	$(CC) src/main.adb -O2 -D build -o build/app && build/app
clean:
	rm -rf build && mkdir -p build
