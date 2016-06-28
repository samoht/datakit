GITHUB=$(shell opam config var github:installed)
APP=Datakit.app

all:
	topkg build -- --with-github $(GITHUB)

clean:
	topkg clean

test:
	topkg build --tests true -- --with-github $(GITHUB)
	topkg test

bundle:
	opam remove tls ssl -y
	$(MAKE) clean
	$(MAKE) GITHUB=false
	rm -rf $(APP)
	mkdir -p $(APP)/Contents/MacOS/
	mkdir -p $(APP)/Contents/Resources/lib/
	cp _build/src/bin/main.native $(APP)/Contents/MacOS/com.docker.db
	./scripts/check-dylib.sh
	dylibbundler -od -b \
	 -x $(APP)/Contents/MacOS/com.docker.db \
	 -d $(APP)/Contents/Resources/lib \
	 -p @executable_path/../Resources/lib

exe:
	opam remove tls ssl -y
	$(MAKE) clean
	$(MAKE) GITHUB=false
	rm -rf $(EXE)
	mkdir -p $(EXE)
	cp _build/src/bin/main.native $(EXE)/datakit.exe
	cp /usr/x86_64-w64-mingw32/sys-root/mingw/bin/zlib1.dll $(EXE)
