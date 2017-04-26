APP=Datakit.app
EXE=Datakit
PINOPTS=-y -k git

BUILD=jbuilder build --dev
RUNTEST=jbuilder runtest

.PHONY: all clean test bundle COMMIT exe ci

all:
	$(BUILD)

datakit:
	$(BUILD) -n datakit
	$(RUNTEST) test/datakit

client:
	$(BUILD) -p datakit-client

server:
	$(BUILD) -p datakit-server

github:
	$(BUILD) -p datakit-github

bridge-local-git:
	$(BUILD) -p datakit-bridge-local-git

bridge-github:
	$(BUILD) -p datakit-bridge-github
	$(RUNTEST) test/datakit-github-bridge

ci:
	$(BUILD) -p datakit-ci -q
	$(RUNTEST) ci/test

clean:
	rm -rf _build
	rm -rf $(APP) $(EXE) _tests
	rm -f examples/ocaml-client/*.native
	rm -f ci/skeleton/exampleCI.native
	rm -f com.docker.db

test:
	jbuilder runtest

bundle:
	opam remove tls ssl -y
	$(MAKE) clean
	$(BUILD) src/datakit/bin/main.exe
	mkdir -p $(APP)/Contents/MacOS/
	mkdir -p $(APP)/Contents/Resources/lib/
	cp _build/default/src/datakit/bin/main.exe $(APP)/Contents/MacOS/com.docker.db
	./scripts/check-dylib.sh
	dylibbundler -od -b \
	 -x $(APP)/Contents/MacOS/com.docker.db \
	 -d $(APP)/Contents/Resources/lib \
	 -p @executable_path/../Resources/lib
	cp $(APP)/Contents/MacOS/com.docker.db .

COMMIT:
	@git rev-parse HEAD > COMMIT

exe:
	opam remove tls ssl -y
	rm -rf _build/
	$(BUILD) src/datakit/bin/main.exe
	mkdir -p $(EXE)
	cp _build/debfault/src/datakit/bin/main.exe $(EXE)/datakit.exe
	cp /usr/x86_64-w64-mingw32/sys-root/mingw/bin/zlib1.dll $(EXE)

REPO=../opam-repository
PACKAGES=$(REPO)/packages

# until we have https://github.com/ocaml/opam-publish/issues/38
pkg-%:
	topkg opam pkg -p $*
	mkdir -p $(PACKAGES)/$*
	cp -r _build/$*.* $(PACKAGES)/$*/
	cd $(PACKAGES) && git add $*

opam-pkg:
	$(MAKE) pkg-datakit
	$(MAKE) pkg-datakit-client
	$(MAKE) pkg-datakit-server
	$(MAKE) pkg-datakit-ci
	$(MAKE) pkg-datakit-github
	$(MAKE) pkg-datakit-bridge-github
	$(MAKE) pkg-datakit-bridge-local-git
