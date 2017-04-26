APP=Datakit.app
EXE=Datakit
PINOPTS=-y -k git
BUILD=jbuilder build --dev

.PHONY: all clean test bundle COMMIT exe ci

all:
	$(BUILD)

datakit:
	$(BUILD) -n datakit
	jbuilder runtest -n datakit

client:
	$(BUILD) -n datakit-client

server:
	$(BUILD) -n datakit-server

github:
	$(BUILD) -n datakit-github -q

bridge-local-git:
	$(BUILD) -n datakit-bridge-local-git

bridge-github:
	$(BUILD) -n datakit-bridge-github
	jbuilder runtest -n datakit-bridge-github

ci:
	$(BUILD) -n datakit-ci -q
	jbuilder runtest -n datakit-ci

clean:
	jbuilder clean
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
	topkg opam pkg -n $*
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
