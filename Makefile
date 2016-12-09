ifeq ($(shell uname -s),Darwin)
ANDROID_SDK=${HOME}/Library/Android/sdk
NDK=${ANDROID_SDK}/ndk-bundle
HOST=darwin-x86_64
else
ANDROID_SDK=/usr/lib/android-sdk
NDK=/usr/lib/android-ndk
HOST=linux-x86_64
endif

export ANDROID_HOME=${ANDROID_SDK}
export ANDROID_NDK_HOME=${NDK}

ANDROID_VER=23
COMP_VER=4.9
TARGET=arm-linux-androideabi
COMP_PATH=${NDK}/toolchains/${TARGET}-${COMP_VER}/prebuilt/${HOST}/bin/

export PATH := ${COMP_PATH}:$(PATH)

RACKETDIR=dist/racket-master/racket
LIBRACKET=${RACKETDIR}/lib/libracket3m.a
RACKETINCLUDE=${RACKETDIR}/include
JNI=project/app/src/main/jni
RACKETDEST=${JNI}/racket

.PHONY: app
app: build_all
	cd project && ./gradlew installArmDebug

.PHONY: simulate
simulate: rkt/csd.rktd.gz
	raco make rkt/app.rkt
	racket -t rkt/app.rkt

.PHONY: build_all
build_all: ${RACKETDEST} ${RACKETDEST}/racket-vm.3m.c ${RACKETDEST}/racket_app.c ${RACKETDEST}/libracket3m.a ${RACKETDEST}/include

clean:
	rm -f ${RACKETDEST}/racket_app.c ${RACKETDEST}/racket-vm.3m.c

size: ${RACKETDEST}/racket_app.c
	du -hac $^ ./project/app/build/outputs/apk/app-arm-debug.apk
	tail $^

rkt/csd.rktd.gz: rkt/app-csd.rkt
	racket -t $^

${RACKETDEST}/racket_app.c: rkt/app.rkt rkt/csd.rktd.gz ${RACKETDEST}
	raco ctool --c-mods $@ $<

${RACKETDEST}/racket-vm.3m.c: ${RACKETDEST}/../racket-vm.c ${RACKETDEST}
	raco ctool --xform $<
	mv -f ${RACKETDEST}/../racket-vm.3m.c ${RACKETDEST}/racket-vm.3m.c

${RACKETDEST}/libracket3m.a: ${LIBRACKET}
	cp $< $@

${RACKETDEST}/include: ${RACKETINCLUDE}
	rsync -a --delete --progress $</ $@/

${RACKETDEST}:
	mkdir -p $@
	touch $@

${RACKETINCLUDE} ${LIBRACKET}: dist/racket-master
	mkdir -p ${RACKETDIR}/src/build && cd ${RACKETDIR}/src//build && ../configure --host=${TARGET} --enable-sysroot="${NDK}/platforms/android-${ANDROID_VER}/arch-arm" --enable-racket=auto --enable-places --enable-foreign --enable-ffipoll && $(MAKE) && $(MAKE) plain-install

dist/racket-master: dist/racket-master.zip
	unzip $^ -d dist
	touch $@

dist/racket-master.zip:
	wget https://github.com/racket/racket/archive/master.zip -O $@ || rm -f $@
