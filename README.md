racket-android - deploying Racket on Android

This project was funded by Black Swan Learning LLC.

# DEPENDENCIES

## If you are using an OS X development machine...

- Android SDK 23 with 23.0.2 build tools installed in
  `~/Library/Android/sdk` (the default location that Android Studio
  puts it in.)

- Android NDK for version 23, compiler version 4.9 installed in
  `~/Library/Android/sdk/ndk-bundle` (default location) (Perhaps we
  can generalize the build process to use a different one, but it is
  tested with this one)

## If you are using a Debian Linux development machine...

 - Android SDK 23 with 23.0.3 build tools installed in
   `/usr/lib/android-sdk` (system location from `apt-get install
   google-android-build-tools-23-installer`)

 - Android NDK for version 23, compiler version 4.9 installed in
   `/usr/lib/android-ndk` (system location from `apt-get install
   google-android-ndk-installer`)

## And also, no matter your development machine...

- A Java JRE/JDK of the 1.8.0-era. (We test with 1.8.0_92.)

- An Android device that supports version 23, uses armeabi, and
  support OpenGL ES 3. We test with the Google Pixel C.

- A Racket version that supports `--enable-ffipoll` (6.6 and later)

- The Racket packages, `lux`, `opengl`, and `mode-lambda`. (Run `raco
  pkg install mode-lambda`, and accept the installation of the
  dependencies.)

# USAGE

Put the program that you want to run in `rkt/app.rkt`. It will use
`rkt/app-csd.rkt` for the compiled sprite database.

Run `make build_all` in the main directory. This will download the
current Racket master repository and build a cross-compiled version of
it for use on Android. It is essential that this version and the
version that `racket` runs are the same version, because they will
share compiled bytecode, so you may want to build this version as
second time and put it in your `$PATH` when working with
`racket-android`.

It will place the appropriate C code into the `project` directory for
use by the Android app. If you run `make app`, then it will build the
Android app and install it to the attached device.

The Android app can be customized in the usual ways by, for instance,
modifying `project/app/src/main/AndroidManifest.xml` (for name and
permissions) or `project/app/src/main/res` (for icons.)

# EXAMPLES

Look at `rkt/basic.rkt` for an example application.

Use `make simulate` to use the simulator, which provides an interface
compatible with the tablet and can be run locally directly from
DrRacket/etc. This requires a OpenGL driver that supports most of the
features of OpenGL ES 3. (You need to adjust the Racket file, too.)

# TODO
- Make 'raco ctool' use gzip for the bytecode to decrease app size.
- Implement some sort of tree-shaking of bytecode to decrease app size.
