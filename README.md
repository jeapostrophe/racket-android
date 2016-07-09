racket-android - deploying Racket on Android

This project was funded by Black Swan Learning LLC.

= USAGE

Put the program that you want to run in `rkt/app.rkt`. Right now, this
must export a function `go` that returns a string, but in the future
it will export a few functions related to interaction with the Android
UI.

Ensure that you have the Android SDK (with NDK) installed in
`~/Library/Android/sdk` (the default location that Android Studio puts
it in.)

Run `make build_all` in the main directory. This will download the
current Racket master repository and build a cross-compiled version of
it for use on Android. It is essential that this version and the
version that `racket` runs are the same version, because they will
share bytecode.

It will place the appropriate C code into the `project` directory for
use by the Android app. If you run `make app`, then it will build the
Android app and install it to the attached device.

The Android app can be customized in the usual ways by, for instance,
modifying `project/app/src/main/AndroidManifest.xml` (for name and
permissions) or `project/app/src/main/res` (for icons.)

= TODO
* Make 'raco ctool' use gzip for the zo to decrease app size.
* Build display, input, and audio interface for Racket and
  corresponding JNI code.
