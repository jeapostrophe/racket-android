#include <string.h>
#include <jni.h>
#include <android/log.h>

#include "racket/include/scheme.h"
#include "racket/racket_app.c"

#define ALOGE(...) __android_log_print(ANDROID_LOG_ERROR, "racket-android", __VA_ARGS__)

char the_string[256] = "Racket didn't work :'(";

static int do_the_work(Scheme_Env *e, int argc, char *argv[]) {
  int d = 0;
#define DEBUG ALOGE("! RAP %d\n", d++)
  
  Scheme_Object *p = NULL, *v = NULL, *a[2] = {NULL, NULL};
  
  MZ_GC_DECL_REG(6);
  MZ_GC_VAR_IN_REG(0, e);
  MZ_GC_VAR_IN_REG(1, p);
  MZ_GC_VAR_IN_REG(2, v);
  MZ_GC_ARRAY_VAR_IN_REG(3, a, 2);

  MZ_GC_REG();

  DEBUG;
  declare_modules(e);

  DEBUG;
  p = scheme_make_pair(scheme_intern_symbol("quote"),
                       scheme_make_pair(scheme_intern_symbol("app"),
                       scheme_make_null()));
  
  DEBUG;
  scheme_namespace_require(p);

  DEBUG;
  a[0] = p;
  a[1] = scheme_intern_symbol("go");
  DEBUG;
  v = scheme_apply(scheme_dynamic_require(2, a), 0, NULL);
  DEBUG;
  v = scheme_char_string_to_byte_string(v);
  DEBUG;
  const char *res = SCHEME_BYTE_STR_VAL(v);
  DEBUG;
  strlcpy(the_string, res, sizeof(the_string));
  DEBUG;
  res = NULL;

  DEBUG;
  MZ_GC_UNREG();
  
  DEBUG;
  return 0;
}

void rap_scheme_console_output( const char *disp, intptr_t len ) {
  // XXX use len
  ALOGE("racket console: %s\n", disp);
}

void rap_scheme_exit( int code ) {
  ALOGE("racket tried to exit with %d\n", code);
}

jstring
Java_org_racketlang_android_project_RacketAndroidProject_go(
 JNIEnv* env,
 jobject thiz ) {
  ALOGE("RAP the_string = %s\n", the_string);
  scheme_console_output = rap_scheme_console_output;
  scheme_exit = rap_scheme_exit;
  scheme_main_setup(1, do_the_work, 0, NULL);
  ALOGE("RAP the_string = %s\n", the_string);
  return (*env)->NewStringUTF(env, the_string);
}
