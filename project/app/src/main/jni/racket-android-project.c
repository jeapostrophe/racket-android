#include <string.h>
#include <jni.h>

#include <android/log.h>
#define LOG_TAG "racket-android"
#define ALOGE(...) __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)
#define ALOGD(...) __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, __VA_ARGS__)

#include "racket/include/scheme.h"
#include "racket/racket_app.c"

char the_string[256] = "Racket didn't work :'(";

static int do_the_work(Scheme_Env *e, int argc, char *argv[]) {
  Scheme_Object *p = NULL, *v = NULL, *a[2] = {NULL, NULL};
  
  MZ_GC_DECL_REG(6);
  MZ_GC_VAR_IN_REG(0, e);
  MZ_GC_VAR_IN_REG(1, p);
  MZ_GC_VAR_IN_REG(2, v);
  MZ_GC_ARRAY_VAR_IN_REG(3, a, 2);

  MZ_GC_REG();

  declare_modules(e);

  p = scheme_make_pair(scheme_intern_symbol("quote"),
                       scheme_make_pair(scheme_intern_symbol("app"),
                       scheme_make_null()));
  
  scheme_namespace_require(p);

  a[0] = p;
  a[1] = scheme_intern_symbol("go");
  v = scheme_apply(scheme_dynamic_require(2, a), 0, NULL);
  v = scheme_char_string_to_byte_string(v);
  const char *res = SCHEME_BYTE_STR_VAL(v);
  strlcpy(the_string, res, sizeof(the_string));
  res = NULL;

  MZ_GC_UNREG();
  
  return 0;
}

void rap_scheme_console_output( const char *disp, intptr_t len ) {
  ALOGE("RC: %.*s\n", len, disp);
}

void rap_scheme_exit( int code ) {
  ALOGE("Racket tried to exit with %d\n", code);
}

static const char *RAP_STDOUT = "stdout";
static const char *RAP_STDERR = "stderr";

intptr_t rap_stdxxx_writes_bytes(Scheme_Output_Port *port, const char *buf, intptr_t off, intptr_t size, int rarely_block, int enable_break) {
  __android_log_print(
   // XXX this doesn't work, it's always FALSE
   scheme_eq(port->name, scheme_intern_symbol(RAP_STDERR)) ? ANDROID_LOG_ERROR : ANDROID_LOG_DEBUG, LOG_TAG, "%.*s", size, buf+off);
  return size;
}

int rap_stdxxx_char_ready(Scheme_Output_Port *port) {
  return 1;
}

void rap_stdxxx_close(Scheme_Output_Port *port) {
  return;
}

void rap_stdxxx_need_wakeup(Scheme_Output_Port *port, void *fds ) {
  return;
}

Scheme_Object *rap_scheme_make_stdxxx(const char *label) {
  Scheme_Object *v = NULL;

  MZ_GC_DECL_REG(1);
  MZ_GC_VAR_IN_REG(0, v);
  MZ_GC_REG();

  v = scheme_intern_symbol(label);
  v = 
    scheme_make_output_port(
     v,
     NULL,
     v,
     NULL,
     rap_stdxxx_writes_bytes,
     rap_stdxxx_char_ready,
     rap_stdxxx_close,
     rap_stdxxx_need_wakeup,
     NULL,
     NULL,
     0 );

  MZ_GC_UNREG();

  return v;
}

Scheme_Object *rap_scheme_make_stdout() {
  return rap_scheme_make_stdxxx(RAP_STDOUT);
}
Scheme_Object *rap_scheme_make_stderr() {
  return rap_scheme_make_stdxxx(RAP_STDERR);
}

jstring
Java_org_racketlang_android_project_RacketAndroidProject_go(
 JNIEnv* env,
 jobject thiz ) {
  scheme_console_output = rap_scheme_console_output;
  scheme_exit = rap_scheme_exit;
  scheme_make_stdout = rap_scheme_make_stdout;
  scheme_make_stderr = rap_scheme_make_stderr;
  scheme_main_setup(1, do_the_work, 0, NULL);

  return (*env)->NewStringUTF(env, the_string);
}
