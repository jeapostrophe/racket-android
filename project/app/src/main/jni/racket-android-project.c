#include <string.h>
#include <jni.h>
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/types.h>
#include <stdint.h>

#include <android/log.h>
#define LOG_TAG "racket-android"
#define ALOGE(...) __android_log_print(ANDROID_LOG_ERROR, LOG_TAG, __VA_ARGS__)
#define ALOGD(...) __android_log_print(ANDROID_LOG_DEBUG, LOG_TAG, __VA_ARGS__)

#include "racket/include/scheme.h"
#include "racket/include/schemegc2.h"
#include "racket/racket_app.c"

// Internals

void rap_scheme_console_output( const char *disp, intptr_t len ) {
  ALOGE("RC: %.*s\n", len, disp);
}

void rap_scheme_exit( int code ) {
  ALOGE("Racket tried to exit with %d\n", code);
}

static const char *RAP_STDOUT = "stdout";
static const char *RAP_STDERR = "stderr";

intptr_t rap_stdio_writes_bytes(Scheme_Output_Port *port, const char *buf, intptr_t off, intptr_t size, int rarely_block, int enable_break) {
  __android_log_print(
   // XXX this doesn't work, it's always FALSE
   scheme_eq(port->name, scheme_intern_symbol(RAP_STDERR)) ? ANDROID_LOG_ERROR : ANDROID_LOG_DEBUG, LOG_TAG, "%.*s", size, buf+off);
  return size;
}

int rap_stdio_char_ready(Scheme_Output_Port *port) {
  return 1;
}

void rap_stdio_close(Scheme_Output_Port *port) {
  return;
}

void rap_stdio_need_wakeup(Scheme_Output_Port *port, void *fds ) {
  return;
}

// Racket VM Interface

pthread_t main_t;
int main_t_fd[2];

#define RAP_onDrawFrame 0
#define RAP_onSurfaceChanged 1
#define RAP_onSurfaceCreated 2
#define RAP_onTouchEvent 3

struct rvm_api_t {
  uint32_t call;
  union {
    int32_t i;
    float f;
  } args[3];
};

void send_to_racket( struct rvm_api_t rpc ) {
  write(main_t_fd[1], (void *)&rpc, sizeof(rpc));
  fsync(main_t_fd[1]);
}

JavaVM* the_JVM;
jobject the_RAPAudio;

Scheme_Object *rap_audio(int argc, Scheme_Object **argv) {
  JNIEnv *env = NULL;
  (*the_JVM)->AttachCurrentThread(the_JVM, &env, NULL);
  jstring str = (*env)->NewStringUTF(env, SCHEME_BYTE_STR_VAL(argv[0]));
  jclass cls = (*env)->GetObjectClass(env, the_RAPAudio);
  jmethodID mid =
    (*env)->GetStaticMethodID(env, cls, "playSound", "(Ljava/lang/String;)V");
  (*env)->CallStaticVoidMethod(env, cls, mid, str);

  (*the_JVM)->DetachCurrentThread(the_JVM);
  
  return NULL;
}

#include "racket/racket-vm.3m.c"

Scheme_Object *rap_scheme_make_stdout() {
  return rap_scheme_make_stdio(RAP_STDOUT);
}
Scheme_Object *rap_scheme_make_stderr() {
  return rap_scheme_make_stdio(RAP_STDERR);
}

void *rvm_thread_init(void *d) {
  scheme_console_output = rap_scheme_console_output;
  scheme_exit = rap_scheme_exit;
  scheme_make_stdout = rap_scheme_make_stdout;
  scheme_make_stderr = rap_scheme_make_stderr;

  scheme_main_stack_setup(1, rvm_init, d);

  return NULL;
}

// Interface to Java

void
Java_org_racketlang_android_project_RLib_onDrawFrame(
 JNIEnv* env,
 jobject thiz ) {
  struct rvm_api_t rpc = { .call = RAP_onDrawFrame };
  return send_to_racket( rpc );
}

void
Java_org_racketlang_android_project_RLib_onSurfaceChanged(
 JNIEnv* env,
 jobject thiz,
 jint w,
 jint h ) {
  struct rvm_api_t rpc = { .call = RAP_onSurfaceChanged,
                           .args = { {.i = w}, {.i = h} } };
  return send_to_racket( rpc );
}

void
Java_org_racketlang_android_project_RLib_onSurfaceCreated(
 JNIEnv* env,
 jobject thiz ) {  
  struct rvm_api_t rpc = { .call = RAP_onSurfaceCreated };
  return send_to_racket( rpc );
  return;
}

void
Java_org_racketlang_android_project_RLib_onTouchEvent(
 JNIEnv* env,
 jobject thiz,
 jint a,
 jfloat x,
 jfloat y) {
  struct rvm_api_t rpc = { .call = RAP_onTouchEvent,
                           .args = { {.i=a}, {.f=x}, {.f=y} } };
  return send_to_racket( rpc );
}

void
Java_org_racketlang_android_project_RLib_onCreate(
 JNIEnv* env,
 jobject thiz,
 jobject rs ) {
  (*env)->GetJavaVM(env, &the_JVM);
  the_RAPAudio = (*env)->NewGlobalRef(env, rs);
  pthread_create(&main_t, NULL, rvm_thread_init, NULL);
  return;
}
