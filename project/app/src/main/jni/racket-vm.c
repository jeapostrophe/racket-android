Scheme_Object *rap_scheme_make_stdio(const char *label) {
  Scheme_Object *v = NULL;

  v = scheme_intern_symbol(label);
  v = 
    scheme_make_output_port(
     v,
     NULL,
     v,
     NULL,
     rap_stdio_writes_bytes,
     rap_stdio_char_ready,
     rap_stdio_close,
     rap_stdio_need_wakeup,
     NULL,
     NULL,
     0 );

  return v;
}

static int rvm_init(void *d) {
  Scheme_Env *e = NULL;
  Scheme_Object *a[3] = {NULL, NULL, NULL};
  Scheme_Object *mp = NULL, *ra = NULL, *v = NULL, *vec = NULL;
  Scheme_Config *config = NULL;
  Scheme_Object *curout = NULL;

  e = scheme_basic_env();
  config = scheme_current_config();
  curout = scheme_get_param(config, MZCONFIG_OUTPUT_PORT);

  pipe(main_t_fd);

  ALOGE("RC: Declaring modules...");
  declare_modules(e);
  ALOGE("RC: Done declaring modules...");

  mp = scheme_make_null();
  v = scheme_intern_symbol("app");
  mp = scheme_make_pair(v, mp);
  v = scheme_intern_symbol("quote");
  mp = scheme_make_pair(v, mp);

  ALOGE("RC: Requiring modules...");
  v = scheme_namespace_require(mp);
  ALOGE("RC: Done requiring modules...");

  a[0] = mp;
  a[1] = scheme_intern_symbol("run-app");
  a[2] = NULL;
  ALOGE("RC: Resolving run-app");
  ra = scheme_dynamic_require(2, a);
  ALOGE("RC: Done resolving run-app");
  
  v = scheme_intern_symbol("main-t-in");
  a[0] = scheme_make_fd_input_port(main_t_fd[0], v, 0, 0);
  a[1] = scheme_make_integer(sizeof(struct rvm_api_t));
  v = scheme_make_null();
  vec = scheme_make_vector(5, v);
  a[2] = vec;
  v = scheme_make_prim_w_arity(rap_audio, "RAPAudio.playSound", 1, 1);
  SCHEME_VEC_ELS(vec)[0] = v;
  v = scheme_make_prim_w_arity(rap_set_label, "RAPSetLabel", 1, 1);
  SCHEME_VEC_ELS(vec)[1] = v;
  v = scheme_make_prim_w_arity(rap_draw_frame_done, "RAPDrawFrameDone", 0, 0);
  SCHEME_VEC_ELS(vec)[2] = v;
  v = scheme_make_prim_w_arity(rap_drive_read, "RAPDrive.read", 1, 1);
  SCHEME_VEC_ELS(vec)[3] = v;
  v = scheme_make_prim_w_arity(rap_drive_write, "RAPDrive.write", 2, 2);
  SCHEME_VEC_ELS(vec)[4] = v;
  ALOGE("RC: Applying run-app");
  v = scheme_apply(ra, 3, a);
  ALOGE("RC: Done applying run-app");
  
  return 0;
}
