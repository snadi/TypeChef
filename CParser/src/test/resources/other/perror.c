int get_pty(int *pty, char *from) {
  int r = 1;

#if defined(USE_USG_PTYS)
  #if defined(__GLIBC__)
    r = 0;
  #elif defined(__MVS__)
    r = 1;
  #else
  #if defined(USE_ISPTS_FLAG)
      if (r) {
  #endif
        r = 2;
  #endif
  #if defined(SVR4) || defined(USE_ISPTS_FLAG)
    if (r) r = 3;
    #if defined(USE_ISPTS_FLAG)
      r = 4;
    }
    #endif
  #endif

#endif
    return r;
}