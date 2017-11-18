// platform-posix.cpp - POSIX platform functionality

// TODO: Clang/gcc compiles on windows.

#ifndef _MSC_VER

#include "arete.hpp"

#include <sys/time.h>
#include <sys/stat.h>

namespace arete {

DefunGroup platform("platform");

//
///// OS SPECIFIC 
//

Value fn_current_millisecond(State& state, size_t argc, Value* argv) {
  struct timeval te; 
  gettimeofday(&te, NULL); // get current time
  size_t milliseconds = te.tv_sec*1000LL + te.tv_usec/1000; // caculate milliseconds

  return Value::make_fixnum(milliseconds);
}
AR_DEFUN("current-millisecond", fn_current_millisecond, 0);

Value fn_file_exists(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "file-exists?";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
  struct stat st;
  if(stat(argv[0].string_data(), &st) == -1) {
    return C_FALSE;
  }
  return C_TRUE;
}
AR_DEFUN("file-exists?", fn_file_exists, 1);

void load_platform_functions(State& state) {
  platform.install(state);
}

}

#endif