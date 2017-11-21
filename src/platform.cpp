// platform-posix.cpp - POSIX platform functionality

// TODO: Clang/gcc compiles on windows.

#include "arete.hpp"

#if AR_OS == AR_POSIX
# include <sys/time.h>
# include <sys/stat.h>
#elif AR_OS == AR_WINDOWS
# include <winsock2.h>
# include <windows.h>
#endif

namespace arete {

DefunGroup platform("platform");

//
///// OS SPECIFIC 
//

Value fn_current_millisecond(State& state, size_t argc, Value* argv) {
#if AR_OS == AR_POSIX
  struct timeval te; 
  gettimeofday(&te, NULL); // get current time
  size_t milliseconds = te.tv_sec*1000LL + te.tv_usec/1000; // caculate milliseconds

  return Value::make_fixnum(milliseconds);
#else
	static const unsigned __int64 epoch = 116444736000000000Ui64;

	FILETIME    file_time;
	SYSTEMTIME  system_time;
	ULARGE_INTEGER ularge;

	GetSystemTime(&system_time);
	SystemTimeToFileTime(&system_time, &file_time);
	ularge.LowPart = file_time.dwLowDateTime;
	ularge.HighPart = file_time.dwHighDateTime;

	long tv_sec = (long) ((ularge.QuadPart - epoch) / 10000000L);
	long tv_usec = (long) (system_time.wMilliseconds * 1000);

	return Value::make_fixnum((tv_sec * 1000LL) + (tv_usec / 1000));
#endif
}
AR_DEFUN("current-millisecond", fn_current_millisecond, 0);

Value fn_file_exists(State& state, size_t argc, Value* argv) {
  static const char* fn_name = "file-exists?";
  AR_FN_EXPECT_TYPE(state, argv, 0, STRING);
#if AR_OS == AR_POSIX
  struct stat st;
  if(stat(argv[0].string_data(), &st) == -1) {
    return C_FALSE;
  }
  return C_TRUE;
#else
	WIN32_FIND_DATA find;
	HANDLE handle = FindFirstFile(argv[0].string_data(), &find);
	if(handle != INVALID_HANDLE_VALUE) {
		FindClose(handle);
		return C_TRUE;
	}
#endif
  return C_FALSE;
}
AR_DEFUN("file-exists?", fn_file_exists, 1);

Value fn_path_separator(State& state, size_t argc, Value* argv) {
#if AR_OS == AR_POSIX
	return state.make_char('/');
#else
	return state.make_char('\\');
#endif
}
AR_DEFUN("path-separator", fn_path_separator, 0);

void load_platform_functions(State& state) {
  platform.install(state);
}

} // namespace arete
