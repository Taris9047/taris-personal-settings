/* confdefs.h */
#define PACKAGE_NAME "glib"
#define PACKAGE_TARNAME "glib"
#define PACKAGE_VERSION "2.38.2"
#define PACKAGE_STRING "glib 2.38.2"
#define PACKAGE_BUGREPORT "http://bugzilla.gnome.org/enter_bug.cgi?product=glib"
#define PACKAGE_URL ""
#define GLIB_MAJOR_VERSION 2
#define GLIB_MINOR_VERSION 38
#define GLIB_MICRO_VERSION 2
#define GLIB_INTERFACE_AGE 2
#define GLIB_BINARY_AGE 3802
#define STDC_HEADERS 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_SYS_STAT_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_MEMORY_H 1
#define HAVE_STRINGS_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_STDINT_H 1
#define HAVE_UNISTD_H 1
#define __EXTENSIONS__ 1
#define _ALL_SOURCE 1
#define _GNU_SOURCE 1
#define _POSIX_PTHREAD_SEMANTICS 1
#define _TANDEM_SOURCE 1
#define GETTEXT_PACKAGE "glib20"
#define GLIB_LOCALE_DIR ""
#define HAVE_DLFCN_H 1
#define LT_OBJDIR ".libs/"
#define HAVE_VPRINTF 1
#define HAVE_ALLOCA_H 1
#define HAVE_ALLOCA 1
#define HAVE_MMAP 1
#define HAVE_POSIX_MEMALIGN 1
#define HAVE_MEMALIGN 1
#define HAVE_VALLOC 1
#define HAVE_FSYNC 1
#define HAVE_PIPE2 1
#define HAVE_ATEXIT 1
#define HAVE_ON_EXIT 1
#define HAVE_TIMEGM 1
#define HAVE_GMTIME_R 1
#define HAVE_LIBC_ENABLE_SECURE 1
#define SIZEOF_CHAR 1
#define SIZEOF_SHORT 2
#define SIZEOF_LONG 8
#define SIZEOF_INT 4
#define SIZEOF_VOID_P 8
#define SIZEOF_LONG_LONG 8
#define SIZEOF___INT64 0
#define HAVE_SIG_ATOMIC_T 1
#define HAVE_LONG_LONG_FORMAT 1
#define G_HAVE___INLINE 1
#define G_HAVE___INLINE__ 1
#define G_HAVE_INLINE 1
#define HAVE_DIRENT_H 1
#define HAVE_FLOAT_H 1
#define HAVE_LIMITS_H 1
#define HAVE_PWD_H 1
#define HAVE_GRP_H 1
#define HAVE_SYS_PARAM_H 1
#define HAVE_SYS_POLL_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_TIMES_H 1
#define HAVE_SYS_WAIT_H 1
#define HAVE_UNISTD_H 1
#define HAVE_VALUES_H 1
#define HAVE_SYS_SELECT_H 1
#define HAVE_SYS_TYPES_H 1
#define HAVE_STDINT_H 1
#define HAVE_INTTYPES_H 1
#define HAVE_SCHED_H 1
#define HAVE_MALLOC_H 1
#define HAVE_SYS_VFS_H 1
#define HAVE_SYS_STATFS_H 1
#define HAVE_SYS_STATVFS_H 1
#define HAVE_MNTENT_H 1
#define HAVE_FSTAB_H 1
#define HAVE_SYS_UIO_H 1
#define HAVE_LINUX_MAGIC_H 1
#define HAVE_SYS_PRCTL_H 1
#define HAVE_SYS_MOUNT_H 1
#define HAVE_SYS_SYSCTL_H 1
#define HAVE_STRUCT_STAT_ST_MTIM_TV_NSEC 1
#define HAVE_STRUCT_STAT_ST_ATIM_TV_NSEC 1
#define HAVE_STRUCT_STAT_ST_CTIM_TV_NSEC 1
#define HAVE_STRUCT_STAT_ST_BLKSIZE 1
#define HAVE_STRUCT_STAT_ST_BLOCKS 1
#define HAVE_STRUCT_STATFS_F_BAVAIL 1
#define HAVE_STRUCT_TM_TM_GMTOFF 1
#define HAVE_DIRENT_H 1
#define HAVE_STRUCT_DIRENT_D_TYPE 1
#define HAVE_LANGINFO_CODESET 1
#define HAVE_STDDEF_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_SETLOCALE 1
#define SIZEOF_SIZE_T 8
#define SIZEOF_SSIZE_T 8
#define HAVE_LSTAT 1
#define HAVE_STRERROR 1
#define HAVE_STRSIGNAL 1
#define HAVE_MEMMOVE 1
#define HAVE_VSNPRINTF 1
#define HAVE_STPCPY 1
#define HAVE_STRCASECMP 1
#define HAVE_STRNCASECMP 1
#define HAVE_POLL 1
#define HAVE_GETCWD 1
#define HAVE_VASPRINTF 1
#define HAVE_SETENV 1
#define HAVE_UNSETENV 1
#define HAVE_GETC_UNLOCKED 1
#define HAVE_READLINK 1
#define HAVE_SYMLINK 1
#define HAVE_MEMMEM 1
#define HAVE_CHOWN 1
#define HAVE_LCHOWN 1
#define HAVE_FCHMOD 1
#define HAVE_FCHOWN 1
#define HAVE_LINK 1
#define HAVE_UTIMES 1
#define HAVE_GETGRGID 1
#define HAVE_GETPWUID 1
#define HAVE_GETRESUID 1
#define HAVE_GETMNTENT_R 1
#define HAVE_SETMNTENT 1
#define HAVE_ENDMNTENT 1
#define HAVE_HASMNTOPT 1
#define HAVE_FALLOCATE 1
#define HAVE_SPLICE 1
/* end confdefs.h.  */
/* Define prlimit to an innocuous variant, in case <limits.h> declares prlimit.
   For example, HP-UX 11i <limits.h> declares gettimeofday.  */
#define prlimit innocuous_prlimit

/* System header to define __stub macros and hopefully few prototypes,
    which can conflict with char prlimit (); below.
    Prefer <limits.h> to <assert.h> if __STDC__ is defined, since
    <limits.h> exists even on freestanding compilers.  */

#ifdef __STDC__
# include <limits.h>
#else
# include <assert.h>
#endif

#undef prlimit

/* Override any GCC internal prototype to avoid an error.
   Use char because int might match the return type of a GCC
   builtin and then its argument prototype would still apply.  */
#ifdef __cplusplus
extern "C"
#endif
char prlimit ();
/* The GNU C library defines this for functions which it implements
    to always fail with ENOSYS.  Some functions are actually named
    something starting with __ and the normal name is an alias.  */
#if defined __stub_prlimit || defined __stub___prlimit
choke me
#endif

int
main ()
{
return prlimit ();
  ;
  return 0;
}
