#ifdef USEX11
#include "udposix.h"
#else
typedef void    *voidp;
#endif
#include <stdlib.h>
#include <signal.h>
				/* and associated macros */

/*
 * Uninterruptible malloc().
 *
 * Because malloc() is "unsafe" in the presence of signals(%),  we insure 
 * that it is not interrupted while doing its job by blocking all signals
 * before calling it, and then restoring the signal mask before returning.
 *
 * (%) See "POSIX--PART 1: System Application Program Interface (API)
 * [C Language]; ISO/IEC 9945-1: 1990, IEEE Std 1003.1-1990; Section
 * 3.3.1.3.
 */
    voidp
umalloc(size)
    size_t	size;
{
    voidp	ptr;
    sigset_t	block_set;
    sigset_t	previous_set;

    (void) sigemptyset(&previous_set);
    (void) sigfillset(&block_set);
#if 0
#ifdef SIGIO
    (void) sigaddset(&block_set, SIGIO);
#endif
#endif

    (void) sigprocmask(SIG_BLOCK, &block_set, &previous_set);
    if (size > 0)
       ptr = malloc(size);
    else
       ptr = NULL;
    (void) sigprocmask(SIG_SETMASK, &previous_set, (sigset_t*)0);

    return ptr;
}


/*
 * Uninterruptible realloc().
 *
 * Same rationale as for umalloc() above.
 */
    voidp
urealloc(p, size)
    voidp	p;
    size_t	size;
{
    voidp	ptr;
    sigset_t	block_set;
    sigset_t	previous_set;

    (void) sigemptyset(&previous_set);
    (void) sigfillset(&block_set);
#if 0
#ifdef SIGIO
    (void) sigaddset(&block_set, SIGIO);
#endif
#endif

    (void) sigprocmask(SIG_BLOCK, &block_set, &previous_set);
    ptr	= realloc(p, size);
    (void) sigprocmask(SIG_SETMASK, &previous_set, (sigset_t*)0);

    return ptr;
}


/*
 * Uninterruptible free().
 *
 * Same rationale as for umalloc().
 */
    void
ufree(ptr)
    voidp	ptr;
{
    sigset_t	block_set;
    sigset_t	previous_set;

    (void) sigemptyset(&previous_set);
    (void) sigfillset(&block_set);
#if 0
#ifdef SIGIO
    (void) sigaddset(&block_set, SIGIO);
#endif
#endif

    (void) sigprocmask(SIG_BLOCK, &block_set, &previous_set);
    if (ptr != NULL)
       free(ptr);
    (void) sigprocmask(SIG_SETMASK, &previous_set, (sigset_t*)0);
}
