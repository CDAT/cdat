#ifndef OSF1

#define R_OK 4
#define W_OK 2
#define X_OK 1
#define F_OK 0

#else

#include <unistd.h>

#endif
