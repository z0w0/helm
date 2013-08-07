#include <stdio.h>
#include "HsFFI.h"

#ifdef __APPLE__
#include <objc/objc.h>
#include <objc/objc-runtime.h>
#endif

#include <SDL.h>

#ifdef __GLASGOW_HASKELL__
extern void sdl_main(void);
#endif

int SDL_main(int argc, char** argv)
{
    int i;

#ifdef __APPLE__
    void* pool =
        objc_msgSend(objc_lookUpClass("NSAutoreleasePool"), sel_getUid("alloc"));
    objc_msgSend(pool, sel_getUid("init"));
#endif

    hs_init(&argc, &argv);

    sdl_main();

    hs_exit();

#ifdef __APPLE__
    objc_msgSend(pool, sel_getUid("release"));
#endif
    return 0;
}