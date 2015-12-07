#ifndef __ofc_noopt_h__
#define __ofc_noopt_h__

#if defined(__clang__)
#define NO_OPT __attribute__((optnone))
#elif defined(__GNUC__)
#define NO_OPT __attribute__((optimize("O0")))
#else
#warning "Code may optimize incorrectly on some compilers."
#endif

#endif
