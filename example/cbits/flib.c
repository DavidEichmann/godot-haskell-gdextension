#include "HsFFI.h"
#include "Rts.h"

// Start the RTS on shared library load
static void flib_init() __attribute__((constructor));
static void flib_init()
{
  static char *argv[] = {"", 0}, **argv_ = argv;
  static int argc = 1;
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  conf.rts_opts = "-N4";
  // conf.rts_opts = "-p -l-au -ol~/game.eventlog";
  hs_init_ghc(&argc, &argv_, conf);
}

// Stop the RTS on shared library unload
static void flib_fini() __attribute__((destructor));
static void flib_fini()
{
  hs_exit();
}
