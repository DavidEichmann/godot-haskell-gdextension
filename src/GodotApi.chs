{# context prefix = "GDExtension" #}
#include "gdextension_interface.h"

module GodotApi where

--
-- All Enums
--

{# enum VariantType {} #}

{# enum VariantOperator {} #}

{# enum CallErrorType {} #}

{# enum ClassMethodFlags {} #} -- TODO this is a bit field!

{# enum ClassMethodArgumentMetadata {} #}

{# enum InitializationLevel {} #}

--
-- All Structs
--

data GodotVersion = GodotVersion {
  major :: Int,
  minor :: Int,
  patch :: Int,
  string :: String
}

--
-- All Function types
--

-- {# typedef InitializationFunction )(GDExtensionInterfaceGetProcAddress p_get_proc_address, GDExtensionClassLibraryPtr p_library, GDExtensionInitialization *r_initialization);
