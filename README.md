# Godot-haskell-gdextension

Goals:

* minimal
  * no lense!
  * simple data types
* No TH
  * important for wasm build
* Connect to remote godot instance (optional)
  * allows for hot reloading

## Development

How this library is built

```bash
# We must first dump the godot header file and api json file
cd api
godot4 --dump-extension-api --dump-gdextension-interface

# Then we must manually :-( update the src/GodotApi.chs file to match that of gdextension_interface.h


```


