# Godot-haskell-gdextension

Goals:

* minimal
  * no lense!
  * simple data types
* No TH
  * important for wasm build
* Connect to remote godot instance (optional)
  * allows for hot reloading

## Build / Run

Make sure you've opened the example project in the godot editor at least once to import resources. Then:

```bash
cd example
cabal build
cp ./dist-newstyle/build/*/*/godot-haskell-gdextension-*/f/hs_gdext_example/build/hs_gdext_example/libhs_gdext_example.so ./example/godot/bin/libhs_gdext_example.so
godot --path ./example/godot
```

## Development

How this library is developed

```bash
# We must first dump the godot header file and api json file
cd api
godot4 --dump-extension-api --dump-gdextension-interface

# Then we must manually update the src/GodotApi.chs file to match that of gdextension_interface.h
# ...  More to come :-)
```


