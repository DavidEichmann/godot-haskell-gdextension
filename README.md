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

Install godot4 so that `godot` is on your $PATH. There are instructions available at https://godotengine.org/.

Clone this repository.

```bash
REPO_DIR=haskell-gdext
git clone git@github.com:DavidEichmann/godot-haskell-gdextension.git $REPO_DIR
```

Open the example project in the godot editor to import resources.

```bash
cd $REPO_DIR
godot example/godot/project.godot
```

Then make the plugin and copy it into place:

```bash
cd $REPO_DIR
cabal build
mkdir -p ./example/godot/bin/
cp ./dist-newstyle/build/*/*/godot-haskell-gdextension-*/f/hs_gdext_example/build/hs_gdext_example/libhs_gdext_example.so ./example/godot/bin/libhs_gdext_example.so
godot --path ./example/godot
```

## Development

How this library is developed

```bash
# We must first dump the godot header file and api json file
cd api
godot --dump-extension-api --dump-gdextension-interface

# Then we must manually update the src/GodotApi.chs file to match that of gdextension_interface.h
# ...  More to come :-)
```


