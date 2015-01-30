### Building a release ###

1. Rename `simple_cache.rel.orig` to `simpl_cache.rel`.

2. Run `make dev` and invoke in the Erlang shell:
```erlang
systools:make_script("simple_cache", [local]).
```
> This will probably produce errors saying that versions of the applications
> in the rel file are incorrect - correct them and run the command again.

The `local` option sets the abolute path so the release is only usable
locally. The command should produce two files in the local directory:
* `simple_cache.boot`,
* `simple_cache.script`.

Make sure that everything works fine: `make cache1`. This make target
uses the `.boot` file to start up the Erlang VM.

3. Create a tarball with the application
```erlang
systools:make_script("simple_cache", []).
systools:make_tar("simple_cache", [{erts, code:root_dir()}]).
```
The `systools:make_tar/2` should produce a `simple_cache.tar.gz` file
with all the application and the ERTS. Let's unpack the tarball and note,
that all the apps are under `$ROOT/lib` and there's also `erts-*` dir
with Erlang ERTS.

However the release is not usable yet - it needs some customizations.

4. Customize the release
In the `erts-*` from the unpacked tarball there's an `bin/erl.src` file
that has `$ROOTDIR` variable to be set. Then the file is supposed to be
renamed to `bin/erl`.

Also it's convenient to run the release using some script instead of
the command like `erl -sname ...`.

All the utility scripts are already created in the `bin/` directory. Let's
copy it to the top-level directory of the unpacked tarball. The scripts
are:
* `install` - it adjusts the aforementioned `erl.src`
* `simple_cache` - it's a startup script for the release

Also it is worth renaming the `releases/0.1.0` directory to
`releases/simple_cache-0.1.0`. That way it is possible to install
another release under the same directory structre
(for example `releaes/simple_cache-0.2.0`) or install this release on
top of another. The unique name will allow to avoid directory name clashes
in the `releases`. Don't forget to adjust paths in the `bin/simple_cache`.

5. Pack the customized release in a new package
```erlang
erl_tar:create("simple_cache-0.3.0.tar.gz", ["erts-6.3", "lib", "releases", "bin"], [compressed]).

