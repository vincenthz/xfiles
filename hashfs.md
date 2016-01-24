hashfs
======

Configuration
-------------

in `$HOME/.hashfs/config`:

`digest` section need to exists with the algorithm one of:

* sha224
* sha256
* blake2-224
* blake2-256

Then follow by `db` section containing a `type`, a `name` and a `path` at minimum.

Example:

```
[digest]
algorithm = sha224

[db]
type = local
name = document
path = /local/document
description = document
preferred-exts = pdf csv txt epub mobi

[db]
type = local
name = photo
path = /local/photo
description = photo stuff
preferred-exts = jpg jpeg

[meta]
type = sqlite3
path = /local/meta.sqlite3
```
