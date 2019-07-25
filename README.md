prelude.fs
==========

One-file solution for various problems writing F#

## What is this?

Prelude.fs extends the standard library with many useful modules and functions.

For example, [the String module](https://github.com/cannorin/prelude.fs/tree/master/src/String.fs) provides functions that were only accessible as member methods, such as `String.replace`.

See [the src directory](https://github.com/cannorin/prelude.fs/tree/master/src/) for details.

## Contributing

Contributions are very welcome!

Note that every source file (inside `src/`) must start with `namespace Prelude`.
`build.fsx` concatenates them into one file, removing the namespace definition.

Also, make sure you execute `fake build` every time before the commit.
If you are using \*nix, you can do it automatically by copying [`copy_this_to_git_hooks_pre-commit`](https://github.com/cannorin/prelude.fs/tree/master/copy_this_to_git_hooks_pre-commit) to `.git/hooks/pre-commit`.
Unfortunately I don't have a Windows PC, so writing a git hook script for Windows will be a big contribution.

It is also lacking documentation, and it will be great to have more useful functions!

## Installation

* You can just copy it to your project or use [Paket](https://github.com/fsprojects/Paket):

```
github cannorin/prelude.fs prelude.fs
```

* It is also available on NuGet: [`dotnet add package Prelude.fs`](https://www.nuget.org/packages/Prelude.fs)

## License

Prelude.fs is licensed under the MIT License.
