---
layout: post
title: Easy incremental Haskell CI builds with GHC 9.4
---

Last year, I spent a little while putting together a GHC patch which changed how GHC determines whether a source file has been modified since a previous compile (and therefore whether it needs to be recompiled).
Until recently, GHC would detect whether source files had been modified by comparing the source file modification time to the object file modification time; if the former was newer than the latter, then GHC would trigger a recompile.
My patch changed this behaviour so that the hash of the contents of the source file would be stored within the corresponding .hi file, and GHC would determine whether the source file had been changed by comparing this hash to the source file's current hash; if the hashes match, the source file can be considered unchanged, even if its modification timestamp has changed. With this patch, source file modification times play no part in recompilation checking.
That patch was rolled into [GHC merge request !5661][]. I'm thrilled that the functionality finally made it into GHC 9.4, which means that getting incremental Haskell builds working in CI is easier than ever!

What does this have to do with CI?
The problem with using source file timestamps for recompilation checking is that in CI, the timestamps of all of your source files are likely to be around the time the current build started, which is going to be newer than any cached build products.
This means that GHC will consider every single source file to have changed, even if most of your source files are unchanged since the previous commit.
One approach you can take is to start modifying source file timestamps manually &mdash; perhaps by setting the modification time of each source file to the time of the commit that most recently touched that source file.
However, modifying source file timestamps is dangerous, because you can easily end up persuading GHC that build products are up to date when they aren't!
Using content hashes for recompilation is much safer; there's basically no way GHC can make mistakes about whether a source file should be considered up to date, other than perhaps as a result of a hash collision.

## Ok, so how do I enable incremental CI builds?

Just cache the directory that your build tool stores its build products in!
By default, with Cabal, that's `dist-newstyle`.
For Stack, it's `.stack-work`.
If you're using GitHub Actions, it may be that all you need to do is add an `actions/cache` block to your CI workflow.
Here's an excerpt from the GitHub Actions configuration I used to get incremental builds in CI for the `persistent` library:

```yaml
- uses: actions/cache@v2
  with:
    path: |
      ${{ '{{' }} steps.setup-haskell-cabal.outputs.cabal-store }}
    key: deps-${{ '{{' }} runner.os }}-${{ '{{' }} matrix.ghc }}-${{ '{{' }} hashFiles('cabal.project.freeze') }}
    restore-keys: |
      deps-${{ '{{' }} runner.os }}-${{ '{{' }} matrix.ghc }}-
- uses: actions/cache@v2
  with:
    path: |
      dist-newstyle
    key: dist-${{ '{{' }} runner.os }}-${{ '{{' }} matrix.ghc }}-${{ '{{' }} github.sha }}
    restore-keys: |
      dist-${{ '{{' }} runner.os }}-${{ '{{' }} matrix.ghc }}-
```

This configuration ought to work for most Haskell projects (perhaps with some minor tweaking).

Note that I've opted to create two separate `actions/cache` blocks.
The first block is for caching compiled Haskell dependencies, and the second is for caching incremental build products for the Haskell modules in your repository.
It makes sense to keep these separate because the dependencies cache will change quite rarely, whereas the incremental build products cache will change after most builds.
By keeping the caches separate, we can control how often each gets uploaded.
This lets us update the incremental build products cache at the end of each build, without needing to upload copies of the usually-unchanged dependencies cache at the end of each build as well.

This configuration assumes that your GHC version is available in `matrix.ghc`.
It's a good idea to include the GHC version you're using in your cache key even if you aren't building with multiple GHC versions, so that you don't waste time or disk space trying to reuse a cache from a previous GHC version when upgrading to a new GHC version.
If your CI uses `stack` instead of `cabal`, you may wish to use the hash of `stack.yaml.lock` instead of `cabal.project.freeze` as the last section of the key for your dependencies cache, and cache the `.stack-work` directory instead of the `dist-newstyle` directory for your incremental build products cache.
If you don't have a freeze file checked in to your repository (either `cabal.project.freeze` or `stack.yaml.lock`) you'll have to create one before the `actions/cache` block.
Cabal projects can do this by running `cabal freeze`.

Here's [an example CI build](https://github.com/hdgarrood/persistent/actions/runs/3122786941/jobs/5064956005) where I added a new exported value `secret` to the module `Database.Persist.Postgresql`.
Note that the only modules in the repository which get rebuilt in this build are `Database.Persist.Postgresql` itself and the `persistent-postgresql` test suite, which are rebuilt because they import `Database.Persist.Postgresql`, whose interface has changed.
Note also that while Cabal threatens to rebuild everything due to configuration changes, once we get to the building stage, no unnecessary rebuilding actually happens.

For `persistent`, this configuration shaves around 2 minutes off the build time (~20%).
Of course, the difference will be more pronounced the larger your project is.
Happy caching!

[GHC merge request !5661]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5661
