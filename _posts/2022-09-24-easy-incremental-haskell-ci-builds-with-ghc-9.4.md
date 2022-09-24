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
You're probably already caching Haskell dependencies in CI, so if you're using GitHub Actions, it may be that all you need to do is add one or both of those directories to your `actions/cache` block.
Here's how I got incremental builds working for the `persistent` library:

```patch
diff --git a/.github/workflows/haskell.yml b/.github/workflows/haskell.yml
index afe4dbf1..8d8260bb 100644
--- a/.github/workflows/haskell.yml
+++ b/.github/workflows/haskell.yml
@@ -78,7 +77,7 @@ jobs:
       - uses: actions/cache@v2
         with:
           path: |
             ${{ '{{' }} steps.setup-haskell-cabal.outputs.cabal-store }}
+            dist-newstyle
           key: ${{ '{{' }} runner.os }}-${{ '{{' }} matrix.ghc }}-${{ '{{' }} hashFiles('cabal.project.freeze') }}
           restore-keys: |
             ${{ '{{' }} runner.os }}-${{ '{{' }} matrix.ghc }}-${{ '{{' }} hashFiles('cabal.project.freeze') }}
```

Here's [an example CI build](https://github.com/hdgarrood/persistent/actions/runs/3118763968/jobs/5058276209) where I added a new exported value `secret` to the module `Database.Persist.Postgresql`.
Note that the only modules in the repository which get rebuilt in this build are `Database.Persist.Postgresql` (because it was changed) and `PgInit`, which is part of the `persistent-postgresql` tests, which is rebuilt because that module imports `Database.Persist.Postgresql`, whose interface has changed.
Note also that while Cabal threatens to rebuild everything due to configuration changes, once we get to the building stage, no unnecessary rebuilding actually happens.

For `persistent`, these incremental builds take the build time from around 9 minutes down to around 5 minutes (for a near no-op build).
Of course, the difference will be more pronounced the larger your project is. 
Happy caching!

[GHC merge request !5661]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/5661
