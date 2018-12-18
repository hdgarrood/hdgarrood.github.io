---
title: 'GSOC progress update #3: Pursuit deployed!'
layout: post
---

The most significant development since my last entry is that Pursuit has been
deployed! See the [announcement post][] I wrote.

Things that have changed since the last update:

### Hoogle

Hoogle integration is now set up and working. The way it works currently is:

1. We go over every package in the database, select the latest version of
   each, and generate a Hoogle input file from each one.
2. We concatenate all of these into one massive input file.
3. We pass this to the `createDatabase` function exported by the Hoogle
   library to create a Hoogle `Database`.
4. We periodically repeat the above 3 steps, and store the resulting `Database`
   in a `TVar` so that later requests can access it.
5. When a query comes in, we parse it (again with the Hoogle library),
   perform a search, and then convert the `Hoogle.Result` values we get back
   into a slightly different format, which makes it a bit easier to do what we
   want to do with them,
6. Finally, we turn this data into either JSON or HTML (based on the
   request's Accept header) and send it back in the response.

It's still quite basic and has a few obvious flaws (notably, rows look
horrendous in the search results, and also searching for rows fails with a
syntax error) but it does work.

It seems as though Hoogle 5 will have an improved API which will make this
process a lot less error prone, and hopefully faster, too, as well as improving
many of the current deficiencies of Pursuit's Hoogle search interface, so I
think we will look at upgrading very soon after it is released.

### Caching

The caching system has been replaced with a simpler one: when a cacheable
page is served, the Yesod application will also write a file with the same
contents as the response, to a location on the disk mirroring the current URL
path. Then, a proxy server in front of the Yesod application (for us, nginx)
can check for the existence of such a file, and serve it if it exists, with
appropriate caching headers. The server knows when any cached file should
expire, since this only happens when new versions of things get uploaded, and
simply deletes cached files where necessary.

### Uploading packages

We got rid of the old 2-step upload-then-verify process, in favour of a
one-step process that requires authentication at the time of upload (using
GitHub OAuth, as before). It's simpler, and we don't have to worry about
generating secure verification links, or the disk filling up with unverified,
uploaded packages.

`psc-publish` now has a `--dry-run` option, which doesn't require the current
commit to be tagged, and doesn't produce any output. The intended use for this
flag is that you run `psc-publish --dry-run` just before tagging a version, to
check that it is possible to produce Pursuit data. If it works, you go ahead
and tag the version, run `psc-publish` for real, and upload the result to
Pursuit.

Soon, I intend to contribute a feature to `pulp` which performs the above for
you, as well as bumping the version and tagging a commit for the release.
Uploading packages to Pursuit directly from the command line should be
possible; the only difficult part is the authentication (which is not *that*
difficult).

### Deployment

The compile-time code is all gone and replaced with configuration via
environment variables at runtime. This is nice because it means that we're free
to compile a binary for production in any environment; on my machine, on
Travis, on the server, wherever.

### User interface

Links to `Prim` types, such as `String` and `Int`, now work. All that was
needed was a pretend `Prim` module (which I created, see
<https://github.com/hdgarrood/purescript-prim>), and a couple of special cases
added inside the HTML documentation rendering module.

A keyboard shortcut for the search box: just hit S and the search box is
focused. This idea was shamelessly copied from Rust's equivalent tool, and I
think it adds a really nice touch.

I added some basic client-side code which validates packages just before you
upload them, and also which displays the package name and version next to the
form, to reassure you that you selected the right JSON file.

I made some CSS adjustments and media queries, so that Pursuit is usable with
pretty much any device width.

Also, various polishing; things like making 404 errors nicer, adding a help
page, and adding an upload link to the top banner.

### Security

Content Security Policy headers, to mitigate the risk of XSS. We already
sanitize HTML, so in theory, the CSP is not necessary. In practice, of course,
it's good to have multiple layers of defence. Now, inline scripts are prevented
from running at all on browsers that support CSP in response headers (which I
think is most of them).

Raw HTML in Markdown code documentation is now disallowed as well.

[announcement post]: {{ site.baseurl }}{% post_url 2015-07-15-pursuit-deployed %}
