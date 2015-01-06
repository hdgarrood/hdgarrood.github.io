---
layout: post
title: Deploying Jekyll on Shared Hosting
---

Recently I have been wanting to have the following:

* a Jekyll site,
* with source hosted on GitHub,
* which uses Jekyll plugins (this rules out GitHub Pages).
* which also is served with HTTPS (which also rules out GitHub Pages, as far as
  I know),
* that is automatically built after each push to GitHub,
* and then deployed on a shared hosting server.

After some Googling, this doesn't appear like a very common thing to do.
Maybe it's that most people who would use Jekyll stay far, far away from shared
hosting. From my experience this seems like a very sensible thing to do.
Unfortunately I'm stuck with it for now.

Anyway, I hacked together a solution recently and felt like sharing.

* A Ruby application ("site-builder") runs on Heroku and listens for webhooks
  from GitHub, which are delivered after each push.
* On receiving a push, it will clone the site repository, build the site,
  create a gzipped tar archive of all its content, and then POST it to a
  configured URL on the shared hosting server.
* A Perl CGI script on the shared hosting server ("upload-site") receives the
  archive and unpacks it into your `public_html` directory.

If this might be useful for you, feel free to use one or both of these:

* <https://github.com/eums/site-builder>
* <https://github.com/eums/upload-site>

**NB: I have only tested this on one shared hosting provider (Namecheap).
This might break your stuff. If that happens I will not accept any
responsibility**. Notably, the current behaviour for upload-site is to delete
everything in the `public_html` directory before copying the new content over,
so please be aware of that.
