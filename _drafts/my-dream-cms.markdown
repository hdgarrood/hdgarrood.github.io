---
layout: post
title: My dream CMS
---

I'm writing this because I am convinced that static sites are the way forward,
and because I think there is a really large gap in the tools we have.

I am not aware of any set of tools aimed at producing static sites that can
really compete with a WordPress installation, except in very specific cases
(eg, when everyone who will ever want to administer the site is a web
developer).

These are my criteria. Any potential candidate must pass all of them.

### Static

It should produce static sites only. Websites that have a genuine need to
perform server-side computation are out of scope.

### Easy to deploy

For example, the thought of having to migrate hosting providers should not
inspire feelings of dread. It should be easy to get a zipped archive of the
whole site, for example. It should also be easy to send changed files up to a
server via FTP or rsync.

### No WSYIWYG

WSYIWYG editors are, without exception, vastly annoying. I should be able to
focus on *either* content *or* presentation at any one time, and not be
distracted by the other.

### Easy to use

It must be accessible for people who are not software developers. In
particular, if I create a site on my own and show it to a
non-software-developer, they should be able to find their own way around, and
work on the content, all without help.

I believe that this requires:

* A pain-free installation process on at least Windows, Mac, and Linux.
  Preferably no installation process at all. Running in the browser seems like
  a very good option.
* An instant, *reliable* preview. This is a necessity because WYSIWYG is out.
* Good help pages. Possibly including a 'philosophy' section, although this
  might be a terrible idea.
* Clear separation of content from everything else. People looking to add a new
  post should not accidentally end up wading through directories containing
  sitemap.xmls, robots.txts, stylesheets, or anything else of the sort.

### History

It should provide comprehensive history, including the ability to see when, by
whom, and why (provided there is a decent commit message) anything changed, and
to revert any change. This probably means "backed by git or GitHub" at the
moment.

### Reliable and hackable

That is, not written in JavaScript. (Sorry not sorry). PureScript is a very
good candidate.

It should either provide a good plugin API, or (like Hakyll) give you basic
tools which you put together to describe how to build your site. I'm not sure
if the Hakyll approach is necessarily compatible with user-friendliness
though.  Maybe the UI needs to be able to make more assumptions than it would
be able to in a tool that uses the Hakyll approach?
