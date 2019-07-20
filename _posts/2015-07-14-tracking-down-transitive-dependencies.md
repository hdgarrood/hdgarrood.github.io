---
title: Tracking down transitive dependencies in Haskell projects
layout: post
---

Mostly a note to myself because I know I'm going to need this later, and I'm
going to forget how to do it otherwise.

The question is: I'm depending on some package transitively in a Haskell
project built using Cabal, and I want to know how that package ended up in the
list of dependencies.

The tool I wanted was `gvpr`. From the man page:

> ### NAME
>
> gvpr - graph pattern scanning and processing language
> 
> ### DESCRIPTION
>
> gvpr (previously known as gpr) is a graph stream editor inspired by awk. It
> copies input graphs to its output, possibly transforming their structure and
> attributes, creating new graphs, or printing arbitrary information. The graph
> model is that provided by libc‐graph(3). In particular, gvpr reads and writes
> graphs using the dot language.

ghc-pkg helpfully includes a command to output a directed graph of package
dependencies in the dot language: `ghc-pkg dot`. In this graph, an edge from A
to B indicates that A depends on B. Therefore, we want to select all of the
edges which point towards the package we're interested in.  Here, I wanted to
know which package was depending on `cpphs`, so I did this:

```bash
$ cabal sandbox hc-pkg dot | \
    gvpr 'E[$.head.name=="cpphs*"]' | \
    dot -Tsvg > deps.svg
```

That argument to `gvpr` is saying to select edges (E) where the head node
($.head) had a name starting with "cpphs". The star means "match any string of
0 or more characters"; it is necessary because the version appears in the node
name, eg "cpphs-1.19".

The result:

![The output of the above command](dependencies.svg)
