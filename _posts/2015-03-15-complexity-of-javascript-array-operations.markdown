---
published: false
title: Complexity of JavaScript Array operations
layout: post
---

A recent drive to improve the PureScript Prelude documentation has had me
wondering about the time complexity of some of JavaScript's array operations.

Unfortunately I've been almost totally unsuccessful in finding any reliable
analysis. The majority of what's turned up has been jsPerf experiments on
arrays with less than 20 elements. This is either a terrible indictment of my
googling skills, or a terrible indictment of the JavaScript community. I'm not
sure which yet. There was [one StackOverflow question][] which seemed pretty
good, but it doesn't cite any evidence.

So I thought it was high time we sorted this out. The one I'm interested for
the moment is `Array.prototype.concat`, because it is how PureScript's `cons`,
also known as `(:)`, is implemented.

For the sake of simplicity, we will consider invocations of the form
`x.concat(y)` only, where `x` and `y` are both Arrays.



<script src="/assets/js/benchmark-1.0.0.js"></script>
<script src="main.js"></script>

[one StackOverflow question]: https://stackoverflow.com/questions/11514308/big-o-of-javascript-arrays
