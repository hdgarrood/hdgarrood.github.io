---
layout: post
title: What to expect from services following Heartbleed
published: false
---

Any web service you use that uses HTTPS could have been vulnerable to the OpenSSL [Heartbleed](http://heartbleed.com) vulnerability. It's been a couple of days now since the vulnerability was made public, so I think it's reasonable to expect any service to have issued some kind of statement.

There are two acceptable formats:

* We never used an affected version of OpenSSL.

**OR**

lol

* We were using an affected version of OpenSSL. We have now:
  * Updated OpenSSL to a fixed version,
  * Recreated SSL keys and certificates,
  * Revoked old SSL certificates.

  We suggest that you log out of any active sessions, and change any passwords
  or authentication tokens.

Simply updating OpenSSL is **not enough**, since (as [appears to be the case](https://twitter.com/1njected/status/453781230593769472)) the vulnerability does allow attackers to extract private keys.

The number of websites that have done this is depressingly small. You should be
especially concerned if no such statement has been made and the certificate in
use was not issued in the last couple of days.

GitHub has issued a [model
example](https://github.com/blog/1818-security-heartbleed-vulnerability).
That's the level of detail we should expect.