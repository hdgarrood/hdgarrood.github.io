---
layout: default
---

<nav>
  <ol>
    <li><a href="/">Harry Garrood</a></li>
  </ol>
</nav>

<h1>Blog</h1>

<ul>
{% for post in site.posts %}
<li class="blog-post">
  <time datetime="{{ post.date | date: "%Y-%m-%d" }}">{{ post.date | date: "%d %b %Y" }}</time> &ndash;
  <a href="{{ post.url }}">{{ post.title }}</a>
</li>
{% endfor %}
</ul>
