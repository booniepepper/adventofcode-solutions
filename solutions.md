---
title: /solutions
---

<ul>
  {% for post in site.posts %}
    <a href="{{ post.url }}">{{ post.title }}</a>
  {% endfor %}
</ul>

---

## day 4

language: JavaScript

```javascript
{% include_relative day-04/solve.js -%}```

```javascript
{% include_relative day-04/solve2.js -%}```

---

## day 3

language: Ruby

```ruby
{% include_relative day-03/solve.rb -%}```

---

## day 2

language: Erlang

```erlang
{% include_relative day-02/solve.erl -%}```

---

## day 1

