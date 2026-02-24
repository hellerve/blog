---
title: "News from The Ministry of Bad Ideas, or: Delayed F-Strings"
date: 2025-08-02
---

I’ve been writing a lot of Python code that does string templating lately. Historically, I’ve been using `format` or [Mustache](https://mustache.github.io/) for that, but in the last few years, f-strings have increasingly become my simple templating engine of choice, especially for shorter templates or those that need embedded logic. I still have to use `jinja2` a whole lot in other contexts, and `string.Template` also exists, but neither sparks joy in me, so I reach for f-strings whenever I can.

Now, the issue with f-strings is that they are *immediate*. They evaluate in place. Why is this sometimes a problem? Occasionally, I want to define a template now and fill it in later:

```python
my_template = f"""{name.capitalize()}:

{description}
"""

# somewhere else entirely in my code
my_template.format(
	name="entity",
	description="description of my entity"
)
```

I could _almost_  do this with trusty old `str.format`, but then I lose the ability to embed logic directly in the template. Of course, this is rarely an issue, and has a simple solution: put it in a function.

```python
def my_template(name, description):
	return f"""{name.capitalize()}:

{description}
"""

# somewhere else entirely in my code
my_template("entity", "description of my entity")
```

But where’s the fun in that? Let’s do something unnecessary and complex instead. It’ll blow up in production and give you the perfect excuse to earn some on-call bucks.

## Building an object

So what we are going to do is create an object we can call `format` on as expected. We want something that behaves like f-strings and `str.format` simultaneously, evaluated on-demand with variable bindings, but still supporting logic. Let’s call it `Fstr`.

```python
class Fstr:
	def __init__(self, string):
		self.string = string
		
	def format(self, **kwargs):
		# TODO: what do we do here?
		pass
		
my_template = Fstr("""{name.capitalize()}:

{description}
""")

# somewhere else entirely in my code
my_template.format(
	name="entity",
	description="description of my entity"
)
```

So far, so unimpressive. But what do we do now? As always, the answer is to use `eval`! It’s dangerous, slow, and therefore perfect.

```python
class Fstr:
	def __init__(self, string):
		self.string = string
		
	def format(self, **kwargs):
		expr = f"f'''{self.string}'''"
		return eval(expr, {}, kwargs)
		
my_template = Fstr("""{name.capitalize()}:

{description}
""")

# somewhere else entirely in my code
my_template.format(
	name="entity",
	description="description of my entity"
)
```

So what’s actually happening here? What does `f"f'''{self.string}'''"` mean? Naturally, we are turning a regular string into an f-string literal using an f-string!

In case that explanation didn’t help, let’s visualize what this expands to.

```python
# the input
string = """{name.capitalize()}:

{description}
"""

# the f-string
f"f'''{string}'''"

# the output
f'''{name.capitalize()}:

{description}
'''
```

So now we have an appropriate string literal. All we need to do to close the loop is `eval` it and give that evaluation the appropriate bindings. Luckily, the bindings already got passed into the function in the appropriate format (keyword argument handling did it for us), and we can just pass them into `eval` as is!

## That’s all, folks!

I hope you enjoyed this slight return into weird meta-programming territory. A bit more tame than what long-time readers might be used to, but it excited me enough to write it up anyway.

See you around!
