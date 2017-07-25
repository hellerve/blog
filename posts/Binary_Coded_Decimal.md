Today I spent some of my day writing [a toy library](http://github.com/hellerve/nibbles)
for [Binary Coded Decimals](https://en.wikipedia.org/wiki/Binary-coded_decimal).
I’m reading through [Write Great Code](https://www.amazon.com/Write-Great-Code-Understanding-Machine/dp/1593270038),
and they were explained in chapter two<sup><a href="#1">1</a></sup>.

Binary coded decimals are an interesting beast in numerical computing—somewhat
out of fashion, but still used in a few select industries, apparently.

I can’t say I’ve ever used them myself, although I’m sure that some libraries I
depend on in some project or other at least implement them. This of course made
me a prime candidate for re-implementing the format.

So I wrote a tiny C library that implements packed Binary-Coded Decimals—that
means that every digit takes up half a byte. It is pretty terrible code, and I
loved every second of working on it. In fine “Write Great Code” tradition most
of the code is an abominable mix of bitmasking and shifting hacks, but it gets
the job done. One of the main restrictions of the library is that it currently
only supports 8–64 bits precision, though it would be fairly trivial to make it
infinte precision. It’s left as an exercise to the reader, let’s say.

Before I leave you, let me give you a loop, slightly more obfuscated than in
my library. I owe the people who find out what it does without reading the
original source a beer, which I expect will get pretty expensive:

```
for(i=0;i<20;i+=2)if(*(x+i/2)){if(!(*(x+i/2)&0xf0))i++;break;}
```
<div class="figure-label">Fig. 1: A loop, evidently.</div>

##### Footnotes
<span id="1">1.</span> As an aside, the book seems quite astoundingly good.
