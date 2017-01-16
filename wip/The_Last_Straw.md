I decided to abandon Firefox. This decision was harder for me than
it probably should have been, because I know most of my peers have
already migrated to Chrome. I'm late to the party. But my reasoning
behind the change might be different from that of most others, and
so I will write a quick and angry post about it, because I understand
that is what you do on the internet.

## From friend to foe

Frontend web development in Firefox has been a real pain for a long
time already, annoying enough that it aggravated me during my quick
stints at tweaking a UI or adding features to forms—a chore that I
have to take up more often than I would like. That's unfortunate,
because I used to enjoy working with the development toolbar that
Firefox provides, believe it or not. But both of us changed over the
years, and so what used to be a feature that enhanced my productivity
became an inconvenience. The toolbar of Chrome is just better these
days. When working on huge web applications I sometimes have to change
to Chrome already, because reloading the page with the toolbar enabled
will leave Firefox begging for mercy until it finally freezes. I hear
that that is due to the debugger not being able to handle a few megabytes
of JavaScript code, which is unacceptable—but on the other hand, so
should be shipping multiple megabytes of application code.

But, stubborn as I am, I stayed true to the browser, even as some of
[my favorite features were killed](https://support.mozilla.org/en-US/kb/tab-groups-removal).
After all, these were mere inconveniences, and Chrome has a host of
problems and it is being developed by a company I am not particularly
fond of. I can live with inconvenience. Sometimes I even need
inconvenience, for the sake of creativity and productivity. So I
ignored these problems, reminding myself what a pain migrating to
another browser would be.

## A love unspoken

I like typography. I am not an expert—which you can probably infer
from the layout of my blog—, but I enjoy thinking about it and I
know typography is important for my audience. I know about kerning
and ligatures. I have a basic grasp on good and bad fonts. I read
[Practical Typography](http://practicaltypography.com/), and I enjoyed
it. I payed the author for the experience, and I use [Charter](http://practicaltypography.com/charter.html)
as my main font for a lot of tasks, such as this blog. And look at
the book I linked to just now. Does the layout seem familiar? I will
admit to it, I initially stole his layout of my blog. But I think I
changed, tweaked, and twisted it enough to rightfully call it my own
now—though I will always have to stick a footnote at the end of that
blurb. In short, typography is one of my secret, if a bit untended to,
love affairs.

Firefox decided good typography probably was not all that important,
or at least that is what their development efforts reflect. Let me
elaborate.

## The last straw

I read the transcript of [a talk](http://unitscale.com/mb/reversing-the-tide/)
given by Matthew Butterick at TYPO today. There, he rightfully laments
how our expectations of typography are lowered by bad engineering—or
rather, wrong priorities in the development of embarassingly typographical
technology. He advises the listener to put their money where their mouth
is, and act on their convictions. That rang true with me, which makes it
even more ironic that while reading the transcript I discovered a bug
in Firefox in the interplay of hyphenation and ligatures. Basically, Firefox
will use ligatures *even if* the ligated letters are on different lines,
as might happen with hyphenation. This leads to ugly glyphs on the affected
letters. “Surely that cannot be due to the very actively maintained font
rendering in my browser”, I thought, so I did a little digging and ended
up staring [at this bug](https://bugzilla.mozilla.org/show_bug.cgi?id=479829)
in disbelief—it will also provide you with screenshots, in case you were
unable to follow my imperfect description of the bug. It was reported in 2009.
Let that sink in for a little bit. It has been a known issue for seven years
now, and, unless there has been active conversation about this issue outside
of the Bugzilla thread, it has been largely ignored. Even worse, if you believe
[comment four](https://bugzilla.mozilla.org/show_bug.cgi?id=479829#c4) in the
thread, it had also been a “known problem” for some time before.

I checked the webpage with Chrome. It works. Webkit does the right thing, as
the thread also details. This was the last straw, and I could not let it stand
without acting on it.

## Petty I am

I know this issue is minor, and I know it will eventually be fixed. It does not
affect my ability to read text on the Web. It will, however, bug me whenever it
comes up, even if it's a rare fringe case. I use the Web mostly as a reader,
and as such I feel like I should optimize this experience as much as I can. As
such, I'll stop using Firefox, because my experience with it in the last few
years is best described by a quote [attributed to Douglas Adams](http://www.azquotes.com/quote/1385591):
“The single raindrop never feels responsible for the flood”. I expect my silent
protest to go largely unnoticed, and that is for the best. This experience is
wholly personal, and people with priorities not lined up with mine will draw
different conclusions than I did. But, for me, the flood has happened, and I
hope Chrome will not let it rain on me any more.
