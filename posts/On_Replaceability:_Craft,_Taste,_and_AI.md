I don’t know about you, but lately I’ve been sensing a lot of anxiety about how
AI is going to replace us. Software engineers are posting worried takes all
over LinkedIn and in private forums, while product-focused people understandably
enjoy the feeling of empowerment a bit of vibe-coding can give them. I’ve even
had a few people book coffee chats or request 1-on-1s to vent, discuss their
approaches, and compare notes.

Personally, I’ve been using AI for code and research extensively for about two
years now—not long enough to be called an early adopter, but long enough to feel
the change under my feet. I’ve built [AI integrations](https://github.com/feenkcom/gt4llm)
and [simple yet sharp tools](https://github.com/cyberwitchery/familiar) for my
work with it, and I have a mostly positive impression of the impact on my work,
broader societal aspects notwithstanding<sup><a href="#1">1</a></sup>.

I’m not worried AI will replace me. Not because I think I’m a special snowflake
with irreplaceable skills, but because I don’t think our value was ever in
pushing out the biggest quantity of code the fastest. If someone’s value is
primarily shipping code for very well-defined issues, then yes, that part is now
cheaper. But that’s not our craft, even if it’s the most visible output, and a
big part of our identity.

Today I want to talk a bit about how I think about questions of AI and
replaceability, and what that means to my craft—productivity, the way my role is
shifting, and how I’m coming to terms with a big pillar of my identity changing.



## AI as part of my craft

Purely producing good code has definitely taken a backseat lately. I still
practice my craft, and I still enjoy doing it. But for a lot of everyday tasks,
I take the productivity gains of prompting a model well, giving it access to my
tools, and then refining and iterating as needed. It’s fun, though definitely
in a different way than plopping into Vim (still my editor of choice, 15 years
and counting) and banging out lines and lines of code to solve the problem.

But we do solve the problem regardless, and in a professional setting, that’s
honestly just as fun for me.

The questions that I didn’t have to ask myself, and that I hear being tossed
around a lot, are questions of identity, belonging, and purpose. We belong to
a tribe, and our tribes were until very recently shaped by our technological
choices. Now that matters less and less, and what matters more and more is
the product aspect, and maybe that’s just not what some of us signed up for,
what gets them up in the morning, what grips them. That’s fine. I feel lost
sometimes, too. But we’re not immutable entities, and we can find meaning
and purpose in lots of different things.

If a lot of your identity is tied to the visible part of the work (shipping code),
then “code becoming abundant” can feel like having to face replaceability. I
don’t think that’s an inevitable conclusion, but I understand why it feels like
that emotionally.

So now I also find purpose in building things that otherwise would have felt
like too much. I can expand my project backlog even more. I shepherd hundreds
of repositories all over the internet, some of them [useful](https://github.com/hellerve/gt4dd), some of them [silly](https://github.com/hellerve/cspfuck),
and I care about most of them. It started to weigh me down a bit when it came
to exploring new things, and I think AI helped me gain back a bit of momentum.
It feels nice.

By understanding AI as a tool first and foremost, it loses a lot of its teeth.
Because it is my tool, and I still wield it. It’s a skill that can be learned
and that is useful, and I know how to and like the process of learning new
things.

But what do I do with it?

### What I use AI for

I use AI as a companion that helps me build things quickly. It bootstraps [CI](https://en.wikipedia.org/wiki/Continuous_integration),
packages software, lays out documentation, and writes code. I also focus on
writing code alongside it, but even more on understanding what it produces,
and keeping the architecture reined in.

The components of a system and how they interact still matter. Finding the
right abstractions and the design language of your environment still matters.
As of now, I wouldn’t let AI create that part, and I take pride in still
producing results that feel like *me* in a way, not just machine output.

And of course, on an even higher level, I still need to think about posture,
about security, about responsibility, accountability, and the positioning in
the wider ecosystem. These are decisions that I need to take, because I will
be accountable. Abundance of solutions doesn’t remove decision-making, it
makes it harder, and maybe even more valuable.

## An experiment

A couple of weeks ago, on a whim, I started an experiment under a moniker
that I had created a long time ago. I’m going all in on AI-first development
within the constraints of [cyberwitchery lab](https://cyberwitchery.com/), a
cheeky little brand that I built for myself (for now without a legal entity
behind it).

The premise is simple: *I want to find out what still matters to me when
raw velocity is less of a concern.*

I’m starting with a few projects that I had in the back of my head anyway, and
that I needed an excuse to work on. [A timetracking app](https://github.com/cyberwitchery/tt).
CLIs and Rust API clients for [Netbox](https://github.com/cyberwitchery/netbox.rs)
and [Nautobot](https://github.com/cyberwitchery/nautobot.rs), and maybe some
other DCIM/IPAM systems. [A tool](https://github.com/cyberwitchery/familiar) to
keep my prompts and agent definitions organized, composable, and lintable.

All of these I could have built by hand, but have now deliberately chosen not to,
simply to learn how an AI-first workflow that is clean, safe, and produces real
output with real value looks like for me. It’s a proper attempt at extending
my skillset in a way that remains aligned with my values.

If you’re curious about the applied side, what “AI-first” means for my day-to-day,
what tools I use as guard rails, and what I’ve learned so far—I’ll write that up over
on [the cyberwitchery blog](https://cyberwitchery.com/log/) soon.

Right now I’m working with Claude, Codex, Gemini, Claude+Ollama. Grok is on my
backlog. I’m trying everything and seeing what sticks for me. I’m expecting
this list to change as all of my other tools have over the years.

And even if my little lab ends up failing, I can say that it produced valuable
output already. I really needed all of the tools it produced, and I think I’m
not alone in that. I call it a lab deliberately, because it is allowed to fail.
I don’t want to make a company, I want a space that I can build and validate or
falsify in, and demonstrate what I actually want to bet on right now: *AI is
not replacing us, but it does change what engineering looks like.*

I want to find out where we’re going<sup><a href="#2">2</a></sup>!

## Fin

I don’t want to diminish your experience if you feel doubt or anxiety around the
topic of AI taking our jobs—especially if you were affected by the weirdnesses
of the current market. All I hope is that we can reframe the problem, and
try to treat AI as a tool first, because that is what I see it as<sup><a href="#3">3</a></sup>. A tool that demands
respect, time, and attention if it is to be learned, surely, but a tool nonetheless.

And I’ve never been one to pass up a good tool.

*Thanks to <!--[Julia Evans](https://jvns.ca/),--> [Moritz Neeb](https://github.com/zormit/)<!--,-->
and [Kai Williams](https://liquidbrain.net/) for reviewing drafts of this post!*

#### Footnotes

<span id="1">1.</span> I’m not blind to the broader societal aspects of AI, but
I think there is room to discuss the tool on its own merits, especially as it
pertains to what we do, without having to face the more unsavory aspects of big
tech and corporate behavior in that particular conversation.

<span id="2">2.</span> If you’re experimenting with this too, [I’d love to compare
notes](https://calendly.com/veit-veitheller/coffee-chat)!

<span id="3">3.</span> Bold claims of people with skin in the game to make the
“line go up” notwithstanding, I cannot survey the current market and see it as
anything more than that. But as with [bitcoin as a new currency](https://blog.veitheller.de/Blockchain:_The_Post_I_Didnt_Want_to_Write.html), there are strong
market forces at play here that want us to lose sight of this. I try to look at
what is there, and not what is presented to us. You are of course entitled to an
entirely different opinion, and I will let my mind be changed quite easily as
new facts come in.
