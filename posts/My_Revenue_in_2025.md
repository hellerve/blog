<link rel="stylesheet" href="./assets/200/stats.css">
<script type="module" src="./assets/200/stats.mjs"></script>

A few weeks back, I asked on the [Recurse Center](https://www.recurse.com/) chat
whether people would be interested to learn about my financials. A lot of people
are interested in starting a career in consulting or contracting, but not many
people know what that actually means.

So, I decided to give all of those people a public data point. Now, as with most
single data points, you should take it with a grain of salt. Your experience is
not mine, but I do hope it gives you a picture of what it _could_ look like.

Caveat emptor: all the numbers in this blog post except for raw revenue are
rounded somewhat.

## Preamble: my situation

To put things into perspective, let’s start by examining my place in life. Everyone’s
situation is different, and it will influence how much time you can spend and how you
end up pricing it.

So, a bit about me. I’m a father of two (preschool and toddler) in his early thirties.
I take over a meaningful chunk of the care work since my partner works shifts, though who
is in the lead changes over time. As I have the more flexible job (at least most of the
time), I am the one that jumps in when the need arises.

I am located in Germany, I have about 13 years of professional experience, and I just
completed my sixth year as an independent technologist.

From what I can tell in the current market, my rates are pretty high for an independent
freelancer if you consider programmers, pretty standard for an agency, and very low
for, say, a fractional CTO (though services like that vary wildly in pricing anyway).
I also have some discounts for long-running contracts or repeat customers in there
which you won’t find in public price lists.

## I: 2025 in review

2025 started out rough for me. My main client for the past few years first cut down
my hours considerably, then completely ended the contract about halfway through the
year. Both of these came with incredibly short notice windows (2 weeks each), so I
had to think on my feet.

Now, this is my fault, and I accept responsibility. I rested on that contract a bit,
and let old connections go into hibernation. When the new year hit, it took a while
for procurement to get started again, and then even longer for the new contracts to
finalize.

I’m in the privileged position to rarely look for projects actively, and it’s not
my strongest suit. Even so, I’m proud of the fact that I was able to pull through
this year with what ended up being an ultra solid, if not record-breaking, final
revenue.

I ended up making around 150,000€ (this is pre-taxes, but without VAT). This is
a solid year for me, though I’d aim for 200,000€ in a regular year, and I took
less time off than I usually do. Still, this is a lot of money, and we’re still
well above typical household income.

But let’s break this down more.

## II: Raw revenue

Let’s start with some numbers, shall we?

First, monthly revenue.

<figure data-plot data-src="./assets/finance_2025/monthly_2025.csv" data-x="month" data-y="total" data-type="bar" data-title="monthly">
<figcaption>
Monthly revenue
</figcaption>
</figure>

We see the scramble in January and February very clearly here. I worked a lot in those
months, mostly unpaid acquisition work to compensate for the immediately lost revenue.
While we had the runway, I won’t pretend that it wasn’t stressful, even after six years
of working in a heavily-fluctuating environment.

In March, things started to pay off, though I’d say it took until October until things
started to feel fully back in control. December was the first truly comfortable
month—more on the outlook later.

<figure data-plot data-src="./assets/finance_2025/monthly_2025.csv" data-x="month" data-y="cumulative" data-type="line" data-title="cumulative">
<figcaption>
Cumulative revenue
</figcaption>
</figure>

Cumulatively, the jump from February to March was the most noticable, and the one that
reassured me most. Around then is when I went from almost-panic mode into a more relaxed
mindset.

In hindsight, this graph almost looks smooth, but that’s not what it felt like. Every
month under 10,000€ revenue felt like a bit of a gut punch, because realistically, that’s
a month “in the red” for me.

## III: Revenue by activity

Now that we know how much money I made, let’s look at where I made that money. Now, these
are buckets that I made up, and they are not “clean”. Especially the bucket named “Management”
is messy: most, though not all, of my engagements see me in a team lead or comparable position,
but this bucket stays small because I only put those engagements in it that explicitly were
“management only”, with no additional components like software development or security.

<figure data-plot data-src="./assets/finance_2025/buckets_2025.csv" data-x="bucket" data-y="revenue" data-type="bar-rotate" data-title="revenue by bucket">
<figcaption>
Revenue by bucket
</figcaption>
</figure>

We see that the three biggest pillars this year were DD, R&D, and security. That makes sense
directionally, and feels closest to what I spent this year thinking about.

Pure software development (nowadays often with some sort of AI twist) and network automation
took back seats this year, but it looks like that might not be the case again next year (more
on 2026 later).

But there is a wrinkle here: raw revenue and time spent don’t actually map cleanly!

<figure data-plot data-src="./assets/finance_2025/buckets_2025.csv" data-x="bucket" data-y="person_days" data-type="bar-rotate" data-title="PD by bucket">
<figcaption>
PD by bucket
</figcaption>
</figure>

Before we talk about the numbers: these are raw paid client work days. I spend considerable
time on procurement, talks with prospective clients, negotiation, and unpaid work-adjacent
things (OSS work, blog posts, etc).

I spent way more time on R&D/Deep Tech projects than on other things, and in the end this
didn’t show up cleanly in revenue. This means two things: R&D is something I do because I
love it, and it’s effectively still a bit “subsidized” by other professional engagements.

To drive this point home even more, we can calculate the daily revenue for each bucket:

<figure data-plot data-src="./assets/finance_2025/buckets_2025.csv" data-x="bucket" data-y="eur_per_day" data-type="bar-rotate" data-title="Effective revenue per day by bucket">
<figcaption>
Effective revenue per day by bucket
</figcaption>
</figure>

My rates across various engagements map pretty well to my rates posted online: somewhere
between 1,000€-1,400€ per day, depending on what we build and how.

My R&D work is well below those rates, at around 666.66€. That’s pretty low! Now, on a
fully booked month, that would still work out to 13,300€ or so and be perfectly livable,
but it’s a stark contrast to my other work.

Still, it is work that continues to bring me tremendous amounts of joy, and I wouldn’t
trade it for anything in the world.

## IV: Revenue by client

The last puzzle piece is the number of projects and clients. In total, I completed 18
different projects in 2025 for 6 different clients. That’s not counting engagements as
a subcontractor, in which case the number would swell to something like 10 different
clients.

For an agency, that would be a pretty high concentration risk, though not unheard of.
For a solo technologist, it’s perfectly comfortable, especially considering the fact
that I’ve had a good rotation of larger clients in the last six years.

Still, it’s important to see how top-heavy we actually are:

<figure data-plot data-src="./assets/finance_2025/clients_2025.csv" data-x="client" data-y="share_pct" data-type="bar-rotate" data-title="Concentration: revenue by client">
<figcaption>
Concentration: revenue by client
</figcaption>
</figure>

Three of the clients this year made up 67% of the revenue. That’s meaningful! But,
as I said, not a red flag for me, more of a sign who to put in my new year’s well
wishes.

## V: Outlook to 2026

I said 2025 was not a typical year, just the year I analyzed. To put some proof
behind that statement, let’s create an outlook for 2026.

I’ve effectively made three buckets for now: projects that were signed, projects
that were agreed on, but not signed, and those where I sent an offer or that are
due for renewal, but haven’t been renewed yet.

<figure data-plot data-src="./assets/finance_2025/pipeline_2026.csv" data-x="stage" data-y="eur" data-type="bar-rotate" data-title="2026 pipeline">
<figcaption>
2026 pipeline
</figcaption>
</figure>

So far, we are looking at around 111,000€ in project volume in the pipeline. I’m
confident that at least 75,000€ of that will materialize, most of it in the first
half of the year. Not in the list is work that I know is coming but cannot quantify
yet, and work that may happen. In total, I’m thinking that the first half of next
year will look much more like December than January 2025.

## Fin

I hope this was an informative look at my financials for 2025, and that reading
it did not give you quite as much anxiety as it did for me to write it. It’s
scary to put yourself out there like that, and I grew up in an environment where
talking about money was kind of taboo.

I do hope you took something away from it, though, and if you are an independent
technologist of any sort, I hope that you’ll consider putting some of your numbers
out there also. It might not look as glamorous as a picture of an expensive vacation
without further context, but it’s a lot more honest.
