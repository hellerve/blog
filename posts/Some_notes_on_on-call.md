---
title: "Some notes on on-call"
date: 2026-03-16
---

As organizations scale, some of them realize they need an on-call structure. And
often, when I watch them get set up, the people doing it seem to believe
their situation is unique enough to warrant doing it from scratch. In my
experience, it rarely is, and the bespoke approach rarely works. The problems
are almost always the same, and the solutions are well-documented (the [Google
SRE book](https://sre.google/sre-book/being-on-call/) alone covers most of the
fundamentals).

As I keep seeing the same things, though, I figured I’d write them down. if you
see different failure modes, let me know.

## What on-call is

On-call means assigning someone responsibility for production systems during a
defined window. When something breaks, they get paged, they assess, and they
either fix it or escalate.

It’s not the same as insisting everyone be always reachable. It’s not an
implicit expectation that people check Slack after
dinner<sup><a href="#1">1</a></sup>. And it’s not the most junior person on the
team getting stuck with the pager because nobody else wants it. If you don’t
have a defined rotation with clear expectation boundaries, you don’t really
have an on-call system I’d be confident relying upon.

## Pitfalls

Here are some things I’ve seen go wrong more than once:

- **No rotation, or a broken one.** The same two people always end up holding
  the pager, either because nobody else knows the system well enough or because
  the rotation only exists on paper. If only one or two people can be on-call,
  I’d say you have a bus factor problem first.
- **Throwing people into the deep end.** New people on the rotation should
  shadow someone experienced for a shift or two before they’re on their own.
  Without that, people either avoid joining the rotation or have a terrible
  first experience (with possible business impact), and neither is good for
  the health of the rotation or the business.
- **No clear escalation path.** The on-call person gets paged, can’t figure out
  the issue, and doesn’t know who to call next. Write it down, with names and
  contact details, so that bleary-eyed old me doesn’t have to figure out phone
  numbers from Slack chats.
- **Alert fatigue.** If most of your pages are noise (flaky health checks,
  transient errors, thresholds that are too tight), the ones that matter will
  get lost. I’d argue that every alert that fires should require a human
  decision (if it doesn’t, it shouldn’t be paging anyone).
- **No compensation.** On-call is work. Being tethered to your laptop on a
  Saturday evening is work, and being woken up at 4AM is definitely work. If it
  goes uncompensated, whether through time off, money, or both, you’re going to
  burn out people and/or lose them.
- **On-call without authority.** Being responsible for production but not being
  able to deploy a fix, roll back a release, or take a service down. If the
  on-call person needs to file a ticket and wait for someone else to push a
  button, you’ve achieved nothing to remediate your problems.
- **No runbooks.** The person who gets paged should not need to reverse-engineer
  the system from scratch. A few sentences per common scenario, some links to
  dashboards, the commands to run is often enough to help people. And if you
  find a new bespoke scenario that might happen again, you can add it!
- **Everything is a priority.** If every alert is treated as urgent, it becomes
  hard to tell what actually is a priority. Without severity tiers and some honest
  criteria for what warrants waking someone up versus what can wait until morning,
  you end up with a lot of false emergencies. A lot of things can wait.
- **Bad release hygiene masquerading as on-call load.** If the on-call person
  spends their shift firefighting bugs that your CI should have caught, you are
  staring into an abyss. On-call will absorb the cost of these problems quietly,
  and it will keep doing so until you address them at the source or burn out your
  talent<sup><a href="#2">2</a></sup>.

## Non-negotiables

A few things I wouldn’t compromise on:

- **Published schedules.** People need to plan their lives around on-call. The
  schedule should be visible well in advance, and swaps should be easy and
  tracked. The tool doesn’t really matter (PagerDuty, Opsgenie, even a shared
  Google calendar), just have a single source of truth and something I can plan
  my vacations around.
- **Post-incident measure.** If the same alert fires multiple times and nothing
  changes, something is wrong with the process. I wrote about
  [post-mortems](/Some_notes_about_post-mortems.html) separately, but the gist
  is that every incident that pages someone should at least result in a
  conversation about how to avoid this page in the future.
- **Sustainable load.** If on-call regularly disrupts sleep or eats into
  weekends, that’s a staffing and/or engineering problem, and I’d want to
  address it as such. Tracking the numbers (pages per shift, time to
  acknowledge, hours spent on incidents) helps, but you should take your
  engineers seriously even without KPIs.

## Fin

On-call should be boring. The default should be that nothing happens. If that’s
not the case, it usually points to something that needs attention: the release
process, the monitoring or alerting configuration, maybe the staffing. The goal
is a setup where the pager rarely goes off, and when it does, the person holding
it knows what to do and can actually do it.

These practices are easy to get right in principle and remarkably easy to get
wrong in practice. I think the gap is mostly about whether someone decided to
care about the details.

#### Footnotes

<span id="1">1.</span> Though I’ve seen this exact pattern more often than I’d
like. It tends to happen "naturally" if nobody actively prevents it, and once
it’s the norm it’s surprisingly hard to walk back.

<span id="2">2.</span> Your on-call engineers are not a substitute for a test
harness. I feel like this should go without saying, but somehow I feel compelled
to say it anyway.
