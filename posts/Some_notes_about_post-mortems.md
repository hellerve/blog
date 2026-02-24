---
title: "Some notes about post-mortems"
date: 2025-10-06
---

I like good post-mortems. They are usually informative, cool, and unexpected. Unfortunately, I don’t get to read them very often, because most companies I see either don’t have an established culture of post-mortems, or the practice has degraded to a bureaucratic exercise in futility.

In this blog post, I want to talk about what I believe post-mortems are and are not, what makes them useful, and a few pointers on how to achieve that, knowing that every incident is different and requires some wiggle room.

## A working definition

A Post-mortem is a document written after an incident or outage that describes the technical and organizational measures that led to the incident, a description of the timeline of the incident, how it was resolved, and how it was (or is being) ensured that it doesn’t happen again.

A few things a post-mortem doesn’t have to be: long, an apology letter, or a judgment about the people involved in either creating or resolving the issue.

From this very simple, very broad definition, we can derive some axes that I judge post-mortems on:

- Blamelessness: a good post-mortem focuses on systems and outcomes, not on people.
- Informative: I want to know what actually led to this issue. I want to understand it, so I can audit if I trust the proposed solution.
- Actionable: A path forward needs to exist, and it needs to outline organizational and/or technical measures to eliminate the chance of this incident repeating.

This sounds pretty straightforward, but it’s hard to do. Candor is hard when you are still in disaster recovery mode, and keeping calm in these situations is a rare skill<sup><a href="#1">1</a></sup>. Nonetheless, if you can’t write a good report (yet), don’t do it. I’d rather have a late post-mortem than a bad one.

It also requires you to do a bit of root cause analysis, and for you to truly understand the incident. This is required to define a good set of future actions.

Oh, and lastly: when I encounter a company that does not have a record of past incidents beyond tribal knowledge, this is a red flag, both in DD contexts and in my consultancy work. You need to track what happened to learn from it.

## Recommendations

I can’t write your post-mortem for you, so I’ll instead give you some things that I’ve used in the past to make my own post-mortems a bit better. I will assume that candor and good technical writing are a given, or at least fall out of scope of this blog post. It’s an important and undertrained skill, but I cannot give you a cheat sheet for it here.

What I will give you is a template that minimizes the amount of writing you will have to do and that gives you guardrails that will help you keep orientation, as well as a set of action types that are useful to ensure incidents do not repeat.

### A template

Here is a template you can use for post-mortems (but please, tweak it for your org):

```
# Post-mortem: <incident title or identifier>
**When:** YYYY-MM-DD HH:MM–HH:MM (UTC)
**Owner:** <name>
**Review date:** YYYY-MM-DD

## Summary (2–3 lines)

Description, impact, duration, customers/users affected.

## Timeline (bullets, UTC)

HH:MM event
(YYYY-MM-DD HH:MM for multi-day, and finer granularity time if necessary for repro/understanding)

## Contributing factors (organizational and technical)

- …

## Actions

(I talk about action types below, define your own set)

| ID | Type (guardrail/detect/procedure/contract/knowledge) | Change | Owner | Proof (test/monitor/diff) |
|----|-------------------------------------------------------|--------|-------|---------------------------|
| A1 | guardrail | 1MB request limit on /upload | alice | test `e2e_upload_413` |
| A2 | detect    | 401/403 spike alert post-deploy | bob | alert `webhook_auth_spike` |

## Verification

Proof A1–A2 landed (test/monitor screenshots, PR links).

## Learnings

(non-blame, no people, outcomes)

- …

## Follow-up

(Define a due date)

Did incidents drop? Are alerts quieter? Close/extend actions (new due date).
``` 

Ideally this goes into your internal knowledge base. For customer-facing post-mortems, some data scrubbing might be necessary, but the general formula can also be retained. Often, however, your public status page (using [UptimeRobot](https://uptimerobot.com/) et al.) already has a feature to write those up. Those might be a bit less informative than this document, but you can still make them worthwhile.

If you spend more than 90 minutes filling the documents out, you’re probably overcomplicating things. An hour is a good time slot to aim for.

Note for audits and DDs: If you want this to impress, provide an incident index (last 12–18 months), include 2–3 redacted 1-pagers of the most "interesting" incidents (and hope it will nerd-snipe the auditor), and a tiny trend table (incidents/month by severity<sup><a href="2">2</a></sup>, MTTR).

### Action types

We should define a few types of actions we can take after an incident. I will give you a set of general action types, tailor them to the needs of your product and business.

- **Guardrail**: technical measure we can take to eliminate the issue or decrease its likelihood (changed defaults, new checks in our codebase etc.).
- **Detection**: introduce new monitoring capabilities so that we can detect the issue sooner next time or before things crash and burn. Usually does not address the root cause, so needs to be used in concert with other measures.
- **Procedure**: ideally we introduce a runbook for recovery if it does happen again, but this action type also encapsulates any organizational measures taken (e.g. four eyes required for changes to the network etc.).
- **Contract**: Introduce SLOs that capture the issue before it goes out of hand (e.g. maximum 99th percentile response latency).
- **Knowledge**: documentation. Always supplementary, but seldom a bad idea.

This is not the be-all-end-all, so go ham defining your own!

## Fin

Post-mortems should not be middle management performance art, and it shouldn’t be a humiliation ritual for your engineers. Done right, they are an opportunity for your organization to grow, mature, and get better in the future.

Mandatory callout: if you need help fixing issues, responding to incidents, or introducing a productive  error culture in your org, give me a call!

#### Footnotes

<span id="1">1.</span> As an aside, this is one of the main reasons ex-military people are relatively common in cybersecurity. They were trained to keep a level head under pressure much more intense than a service outage.

<span id="2">2.</span> This requires you to also keep track of severity levels. Critical/Major/Minor/Low is usually a good classification, but you can also drop Low, since it might not warrant a post-mortem.
