---
title: "Threat Modeling For the Rest of Us"
date: 2025-09-11
---

Threat modeling is weird. Depending on how you approach it, it’s either supremely boring security theater or a fun tabletop game where your team can play the bad guys and also gain valuable insights into the fundamental assumptions about your product.

The reason for this is quite simple: to make it fun, you have to prioritize it, and truly care. If you dive into the process wanting to produce another document for your ISMS, you won’t have a very good time.

Not all organizations need the full [STRIDE](https://en.wikipedia.org/wiki/STRIDE_model) monty, either, especially if they make a threat model for their own benefit rather than a certification. There are other options, and you might be well served just doing [attack path analysis](https://www.rapid7.com/fundamentals/attack-path-analysis/) on demand when you develop your system’s boundaries.

In the end, what methodology you choose doesn’t matter. What you want is to answer the four questions posed in [the threat modeling manifesto](https://www.threatmodelingmanifesto.org/):

1. What are we working on?
2. What can go wrong?
3. What are we going to do about it?
4. Did we do a good enough job?

As long as you can answer these questions confidently, you’ve done a good job.

In this blog post, I’ll try to give some pointers about how to implement threat modeling in an organisation in a way that is lightweight while still being useful and hopefully can generate some buy-in by your team.

## On modeling techniques

STRIDE is probably the industry standard, pioneered by Microsoft and quasi-standardized across the security community. It’s a great way to think about threats, and many gamifying techniques exist to make fun workshops for your team, either internally or led by a consultant. I’ve hosted and taken part in many of these workshops, and they’re always a good time.

The thing is that a full STRIDE assessment is very heavyweight and extending the model as you go can be a pain, especially if the initial creation has been guided by an expert who made the process effortless for you. Many consultants have their own tools at hand to help you; they will provide threat model delta templates for you that you can attach to PRs, add a section to your user story that details the threat implications, and so on.

That’s all well and good, but it’s not true ownership. In the worst case you’ll end up with a single photo of the threat model you came up with during a workshop five years ago, and dozens or even hundreds of delta documents that amend it. Good luck trying to understand your current threat landscape at a glance.

Before I go into what I recommend I also want to mention that STRIDE is not the be-all-end-all. Other frameworks exist, from [PASTA](https://threat-modeling.com/pasta-threat-modeling) to [OCTAVE Allegro](https://www.sei.cmu.edu/library/introducing-octave-allegro-improving-the-information-security-risk-assessment-process/) and MITRE’s [ATT&CK](https://attack.mitre.org/). There is even a [NIST guide on threat modeling](https://csrc.nist.gov/pubs/sp/800/154/ipd). The literature and practice is deep, and I don’t want to pretend that I understand or even know it all. My point, however, is that you don’t need to know all of the ideology to create a process that is valuable for your system.

## The process

With all of the preliminaries out of the way, let’s start sketching a process. Here’s the CliffsNotes version:

1. Assess the status quo. You can make it a fun workshop with your team, either self-guided or mediated by a professional. Just ensure that the deliverable in the end is a document that is in some way extensible. Not just a screenshot that lives on your Wiki for all eternity.
2. Do a lightweight re-assessment for feature PRs that change inputs, auth, or data flow. Produce a threat model delta.
3. After the PR was merged, play the delta back into the model proper.

To make this fully actionable, we’ll expand each of these points in turn.

### Initial assessment

This one is probably the most effort, but it also shouldn’t need to happen very often. I’d do it once initially, and then again if the need arises, i.e. when developing a new service or if a major redesign is being tackled.

The path most traveled here is probably to do STRIDE using some gamified techniques. There are [card games](https://www.microsoft.com/en-us/download/details.aspx?id=20303), “user stories” for attackers, red/blue/green roleplays (attacker/defender/product), and more. These days, even asking an LLM for an agenda for a workshop might lead to good results. There are also many security professionals that are happy to help here.

The end result should be captured in an extensible document, however. In the case of STRIDE, this can be a simple page on your wiki that contains a table of threats, like so:

```
**diagram goes here** [client] -> [service] -> [store]
**boundaries we defined:**
**assets at risk:**

| id | category | risk | asset/boundary | likelihood | impact | mitigation | proof (test/monitor) | owner |
|----|----------|------|----------------|------------|--------|------------|----------------------|-------|
| S1 | Spoofing |      |                | M          | H      |            |                      |       |
| S2 | Tampering|      |                |            |        |            |                      |       |
| S3 | Repudiation |   |                |            |        |            |                      |       |
| S4 | Info Disclosure | |             |            |        |            |                      |       |
| S5 | DoS      |      |                |            |        |            |                      |       |
| S6 | Elevation|      |                |            |        |            |                      |       |

**non-actions:** we accepted the risk that the government might MitM all of our traffic
```

The format of this deliverable depends on the methodology used and your documentation software, of course, so YMMV. This document should have a clearly defined owner, e.g. your CISO or Head of Security. If you don’t have dedicated security personnel, favor someone with technical expertise as owner of this document.

### Re-assessment on features

When a feature is being worked on that touches anything security-related (think authentication, PII, network configurations, and the like), a small re-assessment should be part of the PR flow.

Once again the format depends on your tools and framework, but it could look like this:

```
**data touched:** PII? secrets? network? tokens?

### risks introduced

| id | risk | stride | likelihood | impact | mitigation | proof | owner |
|----|------|--------|------------|--------|------------|-------|-------|
| S1 |      |        | M          | H      |            |       |       |
| S2 |      |        |            |        |            |       |       |

### risks obsoleted

| id | risk | reasoning |
|----|------|--------|
| S1 | request spoofing | we no longer do any network requests |
```

These should be reviewed—and challenged—like any other part of the PR.

### Delta to model

Someone has to do the dirty work. In this case, it should be the person who owns the initial document created in step 1. You may decide the update cadence of the document yourself, but I favor short feedback loops. If you work in an agile environment, you can do it as part of each sprint or cycle, but you can even do it on merge of each PR that contains a threat model delta.

Keep a changelog and a version of the page for each shipped version of your software (i.e. one list for V1, one for V1.1, etc.). These may rely on the versioning of your documentation software, but it’s important that it’s there.

## Fin

I firmly believe that a simple process that is being followed and has buy in is better than a complex process that will get you through your next audit. Hopefully this little walkthrough inspires you to introduce your own, or overhaul your existing ones if they don’t work for you.

You shouldn’t need me to get this off the ground and running, but if you do, I’m here.
