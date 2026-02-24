---
title: "Simple SSDLCs"
date: 2025-09-01
---

[Like with SBOMs](/Some_notes_about_SBOMs.html), another thing that appeared weirdly frequently in my work this year is the [Secure Software (or System) Development Lifecycle (SSDLC)](https://its.ny.gov/secure-system-development-life-cycle-standard). It’s a loosely defined concept, but it basically describes a set of documents describing how security is embedded in your software and systems development and architecture process. As such, it’s a successor in spirit to Microsoft’s SDL, revitalized by our many recent security screw-ups and concomitant legislation (e.g. EU NIS2, US EO 14028).

I personally like to look for one in due diligence (DD) work when the system’s security is in any way interesting or certifications such as ISO-27001 are important. It gives me a good overview of the software development process—usually at least, more on that later. I scan for reproducible builds, SBOMs, and signed artifacts; if they’re absent, the risk goes up.

In this post, I’m going to try to give some guidance on how to establish an SSDLC that works for a team that doesn’t already have one, and how to make it practical. It’s going to be lightweight and probably not enough to withstand a full ISO-27001 audit, but it’s a good starting point to get your act together early, and actually drive value with it rather than just pushing paper.

I’ll also add a simple template for you to use, but please don’t take it as gospel. If you light your hair on fire, please don’t blame me, but do hire me to put it out. Link in bio. Comment "SSDLC" (into your laptop speaker) to subscribe to my Substack. Like, comment, and subscribe. Okay, okay, I’ll stop. [Here it is, do with it as you will](https://gist.github.com/hellerve/80dec6ca52d9e207a3d1f95800ccdba0).

## What is an SSDLC?

Honestly, it’s a pretty fuzzy concept. Any sort of description of your development process and how you embed security into the steps qualifies in my book, but then again, I’m not an auditor.

You can go down the rabbit hole and look at the [NIST SSDF](https://csrc.nist.gov/pubs/sp/800/218/final) or [OWASP SAMM](https://owaspsamm.org/). It will get you there, and you’ll honestly be better off than just following my scrappy Markdown doc above. But I would argue that it’s also much more heavyweight than what most small teams actually benefit from, and you can get most of the benefit just by caring, writing down your intentions, and building some automated guardrails.

Now, to have an SSDLC that works, you will definitely, undoubtedly, unquestionably need a threat model. If you don’t have a threat model, you do not know what you are defending against. You do not know what is in scope. Are you defending against automated portscans or the Mossad spiking your CIO’s drink to get to his laptop? These are fairly different scenarios. Call me when you need help with the first, and someone else when you need the second (but do tell me your stories).

Once you have a threat model, you can derive some system properties from it. Many will not change from project to project. Use strong standard cryptography. Do as little of your RBAC yourself as you can. Restrict root access to production systems. Log, alert, monitor.

Other things will be bespoke to your project, like user roles, how tenants are separated, things like that. They are informed by your threat model, but you will still have to think about them.

As part of your SSDLC, you will also think about the implications of any change to that threat model and your security assumptions, and about the risks you introduce, mitigate, defer, or accept. It should help you formalize a process of continuously monitoring the boundaries of your system and whether the assumptions your team made still hold.

## How to introduce an SSDLC?

Assuming you already have a threat model, you can adopt a simple SSDLC in two to three weeks (or sprints, or cycles, or whatever your preferred unit of measurement is). Let me try to formalize it, but do adjust to your needs:

1. Phase 1: Add security implications and threat model delta to issue/user story template and MR/PR template; define your list of approved cryptography (defer to an authority like BSI or NIST if possible).
2. Phase 2: Add your CI steps to generate an SBOM and sign your artifacts ([read my post](/Some_notes_about_SBOMs.html)); pin your base images (by digest) and whatever else can be pinned. Add as many CI automations as possible ([syft](https://github.com/anchore/syft), [trivy](https://trivy.dev/latest/), [gitleaks](https://github.com/gitleaks/gitleaks), [semgrep](https://semgrep.dev/), [renovate](https://github.com/renovatebot/renovate), [cosign](https://github.com/sigstore/cosign), etc.), but not more than you will actually use and maintain, noone likes looking at stale Dependabot alerts.
3. Phase 3: Turn on CI; add a few smoke tests (covering, for instance, log redaction).

What I wouldn’t include from the get-go includes everything from super heavyweight docs, manual gates, or bespoke “security reviews”. If it can’t be automated or proven, it probably won’t happen unless an auditor is looking<sup><a href="#1">1</a></sup>.

## Fin

My word is certainly not the be-all-end-all here. I’m sure you will encounter bumps and roadblocks I haven’t discussed or thought of, and sometimes it can help hire a professional who can help you develop a solid SSDLC (I know a guy, hint hint). But you don’t always need to. If you have a simple enough threat landscape and technology ecosystem, you can hopefully implement  a SSDLC yourself without any issues, and profit from it. And hopefully this post and [my little template](https://gist.github.com/hellerve/80dec6ca52d9e207a3d1f95800ccdba0) can help you with it!

#### Footnotes

<span id="1">1.</span> Fun fact: I’ve encountered organizations where the developers didn’t even know that an SSDLC was in place.
