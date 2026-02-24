---
title: "Secure-by-default over Manuals"
date: 2025-09-19
---

My last few posts on [SSDLCs](/Simple_SSDLs.html), [SBOMs](/Some_notes_about_SBOMs.html), and [Threat Modeling](/Threat_Modeling_for_the_Rest_of_Us.html) showed a clear personal bias that I’m aware of but have never really spoken much about publicly: I don’t like pushing paper.

Now, don’t get me wrong, I love writing, and good documentation is worth its weight in gold—whether for  onboarding handbooks, architecture drafts, or design documents. But I’m not a big fan of checking boxes for compliance, and I believe neither are most good auditors. I believe most of the players in the industry are acting in good faith, and that required ISMS documentation is derived from useful documentation that can help make a system more resilient.

The problem lies in the specification, execution, and ultimately, in incentives. We don’t get audited deeply enough on whether we actually execute the policies we write up, and most of them end up as dead weight.

Now, professionally, this is not an issue for me: I can invoice endless hours drafting up ISMS documents no one will ever read, let alone execute. But that’s not the kind of person I am, and not the kind of industry I want to be a part of.

So, I always focus on actionable items. Luckily, a lot of our security processes can and should be automated, and while they require some initial investment to set up, they then run silently in the background, much like tests or staging deployments.

The marketing hook for this would probably be something like “the safest document is the one no one has to read”—if you just enforce secure defaults, and make escape hatches loud and obvious, then your system is better off and your team can focus on shipping things.

## Why defaults?

Defaults always win. They reduce the number of choices I have to make and lower the footgun density in your system.

It also means that I have to write fewer tests, because I can write a test case that asserts the base security assumption holds, and everything else falls into place unless I tamper with the default, which should hopefully be obvious to any reviewer.

Relatedly, noise is lower: I don’t have to configure things every time and, worst case, screw it up. I can rest easy knowing that things are made correct for me.

At the same time, they are not prisons. You can override them if you need to, you just need to be explicit about why and provide a good reason for it, and those are good things.

## But what does it mean?

Let me clarify a bit what I mean by secure defaults by giving some examples for good practices I hope we can all agree on.

1. RBAC should be set up to be least-privilege. There should ideally be no wildcards and per-service roles. If you need a “god-mode” admin, protect it accordingly.
2. TLS is on by default. We exclusively use modern ciphers, reject plaintext, and have a defined rotation process.
3. We log aggressively and always with redactions (for instance using a denylist + patterns).
4. We have defined input bounds as they pertain to size, timeout, content types, and so on.
5. We use dependency pinning for everything we can pin (using both lockfiles and image digests etc.).
6. All of our containers run non-root and read-only, and our templates enforce that (they drop caps and perform health probes).

These are examples for easy things we can enforce to elevate our system to a baseline of security that lets us avoid thinking about these issues every time. Of course, there are many more defaults we can apply, from network structure (e.g. central ingress or network segmentation) to workflow requirements (e.g. all known present vulnerabilities in the SBOM should be in VEX, otherwise they are seen as issues).

Overriding these defaults should be costly and it should be obvious: we should be alerted if an unpinned dependency is introduced or a new container does not use our defined baseline. These overrides should be the focus of a periodic audit and should naturally be placed under even greater scrutiny than the rest of the system.

### The economics of trade-offs

What we are doing here is trading off people-time (working on docs, keeping them healthy and up to date, training) for machine-time (keeping everything in line automatically). People resources are scarce and expensive, compute is cheap.

Of course, it is not a one-to-one switch. You still need to maintain your automation stack, and a page of docs does not translate to an automated check in your pipeline. It is, however, worth keeping these costs in mind when considering your options.

The hierarchy of controls goes from defaults and guardrails to manuals and training. The higher you step, the more expensive and less scalable the control. All of the rungs of this hierarchy are important and should be used, but the choice should be explainable.

## Migration pathways

The nice thing about defaults is that they are modular, and a migration doesn’t have to take an all-or-nothing approach.

In an ideal world, such requirements are already documented. The task then becomes going through these documents, and supplement them, paragraph by paragraph, with automated checks.

If you don’t have these things documented to begin with you are essentially working on a green field, and anything you add is the foundation of a new, better security posture. Win-win all around.

## Fin

I’m a big fan of automation, especially when paired with good guidelines. It’s essentially bridging the gap between planning and doing, and lies somewhere between the real world and your system’s goals.

Most developers and operators are good at building checks for themselves, and automating often has better buy-in than a manual. Hopefully this little post helps you leverage that for your system’s benefit!
