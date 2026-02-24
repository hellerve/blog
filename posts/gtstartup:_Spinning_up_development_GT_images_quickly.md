---
title: "gtstartup: Spinning up development GT images quickly"
date: 2025-08-18
---

I’ve been building development images for Glamorous Toolkit for many years, and I rarely work inside a regular released image. The development image gives me direct access to all the core repositories and latest unreleased functionality, so as a core contributor this is just a no-brainer.

In the beginning, I built my images using the script [on the download page of GT](https://gtoolkit.com/download/). This is good enough for many cases. But after a few weeks, I decided to wrap all the necessary CLI work in a script to tighten the feedback loop and ensure a degree of reproducibility.

I’ve documented [the general approach in the Glamorous Toolkit Book](https://book.gtoolkit.com/how-to-build-a-development-version-of-glam-7z621ikatjzvygzgso1uc857v) for everyone who’s interested in simple, reproducible builds.

But over the years, that setup script has grown to encompass all of my various requirements and standard tweaks, from optional GemStone integration to Git and pager preferences. And it’s gotten interesting enough that [I’ve decided to release it](https://github.com/hellerve/gtstartup).

## An overview

The README in the repository linked above covers most information for prospective users, but let’s recap anyway. What’s in the script?

It’s a development build of Glamorous Toolkit first and foremost, wrapping the recommended way of installing a development image for Windows, macOS, and Linux. So far, so straightforward.

What I’ve done over the years that might be more interesting is add scripts that are run during the build process to set up the image as I like it. They install packages I want in my images and set up the environment as I like it.

Since I sometimes work on GemStone packages as well, I also added a flag for that (`--withgs`). It will install GemStone and start it for you, so you can interoperate with it from within GT. Doing this manually is often a hassle, and this is probably the “killer feature” of my scripts.

## Quickstart

If you want to try it, run the following code in your terminal:

```
git clone https://github.com/hellerve/gtstartup
cd gtstartup
bash mk.sh
# or
bash mk.sh --withgs  # with GemStone
```

That’s all. If you try it, I’d love feedback—issues and PRs welcome.
