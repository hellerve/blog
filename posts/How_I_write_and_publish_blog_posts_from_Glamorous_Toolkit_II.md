---
title: "How I write and publish blog posts from Glamorous Toolkit II"
date: 2023-09-11
---

[In a previous blog post](https://blog.veitheller.de/How_I_write_and_publish_blog_posts_from_Glamorous_Toolkit_I.html), we looked at how to publish one of my blog posts from Glamorous Toolkit. This blog post will essentially continue where we left off and explore how to publish all of my blog posts that way.

To make that happen, we will implement a button on the top right of the UI that essentially does automatically what we did manually in the last blog post. The code for this will end up [in the repository for this blog post](github.com/hellerve/blog), in a package called {{gtPackage:VhBlog}}.

When we look at the buttons that are already on the toolbar of the page (you can inspect their definition by pressing alt while clicking on them), you will see that they are methods on {{gtClass:LePage}} that are annotated with a `lePageAction` pragma. For instance, check out the definition of the removal action {{gtMethod:LePage>>#gtRemoveActionFor:}} (you can click on any of the blue-ish code things to expand them).

This means we can create such methods ourselves and they will be added to the toolbar. Let’s try it now, and create a method named {{gtMethod:LePage>>#gtPublishBlogActionFor:}}.

We haven’t looked at {{gtClass:VhBlogExporter}} yet, but if you’ve read the last blog post, you already know what it does. We could pretty much copy the code verbatim into {{gtMethod:LePage>>#gtPublishBlogActionFor:}}, but we probably want to factor out some of the logic into the initialization method. Let’s look at {{gtMethod:VhBlogExporter>>#initialize}}.

Nothing much there, but at least we removed some boilerplate from the actual workhorse method. We referred to that one above already, it is {{gtMethod:VhBlogExporter>>#export:}}.

It’s a pretty long method, but it’s all familar code. Except for the lazy accessors {{gtMethod:VhBlogExporter>>#blogDirectory}} and {{gtMethod:VhBlogExporter>>#layoutFile}} that I added. They are simple accessors that initialize slots with defaults if they aren’t set. Take, for instance, {{gtMethod:VhBlogExporter>>#layoutFile}}.

Now that all of that infrastructure is in place, we should be able to try out the button by clicking on it. And, if you are reading this blog post on my personal blog, it worked!

It was a fair bit of work, and all for a simple button, but at least we can end this blog post with a glamor shot.

![Fig. 1: A rendered blog post.](attachments/6y770oghyxtewl0465m30e2uy/Bildschirmfoto 2023-09-11 um 15.40.39.png)
*Fig. 1: A rendered blog post.*

### Fin

This blog post was quite a bit shorter than the last, no least because moving code from Lepiter into methods really is a triviality. Nonetheless, it has to be done, and I thought I might as well show that part. Now, I suppose, you know how the sausage is made. I hope you nonetheless still enjoy it, and it hasn’t spoiled your appetite for reading more. Perhaps, with this new system, I will be tempted to finally write more again.
