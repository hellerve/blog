I want to present yet another little project of mine that explores how to make
programming as naive as possible. In previous installments we have looked at [a
terminal editor](http://blog.veitheller.de/Braindead_Editing.html) and [a data
transformation
library](http://blog.veitheller.de/Braindead_Data_Transformations.html). This
blog arguably is part of this tradition as well.

What I want to talk about today is [a chat
system](https://github.com/hellerve/hi) I wrote a while ago. It's written in Go
and leverages websockets. The UI's language is inspired by [Material
Design](https://material.io/), but not really adhering to it. It's also not
responsive, because I couldn't be bothered as of yet. Let me walk you through
the ideas and guidelines I adhered to and the design decisions I made on the
way, because I feel like it is a different kind of chat system alltogether.

You can see it in action [here](https://veitheller.de/hi).

## Speed, Size, Simplicity

Let me start off by introducing the components you can see. The frontend is
completely custom, i.e. I don't use any libraries, frameworks, or external
tools. I optimized it for size and speed while not wanting to sacrifice
functionality, and it seems to have worked. It is okay on the eyes and performs
well enough under pressure—though, admittedly, I haven't rigorously verified
that claim.

The interactions are fairly simple: After you provide a username, you will be
logged into the default channel, which is `#general` right now. You will be
greeted by a little channel message and can start typing right away—unless
you're on mobile, in which case all bets for sending messages are off. Typing
the command `/list` will enumerate all the users in a channel, `/channels` will
enumerate all the channels (remember IRC?). You can join a channel by typing
`/join <channel>`—or create it, if it's not there. Inversely, typing `/leave
<channel>` will leave the channel, and destroy it if you're the last user
subscribed to it. I could add fancy buttons for all of these interactions, but I
like a good text-based interface.

For privacy reasons, all interactions are ephemeral. There is no database. All
the things you say are only visible to the people who are subscribed to the
channel at the moment you send them. There is no history, just like in a regular
in-person conversation.

## Is this a good idea?

Now for the part that is not as ordinary. You can send messages to channels
you're not a member of. This again tries to mimic real-world communication, and
is the equivalent to saying something to a group of people and then walking
away. This is generally a dick move and I can construct all kinds of situations
where this abused, but for my experiment I assume that everyone is a well
behaved person on the internet—this is where the naivete comes in.

This is a simple addition, but it completely changes the semantics of a channel.
It is more like a stream, but not in the [Zulip](https://zulipchat.com/) sense.
There are readers and writers, and they are not necessarily the same people. In
case yo're a little confused about the implications of this: I don't think I
know what this means, either. But it is fun exploring that idea.

## A work in progress

Occasionally the websocket connection will terminate for no apparent reason, and
there is no automatic reconnect mechanism yet, because I was too lazy. When you
try to connect with a nickname that's already taken, the UI will take you to the
chat window only to tell you that the username is already taken.

I cut all kinds of corners. But it is a simple, performant chat system that
required me to write around 40 lines of JavaScript and around 200 lines of
Go—and I'm actually fairly happy with its architecture. It is a fun project, and
if any of you are interested in working on it with me, or have any kind of
feedback, you know where to find me.
