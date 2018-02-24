Some companies manage to annoy me enough with arbitrary restrictions for me to
double down and not only try to overcome them, but also blog about it. Last time
we looked at [Lastpass lying to us](https://blog.veitheller.de/LastPass,_Or:_Dont_You_Tell_Me_I_Cant.html).
This time we’re going to look at a long distance bus company from Central
Europe, [FlixBus](https://flixbus.com). If you’re from the US, just imagine I’m
talking about Greyhound, sans social stigma.

I frequently take the public transport for short and long distances, as you
should be able to in a civilized country. Sometimes I take the train,
sometimes the bus. In Germany, FlixBus operates with an approximate market
share of 93% percent of long distance bus travel. This means that I often board
their lime green vehicles. It also conveniently excuses me from having to
justify why I still use their services although I’m unhappy with them: they’re
simply the only real player. Monopolies work that way.

Truth be told, I’m mostly happy with their services. Of course there are the
ridiculous price hikes during the holidays, but that’s not really unexpected.
You have a mostly undisgusting bathroom, regular breaks, snacks and drinks for
sale, and free WiFi. It’s pretty nice, all in all.

Today I want to talk about the WiFi. You see, if you offer me a service, I will
claim my right to complain about it if it’s not up to snuff, even if it’s free.
Let’s jump right into my story, as it might have played out on a six hour bus
ride from Berlin to Dortmund.

## Fighting windmills

I’m not one of those scary internet pirates, I want to work on the bus. So when
I realized  blocked access to YouTube, Spotify, and Bandcamp, I was mildly
annoyed, but still understanding. So I got to work without music.

I accessed the AWS console, and performed some minor chores on my EC2 and S3
setup. To test the configuration, I tried uploading a file to one of my
buckets. Sure enough, it didn’t work. It seemed as if S3 URLs are also blocked.
At this point I was fed up enough to start the in-browser VPN tool of my choice
and start listening to KEXP sessions while continuing to work, just out of
spite.

After a while my “allowance” was reached, and I was unable to continue to work.
For about a minute, anyway, until I had popped open a terminal, changed my MAC
address, and reconnected.

```
sudo ifconfig en0 ether 00:e1:e2:e3:e4:e5
```
<div class="figure-label">Fig. 1: Changing your MAC address.</div>

I soon realized I still couldn’t access AWS from the command line—or from
anywhere but my browser, really—, so I replaced my in-browser VPN with my
trusty [system-wide VPN](https://tunnelblick.net/) and YouTube with Spotify.

At this point I was able to work properly, and annoyed enough that I started
writing this blog post instead.

## Why do this?

Of course it’s completely understandable why FlixBus does this: it keeps a
large percentage of undesired traffic out. Sure, it also inconveniences regular
users, and won’t keep sticklers like me from fighting their way to internet
freedom, but it’s a complete success from the company’s viewpoint.

Except it isn’t, because the story isn’t that simple. I’ve heard from a few of
my friends that they have switched back to travelling by train, because the
busses are even more of a bother to work in, and even though your trek might
take a little longer by train, you get more done. The only thing buses are
making easier is [low-profile smuggling](https://www.swissinfo.ch/eng/cross-border-challenges_flixbus-puts-cameras-on-coaches-to-fight-drug-smuggling/43883714),
it seems, which might or might not be the reason I took the bus for the trip at
hand. I just function better with pot, you see.

Whether these migrations matter in the grand scheme of things I don’t know. I
just care about being able to work whenever I want to. Maybe FlixBus isn’t the
greatest choice for that anymore? Stay tuned for the next blog post when,
inevitably, I’m fed up with the ICE train WiFi!
