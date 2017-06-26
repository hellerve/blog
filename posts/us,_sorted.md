It is hard to be apart from you. Sometimes I'm whiny, sometimes I'm creative,
sometimes I'm both at the same time.

```
# this is all we need to merge.
# when i look at the picture,
# it feels as though i touch you.

from scipy import misc

with open('you.png', 'rb') as you:
    you = misc.imread(you)
with open('me.png', 'rb') as me:
    me = misc.imread(me)
us = me + you
us.sort(1)
misc.imsave('us.png', us)
```

![](/assets/you,_sorted.jpeg)
<div class="figure-label">you, sorted.</div>

![](/assets/me,_sorted.jpeg)
<div class="figure-label">me, sorted.</div>

![](/assets/us,_sorted.jpeg)
<div class="figure-label">us, sorted.</div>
