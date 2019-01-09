I’m currently working on a project where, during the development phase, we have
to use Gitlab CI. Because we already had a Docker setup in place and Gitlab
supports Docker out of the box, the natural fit was to continue using Docker to
run a local Gitlab server. But, because we also had to use Gitlab’s CI
mechanisms, we ran into some problems initially.

After a bit of debugging I found that this had to do with the network setup
within Docker. I want to talk a little about why that is and how to fix it—the
fix is extremely simple.
