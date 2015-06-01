# DEPLOYMENT

# New World: fucking Docker.

Since both Joyent and AWS are jumping on the Docker train (and countless
others), it's probably best to just do a version of it to avoid lock-in and
otherwise make things, uh, pleasant? Idk, I'd rather have a single instance that
I can write silly scripts for than a General Purpose Does Everything Container.

<blockquote class="twitter-tweet" lang="en"><p lang="en" dir="ltr">Docker is
convincing too many young startups that they should first build a shitty version
of Heroku, then build their product.</p>&mdash; Max Lynch (@maxlynch) <a
href="https://twitter.com/maxlynch/status/597768678382305280">May 11,
2015</a></blockquote>
<script async src="//platform.twitter.com/widgets.js" charset="utf-8"></script>

That said, _because_ I've got so little baggage here (Erlang, Java, Cmark,
mostly), it might be easy to build a very tiny image that can run the program.
Major approaches:

* Erlang releases proper — figure out architectures, tarballs, etc.
* Git-based: read from HEAD or a tag, do the massaging needed (manual-compile
  merl and shit). This is gross, I should mostly be working with BEAMs
  goddammit.

## Things to make better

* Env vars for the various pieces like PGHOST, PGPASSWORD, etc.
* Ship pre-built JARs for the owasp-sanitizer, devtools (if I ever finish them).

# Old World: SmartOS, basic steps, scriptable

So I can't just hack this.

I wanted to use SmartOS, but it's been a colossal pain in the ass. The part that
broke me was compiling the app there and ErlyDtl being a heaping pile of
non-compatible shit. Maybe one day I go back and make it work with 17.5, but
now, both another project (cowboy?) and ErlyDtl try to compile their dependency
merl, it breaks unless you go in there and manually `make`. That's fine, but
if you try to then `rebar compile` erlydtl (it's missing a `.hrl` file if you
don't) then it runs out of memory trying to allocate 800MB.

Below is the set of commands that got me most of the way there. I still hadn't
resolved how to compile the `cmark_wrapper` since, after 'installing' cmark, the
headers were still not exposed to gcc.

Just like other places tend to have an 'app password,' we'll have one too. Not
checked in, keep it in a local file.

## High level steps

1. Install nginx on the SmartOS box: https://ecarmi.org/writing/setup-nginx-joyent-solaris-smartmachine/
2. Configure nginx to do basic auth on all connections.
  * Password hasher - http://www.htaccesstools.com/htpasswd-generator/
3. Forward requests to the app.
4. Install Erlang
5. Install Java
6. Run the app.

```
# Make it yours
pkgin in zsh
usermod -s /opt/local/bin/zsh
exit
re-login

# Get Nginx going…
svcadm disable apache
pkgin in nginx
svcadm enable nginx

Add to nginx.conf:
        auth_basic "spooky ghostlight!";
        auth_basic_user_file /opt/local/etc/nginx/.htpasswd;
Add to .htpasswd file (casper : fantasma_porfavor):
        casper:$apr1$A620/0TJ$c8IIL1EDOrMSmXc7cfWAE1

# Get Erlang going
pkgin -y install gmake gcc-compiler binutils scmgit pkg_alternatives autoconf sun-jdk6-6.0.26
curl -O https://raw.githubusercontent.com/spawngrid/kerl/master/kerl
chmod +755 kerl
export KERL_CONFIGURE_OPTIONS="--enable-hipe --enable-smp-support --enable-threads --enable-kernel-poll --enable-m64-build"
./kerl build 17.5 17.5
mkdir erlangs
./kerl install 17.5 erlangs/
. erlangs/activate

# Get the CMark lib built
git clone https://github.com/jgm/cmark
cd cmark && make && cd ..

# Push, unpack a tarball of ghostlight
git clone <ghostlight repo>
> tar -cvzf ghostlight.tgz ghostlight
scp ghostlight.tgz root@<IP>:/root

# Rebar3
git clone https://github.com/rebar/rebar3.git
cd rebar3 && ./bootstrap && cd ..
```

===
Nginx: https://ecarmi.org/writing/setup-nginx-joyent-solaris-smartmachine/

Erlang: http://christophermeiklejohn.com/ruby/smartos/2013/10/15/erlang-on-smartos.html
        https://github.com/yrashk/kerl
