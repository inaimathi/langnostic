This has been one hell of a month, mostly for non-technical reasons, but I think I need to discuss some of them regardless. The following is a journal-style entry, so skip it if you're here for any kind of language discussion.

### Specialization

Firstly, you may have noticed that I've been hacking Erlang lately. It's verbose, it's obtuse, it works at bizarre cross-purposes with itself, but it has endeared itself to me for reasons I've [already discussed](http://langnostic.blogspot.ca/2012/05/assumptions.html). It's not too clear to me why I have this drive to try new languages, and it's not entirely clear whether it gives me an edge or dulls it in the end. It feels like I'm making reasonable progress and gaining perspective on the process of expressing processes precisely, and maybe that's enough. The root of the chain is this bias I have against overspecialization, which may or may not be an evolutionary vestige, but it doesn't seem to have hurt me yet. It seems intuitively obvious that I'd want to avoid the situation where I don't have the right tools for a job, and that means keeping a lot of them around. Admittedly, I haven't practiced this in real life, but cognitive tools don't take up space, and are always at my call, so it's much easier to justify.

I've had conversations with quite a few people I respect that go the other way. That is, they seem to think that going deep is much better than going broad, but that honestly only seems to be true if your goal is to end up as a corporate developer or team lead somewhere. I've also had encounters with people almost hard-wired to a particular language. One or two Lispers I keep in touch with seem genuinely concerned that I've been off doing Erlang or Smalltalk work. Pretty much every C++/C#/Java programmer I've met so far in real life have condescendingly stated that `[their language]` is the only one you should ever consider for production work. To top it off, I've interacted with a worrying number of Haskell douches who aggressively push their preference on other functional programmers.

That *can't* be the correct approach, regardless of how powerful an individual language is.

### Make

The Erlang play I've engaged in has forced me to take a serious look at [`make`](http://www.gnu.org/software/make/manual/make.html). I mentioned a while ago that I reach for [Ruby](http://www.ruby-lang.org/en/) whenever I need to do almost any small bit of scripting. Until about a week ago, this included deployment scripts. It never really occurred to me that `make` was good for something other than compiling C projects, but taking a closer look, it seems like it can do quite a bit. It has conditionals, loops and functions, and it deals with command line arguments a lot more gracefully than scripts in typical general-purpose languages.

```
exclude = .git .gitignore *~ docs/* *org config.lisp log

define deploy
        git checkout $(1);
        rsync -rv $(foreach var, $(exclude), --exclude $(var)) ./ $(2);
        ssh $(3);
endef

deploy-public:
        $(call deploy, master, [user]@[server]:[project-root], [user]@[server])

deploy-client-a:
        $(call deploy, [client-branch], [user]@[server]:hhsc-[project-root], [user]@[server])

deploy-client-b:
        $(call deploy, [client-branch], [user]@[server]:hhsc-[project-root], [user]@[server])

ssh:
        ssh [user]@[server]```

That saved me about 40 lines when compared to the Ruby script that used to do the same job<a name="note-Sat-May-26-230305EDT-2012"></a>[|1|](#foot-Sat-May-26-230305EDT-2012). Granted, the `Makefile` makes me type out the `[user]@[server]` string twice, because `:` is otherwise interpreted as a control character and there's oddly no way to escape it, but that's an acceptable blemish given the overall line savings. Now that's not to say that `make` is more elegant than `Ruby`, just that it's a lot more specialized for the task. Most of the chaff from those 56 lines was doing command-line parsing and some declarations, which again hints that command line argument parsing is a hack.

The other advantage of the `Makefile` is that using it gives me meaningful completions at the command line. In the above, if I tabbed on `make`, it would give me the different tasks as potential entries

```
inaimathi@hermaeus:~/project$ make
deploy-client-a  deploy-client-b  deploy-public  Makefile       ssh
inaimathi@hermaeus:~/project$ make |```

That's going to get more convenient the more clients we start supporting. I'm not going to go through the full `make` syntax; it's fairly self explanatory and [docs](http://www.gnu.org/software/make/manual/make.html) exist in any case. A definition looks like that `define..endef` block, calling a function looks like `$(call fn, arg1, arg2, ...)`, the `exclude` line shows you what a variable looks like, and the bit that looks like `$(foreach ...)` is a loop. That should be enough for pretty much anything you need to do with the tool.

### Music

I had a fit of OCD the other day, and decided to finally organize my music library to prevent my phone from reporting

```
Unknown Artist -- 178 songs```

instead of correctly sorted collections. I did reach for Ruby here, and two scripts turned out to be particularly useful

```ruby
#!/usr/bin/ruby

require 'optparse'
require 'fileutils'

class String
  def naive_title_case(split_by = "-")
    split(split_by).map(&:capitalize).join " "
  end
  def strip_song
    s = self.split("--")
    (s[1] ? s[1] : self).gsub(".ogg", "")
  end
end

ARGV.each do |target|
  artist = target.gsub("/", "").naive_title_case
  FileUtils.cd(target) do
    Dir.entries(".").find_all{|e| e.end_with? ".ogg"}.each do |file|
      `vorbiscomment -t 'ARTIST=#{artist}' -t 'TITLE=#{file.strip_song.naive_title_case}' -w #{file}`
    end
  end
end```

```ruby
#!/usr/bin/ruby

require 'optparse'
require 'fileutils'

$options = {:sub => "", :downcase => nil}
OptionParser.new do |opts|
  opts.on('-r', '--regex REGEX', String,
          'Specify the regular expression to replace') {|reg| $options[:regex] = Regexp.new(reg)}
  opts.on('-s', '--sub SUBSTITUTE', String,
          'Specify what to replace the match with. By default, the empty string (so matches are stripped).') {|$options[:sub]|}
  opts.on('-d', '--downcase', 'If passed, all filenames will be downcased.'){|$options[:downcase]|}
end.parse!

usage unless ARGV.length > 0

def rename(str)
  ($options[:downcase] ?
   str.downcase : str).gsub($options[:regex], $options[:sub])
end

ARGV.each do |target|
  File.rename(target, rename(target))
end```

The first one is a very thin wrapper around <a name="note-Sat-May-26-230331EDT-2012"></a>[|2|](http://linuxcommand.org/man_pages/vorbiscomment1.html">`vorbiscomment`</a> that lets me pass it more than one file at a time and uses my idiosyncratic file storage/naming conventions to infer the title and "artist"[](#foot-Sat-May-26-230331EDT-2012) of the piece. The second one is just a simple regex application script which lets me format many files at once without going through the mind numbing tedium of one `mv` call per file<a name="note-Sat-May-26-230349EDT-2012)[|3|](#foot-Sat-May-26-230349EDT-2012).

What I listen to these days is actually slightly embarrassing. A little while ago, I was working with some friends, obviously enjoying [some](http://www.youtube.com/watch?v=W_utdISM3uk) [tunes](http://www.youtube.com/watch?v=hnCw1zXtaLs) on my headphones, and pretty much froze when one of them passed a speaker wire. I'm not even sure why; we've been friends for a pretty fucking long time at this point, and I knew that *musical preferences* would not be the thing to finally drive us apart, but I still hesitated at listening to [some of this shit](http://www.youtube.com/watch?v=cP0f5rvVkAU) *with another human being*.

Not at all sure where that comes from. I guess it's that I used to be a [rocker](http://en.wikipedia.org/wiki/Nirvana_(band)) [back](http://www.pearljam.com/) in the [day](http://en.wikipedia.org/wiki/Green_Day). The last time I actually bought a related album was [back in 2007](http://toolshed.down.net/lyrics/10kdayslyrics.php). Looking at my current, newly-organized library, it's split about half and half between [pony](http://www.youtube.com/watch?v=YsFQ5gUoS0Y)/[videogame](http://www.youtube.com/watch?v=orpimUSiK8I&ob=av3e) related electronica and [classical](http://www.amazon.com/Essential-Yo-Yo-Ma/dp/B00136Q5XQ) of [some](http://www.youtube.com/watch?v=v7cAd9MXRMg) sort, but I honestly didn't notice the change taking place. I'm not even sure if rock is a thing in general anymore, but it's definitely not a thing I listen to. And I guess I wasn't sure whether my friends knew that yet, since we don't tend to talk about it.

It's really odd how the peripheral pieces of my identity are the ones that cause me the most concern. I remember admitting to myself that I was really a programmer/illustrator and not a Graphic Designer, and that didn't have much of an impact on how I behaved. The little things<a name="note-Sat-May-26-233125EDT-2012"></a>[|4|](#foot-Sat-May-26-233125EDT-2012) seem to perturb me a lot more when I notice them. Maybe it has to do with the fact that they tend to change while I'm not paying attention, rather than being an effort of conscious will...


* * *
##### Footnotes

1 - <a name="foot-Sat-May-26-230305EDT-2012"></a>[|back|](#note-Sat-May-26-230305EDT-2012) - `wc -l` says `deploy.rb` was 56, while the actual `Makefile` clocks in at 20.

2 - <a name="foot-Sat-May-26-230331EDT-2012"></a>[|back|](#note-Sat-May-26-230331EDT-2012) - "Artist" is in quotes because I actually use it to group playlists, rather than Artists in the usual sense.

3 - <a name="foot-Sat-May-26-230349EDT-2012"></a>[|back|](#note-Sat-May-26-230349EDT-2012) - Incidentally, you can see what I mean when I call script arguments a hack, right? More than half of each of those scripts is taken up by a huge, verbose, un-abstractable block whose entire reason for existence is making up for the fact that I'm writing a function that I want to be command-line accessible.

4 - <a name="foot-Sat-May-26-233125EDT-2012"></a>[|back|](#note-Sat-May-26-233125EDT-2012) - How I dress, what I listen to and watch, how I wear my hair.
