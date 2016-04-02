I really do have to update that header more frequently. It's been a good year and a half since I did anything other than some light make scripting in Python, JavaScript may as well be [jQuery](http://jquery.com/) as far as my recent use of it is concerned, and I haven't done much more than some very lightweight playing in Erlang. Most of my time at work has been getting spent ass-deep in PHP which wasn't very pleasant even back when it was one of two languages I knew. The rest of it is tilted heavily towards the lisps (Common Lisp, Elisp, Scheme, in that order), and I'm still trying to get my head around Haskell through some [tutorial](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) and semi-involved [practice](http://snapframework.com/). The practice will actually increase soon; a friend of mine wants some help with his website, and he's convinced that having it written in a lesser-known language will make it less likely to get hacked. I tried to explain that this isn't actually how security works, but he's having none of it. His instructions were "I don't care what you use, as long as it's not PHP".

So, I mean. Score.

The last piece up there is Ruby, which I've had an odd relationship with. I tried out Rails a while back, but didn't like the amount of magic involved[^rails-foibles][^from-the-future]. I also tried out some Windows automation back when "Windows" was a thing I used for work rather than just for playing 10 year old video games. We also run Redmine at the office, so I've had to spend a very little bit of time making cosmetic UI changes. The point is, I've yet to write more than maybe 200 lines of Ruby in one sitting, but I still like it. It's clean somehow. Simple. In a way that Python never felt, even though the syntactic whitespace forces more visual consistency onto it. Despite my low line-count, ruby-full is still firmly wedged in the `languages` section of my installation shell script, and the only reason my installation shell-script isn't itself written in Ruby is that `irb` isn't bundled with Debian.

[^rails-foibles]: And the various "convention vs. configuration"/security exploit stories I keep hearing about through friends aren't exactly tempting me back.
[^from-the-future]: Hello from 2016! I still think Rails is complete shit! It has a bad habit of associating different code modules with each other based on some transformation of their filenames, it gives you extremely minimal compile-time guarantees about the correct configuration of your web app, it has a truly brain-dead load-order for configuration files (alphabetic, in case you care), and it expects you to memorize a metric fuckton of information that'll be completely useless once you realize how shit it is. I currently work at a Rails shop, and my strategy has been to touch it as little as possible, while memorizing as little as possible.

I'm musing on this now, because I recently received a reminder of how beautiful it can be for simple scripting purposes. I had a problem with my XFCE4 setup. Actually, not a problem, just something that wasn't going quite as smoothly as it might have. I use multiple monitors on each of my multiple machines, you see. My desktop has two, my laptops share an external, and my work machine travels with me so it actually has two different monitors to interface with depending on where it is. The key is, no matter where I am, the situation is the same; I just want my monitors arranged left to right, each at the highest possible resolution. XFCE doesn't seem to have an option for that, so my initial approach was just to manually check xrandr output and type out the appropriate combination of --output, --mode and --right-of to get it working. It dawned on me the other day that this is pretty inefficient given how consistent the pattern is, and since I occasionally profess to know how to program, I should be able to do something about it. The problem is that step one of the process is parsing the output from a shell command, which surprisingly few languages care to do. Luckily, Ruby is one of them. My initial pass worked, but it was ugly and I won't inflict it upon you here. After consulting [codereview.SE](http://codereview.stackexchange.com/questions/597/autodetecting-monitors-in-xfce), it was whittled down to

```ruby
#!/usr/bin/ruby

def xrandr_pairs (xrandr_output)
## Returns [[<display name>, <max-resolution>] ...]
  display = /^(\S+)/
  option = /^\s+(\S+)/
  xrandr_output.scan(/#{display}.*\n#{option}/)
end

def xrandr_string (x_pairs)
## Takes [[<display name>, <max-resolution>] ...] and returns an xrandr command string
  cmd = "xrandr --output #{x_pairs[0][0]} --mode #{x_pairs[0][1]}"
  args = x_pairs.each_cons(2).map do |(previous_output, previous_mode), (output, mode)|
      "--output #{output} --mode #{mode} --right-of #{previous_output}"
  end
  [cmd, *args].join(" ")
end

exec xrandr_string(xrandr_pairs(`xrandr`))

```

which is pretty beautiful, as far as I'm concerned.

It's elegant for shell scripting for a two reasons;

First, Ruby has a wide range of options for calling the shell. [exec](http://www.ruby-doc.org/core/classes/Kernel.html#M001438) seems tailor-made for the purpose above (it replaces the current Ruby process with a call to the command you pass it), [spawn](http://www.ruby-doc.org/core/classes/Kernel.html#M001442) is useful if you want to do things in parallel and [`](http://www.ruby-doc.org/core/classes/Kernel.html#M001408) delimits a special string type that gets executed synchronously as a shell command and returns that commands' output.

Second, any string can be a template. This includes special strings like regexes and backticks, which is why you can compose larger regular expressions from simpler pieces as in xrandr_pairs above. You can stitch `#{ }` into any string you like, and the contents can be any expression, not necessarily a string. A minor, but important part is that I didn't have to call a function in order to make a string a template (there's no call to `printf` or `format`), and the contents are inlined (I'm doing `"Foo #{bar} #{baz}"` as opposed to  `"Foo #{1} #{2}" bar baz`) which makes the result that much more obvious. Neither would matter much proportionally in a big project, but when I'm working on a 16 line script, I'll to knock out every bit of cognitive overhead I can.

That's why I still use it. I never liked Ruby for big stuff, or even medium stuff, but I almost instantly reach for it to do anything under 50 lines or so that needs to talk to the shell.
