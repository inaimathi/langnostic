Ok, so I figure it's about time to live up to the title of this blog, since I've spent the vast majority of the language discussion firmly planted in parentheses. Aside from the fact that my company is starting a project in  Erlang, I've also been scripting Python and Ruby pretty heavily.

They're actually not very different languages. Neither is perfect from my perspective[^clarification], and neither sucks. If I had to, I could get work done in both (and having gone through the Ruby chapter in [7 Languages in 7 Weeks](http://pragprog.com/book/btlang/seven-languages-in-seven-weeks), I'm more inclined to look at Ruby for my next big project than I used to be). To start with, here's a boiled down, no-nonsense table that represents my perspective.

[^clarification]: Though, to be clear, my opinion is that Ruby gets a damn sight closer than Python.

### Python vs Ruby

- `len([1, 2, 3])` << `[1, 2, 3].length`
- `"foo " + bar + " baz"` (or `"foo %s baz" % bar`) << `"foo #{bar} baz"`
- `",".join(["one", "two", "three"])` << `["one", "two", "three"].join(",")`
- `map(lambda a: a + 1, [4, 3, 2, 1])` << `[4, 3, 2, 1].map { |a| a + 1}`
- `a = [1, 2, 3]; a.sort(); a[0]` << `[4, 3, 2, 1].sort[0]`
- [nothing.jpg](http://stackoverflow.com/questions/191010/how-to-get-a-complete-list-of-objects-methods-and-attributes) << `foo.methods.sort`
- `import optparse, fileutils` >> `require 'optparse'; require 'pp'; require 'fileutils';`
- `python` >> `sudo apt-get install ruby-full && irb`

- ```python
def aFunction(foo, bar):
   # do stuff
   return baz
```
~=
```ruby
def a_function(foo, bar)
  # do stuff
  baz
end
```
- ```python
with tempfile.NamedTempFile() as tmp:
       tmp.write("Test test\n")
       ##more stuff
       tmp.flush()
       popen(["lp", "-d", "a-printer", tmp.name()])
```
~=
```ruby
Tempfile.open() do |tmp|
   tmp.write("Test test \n")
   ## more stuff
   tmp.flush
   system("lp", "-d", "a-printer", tmp.name)
end
```

So I am slightly biased, but like I said earlier, not enough to actually decry either language. The biggest point in Ruby's favor is its handling of blocks (as seen in that tempfile pseudo-code). I like having an expression that says "Create an entity, do this stuff and then clean up", without having to clean up myself. Python doesn't like that[^thank-you-brendan]. Gotta admit, I boggled at the `join` syntax the first time around. Rhetorically, who the hell decided it makes sense that a `join` operation is something you do to the delimiter, rather than the list? In my opinion, it would even make more sense to make it a standalone function a-la `len`.

[^thank-you-brendan]: Thank you Brendan Miller for pointing me to the `with` statement (documented [here](http://www.python.org/dev/peps/pep-0343/), [here](http://effbot.org/zone/python-with-statement.htm) and [here](http://docs.python.org/release/2.5.2/ref/with.html)) which does emulate blocks well enough for my purposes.

I really *like* the syntactic whitespace in Python[^from-the-future].

```python
def something():
    foo()
    bar()
```

seems like it's cleaner than the Ruby equivalent. Except that when I want to return the result of `bar`[^which-i-do], I need to do so explicitly. Ruby has me waste an additional line on `end`, but returns implicitly. While I'm at it, Python libraries seem to be heavily anti-functional programming. They do the standard "OO" thing of exposing functionality via classes, but they also typically have a heavy reliance on side effects, which makes it harder than it ought to be to compose things. A recent example I had to go through involved using `pyPDF` and `reportlab` to process existing PDFs. You *can* do it, but the amount of fiddling involved is nontrivial if you want to decompose the problem properly because you need to do so by setting up multiple instances of `PdfFileReader`/`canvas` and making destructive changes to them.

[^from-the-future]: Hello from the year 2016! I no longer like syntactic whitespace. It's kid of shit, principally because it completely destroys your ability to auto-indent a file of code. You can still have intelligent line-based indenting, but as soon as indentation has semantic content, you're stuck doing it yourself. This is one of the two big complaints I have against [one of my currently favorite languages](https://www.haskell.org/).
[^which-i-do]: Which I do quite often, given that I much prefer functional programming to OO.

Also not represented in the table is how much easier it is to install `python` packages in Debian. While `gem install` sometimes errors, I've yet to find a package I need that I can't either `apt-get` or retrieve using `python-setuptools` with no trouble at all. That's worth something[^in-fact].

[^in-fact]: In fact, it's worth enough that I've been procrastinating on a ruby port of `get-youtube-series`, which used only included components in Python, but requires several installs in Ruby.

The last thing that table doesn't encompass is the version situation. That's a fairly major one from my perspective, but I'm not sure how serious it actually is. Python 3 has been out for quite a while, but it's not uncommon to see "Supports Python 2.7" on various frameworks/utilities. Squeeze still provides 2.6.6, Django still requires 2.[4-7] and Google app-engine is still asking for 2.5 (with 2.7 being supported as an "experimental" feature). That's less than encouraging. By contrast, Ruby 1.9 is fairly widely supported (though the Debian repos are still at 1.8.7). That just doesn't seem to bode well for the next version, regardless of how enthusiastic Rossum is about it.[^from-the-future-again]

[^from-the-future-again]: Hello again from 2016. Python 3 is now out and available as a non-default package on Debian, as well as `nix`. There are *still* many popular libraries that rely on Python 2.7 or earlier, and it's even odds whether a new Python project will choose to be based on 2.x or 3.x. For what it's worth, the new standard is cleaner in a couple of ways, but not *so much* cleaner that I'd rather use it than Ruby. So, take from that what you will I guess.
