Ok, so I figure it's about time to live up to the title of this blog, since I've spent the vast majority of the language discussion firmly planted in parentheses. Aside from the fact that my company is starting a project in  Erlang, I've also been scripting Python and Ruby pretty heavily.

They're actually not very different languages. Neither is perfect from my perspective<a name="note-Mon-Oct-10-164837EDT-2011"></a>[|1|](#foot-Mon-Oct-10-164837EDT-2011), and neither sucks. If I had to, I could get work done in both (and having gone through the Ruby chapter in [7 Languages in 7 Weeks](http://pragprog.com/book/btlang/seven-languages-in-seven-weeks), I'm more inclined to look at Ruby for my next big project than I used to be). To start with, here's a boiled down, no-nonsense table that represents my perspective.

<table class="comparison-table"> 
  <tr><td colspan=2 style="text-align: center;">### ...is more annoying than...</td></tr>
  <tr>
    <td>
      ```python
len([1, 2, 3])
```
    </td> 
    <td>
      ```ruby
[1, 2, 3].length
```
    </td>
  </tr>
  <tr>
    <td>
      ```python
"foo " + bar + " baz"
```
      <div style="text-align: center;">or</div>
      ```ruby
"foo %s bar" % bar
```
    </td>
    <td>
      ```ruby
"foo #{bar} baz"
```
    </td>
  </tr>
  <tr>
    <td>```python
", ".join(["one", "two", "three"])
```</td>
    <td>```ruby
["one", "two", "three"].join ", "
```</td>
  </tr>
  <tr>
    <td>
      ```python
map(lambda a: a + 1, [4, 3, 2, 1])
## still makes more sense 
## than join or len, though
```
    </td>
    <td>```ruby
[4, 3, 2, 1].map {|a| a + 1}
```</td>
  </tr>
  <tr>
    <td>
      ```python
a = [4, 3, 2, 1].sort()
a[0]
```
    </td>
    <td>
      ```ruby
[4, 3, 2, 1].sort[0]
```
    </td>
  </tr>
  <tr>
    <td>[nothing.jpg](http://stackoverflow.com/questions/191010/how-to-get-a-complete-list-of-objects-methods-and-attributes)</td>
    <td>`foo.methods.sort`</td>
  </tr>
  <tr>
    <td>
      ```ruby
require 'optparse'
require 'pp'
require 'fileutils'
```
    </td>
    <td>
      ```python
import optparse, fileutils
## I also prefer the more granular 
## symbol access I get with python
```
    </td>
  </tr>
  <tr>
    <td>
      ```
sudo apt-get install ruby-full
irb
```
    </td>
    <td>
      ```
python
```
    </td>
  </tr>
  <tr><td colspan=2 style="text-align: center;">### ...is about as annoying as...</td></tr>
  <tr>
    <td>
```python
def aFunction(foo, bar):
    #do stuff
    return baz
```
    </td>
    <td>
```python
def a_function(foo, bar)
  #do stuff
  baz
end
```
    </td>
  </tr>
  <tr>
    <td>
```python
with tempfile.NamedTempFile() as tmp:
    tmp.write("Test test\n")
    ##more stuff    
    tmp.flush()
    popen(["lp", "-d", "a-printer", tmp.name()])
```
    </td>
    <td>
      ```ruby
Tempfile.open() do |tmp|
   tmp.write("Test test \n")
   ## more stuff
   tmp.flush
   system("lp", "-d", "a-printer", tmp.name)
end
```
    </td>
  </tr>
</table>

So I am slightly biased, but like I said earlier, not enough to actually decry either language. The biggest point in Ruby's favor is its handling of blocks (as seen in that tempfile pseudo-code). I like having an expression that says "Create an entity, do this stuff and then clean up", without having to clean up myself. Python doesn't like that.<a name="note-Mon-Oct-17-075930EDT-2011"></a>[|2|](#foot-Mon-Oct-17-075930EDT-2011) Gotta admit, I boggled at the `join` syntax the first time around. Rhetorically, who the hell decided it makes sense that a `join` operation is something you do to the delimiter, rather than the list? In my opinion, it would even make more sense to make it a standalone function a-la `len`.

I really *like* the syntactic whitespace in Python.

```python
def something():
    foo()
    bar()
```

seems like it's cleaner than the Ruby equivalent. Except that when I want to return the result of `bar` (which I do quite often, given that I much prefer functional programming to OO), I need to do so explicitly. Ruby has me waste an additional line on `end`, but returns implicitly. While I'm at it, Python libraries seem to be heavily anti-functional programming. They do the standard "OO" thing of exposing functionality via classes, but they also typically have a heavy reliance on side effects, which makes it harder than it ought to be to compose things. A recent example I had to go through involved using `pyPDF` and `reportlab` to process existing PDFs. You *can* do it, but the amount of fiddling involved is nontrivial if you want to decompose the problem properly because you need to do so by setting up multiple instances of `PdfFileReader`/`canvas` and making destructive changes to them.

Also not represented in the table is how much easier it is to install `python` packages in Debian. While `gem install` sometimes errors, I've yet to find a package I need that I can't either `apt-get` or retrieve using `python-setuptools` with no trouble at all. That's worth something (in fact, it's worth enough that I've been procrastinating on a ruby port of `get-youtube-series`, which used only included components in Python, but requires several installs in Ruby).

The last thing that table doesn't encompass is the version situation. That's a fairly major one from my perspective, but I'm not sure how serious it actually is. Python 3 has been out for quite a while, but it's not uncommon to see "Supports Python 2.7" on various frameworks/utilities. Squeeze still provides 2.6.6, Django still requires 2.[4-7] and Google app-engine is still asking for 2.5 (with 2.7 being supported as an "experimental" feature). That's less than encouraging. By contrast, Ruby 1.9 is fairly widely supported (though the Debian repos are still at 1.8.7). That just doesn't seem to bode well for the next version, regardless of how enthusiastic Rossum is about it.

* * *
##### Footnotes

1 - <a name="foot-Mon-Oct-10-164837EDT-2011"></a>[|back|](#note-Mon-Oct-10-164837EDT-2011) - Though, to be clear, my opinion is that Ruby gets a damn sight closer than Python.

2 - <a name="foot-Mon-Oct-17-075930EDT-2011"></a>[|back|](#note-Mon-Oct-17-075930EDT-2011) - Thank you Brendan Miller for pointing me to the `with` statement (documented [here](http://www.python.org/dev/peps/pep-0343/), [here](http://effbot.org/zone/python-with-statement.htm) and [here](http://docs.python.org/release/2.5.2/ref/with.html)) which does emulate blocks well enough for my purposes.
