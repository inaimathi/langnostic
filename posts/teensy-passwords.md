I've been trying to get to as many [Toronto Lisp Users' Group](http://www.lisptoronto.org/past-meetings/discussion-meeting-november-2011) meetings as I can since missing one a couple months ago, though I'm not entirely sure it's good for me. The `minutes of conversation/interesting thing` ratio is approaching zero, to the point that it might be more accurate to start tracking `interesting things/minute`, which is excellent except that I have this day job thing which keeps me from spending every waking moment on [interesting fiction](http://english.lem.pl/), [crazy languages](http://www.rebol.org/)(closed source warning) and [crazier architectures](http://www.greenarraychips.com/home/products/). That's without even accounting for the fact that my wife and I have been beating down the Thalmor on a regular basis.

The last link is actually what got me off my ass and seriously interested in coding again in my spare time<a name="note-Tue-Dec-13-155321EST-2011"></a>[|1|](#foot-Tue-Dec-13-155321EST-2011). The thing is, as soon as I started looking at it, I had to admit that I have no fucking idea how to go about serious embedded programming, let alone how to do it with [forth](http://en.wikipedia.org/wiki/Forth_(programming_language))<a name="note-Tue-Dec-13-155347EST-2011"></a>[|2|](#foot-Tue-Dec-13-155347EST-2011).

I also mentioned a [little while ago](http://langnostic.blogspot.com/2011/11/false-alarm-also-teensy.html) that I tried poking around at the [Teensy](http://www.pjrc.com/teensy/) and found it to actually work on my machine<a name="note-Tue-Dec-13-155401EST-2011"></a>[|3|](#foot-Tue-Dec-13-155401EST-2011). The other thing I like is that Teensy lets me use Emacs for development, though there isn't a nicely integrated mode for compiling and loading projects/files onto the chip without jumping into an `eshell` buffer. I might want to sink some time into that...

No! Focus! No new shit right now!

Anyway, poking around is all very well, but I find that I really need a project of some sort to work on otherwise I don't internalize the concepts effectively. I do still eventually want to build myself a [chordite](http://chordite.com/) (or [something similar](http://www.youtube.com/watch?v=URqYG-iMcTY); relevant bit at the 2:10 mark), but that looks like it'll take a lot more industrial design and wiring than I'm in the mood for. At about the same time I was thinking about all of this, I stumbled upon a [writeup about a hardware keylogger](http://www.irongeek.com/i.php?page=security/homemade-hardware-keylogger-phukd) and an article entitled [fuck passwords](http://me.veekun.com/blog/2011/12/04/fuck-passwords/).

The second one is all the correct complaints about passwords and authentication that OpenID was supposed to almost solve, and that [SSH keys](http://linuxproblem.org/art_9.html) do solve for those of us lucky enough to be using SSH. The problems remaining are


- Not every system uses OpenID (and even if they did, you still need to authenticate to your OpenID provider, most of whom use passwords :p)
- The ones that don't use OpenID are the ones you want to be most secure (banks and such), and their password constraints make them the hardest to remember
- The most secure thing to do is to come up with a separate password for every service and rotate them regularly, but this is hard to memorize


On top of that, there are a few things<a name="note-Tue-Dec-13-155549EST-2011"></a>[|4|](#foot-Tue-Dec-13-155549EST-2011) that can't be solved by OpenID or RSA keys. Just keep that problem in mind.

The other link is called "[PHUKD](http://www.irongeek.com/i.php?page=security/homemade-hardware-keylogger-phukd)". And a quick glance at that spec page will tell you exactly why. It's a pranking/surveillance tool whose accompanying paper is entitled [Plug and Prey: Malicious USB Devices](http://www.irongeek.com/i.php?page=security/plug-and-prey-malicious-usb-devices). The way that it works ostensibly this. First, you surreptitiously connect the device to your targets' computer. Then, depending on what you want, you either set the thing to record keyboard/mouse input for later collection, or have it emulate a mouse/keyboard to run random malicious programs<a name="note-Tue-Dec-13-155701EST-2011"></a>[|5|](#foot-Tue-Dec-13-155701EST-2011). Like I said, the [phukd library](http://www.irongeek.com/i.php?page=security/programmable-hid-usb-keystroke-dongle#Programming_examples_and_my_PHUKD_library) makes it a fairly explicit goal to screw with people's computers in this way with a separate `CommandAtRunBar` for each of Windows, GNOME and OS X. That's just ... mean.

Putting it all together, and as usual, I'm almost 100% sure that I'm not even remotely the first person to think this up, but why not just put your passwords onto a hardware key?

It'll be something like the actual [teensy keyboard](http://www.pjrc.com/teensy/usb_keyboard.html) implementation, but bind each button to a sequence of N characters that get typed out whenever it's pushed. Here's what I came up with after a bit of fiddling (pardon the giant code block, C isn't quite as easy for me to express ideas in).

```c
#include <avr/io.h>
#include <avr/pgmspace.h>
#include <avr/interrupt.h>
#include <util/delay.h>
#include <string.h>
#include "usb_keyboard.h"

#define LED_CONFIG      (DDRD |= (1<<6))
#define LED_ON          (PORTD &= ~(1<<6))
#define LED_OFF         (PORTD |= (1<<6))
#define CPU_PRESCALE(n) (CLKPR = 0x80, CLKPR = (n))

uint16_t idle_count=0;

int key_from_char(char c){
  if (c >= 'a' && c <= 'z') return c - 93;
  if (c >= 'A' && c <= 'Z') return c - 61;
  if (c >= '1' && c <= '9') return c - 19;
  if (c == '0') return 39; // 0 is low in ASCII, high in the USB key definition
 
  // there's no easy mapping between the other ASCII characters and 
  // USB keyboard keys, so these are all 
  switch (c) {
  case 32: return KEY_SPACE; break;
  case 33: return KEY_1; break;
  case 34: return KEY_QUOTE; break;
  case 35: return KEY_NUMBER; break;
  case 36: return KEY_4; break;
  case 37: return KEY_5; break;
  case 38: return KEY_7; break;
  case 39: return KEY_QUOTE; break;
  case 40: return KEY_9; break;
  case 41: return KEY_0; break;
  case 42: return KEYPAD_[ASTERIX](http://www.asterix.com/); break;
  case 43: return KEYPAD_PLUS; break;
  case 44: return KEY_COMMA; break;
  case 45: return KEYPAD_MINUS; break;
  case 46: return KEY_PERIOD; break;
  case 47: return KEYPAD_SLASH; break;

  case 58: return KEY_SEMICOLON; break;
  case 59: return KEY_SEMICOLON; break;
  case 60: return KEY_COMMA; break;
  case 61: return KEY_EQUAL; break;
  case 62: return KEY_PERIOD; break;
  case 63: return KEY_SLASH; break;
  case 64: return KEY_2; break;

  case 91: return KEY_LEFT_BRACE; break;
  case 92: return KEY_BACKSLASH; break;
  case 93: return KEY_RIGHT_BRACE; break;
  case 94: return KEY_6; break;
  case 95: return KEY_MINUS; break;
  case 96: return KEY_TILDE; break;

  case 123: return KEY_LEFT_BRACE; break;
  case 124: return KEY_BACKSLASH; break;
  case 125: return KEY_RIGHT_BRACE; break;
  case 126: return KEY_TILDE; break;

  default: return 0;
  }
}

int modifier_from_char(char c){
  if ((c >= 'A' && c <= 'Z') ||
      c == 33 || c == 34 ||
      (c >= 36 && c <= 38) ||
      c == 40 || c == 41 ||
      c == 58 || c == 60 ||
      (c >= 62 && c <= 64) ||
      c == 94 || c == 95 ||
      (c >= 123 && c <= 126)) return KEY_SHIFT;
  
  return 0;
}

int8_t usb_keyboard_print(char *s){
  int s_len = strlen(s);
  int i;

  for(i = 0; i < s_len; i++){
    usb_keyboard_press(key_from_char(s[i]), modifier_from_char(s[i]));
  }
}

int main(void) {
  uint8_t b, mask, i;
  uint8_t b_previous=0xFF;
  CPU_PRESCALE(0); // set for 16 MHz clock

  DDRB = 0x00;
  PORTB = 0xFF;
 
  usb_init();
  while (!usb_configured()) /* wait */ ;
  _delay_ms(1000);

  while (1) {
    b = PINB;
    mask = 1;

    for (i=0; i<8; i++) {
      if (((b & mask) == 0) && (b_previous & mask) != 0) {
        usb_keyboard_print("abcdABCD1234!@#$%^&*()_+|~{}:\">?<-=\\`[];',./");
      }
      mask = mask << 1;
    }

    b_previous = b;
    _delay_ms(2);
  }
}
```

<!-- " -->

It's actually very close to the example code that runs the keyboard, except that I've modified it to take ASCII strings as input and pretend to type them out. I didn't complicate things (yet) by trying to figure out how to [hook up an LED](http://www.dorkbotpdx.org/blog/spacewrench/cool_thing_with_broken_teensy) or [micro SD](http://www.pjrc.com/teensy/sd_adaptor.html) card. It just keeps passwords in as part of the program source, and outputs one password per button.

I'm going to develop this over the next little while, at least while my interest is focused enough on it, into a unit that stores a bunch of passwords, lets me select which one I want (indexed by what the password belongs to), and output it. Ideally, I'd also get this down to a package [small enough](http://www.irongeek.com/i.php?page=security/programmable-hid-usb-keystroke-dongle) that I could just put on a physical keychain. That's a pretty good learning project for the short term.

Anyway, full circle with the fucking of passwords, when I'm done, this will be a little machine that I can plug into any USB-capable computer, push a button or two, and have it output (with much better recall and more accuracy than I could) a password that can be as massive and unique as I damn well please. It's not computer-specific (so I don't need to worry about syncing it, except for backup purposes), and it works anywhere I'd want to plug in an actual keyboard. That should kill the security problems dead. Of course, I'll need to keep some sort of safeguard in case it stops working or falls into the wrong hands, but at least I won't be forced to pick between convenience and security anymore.

It seems like a pretty obvious, elegant solution to the problem, conceptually. I can see why it hasn't caught on yet though; if you want to use something like this, you basically need to build it yourself. If I had to buy one of these dongles rather than build one, there's no chance in hell that I'd do it. I'd have to either really, truly, trust the provider enough to believe that they'd never snoop on my passwords or log unrelated information, OR I'd need to get a blank key and the source code and then put them together myself. There are just too many fragile points to do it any other way.

And that's about it. Oh, actually, I did toss this code [up to codereview](http://codereview.stackexchange.com/questions/6796/keyboard-printing-with-teensy), in case someone out there is actually a C coder and wants to tear me a new one about something. I'm still chewing through [clomments](https://github.com/Inaimathi/clomments) issues, and I've actually done some real work on [cl-chan](http://langnostic.blogspot.com/2011/08/cl-chan-clsql-and-hunchentoot-crash.html) which I'll hopefully be kicking up soon; just as soon as I decide how to treat the image manipulation pieces.

* * *
##### Footnotes

1 - <a name="foot-Tue-Dec-13-155321EST-2011"></a>[|back|](#note-Tue-Dec-13-155321EST-2011) -  If you're also interested, but don't want to drop the $500 on a full, two-chip eval board, you can also get a [breadboard kit](http://www.greenarraychips.com/home/documents/budget.html) for about $60 total, though you have to buy the individual chip, power supply and breakout board.

2 - <a name="foot-Tue-Dec-13-155347EST-2011"></a>[|back|](#note-Tue-Dec-13-155347EST-2011) -  My only experience with it was writing a few toy programs in [gforth](http://www.gnu.org/s/gforth/), which I get the feeling won't be very useful.

3 - <a name="foot-Tue-Dec-13-155401EST-2011"></a>[|back|](#note-Tue-Dec-13-155401EST-2011) -  In marked contrast to the more popular [Arduino](http://www.arduino.cc/).

4 - <a name="foot-Tue-Dec-13-155549EST-2011"></a>[|back|](#note-Tue-Dec-13-155549EST-2011) -  Like the actual, physical computer I use everyday.

5 - <a name="foot-Tue-Dec-13-155701EST-2011"></a>[|back|](#note-Tue-Dec-13-155701EST-2011) - Or just commandeer the browser to take your target to [goatse](http://www.youtube.com/watch?v=dQw4w9WgXcQ)
