For those of you just here for the easy, googlable answer. To send an HTML email with `cl-smtp`, do this:

```lisp
(cl-smtp:send-email [server] [from] [to] [subject] 
                    [plaintext message, or possibly NIL] 
                    :html-message [HTML message])
```

Making sure to replace the things with square brackets, obviously. Passing `nil` instead of the mandatory `message` parameter causes all the clients I've tested with so far to automatically display your email as a standard HTML message.

Now then.

The documentation in the [module itself](http://common-lisp.net/project/cl-smtp/) follows the usual Common Lisp standards of being [minimal, verging on nonexistent](http://common-lisp.net/viewvc/cl-smtp/cl-smtp/README?view=markup)<a name="note-Tue-Apr-17-162650EDT-2012"></a>[|1|](#foot-Tue-Apr-17-162650EDT-2012). The best [example I managed to find](http://ryepup.unwashedmeme.com/blog/2008/10/31/some-simple-cl-smtp-examples/) of sending an HTML-formatted email from `cl-smtp` can be seen [here](http://ryepup.unwashedmeme.com/blog/2008/10/31/some-simple-cl-smtp-examples/). The suggestion is to do

```lisp
(cl-smtp:send-email
 +mail-server+ from to subject
 "&lt;html>&lt;body>
    &lt;h2>
      YES. THIS IS DOG.
    &lt;/h2>
      &lt;img src=\"http://my.site.url/dog.jpg\" alt=\"A dog comically answering a phone\"/>
  &lt;/body>&lt;/html>"
 :extra-headers '(("Content-type" "text/html; charset=\"iso-8859-1\"")))
```

And if you do that, it will *seem* to work unless you run into someone with a particularly configured Exchange server. You might be thinking<a name="note-Tue-Apr-17-162701EDT-2012"></a>[|2|](#foot-Tue-Apr-17-162701EDT-2012) "Oh, \fantastic, MS once again cocks up what should be a simple and straightforward task", but I'm not so sure. Lets take a look at the headers produced by using the `:extra-headers` approach above.

```
...
From: from@email.com
To:  someone@else.com
Subject: Serious Business
X-Mailer: cl-smtp(SBCL 1.0.54.0.debian)
Content-type: text/html; charset="iso-8859-1" ## the result of our option 
Mime-Version: 1.0
Content-type: text/plain; charset="UTF-8" ## the default cl-smtp header 
...
```

Now like I said, this seems to get interpreted as intended in most places. Notably, gmail, hotmail, yahoo mail, my companies' exchange server, and probably [mailinator](http://mailinator.com/) as well, all output the result of this multi-`Content-type`-headered email as `text/html`. The thing is, it seems fairly reasonable to parse this strictly and accept the last `Content-type` declaration rather than the most general. So I guess another way of saying it is "this won't work on a properly configured Exchange server".

The actually working way of accomplishing this task is to use the built-in `:html-message` parameter

```lisp
(cl-smtp:send-email
 +mail-server+ from to subject
 "Ok, the HTML version of this email is totally impressive. Just trust me on this."
 :html-message
 "&lt;html>&lt;body>
    &lt;h2>
      YES. THIS IS DOG.
    &lt;/h2>
      &lt;img src=\"http://my.site.url/dog.jpg\" alt=\"A dog comically answering a phone\"/>
  &lt;/body>&lt;/html>")
```

if you don't want to send a plaintext message at all, it's possible<a name="note-Tue-Apr-17-162819EDT-2012"></a>[|3|](#foot-Tue-Apr-17-162819EDT-2012) to pass `nil` as the message `body`

```lisp
(cl-smtp:send-email
 +mail-server+ from to subject nil
 :html-message
 "&lt;html>&lt;body>
    &lt;h2>
      YES. THIS IS DOG.
    &lt;/h2>
      &lt;img src=\"http://my.site.url/dog.jpg\" alt=\"A dog comically answering a phone\"/>
  &lt;/body>&lt;/html>")
```

Doing it this way causes `cl-smtp` to break your message up into a plaintext and HTML version. You then rely on a client showing its user the appropriate one depending on their context<a name="note-Tue-Apr-17-163515EDT-2012"></a>[|4|](#foot-Tue-Apr-17-163515EDT-2012).

```
From: from@email.com
To:  someone@else.com
Subject: Serious Business
X-Mailer: cl-smtp(SBCL 1.0.54.0.debian)
Mime-Version: 1.0
Content-type: multipart/alternative;
 Boundary="_---------_2IQrElfHaDK71IdkZlEq5L3C0etr5t"
Message-Id: blahblahblah

--_---------_2IQrElfHaDK71IdkZlEq5L3C0etr5t
Content-type: text/plain; charset="UTF-8"
Content-Disposition: inline

Ok, the HTML version of this email is totally impressive. Just trust me on this.


--_---------_2IQrElfHaDK71IdkZlEq5L3C0etr5t
Content-type: text/html; charset="UTF-8"
Content-Disposition: inline

&lt;html>&lt;body>&lt;h2>YES. THIS IS DOG.&lt;/h2>&lt;img src=\"http://my.site.url/dog.jpg\" alt=\"A dog comically answering a phone\"/>&lt;/body>&lt;/html>


--_---------_2IQrElfHaDK71IdkZlEq5L3C0etr5t--
```

![](http://3.bp.blogspot.com/-PpRdyt0CEdw/T43R3g2N3zI/AAAAAAAAAL8/JHDjwUqanEA/s400/themoreyouknow.jpg)

* * *
##### Footnotes

1 - <a name="foot-Tue-Apr-17-162650EDT-2012"></a>[|back|](#note-Tue-Apr-17-162650EDT-2012) - Though it does show you a useful example of how to put an attachment in a sent email.

2 - <a name="foot-Tue-Apr-17-162701EDT-2012"></a>[|back|](#note-Tue-Apr-17-162701EDT-2012) - As I did initially.

3 - <a name="foot-Tue-Apr-17-162819EDT-2012"></a>[|back|](#note-Tue-Apr-17-162819EDT-2012) - Though probably not advisable in all cases.

4 - <a name="foot-Tue-Apr-17-163515EDT-2012"></a>[|back|](#note-Tue-Apr-17-163515EDT-2012) - Which most seem to, but there are still [one](http://www.exchange-answers.com/microsoft/Exchange-Clients/30509248/preventing-exchange-from-messing-up-multipartalternative-messages.aspx) or [two](http://social.technet.microsoft.com/Forums/en-US/exchangesvrcompliance/thread/97b5a94f-c948-4d06-ad66-8521fd49ec7e/) Exchange-server related hiccups for some users with particular versions of the software.
