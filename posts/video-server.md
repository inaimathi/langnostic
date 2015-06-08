So I'm starting to get a bit sick of moving my media collection between machines. 

That happened this weekend; the fan on one of my computers was acting up so I had to jump over to a slightly older machine to do some work or I would have gone crazy from the clicking. It would not have been pretty, I'm telling you. Cat blood *everywhere*.

The crappy part for me was that I had to move all of my data over. The code isn't the problem; git-clone is a quick and painless process given the typical repo-size I deal with. The issue was my video and music, which would have taken ages. The options were basically


- scp -r and wait four or so hours
- move the physical hard drives over (and suffer small, but annoying gliches with driver configurations)


So I picked the third option; getting a streaming server running on [orphan](http://langnostic.blogspot.com/2011/02/old-machines.html). That turns out not to be nearly as difficult as I thought it would be. [lighttpd](http://www.lighttpd.net/) turns out to have built-in streaming modules, Chrome (sadly, now my browser of choice) supports HTML5 (for the MP4s), and [flowplayer](http://flowplayer.org/) has a [GPL3 version](http://flowplayer.org/download/index.html) of its offering (for the FLVs).
After installing lighttpd, I had to add six lines to /etc/lighttpd/lighttpd.conf

```
"mod_secdownload",
"mod_flv_streaming",
```

to the server.modules section and

```
flv-streaming.extensions = ( ".flv" )
secdownload.secret = "somesecret" 
secdownload.document-root = "/backup/Videos/"
secdownload.uri-prefix = "/vids/"
secdownload.timeout = 120
```

at the end. And that...was pretty much it. Once I did the above, I just had to put together a script to scan the Videos directory and generate appropriate links to each stream. I did it with lisp, using lighttpds' [mod_proxy](http://redmine.lighttpd.net/wiki/1/Docs:ModProxy) equivalent to hook up hunchentoot, but you can do with any language you like. A link needs to look like

```
/vids/[hashed-param]/[hex-time]/video-name-here.mp4
```

"/vids/" needs to match whatever you put in the secdownload.uri-prefix. [hex-time] is the number of seconds since the unix epoch in 0-padded, 8-digit hex-format. [hashed-param] is the md5 digest of secdownload.secret+"/vide-name-here.mp4"+[hex-time]. The tricky part in Lisp was actually [hex-time]. In PHP, that would be sprintf("%08x", time()). In [Ruby](http://www.ruby-doc.org/core/classes/Time.html), you could do ["%08x" % Time.now](http://www.ruby-doc.org/core/classes/Time.html). Common Lisps' epoch isn't the unix standard Jan 1, 1970 though; it's Jan 1 1900. Which means that the equivalent Lisp code is something like (format nil "~8,'0x"(- (get-universal-time) 2208988800)). 

Woo.

Anyway, once you've generated your link URL, showing a video is just

```html
<head>
  <script src="/flowplayer/example/flowplayer-3.1.4.min.js"></script>
</head>

<video src="[link url]">
  <a href="[link url]" id="player"></a>
  <script language="JavaScript">
    flowplayer("player", "/flowplayer/flowplayer-3.1.5.swf");
  </script>
</video>
```

which looks prettier in [cl-who](http://weitz.de/cl-who/)+[parenscript](http://common-lisp.net/project/parenscript/) than anything else I could write it in, but is about as language-agnostic as you can get. The outside video tags play MP4s if HTML5 is available, the interior a and script calls flowplayer up if HTML5 isn't supported.

After that, all that's left is generating some preview images

```
ffmpeg -i input.dv -r 1  -t 00:00:01 -f image2 images%05d.png
```

and maybe get a half-way decent fullscreen mode going (not really a big deal for me; the video isn't the main focus for the kind of stuff I watch, but hey).

```html
<html>
  <body style="background-color: #000; padding: 0px; margin: 0px;">
    <video src="[link url]" style="width: 100%;" controls="controls" autoplay="autoplay"></video>
  </body>
</html>

<a href="#" onclick="window.open('fulscreen.html', '', 'fullscreen=yes, scrollbars=no, toolbar=no, menubar=no, status=no');">Test</a>
```


