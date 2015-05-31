I was adding some Erlang and Emacs-related items to the Resources section of the sidebar, and the height finally got annoying enough to do something about.

You'll notice that the it now sits neatly inside a [jQueryUI accordion widget](http://jqueryui.com/demos/accordion/). You could easily go inspect that element, but let me save you a right-click and some DOM navigation.

```html
&lt;script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7.2/jquery.min.js" type="text/javascript">&lt;/script>
&lt;script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.8.18/jquery-ui.min.js" type="text/javascript">&lt;/script>
&lt;script type="text/javascript"> $(document).ready(function () { $("#accordion").accordion({autoHeight: false, collapsible : true, active: false}); });&lt;/script>

&lt;link rel="stylesheet" href="http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.17/themes/base/jquery-ui.css" type="text/css" media="all" />

&lt;style type="text/css">
  #sidebar #accordion h3 { padding-left: 25px; font-weight: bold; }
  #sidebar #accordion div ul { list-style: circle; }
  #sidebar #accordion div ul a { text-decoration: underline; color: blue; }
&lt;/style>

&lt;span style="color: #d7d7d7; padding-bottom: 15px; font-size: x-small;">(in no particular order)&lt;/span>
&lt;div id="accordion">
  &lt;h3>Section Title&lt;/h3>
  &lt;div>
    Section Text
    &lt;ul>
      &lt;li>&lt;a href="foo">bar&lt;/a>&lt;/li>
    &lt;/ul>
  &lt;/div>
&lt;/div>
```

As you know unless you've been living under a rock for about five years, Google hosts copies of the [jQuery](https://developers.google.com/speed/libraries/devguide) and [jQueryUI](https://developers.google.com/speed/libraries/devguide) libraries. As you can see from the style link, they also host [the relevant CSS](http://stackoverflow.com/questions/1348559/are-there-hosted-jquery-ui-themes-anywhere) in many themes. I was afraid I'd have to do some JS hacking to get all this working with Blogger, but it's fairly straightforward. Just add an `HTML/JavaScript` widget to your blog, paste in the above code, and play with the styling a bit if you like.

Thanks to jQuery, the sidebar is now markedly less cluttered than it was twenty minutes ago.
