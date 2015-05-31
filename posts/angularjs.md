Before I get to the actual article, there was enough interest in the sparse-array solution to [Life](http://en.wikipedia.org/wiki/Conway%27s_Game_of_Life) this past [Saturday](https://guestlistapp.com/events/130467) that I've added them to the [appropriate Rosetta Code page](http://rosettacode.org/wiki/Conway%27s_Game_of_Life) and my [GitHub](https://github.com/Inaimathi/life). Sadly, the only solutions up are [python](https://github.com/Inaimathi/life/blob/master/life.py), [common lisp](https://github.com/Inaimathi/life/blob/master/life.lisp), [haskell](https://github.com/Inaimathi/life/blob/master/life.hs) and [clojure](https://github.com/Inaimathi/life/blob/master/life.clj). The [Prolog](http://www.swi-prolog.org/) attempt my partner and I made during the event wasn't completed, and I never actually got to try [Forth](http://www.gnu.org/software/gforth/). I might get around to doing some thinking with those; I'll keep you posted.

### Global Day of Code Retreat

Was pretty goddamn awesome. For some reason, I became known as "the Haskell guy", despite the fact that I am nowhere even approaching mastery<a name="note-Mon-Dec-10-161221EST-2012"></a>[|1|](#foot-Mon-Dec-10-161221EST-2012). The attempts I made were

<ol>
  <li>**Prolog** - With someone who is older than me and hadn't done any Prolog since graduating. This wasn't helped by the fact that the entirety of my Prolog experience was two read-throughs of the appropriate chapter in [Seven Languages](http://pragprog.com/book/btlang/seven-languages-in-seven-weeks). We got as far as figuring out how to compute a [Moore neighborhood](http://en.wikipedia.org/wiki/Moore_neighborhood) before time was called.</li>
  <li>**Common Lisp** - With a young man looking to learn the language. Hopefully, I wasn't too enthusiastic in recommending that he drop by the <a name="note-Mon-Dec-10-161300EST-2012"></a>[|2|](http://www.lisptoronto.org/">Toronto Common Lisp User Group</a>[hash tables](#foot-Mon-Dec-10-161300EST-2012), and gave him enough of a taste for Lisp that it won't look alien next time. Unfortunately, the problem we were solving ran smack into Common Lisps' poor support for <a href="http://cl-cookbook.sourceforge.net/hashes.html), so the impression wasn't as positive as it otherwise might have been.</li>
  <li>**Haskell** - With a friend I know through the Common Lisp User group who's looking to get into functional programming in general. We implemented the same gridless solution, except in five lines rather than ~20. Most of the time was actually spent showcasing the functional way of thinking, and the utility of a REPL in problem solving. After this session, a crowd gathered around my laptop and demanded that I incrementally take them through those five lines and prove they actually produced valid output. I did so successfully, which is probably where my reputation started.</li>
  <li>**Smalltalk** - Where I and the same fellow TLUG attendee watched an old Smalltalk hand finally explain how [TDD](http://en.wikipedia.org/wiki/Test-driven_development) makes sense if you've got the proper tools built into the language to support it. It turns out that no language other than Smalltalk does. If you disagree, learn enough Smalltalk to do some TDD in it, then try to do it again in Java/Ruby/what-have-you without throwing up. If you can prove you've done so, I will concede the point. We didn't actually get an implementation going this time because our host was explaining the basics of the environment and the class hierarchy to us, but this was the first grid-based approach I tried the entire day.</li>
  <li>**Clojure** - With a young woman looking to try Clojure, and coming from a Scheme/Java background. We finished the gridless solution, with a printed board this time, just before time was called. Interestingly, this is the first partner I had all day that was used to thinking functionally before we sat down, so it was light work pointing out the differences between Clojure and Scheme to her.</li>
</ol>

As I said, I was *going* to try Forth, but Dann, the only one willing to partner on that language, had to leave before the last session.

My general impression of the event was extremely positive, and I'll certainly be attending the [monthly-ish](http://www.meetup.com/Toronto-Code-Retreat/#calendar), smaller version if I can. The only big surprise I got was how few people actually use functional programming in the course of their work. I sort of assumed that the thought process wouldn't be outright alien, but I seemed to be one of a very small group that did it with any regularity. Oh well I guess; if you want light to be seen, take it into dark places. For my partners' part, they were either eager to learn, or to refresh their memory.

### Web Mote and Angular.js

You haven't heard much about [Web-Mote](https://github.com/Inaimathi/web-mote) lately, and that's mainly because I've been porting it to [Angular.js](http://angularjs.org/), saving myself *quite* a few lines of code in the process. You may remember that [I called all the JS-MVC frameworks shit](http://langnostic.blogspot.ca/2012/09/js-frameworks.html) a little while ago. While my distaste for the needless OO-modeling that permeates most of them remains intact, a fellow Lisping web-developer told me to give [Angular](http://angularjs.org/) another try without reading their [godawful, over-engineered tutorial](http://docs.angularjs.org/tutorial/).

I've almost finished porting, and I must admit that I've become a believer. If you're already using

<ul>
  <li>an HTML [templating library](http://handlebarsjs.com/)</li>
  <li>a JS [routing system](http://backbonejs.org/#Router)</li>
  <li>and [jQuery](http://jquery.com/)</li>
</ul>

you probably could have saved yourself a lot of code and headache by just picking up Angular, which elegantly solves most problems you'd be using the above for. It also turns out that <a name="note-Mon-Dec-10-161810EST-2012"></a>[|3|](http://docs.angularjs.org/api/angular.Module">Angular modules</a> are extremely composeable, *and* fail to get in the way of other frameworks<a href="#foot-Mon-Dec-10-161810EST-2012).

The "templating" in particular deserves special mention, though that's probably the wrong thought model to apply here. As the fellow Lisper explained, Angular gives you a [DSL](http://en.wikipedia.org/wiki/Domain-specific_language) for writing HTML front-ends. To illustrate, here's what the HTML component of the Handlebars/Backbone/jQuery version looked like

```html
&lt;!DOCTYPE HTML&gt;
&lt;html lang="en-US"&gt;
  &lt;head&gt;
    &lt;meta charset="UTF-8"&gt;
    &lt;meta name="viewport" content="width=device-width" /&gt;
    &lt;title&gt;WebMote&lt;/title&gt;
  &lt;/head&gt;
  &lt;body&gt;

    &lt;!-- --------- --&gt;
    &lt;!-- Templates --&gt;
    &lt;!-- --------- --&gt;
    &lt;script id="tmp-folder" type="text/x-handlebars-template"&gt;
      &lt;li class="{{type}}"&gt;
        {{#if buttons}}
        &lt;button class="play btn" onclick="mote.play('{{path}}')"&gt;&lt;i class="icon-play"&gt;&lt;/i&gt;&lt;/button&gt;
        &lt;button class="shuffle btn" onclick="mote.shuffle('{{path}}')"&gt;&lt;i class="icon-random"&gt;&lt;/i&gt;&lt;/button&gt;
        {{/if}}
        &lt;a class="dir-link{{#unless buttons}} buttonless{{/unless}}" href="#navigate{{path}}"&gt;{{name}}&lt;/a&gt;
      &lt;/li&gt;
    &lt;/script&gt;

    &lt;script id="tmp-file" type="text/x-handlebars-template"&gt;
      &lt;li class="{{type}}"&gt;
        &lt;a class="file-link" href="javascript:void(0);" onclick="mote.play('{{path}}')"&gt;{{name}}&lt;/a&gt;
      &lt;/li&gt;
    &lt;/script&gt;

    &lt;script id="tmp-control" type="text/x-handlebars-template"&gt;
      &lt;li class="{{cmd}}"&gt;
        &lt;button class="btn" onclick="mote.command('{{cmd}}');"
                {{#if held}}
                onmousedown="mote.hold('{{cmd}}');" onmouseup="mote.release();" onmouseout="mote.release();"
                {{/if}}&gt;
          &lt;i class="icon-{{cmd}}"&gt;&lt;/i&gt;
        &lt;/button&gt;
      &lt;/li&gt;
    &lt;/script&gt;

    &lt;script id="tmp-control-block" type="text/x-handlebars-template"&gt;
      &lt;ul&gt;
        {{#each this}}
        {{#control-button this}}{{/control-button}}
        {{/each}}
      &lt;/ul&gt;
    &lt;/script&gt;
    
    &lt;!-- ---- --&gt;
    &lt;!-- Body --&gt;
    &lt;!-- ---- --&gt;
    &lt;ul id="file-list"&gt;&lt;/ul&gt;
    &lt;div id="controls"&gt;&lt;/div&gt;
    
    &lt;!-- ------ --&gt;
    &lt;!-- Styles --&gt;
    &lt;!-- ------ --&gt;
    &lt;link rel="stylesheet" href="css/custom-theme/jquery-ui-1.8.13.custom.css" type="text/css" media="screen" /&gt;
    &lt;link rel="stylesheet" href="css/bootstrap.css" type="text/css" media="screen" /&gt;
    &lt;link rel="stylesheet" href="css/bootstrap-responsive.css" type="text/css" media="screen" /&gt;
    &lt;link rel="stylesheet" href="css/style.css" type="text/css" media="screen" /&gt;
    
    &lt;!-- ------- --&gt;
    &lt;!-- Scripts --&gt;
    &lt;!-- ------- --&gt;
    &lt;script type="text/javascript" src="js/jquery.min.js"&gt;&lt;/script&gt;
    &lt;script type="text/javascript" src="js/jquery-ui-1.8.13.custom.min.js"&gt;&lt;/script&gt;
    &lt;script type="text/javascript" src="js/handlebars-1.0.rc.1.js"&gt;&lt;/script&gt;
    &lt;script type="text/javascript" src="js/underscore-min.js"&gt;&lt;/script&gt;
    &lt;script type="text/javascript" src="js/backbone-min.js"&gt;&lt;/script&gt;

    &lt;script type="text/javascript" src="js/web-mote.js"&gt;&lt;/script&gt;

  &lt;/body&gt;
&lt;/html&gt;
```

and *this* is what the exact same front-end looks like expressed in the Angular HTMLDSL

```html
&lt;!DOCTYPE HTML&gt;
&lt;html lang="en-US"&gt;
  &lt;head&gt;
    &lt;meta charset="UTF-8"&gt;
    &lt;meta name="viewport" content="width=device-width" /&gt;
    &lt;title&gt;WebMote&lt;/title&gt;
  &lt;/head&gt;
  &lt;body&gt;

    &lt;div ng-app&gt;
      &lt;ul id="file-list" ng-controller="FileListCtrl"&gt;
        &lt;li class="{{file.type}}" ng-repeat="file in filesList" ng-switch="file.buttons"&gt;
          &lt;span ng-switch-when="true"&gt;
            &lt;button class="play btn" ng-click="play(file.path)"&gt;&lt;i class="icon-play"&gt;&lt;/i&gt;&lt;/button&gt;
            &lt;button class="shuffle btn" ng-click="shuffle(file.path)"&gt;&lt;i class="icon-random"&gt;&lt;/i&gt;&lt;/button&gt;
            &lt;a class="dir-link" ng-click="play(file.path)"&gt;{{file.name}}&lt;/a&gt;
          &lt;/span&gt;
          &lt;a class="dir-link buttonless" ng-switch-default ng-click="play(file.path)"&gt;{{file.name}}&lt;/a&gt;
        &lt;/li&gt;
      &lt;/ul&gt;
      &lt;div id="controls" ng-controller="CommandCtrl"&gt;
        &lt;ul ng-repeat="controlsList in controlTree"&gt;
          &lt;li ng-repeat="control in controlsList" class="{{control.cmd}}" ng-switch="control.held"&gt;
            &lt;button class="btn" ng-switch-when="true" 
                    ng-mousedown="command(control.cmd); hold(control.cmd)"
                    ng-mouseup="release()" ng-mouseleave="release()"&gt;
              &lt;i class="icon-{{control.cmd}}"&gt;&lt;/i&gt;
            &lt;/button&gt;
            &lt;button class="btn" ng-switch-default ng-click="command(control.cmd)"&gt;
              &lt;i class="icon-{{control.cmd}}"&gt;&lt;/i&gt;
            &lt;/button&gt;
          &lt;/li&gt;
        &lt;/ul&gt;
      &lt;/div&gt;
    &lt;/div&gt;
    
    &lt;!-- ------ --&gt;
    &lt;!-- Styles --&gt;
    &lt;!-- ------ --&gt;
    &lt;link rel="stylesheet" href="css/bootstrap.css" type="text/css" media="screen" /&gt;
    &lt;link rel="stylesheet" href="css/bootstrap-responsive.css" type="text/css" media="screen" /&gt;
    &lt;link rel="stylesheet" href="css/style.css" type="text/css" media="screen" /&gt;
    
    &lt;!-- ------- --&gt;
    &lt;!-- Scripts --&gt;
    &lt;!-- ------- --&gt;
    &lt;script type="text/javascript" src="js/jquery.min.js"&gt;&lt;/script&gt;
    &lt;script type="text/javascript" src="js/angular.min.js"&gt;&lt;/script&gt;
    &lt;script type="text/javascript" src="js/angular-resource.min.js"&gt;&lt;/script&gt;

    &lt;script type="text/javascript" src="js/mote.js"&gt;&lt;/script&gt;

  &lt;/body&gt;
&lt;/html&gt;
```

What you don't see above is that the amount of JavaScript required for this new approach is easily 1/2 of what I needed to write to get comparable functionality with separate templating/routing/DOM libraries. Most of it, I get the feeling, is Angulars' use of [reactive programming](http://en.wikipedia.org/wiki/Reactive_programming), but I can't really be sure of that. Since I haven't done much testing yet, I also don't know what kind of performance hit I'm going to be taking by using such a transformative approach.

Add that to the massive list of things I need to keep you posted about, I guess.

* * *
##### Footnotes
1 - <a name="foot-Mon-Dec-10-161221EST-2012"></a>[|back|](#note-Mon-Dec-10-161221EST-2012) - But give me ten years or so.

2 - <a name="foot-Mon-Dec-10-161300EST-2012"></a>[|back|](#note-Mon-Dec-10-161300EST-2012) - Where we actually discuss many things of interest to functional programmers, not just Common Lisp.

3 - <a name="foot-Mon-Dec-10-161810EST-2012"></a>[|back|](#note-Mon-Dec-10-161810EST-2012) - So you can still use jQuery for the [one](http://api.jquery.com/jQuery.ajax/) or [two](http://api.jquery.com/jQuery.browser/) things Angular doesn't bother with.
