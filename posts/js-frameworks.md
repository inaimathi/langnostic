So I've spent the past week or so working through some examples with the various [JavaScript MVC front-end frameworks](http://codebrief.com/2012/01/the-top-10-javascript-mvc-frameworks-reviewed/). Let me save you the trouble: they're all shit. If you absolutely, positively can not live without a framework of some sort, use [Backbone](http://backbonejs.org/) or [Spine](http://spinejs.com/)<a name="note-Sun-Sep-23-214748EDT-2012"></a>[|1|](#foot-Sun-Sep-23-214748EDT-2012), because they seem to be as close to "minimal" as you can get, they won't get in your way too much, they help a little, and it's perfectly possible to run them alongside [jQuery](http://jquery.com/) or similar if you feel like rolling certain pieces on your own.

While they do *get* in your way, the Javascript MVC movement is getting a couple of things profoundly right. Things I didn't really notice, or didn't think through all the way in the past, so I'm kind of shamefaced about having missed them, but they definitely seem like the *right* approach<a name="note-Sun-Sep-23-214859EDT-2012"></a>[|2|](#foot-Sun-Sep-23-214859EDT-2012). The reason I say

> [The frameworks are] all shit   
> --me  

above is that none of them seem to be necessary to do the Right Thing©™<a name="note-Sun-Sep-23-214921EDT-2012"></a>[|3|](#foot-Sun-Sep-23-214921EDT-2012), and none of them seem to help much with the big detriments of the approach.

### <a name="anecdote-time" href="#anecdote-time"></a>Anecdote Time

Being that I haven't put a large project together with any of these techniques yet, this is the only example I'm willing to show, but it is illustrative. So I had a particular place where I needed to reorder pieces of information on the client side, then send them out to the server for persistence. Luckily, I was [not the only one](http://stackoverflow.com/questions/10147969/saving-jquery-ui-sortables-order-to-backbone-js-collection) who had this problem and thought of using backbone to do it. Here's [the solution recommended in that question](http://jsfiddle.net/7X4PX/4/):

```javascript
Application = {};
Application.Collection = {};
Application.Model = {};
Application.View = {};

Application.Model.Item = Backbone.Model.extend();
Application.View.Item = Backbone.View.extend({
    tagName: 'li',
    className: 'item-view',
    events: {
        'drop' : 'drop'
    },
    drop: function(event, index) {
        this.$el.trigger('update-sort', [this.model, index]);
    },
    render: function() {
        $(this.el).html(this.model.get('name') + ' (' + this.model.get('id') + ')');
        return this;
    }
});
Application.Collection.Items = Backbone.Collection.extend({
    model: Application.Model.Item,
    comparator: function(model) {
        return model.get('ordinal');
    },
});
Application.View.Items = Backbone.View.extend({
    events: {
        'update-sort': 'updateSort'
    },
    render: function() {
        this.$el.children().remove();
        this.collection.each(this.appendModelView, this);
        return this;
    },
    appendModelView: function(model) {
        var el = new Application.View.Item({model: model}).render().el;
        this.$el.append(el);
    },
    updateSort: function(event, model, position) {
        this.collection.remove(model);
        
        this.collection.each(function (model, index) {
            var ordinal = index;
            if (index >= position)
                ordinal += 1;
            model.set('ordinal', ordinal);
        });

        model.set('ordinal', position);
        this.collection.add(model, {at: position});

        // to update ordinals on server:
        var ids = this.collection.pluck('id');
        $('#post-data').html('post ids to server: ' + ids.join(', '));

        this.render();
    }
});

var Instance = {};
Instance.collection = new Application.Collection.Items();
Instance.collection.add(new Application.Model.Item({id: 1, name: 'a', ordinal: 0}));
Instance.collection.add(new Application.Model.Item({id: 2, name: 'b', ordinal: 1}));
Instance.collection.add(new Application.Model.Item({id: 3, name: 'c', ordinal: 2}));

Instance.collectionView = new Application.View.Items({
    el: '#collection-view',
    collection: Instance.collection
});

Instance.collectionView.render();

$(document).ready(function() {
    $('#collection-view').sortable({
        stop: function(event, ui) {
            ui.item.trigger('drop', ui.item.index());
        }
    });
});
```
​
```css
#collection-view {
   margin-bottom: 30px;
}

.item-view {
   border: 1px solid black;
   margin: 2px;
   padding: 10px;
   width: 30px;
}&#8203;
```

```html
&lt;ul id='collection-view'>&lt;/ul>
&lt;div id='post-data'>&lt;/div>
```

And once you have all that in place, what you can do is drag the given elements around, and have returned a set of IDs in the order that they appear on the users screen! [Isn't that amazing!?](https://en.wikipedia.org/wiki/Sarcasm) I gave it the benefit of the doubt, and tried to fit the code into my head for about half an hour before [I realized something](http://jsfiddle.net/RK2GV/2/).

```javascript
var util = {
    log : function (message) {
 $("#console").append(JSON.stringify(message)).append("&lt;br />");    
    }
};

var templates = {
    rule : Handlebars.compile($("#tmp-list").html())
}

var rules = {
    render: function (rules) {
    $.each(rules, function (i, aRule) {
        $("#rules-list").append(templates.rule(aRule));
    })
    }
}

$(document).ready(function() {
    rules.render([{"id": 1, "name": "a"},
                  {"id": 2, "name": "b"},
                  {"id": 3, "name": "c"}]);
    $('#rules-list').sortable({
        stop: function(event, ui) {
        var ids = $("#rules-list li").map(function (i, elem) {
            return $(elem).find(".id").attr("title");
        }).get();
        util.log(ids);
        }
    });
});
```

```css
#rules-list {
    margin-bottom: 30px;
}

#rules-list li{    
    border: 1px solid black;
    margin: 5px;
    padding: 7px;
    width: 50px;
}
```

```html
&lt;ul id="rules-list">&lt;/ul>
&lt;script id="tmp-list" type="text/x-handlebars-template">
   &lt;li>&lt;span class="id" title="{{id}}">&lt;/span>{{name}} -- {{id}}&lt;/li>
&lt;/script>
&lt;div id="console">&lt;/div>
```

There. That's a solution weighing in at under half the SLOC, which gives precisely zero fucks about MVC frameworks and accomplishes the same task. Incidentally, I include `underscore-min.js` and `backbone-min.js` in that fiddle link because this was refactored from the above [Java-style OOP soup](http://jsfiddle.net/7X4PX/4/), but I'm fairly certain that they're both unnecessary for this approach.

Note that I use, and wholly endorse [Handlebars.js](http://handlebarsjs.com/), or any of the [similar](http://beebole.com/pure/) standalone [JS templating](http://ejohn.org/blog/javascript-micro-templating/) engines. I'll discuss why this is a good idea in a bit, when I clearly define the Right Thing©™, and what it implies.

### <a name="the-right-thing" href="#the-right-thing"></a>The Right Thing...

...is separating out your front end into an entirely different application from your backend. Set them apart entirely, and have them communicate through JSON feeds and AJAX requests. It seems like an awkward thing to do principally because of how much harder it is to generate/template HTML inside of Javascript than it is [outside](http://weitz.de/cl-who/), in [server-side](https://github.com/weavejester/hiccup) [languages](http://hackage.haskell.org/package/heist). `Handlebars` and similar libraries provide enough of a stopgap that the separation starts looking worth while.

Those of you who are already network programmers, or have dabbled with [actors](https://en.wikipedia.org/wiki/Actor_model) will intuitively understand why this is good. For the rest, let me try to explain what you gain and what you lose.

## <a name="bidirectionally-agnostic-components" href="#bidirectionally-agnostic-components"></a>Bi-Directionally Agnostic Components

That's just a fancy way of saying that neither the front-end nor the back end really care what's on the other side of the channel, as long as it responds to the appropriate requests with well-formatted JSON<a name="note-Sun-Sep-23-215309EDT-2012"></a>[|4|](#foot-Sun-Sep-23-215309EDT-2012). That means that you could conceivably port your entire back-end without changing any client-side code if you really wanted to, or have certain requests get handled by specialized servers<a name="note-Sun-Sep-23-215334EDT-2012"></a>[|5|](#foot-Sun-Sep-23-215334EDT-2012), or write multiple front-ends, or document your interfaces and let others write additional front-ends.

Hell, as long as it sent the right requests, and interpreted them correctly, there's no particular reason you couldn't ship a native desktop or native mobile front end that connected out to a production server this way. Decoupling project components to this extent also makes it much easier to make less radical and more controlled changes than the ones proposed above.

## <a name="simpler-components" href="#simpler-components"></a>Simpler Components

Because each component can be made responsible for a particular concern, you get less code overlap. In retrospect, this has been a problem with most projects I've been on; if your server-side needs to be involved in templating, it's very tempting to "optimize" by having it emit ready-to-`$.append()` pieces. The problem is that these optimized pieces are harder to change later, and they sometimes require changes even when no other piece of server-side alters.

Doing the JSON communication thing completely tears this problem down. One end emits a series of expressions, the other consumes it. That means that your server has no involvement whatsoever in how the data is displayed to the user, and the client doesn't give a flying fuck about how it's stored on the back-end.

## <a name="security-concerns" href="#security-concerns"></a>Security Concerns

One not-entirely-good part of the situation is that as soon as you decide to architect your application as a discrete set of network-communicating components, you have to solve one or two big problems that you could otherwise avoid. Specifically, you need to start dealing with throttling, [authentication](http://langnostic.blogspot.com/2012/06/authentication-authentication.html) and network security right away, rather than leaving them for the point when you start scaling up. "Dealing with" doesn't necessarily mean "building", by the way, a legitimate choice is to make all of your handlers publicly accessible, but in that case you still need to make sure that no private information leaks out.

You also need to invest some thought into storage layout, since you won't necessarily be able to assume that the entire application is on the same machine.

## <a name="rich-client-side" href="#rich-client-side"></a>Rich Client Side

This isn't necessarily a good thing. Yes there's a somewhat better interactive experience for the user, but they absolutely *have* to have JS enabled if they're frequenting your site. You can still bolt together a much simpler, pure HTML interface, but that will likely have to be an entirely separate piece if you want to keep any of the benefits of the decoupled approach. You still *should* take the approach, I think, but I wanted to note that there's a little bit more involved with supporting security-conscious users and older browsers.

## <a name="html-and-javascript" href="#html-and-javascript"></a>HTML and Javascript

Barring a native front-end, you're stuck developing your client side in Javascript. The bad news is that it's Javascript. The good news is that a lot of people out there know it, and it means that your UI guys don't necessarily need to be up on their type theory or compiler concepts in order to be productive. *And* there's no reason for the back-end programmers not to use whatever language they find most productive<a name="note-Sun-Sep-23-215659EDT-2012"></a>[|6|](#foot-Sun-Sep-23-215659EDT-2012).

You could [Coffee Script](http://coffeescript.org/) or [Parenscript](http://common-lisp.net/project/parenscript/) or [Clojurescript](http://clojurescriptone.com/) your way out of the worst of it, but any of those approaches necessarily couples you to some language or technology less common and less commonly known than Javascript. That particular tradeoff<a name="note-Sun-Sep-23-215748EDT-2012"></a>[|7|](#foot-Sun-Sep-23-215748EDT-2012) is a conversation I plan to have with myself another day though, lets get back on topic.

### <a name="what-the-frameworks-dont" href="#what-the-frameworks-dont"></a>What The Frameworks Do`(n't)?`

So back to **They're All Shit**. You'll notice that of the implications above, a framework can<a name="note-Sun-Sep-23-215819EDT-2012"></a>[|8|](#foot-Sun-Sep-23-215819EDT-2012) conceivably mitigate one; the security concerns. The rest of them are either inherent advantages, or inherent disadvantages so fundamentally baked into the approach that no amount of JS-based syntactic sugar *could* make a difference.

The one extremely annoying thing each framework seems to do is try to layer additional object hierarchies on top of the DOM; an already existing object hierarchy that perfectly expresses the `view` end of an application. You're expected to maintain a view and a model tree in addition to that. I guess some people are already used to typing reams upon reams of code to perform basic tasks? And most of them [work at Google](http://angularjs.org/)? Ok, ok, maybe that's not entirely fair. It's certainly possible that the approach allows larger applications to be put together more easily, but I'm honestly not seeing it. Having taken in tutorials for [Ember](http://www.andymatthews.net/read/2012/03/07/Getting-Started-With-EmberJS), [Spine](https://github.com/maccman/spine.todos), [Backbone](http://coenraets.org/blog/2011/12/backbone-js-wine-cellar-tutorial-part-1-getting-started/), [Angular](http://docs.angularjs.org/tutorial/), and [Batman](http://batmanjs.org/alfred.html), the things they all have in common are:


-   getting a simple task done is a lot more complicated with framework code than without it
-   components built with frameworks are *less composeable* than components built without them
-   everyone really, *really*, ***really*** wants you to declare a tree of classes before you do anything


It seems like all of those would add up to significantly increase complexity in a larger project, and I sort of had this goofy idea that complexity is a thing we're trying to *reduce* when we reach for library code.

In any case, I'm going to keep pushing straight up jQuery with Handlebars for the time being. With small, deliberate pinches of [Underscore](http://underscorejs.org/) here and there. And I'll keep an eye out for pitfalls that might be pre-resolvable using some of the approaches I've seen this week. If something pops up and kicks my ass hard enough to change my mind, I'll write a follow-up, but until then I can't honestly recommend that you go with what seems to be the flow of web development on this one.


* * *
##### Footnotes

1 - <a name="foot-Sun-Sep-23-214748EDT-2012"></a>[|back|](#note-Sun-Sep-23-214748EDT-2012) - Depending on how comfortable you are with Coffee Script, and how much you hate JS. Links to both in the sidebar.

2 - <a name="foot-Sun-Sep-23-214859EDT-2012"></a>[|back|](#note-Sun-Sep-23-214859EDT-2012) - Again, I've only thrown the past week or so at this, so take that with a grain of salt.

3 - <a name="foot-Sun-Sep-23-214921EDT-2012"></a>[|back|](#note-Sun-Sep-23-214921EDT-2012) - And, in fact, seem to make it much harder, more ponderous and more complicated to do the Right Thing©™.

4 - <a name="foot-Sun-Sep-23-215309EDT-2012"></a>[|back|](#note-Sun-Sep-23-215309EDT-2012) - Or XML, or YAML, or whatever markup you end up actually using for communication. JSON seems like the right approach since it's extremely simple, and extremely easy to work with from within Javascript.

5 - <a name="foot-Sun-Sep-23-215334EDT-2012"></a>[|back|](#note-Sun-Sep-23-215334EDT-2012) - For instance, you could have most of your app written in a [powerful, expressive language](http://www.webnoir.org/) without regard for performance, but have any real-time pieces handled by [a server optimized for high concurrent throughput](http://yaws.hyber.org/).

6 - <a name="foot-Sun-Sep-23-215659EDT-2012"></a>[|back|](#note-Sun-Sep-23-215659EDT-2012) - As long as it supports easy serialization to and from whatever data format you guys have settled on.

7 - <a name="foot-Sun-Sep-23-215748EDT-2012"></a>[|back|](#note-Sun-Sep-23-215748EDT-2012) -Number of practitioners vs. Quality of hiring pool.

8 - <a name="foot-Sun-Sep-23-215819EDT-2012"></a>[|back|](#note-Sun-Sep-23-215819EDT-2012) - This doesn't mean they *do*, incidentally; from what I've seen, none of the current JS MVCs bother making it easier to do authentication, or secure requests.
