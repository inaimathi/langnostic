I've been working with [Angular.js](http://angularjs.org/) for the past little while, both on [Web-Mote](https://github.com/Inaimathi/web-mote) (still listed as a Perl project for some reason) and various work projects. Overall, the impression is very good. For the most part, the Angular approach saves a lot of lines and cycles over [jQuery](http://jquery.com/)-style DOM-traversal and manipulation. It *always* saves a lot of lines over Handlebars-style HTML templating, which was honestly a bit of a surprise at first. Proper routing is slightly more annoying than its backbone.js counterpart, but forces you to break your app out into discrete, composeable pieces, which seems like it would help you scale up down the line.

There are a couple of small places where DOM-traversal seems to be the easier way forward<a name="note-Thu-Dec-27-100616EST-2012"></a>[|1|](#foot-Thu-Dec-27-100616EST-2012), and there's one omission made by the Angular guys<a name="note-Thu-Dec-27-100622EST-2012"></a>[|2|](#foot-Thu-Dec-27-100622EST-2012), but otherwise, I can heartily recommend the library, even after initial frustration.

The big perspective change you need to come to grips with is the shift from imperative/functional-ish programming to a model-centric, [reactive](http://en.wikipedia.org/wiki/Reactive_programming) approach. Using plain jQuery, you might define a dynamic list as a `$.each/append` call wrapped in a `render` method somewhere. You might define the template manually in your js code if it's simple enough, or do it using [Handlebars](http://handlebarsjs.com/) or a similar HTML-templating if it's more involved. If you needed to collect the contents/order of the list later, you traverse the DOM and pull out the chunks you need.

It's not an unreasonable way of going about things, but Angular does it better; the same task is done by using the HTML-DSL to describe the relationship of a model (a literal JS list of objects) to the markup, and then populating that model. The framework reactively updates the DOM whenever model changes occur. Later, when you need the data back, you don't need to traverse anything. You just send the model out to wherever it needs to go.

Lets go through some before and after shots of web-mote for illustrative purposes. Specifically, lets take a look at the controls, since that's the simpler piece. Incidentally, I'm not claiming that this is the most elegant code either before or after. I just want to show you the structural and philosophical differences between approaches.

### <a name="before" href="#before"></a>Before

First, the relevant HTML markup

```html
...

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

...

&lt;div id="controls"&gt;&lt;/div&gt;

...```

The target element gets its own `id` so that we can refer to it from our jQuery code. The `script` blocks are [Handlebars](http://handlebarsjs.com/) template declarations. I've elided the rest of the HTML markup because it's all include/template/meta overhead, but you can see it in [the appropriate Web-Mote commit](https://github.com/Inaimathi/web-mote/tree/f349e240ac88f0344de8b8a3a8738e08a8aa9c29/static) if you are so inclined.

Here are the relevant JS declarations

```javascript
Handlebars.registerHelper("control-button", function (ctrl) {
    return new Handlebars.SafeString(templates.control(ctrl));
});

var templates = {
    control: Handlebars.compile($("#tmp-control").html()),
    controlBlock : Handlebars.compile($("#tmp-control-block").html())
}

var mote = {
    pressed: false,
    hold: function (cmd) {
        mote.release();
        mote.pressed = setInterval(function(){
            mote.command(cmd);
        }, 200);
    },
    release: function () {
        clearInterval(mote.pressed);
        mote.pressed = false;
    },
    renderControls: function (controlLists) {
        $.each(controlLists,
               function (index, e) {
                   $("#controls").append(templates.controlBlock(e));
               })
            },
    command: function (cmd) {
        console.log(cmd);
        $.post("/command", {"command": cmd},
               function () {
                   if (cmd == "pause") {
                       var btn = templates.control({cmd: "play"});
                       $("#controls .pause").replaceWith(btn);
                   } else if (cmd == "play") {
                       var btn = templates.control({cmd: "pause"});
                       $("#controls .play").replaceWith(btn);
                   }
               })
    }
}

// older versions of safari don't like `position: fixed`.
// they also don't like when you set `position: fixed` in a stylesheet,
//   then override that with inline styles.
// what I'm saying is that older versions of safari are assholes
if ($.browser.safari) {
    $("#controls").css({ "position": 'absolute' });
    window.onscroll = function() {
        $("#controls").css({ 
            "top" : window.pageYOffset + 'px'
        });
    };
} else {
    $("#controls").css({ "position": 'fixed' });    
}```

`command` is only relevant because it switches out the `pause` button for a `play` button when its pressed successfully. Observe that all of the rendering here is happening through DOM manipulations. We run `.append` over the result of calling the `controlBlock` template on each group of player controls, and each call to `controlBlock` itself applies the `control` template. When we need to do that button switch I mentioned, we do it by calling `.replaceWith` on the appropriate DOM selector. We probably could have avoided going to sub-templates for control buttons, but that would have saved us five lines at the outside; just the `script` tag boilerplate in the HTML markup, and that `Handlebars` helper definition.

Finally, here's the `.ready()` call

```javascript
$(document).ready(function() {
    mote.renderControls(
        [[//{cmd: "step-backward"},
            {cmd: "backward", held: true},
            {cmd: "stop"},
            {cmd: "pause"},
            {cmd: "forward", held: true}
          //{cmd: "step-forward"}
        ],
         [{cmd: "volume-down", held: true}, 
          {cmd: "volume-off"}, 
          {cmd: "volume-up", held: true}]]);
});```

That's that. Like I said, this isn't the most elegant code I've ever written. If I really put my mind to it, I might be able to shave off ten lines or so, and clarify my intent in a couple of places, but I think it would be pretty difficult to do *much* better without fundamentally changing the approach.

### <a name="after" href="#after"></a>After

HTML markup first

```html
&lt;div id="controls" ng-controller="CommandCtrl" ng-style="style"&gt;
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
&lt;/div&gt;```

It should be fairly self-explanatory. That's not the clearest code you're likely to find, but it's illustrative. We've got a bunch of non-HTML directives strewn about; all the stuff starting with `ng-` is part of the Angular DSL. While we need to do the `{{}}` thing to evaluate code inside of standard HTML properties, any code inside of `ng-` properties is automatically run in the context of the `controller` `CommandCtrl`.

```javascript
function CommandCtrl ($scope, $http) {
// older versions of safari don't like `position: fixed`.
// they also don't like when you set `position: fixed` in a stylesheet,
//   then override that with inline styles.
// what I'm saying is that older versions of safari are assholes
    if (util.browser().agent == 'safari') {
        window.onscroll = function() { 
            $scope.style = { position: "absolute", top : window.pageYOffset + 'px' };
        };
    } else {
        $scope.style = { position: "fixed" };
    }

    $scope.held = false;

    $scope.controlTree = [
        [ //{cmd: "step-backward"},
            {cmd: "backward", held: true},
            {cmd: "stop"},
            {cmd: "pause"},
            {cmd: "forward", held: true}
            //{cmd: "step-forward"}
        ],
        [{cmd: "volume-down", held: true}, 
         {cmd: "volume-off"}, 
         {cmd: "volume-up", held: true}]
    ]

    $scope.command = function (cmd) { 
        util.post($http, "/command", {"command": cmd})
            .success(function (data, status, headers, config) {
                $scope.data = data;
                if (cmd == "pause") $scope.controlTree[0][2] = {cmd: "play"}
                else if (cmd == "play") $scope.controlTree[0][2] = {cmd: "pause"}
            })
    }

    $scope.hold = function (cmd) {
        $scope.held = setInterval(function() { $scope.command(cmd) }, 200);
    }

    $scope.release = function (cmd) { 
        clearInterval($scope.held);
        $scope.held = false;
    }
}```

That's all, by the way. You've seen all the code for the Angular version, and the two are functionally identical from the users' point of view.

Unlike in the jQuery solution, there's no DOM manipulation here. We've got a model called `controlTree` which contains the same specification of controls that the earlier version did, but this time, the actual construction of relevant templates is taken care of by the framework. We just specify the relationship between that model and the front-end in the form of the HTML code above, and Angular automatically updates. The clearest demonstration of that is these lines

```javascript
if (cmd == "pause") $scope.controlTree[0][2] = {cmd: "play"}
else if (cmd == "play") $scope.controlTree[0][2] = {cmd: "pause"}```

That's part of sending a command, and all it does is change the contents of our model. The view is updated as soon as this change is made. The equivalent from **"Before"** is 

```javascript
if (cmd == "pause") {
    var btn = templates.control({cmd: "play"});
    $("#controls .pause").replaceWith(btn);
} else if (cmd == "play") {
    var btn = templates.control({cmd: "pause"});
    $("#controls .play").replaceWith(btn);
}```

Where we're back to templating ourselves. You can also see the same principles affecting that code hacking around older versions of Safari; we're just setting up some objects rather than doing DOM traversal ourselves. 

```javascript
if ($.browser.safari) {
  $("#controls").css({ "position": 'absolute' });
  window.onscroll = function() {
    $("#controls").css({ 
        "top" : window.pageYOffset + 'px'
    });
  };
} else {
  $("#controls").css({ "position": 'fixed' });    
}```

vs

```javascript
if (util.browser().agent == 'safari') {
  window.onscroll = function() { 
    $scope.style = { position: "absolute", top : window.pageYOffset + 'px' };
  };
} else {
  $scope.style = { position: "fixed" };
}```

The effect is the same, but the particulars of updating and rendering are kept comfortably away from us.

As I said, the above example was picked to clearly illustrate the differences between approaches, not necessarily because it's the biggest gain in clarity I've gotten out of porting over<a name="note-Thu-Dec-27-101739EST-2012"></a>[|3|](#foot-Thu-Dec-27-101739EST-2012). I'm sure a headache or two will pop up down the line, but I submit that this is a fundamentally more humane way to craft responsive web front-ends than the alternatives.

And I'll be [using it](http://angularjs.org/) where I can from now on.


* * *
##### Footnotes

1 - <a name="foot-Thu-Dec-27-100616EST-2012"></a>[|back|](#note-Thu-Dec-27-100616EST-2012) -  (re-ordering complex elements is really the only one I've observed; stuff that's too complex to do like [this](http://jsfiddle.net/g/hKYWr/), but where you still need to pass the current order of some set of UI elements back to the server for persistence. As I said already, [angular-ui](http://angular-ui.github.com/) does it just fine for simple constructs, but for anything more complicated, the Angular solution is ~30-lines of sub-module, where the DOM-traversal solution is a mere 5)

2 - <a name="foot-Thu-Dec-27-100622EST-2012"></a>[|back|](#note-Thu-Dec-27-100622EST-2012) -  (the `$http.post` function *doesn't* do the jQuery thing of encoding an object as `POST` parameters. The default behavior is to dump the parameter object to a JSON string and pass that to the server as a post body. I could actually see that being the easier approach if you had perfect control of the server, since that would let you do some not-exactly-HTTP processing on the incoming structure. If you're using a pre-built one, though, you're probably stuck doing something manual and annoying like this

```javascript
...
post: function ($http, url, data) {
  var encoded = _.map(data, function (val, k) { return encodeURI(k) + "=" + encodeURI(val); });
  $http.defaults.headers.post["Content-Type"] = "application/x-www-form-urlencoded";
  return $http.post(url, encoded.join("&"));
},
...```

or (if you're concerned about going with the grain of the framework) this

```javascript
myModule.config(function ($httpProvider) {
  $httpProvider.defaults.headers.post['Content-Type'] = 'application/x-www-form-urlencoded';
  $httpProvider.defaults.transformRequest = function(data){
    return _.map(data, function (val, k) { return encodeURI(k) + "=" + encodeURI(val); });
  }
});```

Not *too* ugly once you throw in the usual pinch of [underscore](http://underscorejs.org/), but this is the sort of thing that really seems like it should be built in as a default behavior. Unless the Angular devs really think some large portion of their users are going to build their own servers to work the other way)

3 - <a name="foot-Thu-Dec-27-101739EST-2012"></a>[|back|](#note-Thu-Dec-27-101739EST-2012) -  (in fact, this is probably the least clarity I've gained by moving over to the reactive approach. As I said earlier, the line-count is usually halved without breaking a sweat)
