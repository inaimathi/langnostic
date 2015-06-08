Firstly, those notes to self I mentioned last time have been applied. You can see the results [here](https://github.com/Inaimathi/goget), or more specifically, [here](https://github.com/Inaimathi/goget/blob/master/Handlers.hs#L23-L42) among other places. Those used to be separate functions that each edited specific things, rather than each calling out to one actual editor. Also, the `countItem` and `commentItem` pieces were one function that accepted multiple `Maybe`s and only applied the relevant ones. That was more complicated than it needed to be, so it has been separated into what you see there. Thinking about that function more thoroughly also pointed me to a bug I had previously missed<a name="note-Sat-Feb-16-205443EST-2013"></a>[|1|](#foot-Sat-Feb-16-205443EST-2013).

Secondly, I ended up having to put together two front-ends; an Angular-based page and something a bit more traditional with jQuery. The reason was that the Angular.js version refused to work on my phone for some bizarre reason. It showed the intro screen fine, and displayed auth errors like it was supposed to, but refused to show the main screen on a successful authentication. I have no idea why that was, but since half the point of this app was that I could check it from my phone on the way from work, it wasn't going to fly. Luckily, the API-friendly back-end technique I'm trying out here made it a breeze to create a new front-end without touching the rest of the application. The changes involved a [couple of CSS tweaks and re-writes of goget.js and index.html](https://github.com/Inaimathi/goget/commit/8e6760d45871a531b5dfd62c527ce8aa7f735b23). Also, I had to throw [handlebars](http://handlebarsjs.com/) back in there.

Thirdly, I [deployed it](http://goget.inaimathi.ca/). It doesn't run under HTTPS yet, so don't put in anything illegal or embarrassing, but that's a usable shopping list synchronizer which I intend to use. Let me know if you try it and anything explodes.

### <a name="on-to-the-code" href="#on-to-the-code"></a>On to the code!

At the moment, I've got the [jQuery](http://jquery.com/) and [Angular](http://angularjs.org/) versions separated into different branches, but I'll merge them shortly and just provide each as a separate front-end<a name="note-Sat-Feb-16-205448EST-2013"></a>[|2|](#foot-Sat-Feb-16-205448EST-2013). On a scale this small, it turns out not to matter much how you write the interface. If you check out the line-count on both those front-ends, the reactive version saves about 10 lines of HTML and 15 of JavaScript. It stacks up in larger applications, and if there's an option to use less JS, I'll take it, but in this case, the elegant solution doesn't work, so whatever. Lets start with the HTML markup first. Here's the **Angular**

```html
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>GoGet - Because I Can't Be Expected to Remember This Shit</title>
  </head>
  <body ng-app="goget">
    
    <div ng-controller="GoGetCtrl">
      <div ng-show="!user.loggedIn" class="user-form">
        <div ng-show="authError" class="error">{{authError}}</div>
        <input type="text" placeholder="User Name" ng-model="user.name" />
        <input type="password" placeholder="Passphrase" ng-model="user.passphrase" />
        <a class="register" ng-click="register(user.name, user.passphrase)">Register</a>
        <button class="btn login" ng-click="login(user.name, user.passphrase)"><i class="icon-check"></i> Login</button>
      </div>
        
      <ul ng-show="user.loggedIn" class="shopping-list">
        <li class="{{itm.status}}" ng-repeat="itm in itemList"
            ng-mouseover="itm.hovered = true" ng-mouseout="itm.hovered = false">
          <span class="count">{{itm.count}}x</span> 
          <span class="name">{{itm.name}}</span> 
          <button class="btn" ng-click="got(itm.name)" ng-show="itm.status=='Need'"><i class="icon-check"></i></button>
          <button class="btn" ng-click="need(itm.name)" ng-show="itm.status=='Got'"><i class="icon-exclamation-sign"></i></button>
          <p class="comment" ng-show="itm.hovered">{{itm.comment}}</p>
        </li>
        <li class="controls">
          <input type="text" placeholder="Item Name" ng-model="newItem.name" /> 
          <input type="text" placeholder="Comment" ng-model="newItem.comment" />
          <input type="text" placeholder="Count" ng-model="newItem.count">
          <button class="btn" ng-click="add(newItem.name, newItem.comment, newItem.count)"><i class="icon-plus"></i></button>
        </li>
      </ul>
    </div>

    <!-- ------ -->
    <!-- Styles -->
    <!-- ------ -->
    <link rel="stylesheet" href="/static/css/bootstrap.min.css" type="text/css" media="screen" />
    <link rel="stylesheet" href="/static/css/bootstrap-responsive.min.css" type="text/css" media="screen" />

    <link rel="stylesheet" href="/static/css/style.css" type="text/css" media="screen" />    

    <!-- ------- -->
    <!-- Scripts -->
    <!-- ------- -->
    <script src="/static/js/underscore-min.js" type="text/javascript"></script>
    <script src="/static/js/angular.min.js" type="text/javascript"></script>    
    
    <script src="/static/js/goget.js" type="text/javascript"></script>
  </body>
</html>
```

and here's the **jQuery**

```html
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
  <head>
    <meta charset="UTF-8" />
    <title>GoGet - Because I Can't Be Expected to Remember This Shit</title>
  </head>
  <body> 

    <!-- templates -->
    <script id="tmp-item" type="text/x-handlebars-template">
      <li class="{{status}}">
        <span class="count">{{count}}x</span>
        <span class="name">{{name}}</span>
        {{#controls this}}{{/controls}}
        <!-- <p class="comment">{{comment}}</p> -->
      </li>
    </script>

    <script id="tmp-item-controls" type="text/x-handlebars-template">
      <button class="btn" onclick="goget.{{fn}}(jQuery(this).siblings('.name').text())"><i class="{{iconClass}}"></i></button>
    </script>

    <!-- body -->
    <div>
      <div class="user-form">
        <div class="error"></div>
        <input type="text" class="user-name" placeholder="User Name" />
        <input type="password" class="passphrase" placeholder="Passphrase" />
        <a class="register" onclick="util.applyToUser(goget.register)">Register</a>
        <button class="btn login" onclick="util.applyToUser(goget.login)">
          <i class="icon-check"></i> Login
        </button>
      </div>

      <ul class="shopping-list">
      </ul>
      <ul class="shopping-list-controls">
        <li class="controls">
          <input type="text" class="name" placeholder="Item Name" /> 
          <input type="text" class="comment" placeholder="Comment" />
          <input type="text" class="count" placeholder="Count" value="1" />
          <button class="btn" onclick="util.applyToVals(goget.add, '.controls ', ['.name', '.comment', '.count'])"><i class="icon-plus"></i></button>
        </li>
      </ul>
    </div>

    <!-- ------ -->
    <!-- Styles -->
    <!-- ------ -->
    <link rel="stylesheet" href="/static/css/bootstrap.min.css" type="text/css" media="screen" />
    <link rel="stylesheet" href="/static/css/bootstrap-responsive.min.css" type="text/css" media="screen" />

    <link rel="stylesheet" href="/static/css/style.css" type="text/css" media="screen" />    

    <!-- ------- -->
    <!-- Scripts -->
    <!-- ------- -->
    <script src="/static/js/underscore-min.js" type="text/javascript"></script>
    <script src="/static/js/handlebars.js" type="text/javascript"></script>    
    <script src="/static/js/jquery.min.js" type="text/javascript"></script>    
    
    <script src="/static/js/goget.js" type="text/javascript"></script>
  </body>
</html>
```

There's a bunch of common boilerplate at the bottom and top that you can safely ignore. The meat begins at the `body` tag and stops at the block comment denoting the `Styles` section. The only real difference is that you can see some unfamiliar `util` calls and explicit templates in the jQuery version. Oh, the Angular version also controls its visibility explicitly through those `ng-show` attributes; the jQuery version relies on CSS to do the same. The actual differences are readily on display in the JS code though. First, Angular

```javascript
var App = angular.module("goget", [])
    .config(function ($httpProvider) {
        /// Angular's post doesn't do the correct default thing with POST parameters
        $httpProvider.defaults.headers.post['Content-Type'] = 'application/x-www-form-urlencoded; charset=UTF-8';
        $httpProvider.defaults.transformRequest = function(data){
            return _.map(data, function (val, k) { return encodeURIComponent(k) + "=" + encodeURIComponent(val); }).join("&");
        }
    });

App.controller('GoGetCtrl', function ($scope, $http) {
    $scope.itemList = [];
    $scope.newItem = { count: 1 };
    $scope.user = { id: false, loggedIn: false, passphrase: "" };

    function itemPost (uri, params) {
        $http.post(uri, params)
            .success(function (data) {
                $scope.itemList = data;
            })
            .error(function (data) {
                console.log(data);
            })
    }

    function userPost (uri, params) {
        console.log("Sending " + uri + " request...")
        $http.post(uri, params)
            .success(function (data) {
                $scope.user.id = data.id;
                $scope.user.loggedIn = true;
                $scope.itemList = data.items;
            })
            .error(function (data) {
                $scope.authError = data;
                console.log(data)
            })
    }

    $scope.login = function (name, pass) {
        userPost("/auth/login", {name : name, passphrase: pass});
    }

    $scope.register = function (name, pass) {
        userPost("/auth/register", {name : name, passphrase: pass});
    }

    $scope.add = function (itemName, comment, count) {
        $http.post("/app/new", {itemName: itemName, comment: comment, count: count})
            .success(function (data) {
                $scope.itemList = data;
                $scope.newItem = { count: 1 }
            })
    }
    
    $scope.need = function (itemName) {
        itemPost("/app/item/need", {itemName: itemName});
    }
    
    $scope.got = function (itemName) {
        itemPost("/app/item/got", {itemName: itemName});
    }

});
```

and then jQuery

```javascript
var util = {
    hcompile: function (template) {
        return Handlebars.compile($("#tmp-" + template).html())
    },
    vals: function (listOfDOMSelectors) {
        return _.map(listOfDOMSelectors, function (s) { return $(s).val() })
    },
    under: function (DOMContext, listOfDOMSelectors) {
        return _.map(listOfDOMSelectors, function (s) { return DOMContext + s })
    },
    applyToVals: function (fn, DOMContext, listOfDOMSelectors) {
        return fn.apply({}, util.vals(util.under(DOMContext, listOfDOMSelectors)));
    },
    applyToUser: function (fn) {
        return util.applyToVals(fn, '.user-form ', ['.user-name', '.passphrase']);
    }
}

Handlebars.registerHelper("controls", function (anItem) {
    if (anItem.status == 'Got') {
        var ctrl = {fn: 'need', iconClass: "icon-exclamation-sign"}
    } else {
        var ctrl = {fn: 'got', iconClass: "icon-check"}
    }
    return new Handlebars.SafeString(templates.itemButtons(ctrl));
})

var templates = {
    item: util.hcompile("item"),
    itemButtons: util.hcompile("item-controls")
}

var goget = {
    render: function (itemList) {
        $(".shopping-list-controls").show()
        $(".shopping-list").empty();
        $.each(itemList, function (ix, anItem) {
            $(".shopping-list").append(templates.item(anItem));
        })
    },
    itemPost: function (uri, params) {
        $.post(uri, params)
            .done(function (data, textStatus, jqXHR) {
                goget.render($.parseJSON(jqXHR.responseText))
            })
            .fail(function (data, textStatus, jqXHR) {
                console.log(["Failed!", data, textStatus, jqXHR])
                // something odd happened; either invalid item, or failed connection
            })
    },
    userPost: function (uri, params) {
        $.post(uri, params)
            .done(function (data, textStatus, jqXHR) {
                $(".user-form").hide();
                goget.render($.parseJSON(jqXHR.responseText).items);
            })
            .fail(function (data) {
                console.log(["Failed!", data.responseText])
                $(".user-form .error").text(data.responseText).show()
            })
    },
    login: function (name, pass) {
        goget.userPost("/auth/login", { name: name, passphrase: pass });
    },
    register: function (name, pass) {
        goget.userPost("/auth/register", { name: name, passphrase: pass });
    },
    add: function (itemName, comment, count) {
        goget.itemPost("/app/new", {itemName: itemName, comment: comment, count: count})
    },
    need: function (itemName) {
        goget.itemPost("/app/item/need", {itemName: itemName});
    },
    got: function (itemName) {
        goget.itemPost("/app/item/got", {itemName: itemName});
    }
    
}
```

`util` is a bunch of shortcut functions that make it relatively simple to do things which are trivial in the reactive version. Take `applyToVals`, for instance.

```javascript
    applyToVals: function (fn, DOMContext, listOfDOMSelectors) {
        return fn.apply({}, util.vals(util.under(DOMContext, listOfDOMSelectors)));
    },
```

This is only necessary because in order to get values out of inputs, I have to do DOM traversals. Take a look at the sample invocation back in the jQuery-style HTML file

```javascript
onclick="util.applyToVals(goget.add, '.controls ', ['.name', '.comment', '.count'])"
```

So, in other words, there are three controls in the DOM somewhere, and I'd like to grab their values and pass them to the function `goget.add`. I could call `goget.add($(".controls .name").val(), $(".controls .comment").val(), $(".controls .count").val(),)`, but that seems more than mildly annoying if I have to do it multiple times. So I pulled out the pattern; `applyToVals` takes a function, a DOM context<a name="note-Sat-Feb-16-205520EST-2013"></a>[|3|](#foot-Sat-Feb-16-205520EST-2013) and a list of element selectors. It then concatenates the DOM context onto each of the selectors, and returns a list of the values of the elements specified by those selectors.

How do we do that in Angular?

```html
        <li class="controls">
          <input type="text" placeholder="Item Name" ng-model="newItem.name" /> 
          <input type="text" placeholder="Comment" ng-model="newItem.comment" />
          <input type="text" placeholder="Count" ng-model="newItem.count">
          <button class="btn" ng-click="add(newItem.name, newItem.comment, newItem.count)"><i class="icon-plus"></i></button>
        </li>
```

That's actually a snippet from the Angular-style HTML file, and only about 1/5th of it is responsible for the equivalent. Each of the inputs we care about has an `ng-model` property, and that we then just pass those models into `add`. If it worked where I needed it to, I wouldn't have bothered finding a better solution than this.

Most of the rest of the `util` namespace is actually just intermediate definitions for `util.applyToVals`, and there's one definition that uses it specifically to pull out data from the user form. Oh, and a shorthand for compiling a particular `Handlebars` template. There's a snippet where we define a helper function for the main template, and a place in the `goget` namespace wherein we call `render`, which is famously missing from Angular, and that's really it. The rest of it is transliterated pretty clearly.

The only other thing I'll highlight is that the Angular version contains this:

```javascript
    .config(function ($httpProvider) {
        /// Angular's post doesn't do the correct default thing with POST parameters
        $httpProvider.defaults.headers.post['Content-Type'] = 'application/x-www-form-urlencoded; charset=UTF-8';
        $httpProvider.defaults.transformRequest = function(data){
            return _.map(data, function (val, k) { return encodeURIComponent(k) + "=" + encodeURIComponent(val); }).join("&");
        }
    });
```

which the jQuery version doesn't. As far as I'm concerned, this is the one place where the Angular devs are just plain wrong. I know other approaches are possible here, so I guess it's a good thing that there's an option. But as far as I'm aware, all the widely used HTTP servers out there right now expect POST parameters to be encoded in the `www-form` format by default. And that's *not* what Angular does with JSON objects by default.

```javascript
$http.post("/foo", { bar: 1, baz: 2 });
```

will actually send the server a POST body that looks like `{"bar":1,"baz":2}`. At that point it's up to you to grab the raw request and parse that body with a JSON interpreter. What you likely want, because most HTTP servers will parse it appropriately by default, is `bar=1&baz=2`, with both the keys and values getting URI-encoded just in case. The way you do that is by using this config option I've got above. The jQuery equivalent doesn't need this, because `$.post` does the right thing with no additional prodding<a name="note-Sat-Feb-16-205531EST-2013"></a>[|4|](#foot-Sat-Feb-16-205531EST-2013).


* * *
##### Footnotes

1 - <a name="foot-Sat-Feb-16-205443EST-2013"></a>[|back|](#note-Sat-Feb-16-205443EST-2013) - Specifically, it had to do with reading the `count` parameter. You can see the fix [here](https://github.com/Inaimathi/goget/blob/master/goget.hs#L98-L102) and [here](https://github.com/Inaimathi/goget/blob/master/goget.hs#L74-L78). Short version: `count` needs to be readable as an `Integer` for the back-end to proceed, but it's coming from the outside, which means I can't guarantee that. The initial version of the code was optimistic, simply using `read :: Integer` assuming it could work. If a malicious front-end sent back something that couldn't be read as a number, that would have given me a run-time error. I'm under the impression that these are to be avoided in Haskell..

2 - <a name="foot-Sat-Feb-16-205448EST-2013"></a>[|back|](#note-Sat-Feb-16-205448EST-2013) - Defaulting to jQuery because I want to use it from my phone, and putting the angular version at `/angular/*` rather than at root.

3 - <a name="foot-Sat-Feb-16-205520EST-2013"></a>[|back|](#note-Sat-Feb-16-205520EST-2013) - The common selector prefix of any elements I'll need to grab from.

4 - <a name="foot-Sat-Feb-16-205531EST-2013"></a>[|back|](#note-Sat-Feb-16-205531EST-2013) - I have no idea whether there are options to do it another way in jQuery.
