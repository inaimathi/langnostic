[cl-inception.jpg]

That's just about what I spent the last four hours feeling like. I've been working semi-feverishly on a version of the formlet macros for Common Lisp.  Automatic validation is surprisingly difficult when you break it down. If you want to do it well, I mean. The thing could easily have been half-assed in half the time, and three-quarters the code. But I meant to do it well, so it took a while. It's still not anywhere near perfect. If you tried to get it to generate a form called "form-values", I imagine it would snarl at you like some lovecraftean horror. I'll be plugging the holes over the next little while with judicious use of [with-gensyms](http://www.gigamonkeys.com/book/macros-defining-your-own.html), but that'll only add one layer to a construct already six dreams deep.

It's fucking bizarre, I tell you. On days like this, I can sort of see why people stay away from LISPS in general. Not all minds can be made to twist in on themselves indefinitely; mine was barely capable of six levels, like I said. By which I mean, the formlet system I wrote is made up of a function and a macro (show-[name]-form and def-form). show-[name]-form is a function that calls the show-form macro to generate partial HTML and invoke the form-element macro, which expands into the actual low-level HTML boilerplate. def-form is a macro that expands into the appropriate show-[name]-form function, and defines a validator function, itself composed of no less than four nested macros.

It sounds like a complete goddamn birdsnest, and it kind of is, but every layer of complexity is warranted, as far as I know (if it isn't, please, PLEASE tell me that, and point me to simpler code that does what this does). 

Here's the thing. If you're just looking at how to generate the HTML on forms (a-la the PLT Formlet system), it's ridiculously easy. I could have gotten away with a macro and a half instead of the layer cake that I ended up with, but the issue there is that that way wouldn't have saved me much time or code. The hard part on a form is not displaying it. That is the easy part. If I may say so, the trivial part. If your system does nothing else, then it is balanced on a precarious edge between the twin pits of "break-even" and "not worth using". The difficult part is validation. At the high level, it sounds simple (which is probably why I was foolhearty enough to attempt it); 


1.   Take in the form results and a list of predicates.
1.   Run the predicates over the results.
1.   If they all passed, do something, otherwise, send them back with a little note saying what they need to fix.


Tadaah!

But those three steps (if you wanna do them properly) have so many moving parts that it necessitates many, many macros. For starters, there are fundamentally two types of forms; the type where you need to validate each field (like a registration form, or other long list of inputs), and the type where you need to check the whole form in aggregate (like a login form, where you really only care whether you were just handed a valid name/password pair, and in fact you don't want to tell the user which of the two they got wrong).

That second group runs out of situations fairly quickly; you just need to display one error message for the form and send the user back, or let them through if they got it right. Done.

The first group is what caused most of the work. First off, each input needs its own predicate, and its own failure message. If you want to provide good signage, it's also not sufficient to and the list of predicates over the inputs; you want to iterate through the full list no matter how many mistakes you find, and then mark them all off in the error listing. When you get back, you need to display each error next to the appropriate input, and (for non-passwords), you want to keep any inputs the user sent you that validated ok. As if that weren't enough, those pesky users like labels too, so that they can see what they're filling out, and you need to support (at minimum) input type=text, input type=password, textarea, select, checkbox if you want to be useful. option can be useful, but you can get by with select in a pinch. The end result is that you need to track


1.   field names
1.   field types
1.   user input for each field
1.   a predicate for each field
1.   an error message for each field


and good god, do the interactions make me want to headdesk. #2 specifically sounds easy, but gets mean fast when you think about it. I won't go into the details, but keep in mind that it's not enough to just keep track of a type property and switch it out; a select has a fundamentally different tree structure underneath than a textarea, which is again fundamentally different from an input. When I say different, I mean that they track user input in different ways, need different things changed out when they error, have different consequences when setting their value (and different methods of setting it, too) and behave differently on screen.

I'm not posting the code here, it'll be at [github](http://github.com/Inaimathi/cl-fomlets). If you can do better, please, please do so, and let me know.

In the meantime, I honestly feel like I should be spinning a top and eying it warily.
