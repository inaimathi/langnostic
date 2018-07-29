So for a while, [Slack](TODO) has been low-key bothering me. Mainly the usability aspects, I have to admit. The way they handle a user who's in multilpe organizations that use slack has always struck me as mildly bizarre. The fact that lots of dev shops and related organizations all use it just compounds this problem.

At some point I realized that, for non-paying organizations such as the [Toronto Computer Science Reading Group](TODO), Slack actively holds historic data hostage. This is particularly annoying for me, because that group had been using slack extensively to keep notes and share interesting things.

That was the point at which I actively started discouraging Slack use for any meaningful sort of conversation, instead directing people who were interested in talking towards my github or gmail.

Recently, I got into the situation where I had to export messages from some private channels. And because Slack is Slack, their [export tools for private channels]() are a premium service.

I think at this point, I'm done cutting Slack any slack.

## Slack Private Channel Export Bookmarklet

Here's a bookmarklet you can use to export a single Slack channel from your browser into a local file in JSON format. If there's interest, it wouldn't be too hard to extend this to export all your visible channels rather than just one.

```
(function () {
    var saveAs = saveAs || (function(view) {
	"use strict";
	// IE <10 is explicitly unsupported
	if (typeof view === "undefined" || typeof navigator !== "undefined" && /MSIE [1-9]\./.test(navigator.userAgent)) {
	    return;
	}
	var
	doc = view.document
	// only get URL when necessary in case Blob.js hasn't overridden it yet
	, get_URL = function() {
	    return view.URL || view.webkitURL || view;
	}
	, save_link = doc.createElementNS("http://www.w3.org/1999/xhtml", "a")
	, can_use_save_link = "download" in save_link
	, click = function(node) {
	    var event = new MouseEvent("click");
	    node.dispatchEvent(event);
	}
	, is_safari = /constructor/i.test(view.HTMLElement) || view.safari
	, is_chrome_ios =/CriOS\/[\d]+/.test(navigator.userAgent)
	, setImmediate = view.setImmediate || view.setTimeout
	, throw_outside = function(ex) {
	    setImmediate(function() {
		throw ex;
	    }, 0);
	}
	, force_saveable_type = "application/octet-stream"
	// the Blob API is fundamentally broken as there is no "downloadfinished" event to subscribe to
	, arbitrary_revoke_timeout = 1000 * 40 // in ms
	, revoke = function(file) {
	    var revoker = function() {
		if (typeof file === "string") { // file is an object URL
		    get_URL().revokeObjectURL(file);
		} else { // file is a File
		    file.remove();
		}
	    };
	    setTimeout(revoker, arbitrary_revoke_timeout);
	}
	, dispatch = function(filesaver, event_types, event) {
	    event_types = [].concat(event_types);
	    var i = event_types.length;
	    while (i--) {
		var listener = filesaver["on" + event_types[i]];
		if (typeof listener === "function") {
		    try {
			listener.call(filesaver, event || filesaver);
		    } catch (ex) {
			throw_outside(ex);
		    }
		}
	    }
	}
	, auto_bom = function(blob) {
	    // prepend BOM for UTF-8 XML and text/* types (including HTML)
	    // note: your browser will automatically convert UTF-16 U+FEFF to EF BB BF
	    if (/^\s*(?:text\/\S*|application\/xml|\S*\/\S*\+xml)\s*;.*charset\s*=\s*utf-8/i.test(blob.type)) {
		return new Blob([String.fromCharCode(0xFEFF), blob], {type: blob.type});
	    }
	    return blob;
	}
	, FileSaver = function(blob, name, no_auto_bom) {
	    if (!no_auto_bom) {
		blob = auto_bom(blob);
	    }
	    // First try a.download, then web filesystem, then object URLs
	    var
	    filesaver = this
	    , type = blob.type
	    , force = type === force_saveable_type
	    , object_url
	    , dispatch_all = function() {
		dispatch(filesaver, "writestart progress write writeend".split(" "));
	    }
	    // on any filesys errors revert to saving with object URLs
	    , fs_error = function() {
		if ((is_chrome_ios || (force && is_safari)) && view.FileReader) {
		    // Safari doesn't allow downloading of blob urls
		    var reader = new FileReader();
		    reader.onloadend = function() {
			var url = is_chrome_ios ? reader.result : reader.result.replace(/^data:[^;]*;/, 'data:attachment/file;');
			var popup = view.open(url, '_blank');
			if(!popup) view.location.href = url;
			url=undefined; // release reference before dispatching
			filesaver.readyState = filesaver.DONE;
			dispatch_all();
		    };
		    reader.readAsDataURL(blob);
		    filesaver.readyState = filesaver.INIT;
		    return;
		}
		// don't create more object URLs than needed
		if (!object_url) {
		    object_url = get_URL().createObjectURL(blob);
		}
		if (force) {
		    view.location.href = object_url;
		} else {
		    var opened = view.open(object_url, "_blank");
		    if (!opened) {
			// Apple does not allow window.open, see https://developer.apple.com/library/safari/documentation/Tools/Conceptual/SafariExtensionGuide/WorkingwithWindowsandTabs/WorkingwithWindowsandTabs.html
			view.location.href = object_url;
		    }
		}
		filesaver.readyState = filesaver.DONE;
		dispatch_all();
		revoke(object_url);
	    }
	    ;
	    filesaver.readyState = filesaver.INIT;

	    if (can_use_save_link) {
		object_url = get_URL().createObjectURL(blob);
		setImmediate(function() {
		    save_link.href = object_url;
		    save_link.download = name;
		    click(save_link);
		    dispatch_all();
		    revoke(object_url);
		    filesaver.readyState = filesaver.DONE;
		}, 0);
		return;
	    }

	    fs_error();
	}
	, FS_proto = FileSaver.prototype
	, saveAs = function(blob, name, no_auto_bom) {
	    return new FileSaver(blob, name || blob.name || "download", no_auto_bom);
	}
	;

	// IE 10+ (native saveAs)
	if (typeof navigator !== "undefined" && navigator.msSaveOrOpenBlob) {
	    return function(blob, name, no_auto_bom) {
		name = name || blob.name || "download";

		if (!no_auto_bom) {
		    blob = auto_bom(blob);
		}
		return navigator.msSaveOrOpenBlob(blob, name);
	    };
	}

	// todo: detect chrome extensions & packaged apps
	//save_link.target = "_blank";

	FS_proto.abort = function(){};
	FS_proto.readyState = FS_proto.INIT = 0;
	FS_proto.WRITING = 1;
	FS_proto.DONE = 2;

	FS_proto.error =
	    FS_proto.onwritestart =
	    FS_proto.onprogress =
	    FS_proto.onwrite =
	    FS_proto.onabort =
	    FS_proto.onerror =
	    FS_proto.onwriteend =
	    null;

	return saveAs;
    }(
	typeof self !== "undefined" && self
	    || typeof window !== "undefined" && window
	    || this
    ));

    var res = [];
    var date = null;
    var latestSender = null;
    var latestTime = null;
    document.querySelectorAll("div.c-virtual_list__item")
        .forEach(function (el) {
            var day = el.querySelector(".c-message_list__day_divider__label__pill");
            var msg = el.querySelector(".c-message__content");
            if (day) {
                date = day.innerText;
            } else if (msg) {
                var s = msg.querySelector(".c-message__sender");
                if (s) {
                    latestSender = msg.querySelector(".c-message__sender").innerText
                    latestTime = msg.querySelector(".c-timestamp__label").innerText
                }
                res.push({
                    date: date
                    , time: latestTime
                    , sender: latestSender
                    , message: msg.querySelector(".c-message__body").innerHTML
                })
            } else {
                // console.log("GOT SOMETHING ELSE!")
            }
        })
    var blob = new Blob([JSON.stringify(res, null, 2)], {type: "application/json;charset=utf-8"});
    saveAs(blob, "slack-export.json")
    return res;
})()
```

- javascript bookmarklet (TODO - do the semi-literate coding thing here, and call out FileSave.js as being inclduded above)
- also TODO, make the bookmarklet friendly and get a git repo going
