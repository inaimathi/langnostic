I got some feedback on `trivialai` from a friend and made some changes.

#### The `define` method is now optionally a decorator

This means that you can equivalently do

```
tls.define(_some_function)
```
or

```
@tls.define()
def _some_function(some, args):
   ...
```

The new definition of the `define` method is 

```
...
    def define(self, fn=None, *, name=None, type=None, description=None):
        if fn is None:
            # If no function is passed, return a decorator
            def decorator(f):
                self._define_function(f, name, type, description)
                return f
            return decorator
        # If function is passed directly, define it
        return self._define_function(fn, name, type, description)
...
```

There's exactly one notable thing about this, but I'll get to it in a sec.

#### Transform functions are a bit more pythonic

Specifically, the type of `client.generate_checked` went from being `(String -> Maybe a) -> System -> Prompt -> Maybe a` to `(String -> a) -> System -> Prompt -> a Throws TransformError`. Concretely, this means going from things that look like

```
def loadch(resp):
    if resp is None:
        return None, False
    try:
        return (
            json.loads(
                re.sub("^```\\w+\n", "", resp.strip()).removesuffix("```").strip()
            ),
            True,
        )
    except (TypeError, json.decoder.JSONDecodeError):
        pass
    return None, False
```

to things that look like


```
def loadch(resp):
    if resp is None:
        raise TransformError("no-message-given")
    try:
        return json.loads(strip_md_code(resp.strip()))
    except (TypeError, json.decoder.JSONDecodeError):
        pass
    raise TransformError("parse-failed")
```

It's a bit gnarlier to my eye, but my friend pointed out that using exceptions is a way more common approach to control flow in Python than in the functional languages, and given the native constructs available, it also composes more naturally so lets go with it.


## Next Steps

Ok, I mentioned that the updated version of `define` above has one notable about it. The notable thing is that I generated it by doing

```
>>> from src.trivialai import ollama
>>> client = ollama.Ollama("mannix/llama3.1-8b-abliterated:latest", "http://localhost:11434/")
>>> client.edit_directory(
  "~/projects/trivialai/", 
  "Change the method in src.trivialai.tools so that it can be called as a decorator")
/home/inaimathi/projects/trivialai/
   Considering ['src/trivialai/core.py', 'src/trivialai/tools.py']
   Changing ['src/trivialai/tools.py']
>>> 
```

Which is... pretty close to magic? This is step one at a programming-oriented robot. The next step is going to be trying to generalize this up to a full development process, but here's the current state of play:

```
import os

from . import util


class FilesystemMixin:
    def edit_file(
        self, file_path, system, prompt, after_save=None, consider_current=True
    ):
        full_system = "\n".join(
            [
                system,
                f"The current contents of {file_path} is {util.slurp(file_path)}"
                if (os.path.isfile(file_path) and consider_current)
                else f"The file {file_path} currently doesn't exist.",
                f"What changes would you make to the file {file_path}? Return only the new contents of {file_path} and no other information.",
            ]
        )
        cont = self.generate(full_system, prompt).content
        util.spit(file_path, util.strip_md_code(cont))
        if after_save is not None:
            after_save(file_path)

    def edit_directory(
        self,
        in_dir,
        prompt,
        after_save=None,
        out_dir=None,
        ignore_regex=None,
        retries=5,
    ):
        base = "You are an extremely experienced and knowledgeable programmer. A genie in human form, able to bend source code to your will in ways your peers can only marvel at."
        in_dir = os.path.expanduser(in_dir)
        if out_dir is None:
            out_dir = in_dir
        else:
            out_dir = os.path.expanduser(out_dir)

        if ignore_regex is None:
            ignore_regex = r"(^__pycache__|^node_modules|^env|^venv|^\..*|~$|\.pyc$|Thumbs\.db$|^build[\\/]|^dist[\\/]|^coverage[\\/]|\.log$|\.lock$|\.bak$|\.swp$|\.swo$|\.tmp$|\.temp$|\.class$|^target$|^Cargo\.lock$)"
        elif not ignore_regex:
            ignore_regex is None

        print(in_dir)
        project_tree = util.tree(in_dir, ignore_regex)
        files_list = self.generate_checked(
            util.mk_local_files(in_dir),
            "\n".join(
                [
                    base,
                    f"The directory tree of the directory you've been asked to work on is {project_tree}. What files does the users' query require you to consider? Return a JSON-formatted list of relative pathname strings and no other content.",
                ]
            ),
            prompt,
        ).content
        print(f"   Considering {files_list}")
        files = {fl: util.slurp(os.path.join(in_dir, fl)) for fl in files_list}

        change_files_list = self.generate_checked(
            util.mk_local_files(in_dir, must_exist=False),
            "\n".join(
                [
                    base,
                    f"The project tree of the project you've been asked to work on is {project_tree}.",
                    f"You've decided that these are the files you needed to consider: {files}",
                    "What files does the users' query require you to make changes to? Return a JSON-formatted list of relative pathnames and no other content",
                ]
            ),
            prompt,
        ).content

        print(f"   Changing {change_files_list}")
        for pth in change_files_list:
            self.edit_file(
                os.path.join(out_dir, pth),
                "\n".join(
                    [
                        base,
                        f"The project tree of the project you've been asked to work on is {project_tree}.",
                        f"You've decided that these are the files you needed to consider: {files}",
                    ]
                ),
                prompt,
                after_save=after_save,
            )
```

Firstly...

```
...
    def edit_file(
        self, file_path, system, prompt, after_save=None, consider_current=True
    ):
        full_system = "\n".join(
            [
                system,
                f"The current contents of {file_path} is {util.slurp(file_path)}"
                if (os.path.isfile(file_path) and consider_current)
                else f"The file {file_path} currently doesn't exist.",
                f"What changes would you make to the file {file_path}? Return only the new contents of {file_path} and no other information.",
            ]
        )
        cont = self.generate(full_system, prompt).content
        util.spit(file_path, util.strip_md_code(cont))
        if after_save is not None:
            after_save(file_path)
...
```
we can edit a file by specifying the path, giving it a system prompt and prompt, and providing an `after_save` hook. We can also have it consider the current state of the file by passing the appropriate parameter. If the file exists, and `consider_current` is truthy, we `slurp` the path and add it to the system prompt, otherwise we add a note that the file doesn't currently exist. We run the `generate` routine, `spit` the resulting file (after trimming `markdown` block markers), and then run the `after_save` hook if any.

The point of `after_save` is, for example, to be able to run a [linter or formatter](https://github.com/psf/black) on the resulting output.

Secondly...

```
...
    def edit_directory(
        self,
        in_dir,
        prompt,
        after_save=None,
        out_dir=None,
        ignore_regex=None,
        retries=5,
    ):
        base = "You are an extremely experienced and knowledgeable programmer. A genie in human form, able to bend source code to your will in ways your peers can only marvel at."
        in_dir = os.path.expanduser(in_dir)
        if out_dir is None:
            out_dir = in_dir
        else:
            out_dir = os.path.expanduser(out_dir)

        if ignore_regex is None:
            ignore_regex = r"(^__pycache__|^node_modules|^env|^venv|^\..*|~$|\.pyc$|Thumbs\.db$|^build[\\/]|^dist[\\/]|^coverage[\\/]|\.log$|\.lock$|\.bak$|\.swp$|\.swo$|\.tmp$|\.temp$|\.class$|^target$|^Cargo\.lock$)"
        elif not ignore_regex:
            ignore_regex is None

        print(in_dir)
        project_tree = util.tree(in_dir, ignore_regex)
        files_list = self.generate_checked(
            util.mk_local_files(in_dir),
            "\n".join(
                [
                    base,
                    f"The directory tree of the directory you've been asked to work on is {project_tree}. What files does the users' query require you to consider? Return a JSON-formatted list of relative pathname strings and no other content.",
                ]
            ),
            prompt,
        ).content
        print(f"   Considering {files_list}")
        files = {fl: util.slurp(os.path.join(in_dir, fl)) for fl in files_list}

        change_files_list = self.generate_checked(
            util.mk_local_files(in_dir, must_exist=False),
            "\n".join(
                [
                    base,
                    f"The project tree of the project you've been asked to work on is {project_tree}.",
                    f"You've decided that these are the files you needed to consider: {files}",
                    "What files does the users' query require you to make changes to? Return a JSON-formatted list of relative pathnames and no other content",
                ]
            ),
            prompt,
        ).content

        print(f"   Changing {change_files_list}")
        for pth in change_files_list:
            self.edit_file(
                os.path.join(out_dir, pth),
                "\n".join(
                    [
                        base,
                        f"The project tree of the project you've been asked to work on is {project_tree}.",
                        f"You've decided that these are the files you needed to consider: {files}",
                    ]
                ),
                prompt,
                after_save=after_save,
            )
...
```

... we can edit a directory in more or less the same way. We pass a directory and a prompt. No system prompt here; we're going to construct that from the directory, and we can optionally also specify a `retries`, `ignore_regex`, separate `out_dir` and `after_save` to pass to any child `edit_file` calls.

Once we've got the inputs, the process is

1. Feed the model a prompt bigging it up as a good programmer, and containing the directory tree of the input directory, ask it to decide which files it needs to look at in order to make the changes
2. Feed the model a similar prompt, but this time including the contents of the files it mentioned last time, and ask it to decide which files it needs to edit in order to make the changes
3. For each returned path, call `change_files_list` with the appropriate seystem and user prompts.

The result ends up being surprisingly good? Like there's a few cleanup tasks I've done with this and I can generally use the results as a PR without too much editing. I _haven't_ tried it on huge codebases, pervasive changes, or any non-mainstream languages yet (just python, javascript and typescript), but so far it's going pretty well.

## Next Steps

This is step one. Or I guess if you count [my local editor bindings](https://github.com/inaimathi/machine-setup/blob/master/emacs/aidev.el), step two? The end goal is a machine that can make _most_ of the edits that I'd want it to make pretty autonomously given a system spec. I think I'm within striking distance of putting together something that produces working output in most situations that takes minimal cleanup from me. The cursory experimentation I've been running so far tells me that as long as you keep the prompts sufficiently on-point, you get decent results. You can't load it up with a _huge_ codebase and expect it to go well, but you can do reasonably well. I think there's a few more clever things I can do to reduce scope down to just the bits that are necessary for the current edit and then go through it piece-wise, but given that we're still not clear of the performance asymptote, maybe I just say "fuck it" and wait for effective context window size to grow a bit more? Maybe I start coding in a way that emphasizes small modules even more than I have been already? There's definitely solutions to hand, I just need to decide which one I want and tune.

I'm _much less_ hopeful about non-coding tasks. It's been relatively simple to have this system generate working Python and Clojure, but it consistently fails to impress me when I try to get it to write a blog post in my "voice" from notes. I'm definitely not posting machine-generated prose here without telling you in advance that it's what I'm doing, but at current levels, I think you'd spot it even if I didn't.

As always, I'll let you know how it goes, and I'll probably have a few demos of what I'm doing with it.
