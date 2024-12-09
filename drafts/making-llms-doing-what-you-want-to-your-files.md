I got some feedback on `trivialai` from a friend and made some changes.

## The `define` method is now optionally a decorator

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

The new definition of the define method is 

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

There's exactly notable thing about this.

## Transform functions are a bit more pythonic

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

Ok, I mentioned that the updated version of `define` above has something interesting about it. The interesting thing is that I generated it by doing

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
