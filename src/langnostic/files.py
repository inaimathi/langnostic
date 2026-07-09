import functools
import os
import re

import mistune

RESOURCES = os.path.realpath(os.path.abspath("./resources"))


def isFileIn(file_path: str, base_path: str) -> bool:
    file_canonical = os.path.realpath(os.path.abspath(file_path))
    base_canonical = os.path.realpath(os.path.abspath(base_path))
    return file_canonical.startswith(base_canonical)


def isFileInResources(file_path: str) -> bool:
    """Check if a file is inside the resources directory."""
    return isFileIn(file_path, RESOURCES)


class LinkableHeaderRenderer(mistune.HTMLRenderer):
    def heading(self, text: str, level: int, **attrs) -> str:
        """Add linkable anchors to headings."""
        name = re.sub(r"\W+", "-", text.lower())
        return f'<h{level}><a name="{name}"></a><a href="#{name}">{text}</a></h{level}>'


_markdown_parser = mistune.create_markdown(
    renderer=LinkableHeaderRenderer(), plugins=["footnotes"]
)


@functools.lru_cache(maxsize=None)
def file_content(file_path: str) -> str:
    try:
        with open(file_path, "r", encoding="utf-8") as f:
            content = f.read()
            return _markdown_parser(content)
    except Exception as e:
        print(f"Error reading file {file_path}: {e}")
        return f"<p>Error reading file: {file_path}</p>"
