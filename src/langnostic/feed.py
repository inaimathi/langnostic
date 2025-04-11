from typing import Any, Dict, List
from xml.sax.saxutils import escape

import pyccup.core

from . import posts


def atom_feed(post_list: List[Dict[str, Any]]) -> str:
    """Generate an Atom feed from the given posts."""

    def format_entry(post):
        return [
            "entry",
            ["title", post.get("title", "")],
            ["updated", post.get("posted", "")],
            [
                "link",
                {
                    "href": f"http://langnostic.inaimathi.ca/posts/{post.get('file', '')}"
                },
            ],
            ["author", ["name", "inaimathi"]],
            ["content", {"type": "html"}, escape(posts.post_content(post))],
        ]

    # Take the latest 20 posts, sorted by posted date in reverse
    sorted_posts = sorted(post_list, key=lambda p: p.get("posted", ""), reverse=True)[
        :20
    ]
    entries = [format_entry(post) for post in sorted_posts]

    feed_tree = [
        ["title", "Language Agnostic"],
        ["subtitle", "Langnostic Atom Feed"],
        ["link", {"href": "http://langnostic.inaimathi.ca/feed/atom", "rel": "self"}],
        ["link", {"href": "http://langnostic.inaimathi.ca"}],
    ]
    feed_tree.extend(entries)

    return pyccup.core.xml(feed_tree, "feed")
