from datetime import datetime
from typing import Any, Dict, List

import pyccup.core

from . import posts


def post_href(post: Dict[str, Any]) -> str:
    """Generate the URL for a post."""
    return f"/posts/{post.get('file', '')}"


def post_audio_href(post: Dict[str, Any]) -> str:
    """Generate the URL for post audio."""
    return f"/static/audio/{post.get('file', '')}.ogg"


def post_links(post: Dict[str, Any]) -> List:
    """Generate navigation links to previous and next posts."""
    all_posts = posts.all_posts()
    post_id = post.get("id", 0)

    prev_id = post_id - 1
    prev_post = next((p for p in all_posts if p.get("id") == prev_id), None)
    next_id = post_id + 1
    next_post = next((p for p in all_posts if p.get("id") == next_id), None)

    return [
        "div",
        {"class": "post-nav"},
        (
            [
                "a",
                {"class": "prev-post", "href": post_href(prev_post)},
                "<-",
                prev_post.get("title", ""),
            ]
            if prev_post
            else None
        ),
        (
            [
                "a",
                {"class": "next-post", "href": post_href(next_post)},
                next_post.get("title", ""),
                "->",
            ]
            if next_post
            else None
        ),
    ]


# SVG audio icon definition
audio_icon = [
    "svg",
    {
        "class": "audio-icon",
        "viewBox": "0 0 209.50009 184.00235",
        "xmlns": "http://www.w3.org/2000/svg",
    },
    [
        "path",
        {
            "d": "M 178.7,30.202356 A 103.1,103.1 0 0 0 105.5,0.0023558 h -0.8 A 104,104 0 0 0 2.6681619e-8,104.00236 v 56 A 24.1,24.1 0 0 0 24,184.00236 h 16 a 24.1,24.1 0 0 0 24,-24 v -40 A 24.1,24.1 0 0 0 40,96.002356 H 16.4 a 87.8,87.8 0 0 1 88.3,-80 h 0.1 a 88,88 0 0 1 88.3,80 h -23.6 a 24,24 0 0 0 -24,24.000004 v 40 a 24,24 0 0 0 24,24 h 16 a 24.1,24.1 0 0 0 24,-24 v -56 A 103.5,103.5 0 0 0 178.7,30.202356 Z"
        },
    ],
]


def post(post_data: Dict[str, Any]) -> List:
    """Generate hiccup structure for a post."""
    import os

    # Format the date
    posted_date = post_data.get("posted", datetime.now())
    formatted_date = posted_date.strftime("%a %b %d, %Y")

    audio_path = f"resources/public/audio/{post_data.get('file', '')}.ogg"

    return [
        "div",
        ["h1", ["a", {"href": post_href(post_data)}, post_data.get("title", "")]],
        ["span", {"class": "posted"}, formatted_date],
        (
            [
                "a",
                {
                    "class": "post-audio",
                    "href": post_audio_href(post_data),
                    "target": "blank",
                },
                audio_icon,
                "Listen to this post",
            ]
            if os.path.exists(audio_path)
            else None
        ),
        posts.post_content(post_data),
        post_links(post_data),
    ]


def latest_post() -> str:
    """Return HTML for the latest post."""
    all_posts = posts.all_posts()
    if not all_posts:
        return "<p>No posts available</p>"

    # Get the latest post (assuming it's the last in the list)
    latest = sorted(all_posts, key=lambda p: p.get("id", 0))[-1]

    return pyccup.core.html(post(latest))


def archive(post_list: List[Dict[str, Any]]) -> List:
    """Generate an archive view of posts grouped by year."""
    print(f"POST LIST: {post_list}")
    archive_div = ["div"]

    # Group posts by year and sort in reverse order
    posts_by_year = {}
    for p in post_list:
        year = p.get("posted", datetime.now()).year
        if year not in posts_by_year:
            posts_by_year[year] = []
        posts_by_year[year].append(p)

    # Add year groups
    for year in sorted(posts_by_year.keys(), reverse=True):
        year_span = ["span"]
        year_span.append(["h2", str(year)])

        # Create list of posts for this year
        post_ul = ["ul"]
        for p in sorted(
            posts_by_year[year], key=lambda p: p.get("id", 0), reverse=True
        ):
            post_item = [
                "li",
                (
                    [
                        "a",
                        {
                            "class": "audio-link",
                            "href": post_audio_href(p),
                            "target": "blank",
                        },
                        audio_icon,
                    ]
                    if p.get("audio?")
                    else None
                ),
                ["a", {"href": post_href(p)}, p.get("title", "")],
            ]

            post_ul.append(post_item)

        year_span.append(post_ul)
        archive_div.append(year_span)

    # Add tags section
    archive_div.append(["h3", "Tags"])

    # Count frequencies of tags
    tag_counts = {}
    for p in post_list:
        for tag in p.get("tags", []):
            tag_counts[tag] = tag_counts.get(tag, 0) + 1

    # Create tag list
    tags_ul = ["ul", {"class": "tags-list"}]
    for tag, count in sorted(tag_counts.items()):
        tags_ul.append(
            ["li", ["a", {"href": f"/archive/by-tag/{tag}"}, tag], f"({count})"]
        )

    archive_div.append(tags_ul)

    return archive_div


def nav_bar(section: str) -> List:
    """Generate the navigation bar."""
    nav_items = ["blog", "archive", "links", "meta", "tipjar", "feed"]

    menu_container = [
        "div",
        {"class": "top-menu-container"},
        ["ul", {"class": "top-menu"}],
    ]

    for name in nav_items:
        if name == section:
            menu_container[2].append(["li", name])
        else:
            menu_container[2].append(["li", ["a", {"href": f"/{name}"}, name]])

    return menu_container


# Footer with license information
footer = [
    "div",
    {"class": "license"},
    [
        "a",
        {"rel": "license", "href": "http://creativecommons.org/licenses/by-sa/3.0/"},
        [
            "img",
            {
                "alt": "Creative Commons License",
                "style": "border-width:0;float: left; margin: 0px 15px 15px 0px;",
                "src": "https://i.creativecommons.org/l/by-sa/3.0/88x31.png",
            },
        ],
    ],
    [
        "p",
        [
            "span",
            {"xmlns:dct": "https://purl.org/dc/terms/", "property": "dct:title"},
            "all articles at langnostic",
        ],
        " are licensed under a ",
        [
            "a",
            {
                "rel": "license",
                "href": "https://creativecommons.org/licenses/by-sa/3.0/",
            },
            "Creative Commons Attribution-ShareAlike 3.0 Unported License",
        ],
    ],
    [
        "p",
        "Reprint, rehost and distribute freely (even for profit), but attribute the work and allow your readers the same freedoms. ",
        [
            "a",
            {
                "href": "https://creativecommons.org/choose/results-one?license_code=by-sa&amp;jurisdiction=&amp;version=3.0&amp;lang=en&amp;field_format=&amp;field_worktitle=this+langnostic+article&amp;field_attribute_to_name=Inaimathi&amp;field_attribute_to_url=http%3A%2F%2Flangnostic.inaimathi.com&amp;field_sourceurl=http%3A%2F%2Flangnostic.inaimathi.com&amp;field_morepermissionsurl=&amp;lang=en_US&amp;n_questions=3"
            },
            "Here's",
        ],
        " a license widget you can use.",
    ],
    [
        "p",
        "The menu background image is ",
        [
            "a",
            {"href": "https://www.flickr.com/photos/danzen/2360096926/in/photostream/"},
            "Jewel Wash",
        ],
        ", taken from ",
        ["a", {"href": "https://www.flickr.com/photos/danzen/"}, "Dan Zen's"],
        " flickr stream and released under a ",
        [
            "a",
            {"href": "https://creativecommons.org/licenses/by/2.0/"},
            "CC-BY license",
        ],
    ],
]


def stylesheet(url: str) -> List:
    """Generate a stylesheet link."""
    return [
        "link",
        {"rel": "stylesheet", "href": url, "type": "text/css", "media": "screen"},
    ]


def template(section: str, page_title: str, content: Any) -> str:
    """Generate a complete HTML page."""
    # Convert content to string if it's already HTML
    if isinstance(content, str):
        content_html = content
    else:
        # Otherwise render it as hiccup
        content_html = pyccup.core.html(content)

    html5_doc = [
        "html",
        {"lang": "en"},
        [
            "head",
            ["title", f"{page_title} - langnostic"],
            [
                "link",
                {
                    "href": "/feed/atom",
                    "type": "application/atom+xml",
                    "rel": "alternate",
                    "title": "Site-wide Langnostic Atom Feed",
                },
            ],
            stylesheet("/static/css/langnostic.css"),
            stylesheet("/static/css/default.css"),
            [
                "script",
                {"type": "text/javascript", "src": "/static/js/highlight.pack.js"},
            ],
            [
                "script",
                {"type": "text/javascript"},
                "hljs.initHighlightingOnLoad();",
            ],
        ],
        [
            "body",
            [
                "a",
                {"href": "/"},
                ["img", {"class": "logo-bar", "src": "/static/img/langnostic.png"}],
            ],
            nav_bar(section),
            ["div", {"class": "content"}, content_html],
            ["hr"],
            footer,
            [
                "script",
                {"type": "text/javascript"},
                "hljs.initHighlighting();",
            ],
        ],
    ]

    return "<!DOCTYPE html>" + pyccup.core.html(html5_doc)
