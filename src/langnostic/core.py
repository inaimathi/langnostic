import asyncio
import os
from os import environ as ENV

import tornado.web
from pycronado.core import PublicJSONHandler, getLogger, start
from watchdog.events import FileSystemEventHandler
from watchdog.observers import Observer

# Import the modules we're assuming exist
from . import feed, files, pages, posts

logger = getLogger("langnostic.core")


class Error404Handler(PublicJSONHandler):
    def get(self):
        self.set_status(404)
        self.set_header("Content-Type", "text/html")
        self.write(
            pages.template(
                "404", "404", files.file_content("resources/public/content/404.md")
            )
        )


class StaticPageHandler(tornado.web.RequestHandler):
    def initialize(self, name):
        self.name = name

    def get(self):
        file = os.path.join("resources/public/content", f"{self.name}.md")

        if files.isFileInResources(file):
            self.set_status(200)
            self.set_header("Content-Type", "text/html")
            self.write(
                pages.template(file, self.name.capitalize(), files.file_content(file))
            )
        else:
            self.set_status(404)
            self.set_header("Content-Type", "text/html")
            self.write(
                pages.template(
                    "404", "404", files.file_content("resources/public/content/404.md")
                )
            )


class PostHandler(tornado.web.RequestHandler):
    def initialize(self, name=None):
        self.name = name

    def get(self, name=None):
        # Use name from path parameter if provided
        slug = name if name else self.name
        post = posts.find_by_slug(slug)

        if post:
            self.set_status(200)
            self.set_header("Content-Type", "text/html")
            self.write(pages.template("blog", post.get("title"), pages.post(post)))
        else:
            self.set_status(404)
            self.set_header("Content-Type", "text/html")
            self.write(
                pages.template(
                    "404", "404", files.file_content("resources/public/content/404.md")
                )
            )


class HomeHandler(tornado.web.RequestHandler):
    def get(self):
        self.set_status(200)
        self.set_header("Content-Type", "text/html")
        self.write(
            pages.template(
                "blog",
                "Welcome",
                f"<div>{files.file_content('resources/public/content/intro.md')}<hr>{pages.latest_post()}</div>",
            )
        )


class ArchiveHandler(tornado.web.RequestHandler):
    def get(self, tag=None):
        if tag is None:
            post_list = posts.all_posts()
        else:
            post_list = posts.find_by_tag(tag)
        self.set_status(200)
        self.set_header("Content-Type", "text/html")
        self.write(pages.template("archive", "Archive", pages.archive(post_list)))


class AtomFeedHandler(tornado.web.RequestHandler):
    def initialize(self, post_list=None):
        self.post_list = post_list if post_list else posts.all_posts()

    def get(self):
        self.set_status(200)
        self.set_header("Content-Type", "application/atom+xml")
        self.write(feed.atom_feed(self.post_list))


class PostsWatcher(FileSystemEventHandler):
    def reload_if_relevant(self, event):
        if event.is_directory:
            return

        path = event.src_path
        relevant = (
            path == "resources/posts.json"
            or path.startswith("resources/posts/")
            or path.startswith("resources/public/audio/")
        )

        if relevant:
            logger.info(f"Reloading posts after FS change: {path}")
            posts.load_posts()
            files.file_content.cache_clear()
            logger.info("posts reloaded")

    def on_created(self, event):
        self.reload_if_relevant(event)

    def on_modified(self, event):
        self.reload_if_relevant(event)

    def on_deleted(self, event):
        self.reload_if_relevant(event)

    def on_moved(self, event):
        self.reload_if_relevant(event)


ROUTES = [
    (r"/", HomeHandler),
    (r"/blog", HomeHandler),
    (r"/posts/([^/]+)", PostHandler, {}),
    (r"/archive", ArchiveHandler),
    (r"/archive/by-tag/([^/]+)", ArchiveHandler),
    (r"/links", StaticPageHandler, {"name": "links"}),
    (r"/tipjar", StaticPageHandler, {"name": "tipjar"}),
    (r"/meta", StaticPageHandler, {"name": "meta"}),
    (r"/feed", AtomFeedHandler, {"post_list": posts.all_posts()}),
    (r"/feed/atom", AtomFeedHandler, {"post_list": posts.all_posts()}),
    (
        r"/feed/atom/([^/]+)",
        AtomFeedHandler,
        lambda path, **kwargs: {"post_list": posts.find_by_tag(kwargs["tag"])},
    ),
    (
        r"/feed/atom/by-tag/([^/]+)",
        AtomFeedHandler,
        lambda path, **kwargs: {"post_list": posts.find_by_tag(kwargs["tag"])},
    ),
    (r"/static/(.*)", tornado.web.StaticFileHandler, {"path": "resources/public"}),
]


async def main(port=8000):
    print("Loading posts...")
    posts.load_posts()

    print("Watching FS resources...")
    observer = Observer()
    handler = PostsWatcher()
    observer.schedule(handler, "resources/", recursive=True)
    observer.start()

    print(f"Listening on port {port}...")
    await start(
        "langnostic",
        port,
        ROUTES,
        static_path="resources/public",
        static_url_prefix="/static/",
        default_handler_class=Error404Handler,
        debug=True,
    )


def run_server(port=8000):
    asyncio.run(main(port))


if __name__ == "__main__":
    import sys

    port = int(ENV.get("PORT", sys.argv[1] if len(sys.argv) > 1 else 8000))
    run_server(port)
