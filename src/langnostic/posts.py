import json
import os
from datetime import datetime
from typing import Any, Dict, List, Optional

from . import files

POSTS = []


def parsePost(old_posts_map: Dict[str, Any], line: str) -> Dict[str, Any]:
    """Parse a JSON line into a post object with proper type conversions."""
    try:
        raw = json.loads(line)

        # Convert keys to lowercase
        raw = {k.lower(): v for k, v in raw.items()}

        # Check if audio file exists
        audio_path = f"resources/public/audio/{raw.get('file', '')}.ogg"
        has_audio = os.path.exists(audio_path)

        # Convert timestamp to datetime
        posted_time = datetime.fromtimestamp(raw.get("posted", 0))

        # Create post object
        post = {
            **{k: v for k, v in raw.items() if k != "edited"},
            "posted": posted_time,
            "tags": set(raw.get("tags", [])),
            "audio?": has_audio,
        }

        return post
    except Exception as e:
        print(f"Error parsing post: {e}")
        return {}


def load_posts() -> None:
    """Load posts from the posts.json file."""
    global POSTS

    old_posts_map = {post.get("id"): post for post in POSTS}

    try:
        with open("resources/posts.json", "r", encoding="utf-8") as f:
            new_posts = []

            for line in f:
                if line.strip():
                    post = parsePost(old_posts_map, line)

                    if not post:
                        continue

                    if not os.path.exists(post_file_path(post)):
                        print(
                            f"Skipping missing post file for slug: {post.get('file')}"
                        )
                        continue

                    new_posts.append(post)

            POSTS = new_posts

    except Exception as e:
        print(f"Error loading posts: {e}")


def all_posts() -> List[Dict[str, Any]]:
    """Return all posts."""
    return POSTS


def find_by_slug(slug: str) -> Optional[Dict[str, Any]]:
    """Find a post by its slug/file name."""
    for post in POSTS:
        if post.get("file") == slug:
            return post
    return None


def find_by_tag(tag: str) -> List[Dict[str, Any]]:
    """Find all posts with the given tag."""
    return [post for post in POSTS if tag in post.get("tags", [])]


def post_file_path(post: Dict[str, Any]) -> str:
    return f"resources/posts/{post.get('file', '')}.md"


def post_content(post: Dict[str, Any]) -> str:
    """Get post content, loading from file if necessary."""
    file_path = post_file_path(post)
    return files.file_content(file_path)
