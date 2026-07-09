import json
import os
import re
import shutil
import time
from collections import Counter
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Union

RESOURCES_DIR = Path("resources")
POSTS_JSON = RESOURCES_DIR / "posts.json"
POSTS_DIR = RESOURCES_DIR / "posts"
DRAFTS_DIR = Path("drafts")


Pathish = Union[str, os.PathLike]


def _read_json_lines(path: Path = POSTS_JSON) -> List[Dict]:
    with path.open("r", encoding="utf-8") as f:
        return [json.loads(line) for line in f if line.strip()]


def next_id() -> int:
    """Return one greater than the id of the last post in resources/posts.json."""
    records = _read_json_lines(POSTS_JSON)
    return int(records[-1]["id"]) + 1


def all_tags() -> Dict[str, int]:
    """Return tag frequencies from resources/posts.json, sorted by tag."""
    tag_counts = Counter()

    for raw in _read_json_lines(POSTS_JSON):
        post = {k.lower(): v for k, v in raw.items()}
        tag_counts.update(post.get("tags", []))

    return dict(sorted(tag_counts.items()))


def slug_from_file(file: Pathish) -> str:
    """Return the filename without a trailing .md extension."""
    name = Path(file).name
    return re.sub(r"\.md$", "", name)


def title_from_file(file: Pathish) -> str:
    """Generate a title from a markdown filename slug."""
    small_words = {"and", "of"}

    words = []
    for word in slug_from_file(file).split("-"):
        if word in small_words:
            words.append(word)
        else:
            words.append(word.capitalize())

    return " ".join(words)


def drafts_by_prefix(prefix: str) -> List[Path]:
    """
    Return draft files whose filename starts with prefix,
    excluding editor backup files ending in ~.
    """
    if not DRAFTS_DIR.exists():
        return []

    return [
        path
        for path in DRAFTS_DIR.rglob("*")
        if path.is_file()
        and path.name.startswith(prefix)
        and not path.name.endswith("~")
    ]


def new(
    file_prefix: str,
    *,
    title: Optional[str] = None,
    tags: Optional[Iterable[str]] = None,
) -> Optional[Dict]:
    """
    Publish exactly one matching draft.

    Copies the draft into resources/posts/, deletes the draft, and appends a
    JSON-lines record to resources/posts.json.

    Example:
        scratch.new("my-post-prefix", tags=["python", "blog"])
        scratch.new("my-post-prefix", title="Custom Title", tags=["meta"])
    """
    drafts = drafts_by_prefix(file_prefix)

    if len(drafts) != 1:
        print("MULTIPLE DRAFTS FOUND", drafts)
        return None

    file = drafts[0]
    slug = slug_from_file(file)
    post_title = title or title_from_file(file)

    record = {
        "id": next_id(),
        "title": post_title,
        "file": slug,
        "posted": int(time.time()),
        "tags": list(tags or []),
    }

    POSTS_DIR.mkdir(parents=True, exist_ok=True)

    destination = POSTS_DIR / file.name
    shutil.copy2(file, destination)
    file.unlink()

    with POSTS_JSON.open("a", encoding="utf-8") as f:
        f.write(json.dumps(record, separators=(",", ":")) + "\n")

    return record
