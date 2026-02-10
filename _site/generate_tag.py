#!/usr/bin/env python

"""
tag_generator.py
Copyright 2017 Long Qian
Contact: lqian8@jhu.edu
This script creates tags for your Jekyll blog hosted by Github page.
No plugins required.
"""

import glob
import os
import re
from typing import List

POST_DIR = "_posts/"
TAG_DIR = "tag/"
FRONT_MATTER_DELIM = "---"


def _normalize_tag(token: str) -> str:
    return token.strip().strip("'\"")


def _split_inline_tags(raw_value: str) -> List[str]:
    value = raw_value.strip()
    if not value:
        return []
    if value.startswith("[") and value.endswith("]"):
        value = value[1:-1]
        return [_normalize_tag(part) for part in value.split(",") if _normalize_tag(part)]
    value = value.replace(",", " ")
    return [_normalize_tag(part) for part in value.split() if _normalize_tag(part)]


def extract_front_matter_tags(filename: str) -> List[str]:
    with open(filename, "r", encoding="utf-8") as f:
        lines = f.readlines()

    if not lines or lines[0].strip() != FRONT_MATTER_DELIM:
        return []

    front_matter: List[str] = []
    for line in lines[1:]:
        if line.strip() == FRONT_MATTER_DELIM:
            break
        front_matter.append(line.rstrip("\n"))

    tags: List[str] = []
    in_yaml_list = False
    for line in front_matter:
        tags_match = re.match(r"^\s*tags\s*:\s*(.*)$", line)
        if tags_match:
            in_yaml_list = True
            tags.extend(_split_inline_tags(tags_match.group(1)))
            continue

        if in_yaml_list:
            item_match = re.match(r"^\s*-\s*(.+)$", line)
            if item_match:
                tag = _normalize_tag(item_match.group(1))
                if tag:
                    tags.append(tag)
                continue
            if line.strip() and not line.startswith(" "):
                in_yaml_list = False

    return tags


def main() -> None:
    filenames = glob.glob(os.path.join(POST_DIR, "*.md"))
    total_tags = sorted(set(tag for path in filenames for tag in extract_front_matter_tags(path)))

    os.makedirs(TAG_DIR, exist_ok=True)
    for tag_file in glob.glob(os.path.join(TAG_DIR, "*.md")):
        os.remove(tag_file)

    for tag in total_tags:
        tag_filename = os.path.join(TAG_DIR, f"{tag}.md")
        with open(tag_filename, "w", encoding="utf-8") as f:
            f.write(
                "---\n"
                "layout: tagpage\n"
                f'title: "Tag: {tag}"\n'
                f"tag: {tag}\n"
                "robots: noindex\n"
                "---\n"
            )

    print("Tags generated, count", len(total_tags))


if __name__ == "__main__":
    main()
