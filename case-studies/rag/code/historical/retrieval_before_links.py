"""Lexical child search and parent-file retrieval for the workshop corpus."""
from collections.abc import Sequence
from dataclasses import dataclass
import math
import re


@dataclass(frozen=True)
class Chunk:
    source: str
    text: str

    @property
    def document(self) -> str:
        return self.source.rsplit(":L", 1)[0]


@dataclass(frozen=True)
class ParentDocument:
    document: str
    chunks: tuple[Chunk, ...]


@dataclass(frozen=True)
class RetrievalResult:
    child_hits: list[tuple[float, Chunk]]
    parent_documents: list[ParentDocument]


STOP_WORDS = set("a an the is are was were be to of for in on at it i can how what when do does and or my me".split())
# One-way search hints for this workshop corpus, not factual equivalences.
# Targets may cover several candidate passages (e.g. tool and laser bookings).
# Match whole tokens; keep ambiguous words such as "driver" out on their own.
QUERY_ALIASES: dict[str, str] = {
    # Tools and borrowing.
    "cordless driver": "drill",
    "cordless drivers": "drill",
    "power drill": "drill",
    "drills": "drill",
    "ladder": "ladders",
    "sanders": "sander",
    "take home": "borrow loan",
    "check out": "borrow loan",
    "checkout": "borrow loan",
    "borrowing": "borrow borrowed loan",
    "renew": "renewed",
    "renewal": "renewed",
    "extend": "renewed",
    "reservation": "reserved bookings",
    "reservations": "reserved bookings",
    "book": "bookings reserved",
    "booking": "bookings",
    # Access and opening times. Weekday names point to the weekday-hours rule.
    "opening hours": "open",
    "opening times": "open",
    "weekdays": "monday friday",
    "tuesday": "monday friday",
    "wednesday": "monday friday",
    "thursday": "monday friday",
    "saturday": "weekends closed",
    "sunday": "weekends closed",
    "orientation": "induction",
    "introductory session": "induction",
    "entry": "entry induction",
    # Laser operation and supervision.
    "laser cutting": "laser cutter",
    "qualified": "trained training",
    "certified": "trained training",
    "supervisor": "steward",
    "staff": "steward",
    "supervision": "steward present unsupervised",
    "supervised": "steward present",
    "alone": "unsupervised",
    # Inspection notices: retrieve both accounts, without resolving the conflict.
    "inspection": "inspected",
    "inspect": "inspected",
    "checks": "inspected",
    "safety check": "inspected",
    "maintenance": "sander inspected",
}


def words(text: str) -> set[str]:
    return set(re.findall(r"[^\W_]+", text.lower())) - STOP_WORDS


def query_words(question: str) -> set[str]:
    """Add alias targets for retrieval; preserve the original question/evidence."""
    tokens = re.findall(r"[^\W_]+", question.lower())
    terms = words(question)
    for phrase, target in QUERY_ALIASES.items():
        phrase_tokens = phrase.split()
        size = len(phrase_tokens)
        if any(tokens[i:i + size] == phrase_tokens
               for i in range(len(tokens) - size + 1)):
            terms.update(words(target))
    return terms


def retrieve_chunks(
    question: str, chunks: Sequence[Chunk], top_k: int = 3,
) -> list[tuple[float, Chunk]]:
    """Rank individual paragraphs; scores do not apply to their parents."""
    if top_k < 1:
        raise ValueError("top-k must be positive")
    query = query_words(question)
    scored: list[tuple[float, Chunk]] = []
    for chunk in chunks:
        terms = words(chunk.text)
        overlap = len(query & terms)
        if overlap:
            score = overlap / math.sqrt(len(query) * len(terms))
            scored.append((score, chunk))
    return sorted(scored, key=lambda hit: (-hit[0], hit[1].source))[:top_k]


def expand_parents(
    hits: Sequence[tuple[float, Chunk]], chunks: Sequence[Chunk],
) -> list[ParentDocument]:
    """Deduplicate files in hit order; keep their paragraphs in corpus order."""
    documents: dict[str, list[Chunk]] = {}
    for chunk in chunks:
        documents.setdefault(chunk.document, []).append(chunk)
    parents: list[ParentDocument] = []
    seen: set[str] = set()
    for _, chunk in hits:
        if chunk.document in seen:
            continue
        seen.add(chunk.document)
        if chunk.document not in documents:
            raise ValueError(f"hit document absent from corpus: {chunk.document}")
        parents.append(ParentDocument(chunk.document, tuple(documents[chunk.document])))
    return parents


def retrieve(
    question: str, chunks: Sequence[Chunk], top_k: int = 3,
) -> RetrievalResult:
    """Return scored child hits and complete parents, before context budgeting."""
    hits = retrieve_chunks(question, chunks, top_k)
    return RetrievalResult(hits, expand_parents(hits, chunks))
