#!/usr/bin/env python3
"""
Limitless to Omi Importer

Imports Limitless lifelog markdown files into Omi as conversations.
https://github.com/[your-username]/limitless-to-omi
"""

import os
import re
import json
import requests
import argparse
import time
import sys
import threading
from pathlib import Path
from datetime import datetime, timezone
from typing import Optional, Tuple, List, Dict, Any
from concurrent.futures import ThreadPoolExecutor, as_completed

# Configuration via environment variables
OMI_API_KEY = os.environ.get("OMI_API_KEY", "")
OMI_API_BASE = "https://api.omi.me/v1/dev"

# Rate limiting (Omi allows 100 requests/minute)
OMI_REQUESTS_PER_MINUTE = 100
OMI_MIN_DELAY = 60.0 / OMI_REQUESTS_PER_MINUTE  # 0.6 seconds between requests

# Conversation limits
OMI_MAX_CHARS = 90000  # Split before 100k limit for safety margin

# Default parallel workers
DEFAULT_WORKERS = 3


class RateLimiter:
    """Thread-safe rate limiter for API requests."""

    def __init__(self, min_delay: float):
        self._lock = threading.Lock()
        self._last_request_time = 0.0
        self._min_delay = min_delay

    def wait(self):
        """Wait if necessary to respect rate limits."""
        with self._lock:
            now = time.time()
            elapsed = now - self._last_request_time
            if elapsed < self._min_delay:
                time.sleep(self._min_delay - elapsed)
            self._last_request_time = time.time()


# Global rate limiter instance
_rate_limiter = RateLimiter(OMI_MIN_DELAY)


def parse_filename(filename: str) -> Tuple[Optional[datetime], str]:
    """
    Parse Limitless filename to extract datetime and title.
    
    Expected format: 2025-05-08_05h46m33s_Title-slug.md
    
    Returns:
        Tuple of (datetime or None, title string)
    """
    pattern = r'^(\d{4}-\d{2}-\d{2})_(\d{2})h(\d{2})m(\d{2})s_(.+)\.md$'
    match = re.match(pattern, filename)
    
    if not match:
        # Fallback: try to extract title from filename
        title = filename.replace('.md', '').replace('-', ' ').replace('_', ' ')
        return None, title
    
    date_str, hour, minute, second, title_slug = match.groups()
    
    try:
        dt = datetime.strptime(f"{date_str} {hour}:{minute}:{second}", "%Y-%m-%d %H:%M:%S")
        dt = dt.replace(tzinfo=timezone.utc)
    except ValueError:
        return None, title_slug.replace('-', ' ')
    
    title = title_slug.replace('-', ' ')
    return dt, title


def extract_timestamps_from_content(content: str) -> Tuple[Optional[int], Optional[int]]:
    """
    Extract the earliest startMs and latest endMs from markdown content.
    
    Limitless uses format: > [speaker_id](#startMs=1234567890&endMs=1234567899): text
    
    Returns:
        Tuple of (start_ms or None, end_ms or None)
    """
    pattern = r'startMs=(\d+)&endMs=(\d+)'
    matches = re.findall(pattern, content)
    
    if not matches:
        return None, None
    
    start_times = [int(m[0]) for m in matches]
    end_times = [int(m[1]) for m in matches]
    
    return min(start_times), max(end_times)


def extract_transcript(content: str) -> str:
    """
    Extract clean transcript text from Limitless markdown content.
    
    Removes headers and formatting, keeps dialogue with speaker labels.
    """
    lines = content.split('\n')
    transcript_lines = []
    
    for line in lines:
        # Skip empty lines and headers
        if not line.strip() or line.startswith('#'):
            continue
        
        # Process dialogue lines
        # Format: > [speaker_id](#startMs=...&endMs=...): text
        if line.startswith('> ['):
            match = re.match(r'> \[(\d+)\]\([^)]+\):\s*(.+)', line)
            if match:
                speaker_id, text = match.groups()
                transcript_lines.append(f"[Speaker {speaker_id}]: {text}")
            else:
                # Fallback: clean up the line
                clean = re.sub(r'> \[\d+\]\([^)]+\):\s*', '', line)
                if clean:
                    transcript_lines.append(clean)
        elif line.startswith('>'):
            # Continuation line
            clean = line.lstrip('> ').strip()
            if clean:
                transcript_lines.append(clean)
    
    return '\n'.join(transcript_lines)


def split_transcript(transcript: str, max_chars: int = OMI_MAX_CHARS) -> List[str]:
    """
    Split a large transcript into chunks that fit within Omi's limits.

    Splits at line boundaries to preserve conversation flow.

    Args:
        transcript: The full transcript text
        max_chars: Maximum characters per chunk (default: 90000)

    Returns:
        List of transcript chunks
    """
    if len(transcript) <= max_chars:
        return [transcript]

    chunks = []
    lines = transcript.split('\n')
    current_chunk = []
    current_length = 0

    for line in lines:
        line_length = len(line) + 1  # +1 for newline

        # If single line exceeds max, split it
        if line_length > max_chars:
            # Save current chunk first
            if current_chunk:
                chunks.append('\n'.join(current_chunk))
                current_chunk = []
                current_length = 0

            # Split long line by sentences or at max_chars
            remaining = line
            while len(remaining) > max_chars:
                # Try to split at sentence boundary
                split_point = remaining[:max_chars].rfind('. ')
                if split_point < max_chars // 2:
                    # No good sentence boundary, split at space
                    split_point = remaining[:max_chars].rfind(' ')
                if split_point < max_chars // 2:
                    # No good space, hard split
                    split_point = max_chars - 1

                chunks.append(remaining[:split_point + 1])
                remaining = remaining[split_point + 1:].lstrip()

            if remaining:
                current_chunk = [remaining]
                current_length = len(remaining)
            continue

        # Check if adding this line would exceed limit
        if current_length + line_length > max_chars:
            # Save current chunk and start new one
            chunks.append('\n'.join(current_chunk))
            current_chunk = [line]
            current_length = line_length
        else:
            current_chunk.append(line)
            current_length += line_length

    # Don't forget the last chunk
    if current_chunk:
        chunks.append('\n'.join(current_chunk))

    return chunks


def ms_to_iso(ms: int) -> str:
    """Convert milliseconds timestamp to ISO 8601 format with Z suffix."""
    dt = datetime.fromtimestamp(ms / 1000, tz=timezone.utc)
    return dt.strftime('%Y-%m-%dT%H:%M:%SZ')


def create_omi_conversation(
    text: str,
    started_at: Optional[str] = None,
    finished_at: Optional[str] = None,
    language: str = "en",
    source_file: str = "",
    part_info: Optional[Tuple[int, int]] = None
) -> Dict[str, Any]:
    """
    Create a conversation in Omi via the Developer API.

    Args:
        text: The transcript text
        started_at: ISO 8601 timestamp when conversation started
        finished_at: ISO 8601 timestamp when conversation ended
        language: Language code (default: "en")
        source_file: Original filename for reference
        part_info: Optional tuple of (part_number, total_parts) for split conversations

    Returns:
        API response dict or error dict
    """
    if not OMI_API_KEY:
        return {"error": "OMI_API_KEY environment variable not set"}

    # Thread-safe rate limiting
    _rate_limiter.wait()

    payload = {
        "text": text,
        "text_source": "other_text",
        "text_source_spec": "limitless_import",
        "language": language
    }

    if started_at:
        payload["started_at"] = started_at
    if finished_at:
        payload["finished_at"] = finished_at

    headers = {
        "Authorization": f"Bearer {OMI_API_KEY}",
        "Content-Type": "application/json"
    }

    url = f"{OMI_API_BASE}/user/conversations"

    try:
        response = requests.post(url, json=payload, headers=headers, timeout=30)

        if response.status_code == 429:
            # Back off and retry once on rate limit
            time.sleep(2)
            _rate_limiter.wait()
            response = requests.post(url, json=payload, headers=headers, timeout=30)
            if response.status_code == 429:
                return {"error": "Rate limited (429). Please wait and try again."}

        # Capture detailed error info for non-success responses
        if not response.ok:
            error_detail = f"HTTP {response.status_code}"
            try:
                error_body = response.json()
                if isinstance(error_body, dict):
                    # Handle common API error formats
                    detail = error_body.get("detail") or error_body.get("message") or error_body.get("error")
                    if detail:
                        error_detail = f"HTTP {response.status_code}: {detail}"
                    else:
                        error_detail = f"HTTP {response.status_code}: {error_body}"
                else:
                    error_detail = f"HTTP {response.status_code}: {error_body}"
            except (ValueError, json.JSONDecodeError):
                # Response isn't JSON, use text
                if response.text:
                    error_detail = f"HTTP {response.status_code}: {response.text[:200]}"
            return {"error": error_detail, "status_code": response.status_code}

        return response.json()

    except requests.exceptions.Timeout:
        return {"error": "Request timed out (30s)"}
    except requests.exceptions.ConnectionError as e:
        return {"error": f"Connection error: {e}"}
    except requests.exceptions.RequestException as e:
        return {"error": f"Request failed: {e}"}


def process_file(filepath: Path) -> Dict[str, Any]:
    """
    Process a single Limitless lifelog markdown file.
    
    Returns:
        Dict with processing results
    """
    filename = filepath.name
    
    # Parse filename
    file_dt, title = parse_filename(filename)
    
    # Read content
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            content = f.read()
    except Exception as e:
        return {
            "file": str(filepath),
            "error": f"Failed to read file: {e}"
        }
    
    # Extract timestamps from content
    start_ms, end_ms = extract_timestamps_from_content(content)
    
    # Convert to ISO format
    started_at = ms_to_iso(start_ms) if start_ms else None
    finished_at = ms_to_iso(end_ms) if end_ms else None
    
    # Fallback to filename datetime if no content timestamps
    if not started_at and file_dt:
        started_at = file_dt.strftime('%Y-%m-%dT%H:%M:%S+00:00')
    
    # Extract transcript
    transcript = extract_transcript(content)

    if not transcript.strip():
        return {
            "file": str(filepath),
            "skipped": True,
            "reason": "empty transcript"
        }

    # Split large transcripts into chunks
    chunks = split_transcript(transcript)

    return {
        "file": str(filepath),
        "title": title,
        "started_at": started_at,
        "finished_at": finished_at,
        "transcript_length": len(transcript),
        "transcript_chunks": chunks,
        "num_parts": len(chunks)
    }


def find_markdown_files(directory: Path) -> List[Path]:
    """
    Recursively find all markdown files in directory.
    
    Returns:
        Sorted list of Path objects
    """
    return sorted(directory.rglob('*.md'))


def _import_single_file(
    processed: Dict[str, Any],
    dry_run: bool,
    verbose: bool
) -> Dict[str, Any]:
    """
    Import a single processed file to Omi. Used by parallel executor.

    Args:
        processed: Processed file dict with transcript_chunks
        dry_run: If True, don't actually create conversations
        verbose: Print detailed progress

    Returns:
        Updated processed dict with results
    """
    if dry_run:
        processed["status"] = "would_import"
        processed["conversations_created"] = processed.get("num_parts", 1)
        return processed

    chunks = processed.get("transcript_chunks", [])
    num_parts = len(chunks)
    conversation_ids = []
    errors = []

    for i, chunk in enumerate(chunks):
        part_info = (i + 1, num_parts) if num_parts > 1 else None

        api_result = create_omi_conversation(
            text=chunk,
            started_at=processed.get("started_at"),
            finished_at=processed.get("finished_at"),
            language="en",
            source_file=processed.get("file", ""),
            part_info=part_info
        )

        if api_result.get("error"):
            errors.append(f"Part {i+1}: {api_result['error']}")
        else:
            conversation_ids.append(api_result.get("id"))

    # Update processed dict with results
    processed["conversation_ids"] = conversation_ids
    processed["conversations_created"] = len(conversation_ids)

    if errors:
        processed["api_errors"] = errors
        if conversation_ids:
            processed["status"] = "partial"
        else:
            processed["status"] = "failed"
    else:
        processed["status"] = "success"

    return processed


def print_progress(current: int, total: int, status: str = "", width: int = 40):
    """Print a progress bar to stderr."""
    percent = current / total if total > 0 else 0
    filled = int(width * percent)
    bar = "█" * filled + "░" * (width - filled)
    sys.stderr.write(f"\r  Progress: |{bar}| {current}/{total} {status}")
    sys.stderr.flush()


def import_lifelogs(
    lifelogs_dir: str,
    dry_run: bool = True,
    limit: Optional[int] = None,
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    chronological: bool = False,
    verbose: bool = False,
    workers: int = DEFAULT_WORKERS
) -> Dict[str, Any]:
    """
    Import all lifelogs from the directory structure.

    Args:
        lifelogs_dir: Path to the lifelogs folder
        dry_run: If True, don't actually create conversations
        limit: Maximum number of files to process
        start_date: Only process files from this date onwards (YYYY-MM-DD)
        end_date: Only process files up to this date (YYYY-MM-DD)
        chronological: Import oldest files first (recommended)
        verbose: Print detailed progress
        workers: Number of parallel workers (default: 3)

    Returns:
        Summary dict with results
    """
    lifelogs_path = Path(lifelogs_dir)

    if not lifelogs_path.exists():
        return {"error": f"Directory not found: {lifelogs_dir}"}

    md_files = find_markdown_files(lifelogs_path)

    # Reverse for chronological order (oldest first)
    if chronological:
        md_files = list(reversed(md_files))

    # Apply date filtering
    filtered_files = []
    for filepath in md_files:
        if start_date or end_date:
            file_dt, _ = parse_filename(filepath.name)
            if file_dt:
                file_date = file_dt.strftime('%Y-%m-%d')
                if start_date and file_date < start_date:
                    continue
                if end_date and file_date > end_date:
                    continue
        filtered_files.append(filepath)

    # Apply limit
    if limit:
        filtered_files = filtered_files[:limit]

    print(f"Found {len(md_files)} markdown files, {len(filtered_files)} after filtering")

    results = {
        "total_found": len(md_files),
        "files_to_process": len(filtered_files),
        "processed": 0,
        "imported": 0,
        "conversations_created": 0,
        "skipped": 0,
        "errors": 0,
        "details": []
    }

    if not filtered_files:
        return results

    # Phase 1: Process all files (extract transcripts, split if needed)
    print("\nPhase 1: Processing files...")
    processed_files = []
    skipped_count = 0
    error_count = 0
    total_conversations = 0

    for i, filepath in enumerate(filtered_files):
        if verbose:
            print(f"  [{i+1}/{len(filtered_files)}] {filepath.name}")

        processed = process_file(filepath)

        if processed.get("error"):
            error_count += 1
            if verbose:
                print(f"    ERROR: {processed['error']}")
            results["details"].append(processed)
            continue

        if processed.get("skipped"):
            skipped_count += 1
            if verbose:
                print(f"    SKIP: {processed.get('reason')}")
            results["details"].append(processed)
            continue

        processed_files.append(processed)
        total_conversations += processed.get("num_parts", 1)

        if processed.get("num_parts", 1) > 1:
            print(f"  [{i+1}] {filepath.name} ({processed['transcript_length']} chars) -> {processed['num_parts']} parts")
        elif not verbose:
            print_progress(i + 1, len(filtered_files))

    if not verbose:
        print()  # Newline after progress bar

    results["processed"] = len(filtered_files)
    results["skipped"] = skipped_count
    results["errors"] = error_count

    print(f"\n  Files to import: {len(processed_files)}")
    print(f"  Total conversations to create: {total_conversations}")
    if skipped_count:
        print(f"  Skipped (empty): {skipped_count}")
    if error_count:
        print(f"  Errors: {error_count}")

    if not processed_files:
        return results

    # Estimate time
    if not dry_run:
        est_seconds = total_conversations * OMI_MIN_DELAY
        print(f"  Estimated time: {est_seconds/60:.1f} minutes (with {workers} workers)")

    # Phase 2: Import to Omi (parallel)
    print(f"\nPhase 2: {'Simulating import (dry run)' if dry_run else f'Importing to Omi ({workers} workers)'}...")

    completed = 0
    success_count = 0
    partial_count = 0
    fail_count = 0
    total_convos_created = 0
    error_messages = {}  # Track error types for summary
    failed_files = []  # Track failed files for detailed output
    start_time = time.time()

    # Use ThreadPoolExecutor for parallel imports
    with ThreadPoolExecutor(max_workers=workers) as executor:
        # Submit all tasks
        future_to_file = {
            executor.submit(_import_single_file, pf, dry_run, verbose): pf
            for pf in processed_files
        }

        # Process completed tasks
        for future in as_completed(future_to_file):
            completed += 1
            result = future.result()

            status = result.get("status", "unknown")
            convos = result.get("conversations_created", 0)
            total_convos_created += convos

            if status == "success" or status == "would_import":
                success_count += 1
                status_char = "✓" if convos == 1 else f"✓({convos})"
            elif status == "partial":
                partial_count += 1
                status_char = f"~({convos})"
            else:
                fail_count += 1
                status_char = "✗"
                # Collect error info
                api_errors = result.get("api_errors", [])
                if api_errors:
                    for err in api_errors:
                        # Extract error type for grouping
                        error_messages[err] = error_messages.get(err, 0) + 1
                    failed_files.append({
                        "file": Path(result.get("file", "")).name,
                        "errors": api_errors
                    })

            # Don't store full transcript chunks in results
            result.pop("transcript_chunks", None)
            results["details"].append(result)

            title = Path(result.get("file", "")).name[:30]
            print_progress(completed, len(processed_files), f"{status_char} {title}")

    print()  # Newline after progress bar

    # Print error summary if there were failures
    if error_messages and not dry_run:
        print("\n  Error Summary:")
        # Group and show unique errors with counts
        for error_msg, count in sorted(error_messages.items(), key=lambda x: -x[1]):
            # Truncate long error messages
            display_msg = error_msg[:80] + "..." if len(error_msg) > 80 else error_msg
            print(f"    [{count}x] {display_msg}")

        # Show first few failed files if verbose
        if verbose and failed_files:
            print("\n  Failed files (first 10):")
            for ff in failed_files[:10]:
                print(f"    - {ff['file']}")
                for err in ff['errors'][:2]:
                    print(f"        {err}")

    elapsed = time.time() - start_time
    results["imported"] = success_count + partial_count
    results["conversations_created"] = total_convos_created
    results["errors"] += fail_count
    results["elapsed_seconds"] = elapsed
    results["error_summary"] = error_messages

    return results


def main():
    parser = argparse.ArgumentParser(
        description='Import Limitless lifelogs to Omi',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Preview what would be imported (dry run)
  python limitless_to_omi.py /path/to/lifelogs

  # Import all files (oldest first - recommended)
  python limitless_to_omi.py /path/to/lifelogs --execute --chronological

  # Import only first 10 files
  python limitless_to_omi.py /path/to/lifelogs --execute --limit 10

  # Import only files from a specific date range
  python limitless_to_omi.py /path/to/lifelogs --execute --start-date 2025-01-01 --end-date 2025-06-30

  # Import with more parallel workers (faster but more API load)
  python limitless_to_omi.py /path/to/lifelogs --execute --workers 5

Environment:
  OMI_API_KEY    Your Omi Developer API key (required)
                 Get it from: Omi App > Settings > Developer > Create Key
        """
    )
    
    parser.add_argument('lifelogs_dir', help='Path to Limitless lifelogs directory')
    parser.add_argument('--execute', action='store_true',
                        help='Actually import to Omi (default is dry-run)')
    parser.add_argument('--chronological', action='store_true',
                        help='Import oldest files first (recommended for correct relative ordering)')
    parser.add_argument('--limit', type=int,
                        help='Maximum number of files to process')
    parser.add_argument('--start-date',
                        help='Only import files from this date onwards (YYYY-MM-DD)')
    parser.add_argument('--end-date',
                        help='Only import files up to this date (YYYY-MM-DD)')
    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Show detailed progress')
    parser.add_argument('--workers', '-w', type=int, default=DEFAULT_WORKERS,
                        help=f'Number of parallel workers (default: {DEFAULT_WORKERS})')
    parser.add_argument('--output', '-o',
                        help='Save results to JSON file')

    args = parser.parse_args()
    
    # Check API key
    if not OMI_API_KEY:
        print("ERROR: OMI_API_KEY environment variable is not set.")
        print()
        print("To get your API key:")
        print("1. Open the Omi app")
        print("2. Go to Settings > Developer")
        print("3. Under 'Developer API Keys', click 'Create Key'")
        print("4. Copy the key and set it:")
        print()
        print("   export OMI_API_KEY='omi_dev_your_key_here'")
        print()
        sys.exit(1)
    
    dry_run = not args.execute
    
    # Header
    print("=" * 60)
    if dry_run:
        print("DRY RUN MODE - No data will be imported")
        print("Use --execute to actually import to Omi")
    else:
        print("EXECUTE MODE - Data will be imported to Omi")
    print("=" * 60)
    print()
    
    # Confirmation for execute mode
    if not dry_run:
        confirm = input("Type 'yes' to confirm import: ")
        if confirm.lower() != 'yes':
            print("Aborted.")
            sys.exit(0)
        print()
    
    # Run import
    results = import_lifelogs(
        lifelogs_dir=args.lifelogs_dir,
        dry_run=dry_run,
        limit=args.limit,
        start_date=args.start_date,
        end_date=args.end_date,
        chronological=args.chronological,
        verbose=args.verbose,
        workers=args.workers
    )
    
    # Check for error
    if results.get("error"):
        print(f"ERROR: {results['error']}")
        sys.exit(1)
    
    # Summary
    print()
    print("=" * 60)
    print("SUMMARY")
    print("=" * 60)
    print(f"Total files found:      {results['total_found']}")
    print(f"Files processed:        {results['processed']}")
    print(f"Files imported:         {results['imported']}" + (" (would import)" if dry_run else ""))
    print(f"Conversations created:  {results.get('conversations_created', results['imported'])}" + (" (would create)" if dry_run else ""))
    print(f"Skipped (empty):        {results['skipped']}")
    print(f"Errors:                 {results['errors']}")
    if results.get('elapsed_seconds'):
        elapsed = results['elapsed_seconds']
        print(f"Time elapsed:           {elapsed/60:.1f} minutes ({elapsed:.1f}s)")
    
    # Save results
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        print(f"\nResults saved to: {args.output}")
    
    # Exit code
    sys.exit(0 if results['errors'] == 0 else 1)


if __name__ == '__main__':
    main()
