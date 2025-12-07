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
from pathlib import Path
from datetime import datetime, timezone
from typing import Optional, Tuple, List, Dict, Any

# Configuration via environment variables
OMI_API_KEY = os.environ.get("OMI_API_KEY", "")
OMI_API_BASE = "https://api.omi.me/v1/dev"

# Rate limiting (Omi allows 100 requests/minute)
REQUESTS_PER_MINUTE = 60
DELAY_BETWEEN_REQUESTS = 60 / REQUESTS_PER_MINUTE


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


def ms_to_iso(ms: int) -> str:
    """Convert milliseconds timestamp to ISO 8601 format with Z suffix."""
    dt = datetime.fromtimestamp(ms / 1000, tz=timezone.utc)
    return dt.strftime('%Y-%m-%dT%H:%M:%SZ')


def create_omi_conversation(
    text: str,
    started_at: Optional[str] = None,
    finished_at: Optional[str] = None,
    language: str = "en",
    source_file: str = ""
) -> Dict[str, Any]:
    """
    Create a conversation in Omi via the Developer API.
    
    Args:
        text: The transcript text
        started_at: ISO 8601 timestamp when conversation started
        finished_at: ISO 8601 timestamp when conversation ended
        language: Language code (default: "en")
        source_file: Original filename for reference
    
    Returns:
        API response dict or error dict
    """
    if not OMI_API_KEY:
        return {"error": "OMI_API_KEY environment variable not set"}
    
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
            return {"error": "Rate limited. Please wait and try again."}
        
        response.raise_for_status()
        return response.json()
        
    except requests.exceptions.Timeout:
        return {"error": "Request timed out"}
    except requests.exceptions.RequestException as e:
        return {"error": str(e)}


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
    
    # Truncate if too long (Omi limit is 100,000 chars)
    if len(transcript) > 100000:
        transcript = transcript[:100000]
    
    return {
        "file": str(filepath),
        "title": title,
        "started_at": started_at,
        "finished_at": finished_at,
        "transcript_length": len(transcript),
        "transcript": transcript
    }


def find_markdown_files(directory: Path) -> List[Path]:
    """
    Recursively find all markdown files in directory.
    
    Returns:
        Sorted list of Path objects
    """
    return sorted(directory.rglob('*.md'))


def import_lifelogs(
    lifelogs_dir: str,
    dry_run: bool = True,
    limit: Optional[int] = None,
    start_date: Optional[str] = None,
    end_date: Optional[str] = None,
    chronological: bool = False,
    verbose: bool = False
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
    
    print(f"Found {len(md_files)} markdown files")
    
    results = {
        "total_found": len(md_files),
        "processed": 0,
        "imported": 0,
        "skipped": 0,
        "errors": 0,
        "details": []
    }
    
    for filepath in md_files:
        # Check limit
        if limit and results["processed"] >= limit:
            break
        
        # Date filtering based on filename
        if start_date or end_date:
            file_dt, _ = parse_filename(filepath.name)
            if file_dt:
                file_date = file_dt.strftime('%Y-%m-%d')
                if start_date and file_date < start_date:
                    continue
                if end_date and file_date > end_date:
                    continue
        
        results["processed"] += 1
        
        # Process file
        processed = process_file(filepath)
        
        if processed.get("error"):
            results["errors"] += 1
            print(f"[{results['processed']}] ERROR: {filepath.name}")
            if verbose:
                print(f"    {processed['error']}")
            results["details"].append(processed)
            continue
        
        if processed.get("skipped"):
            results["skipped"] += 1
            if verbose:
                print(f"[{results['processed']}] SKIP: {filepath.name} ({processed.get('reason')})")
            results["details"].append(processed)
            continue
        
        print(f"[{results['processed']}] {filepath.name} ({processed['transcript_length']} chars)")
        
        # Import to Omi
        if not dry_run:
            api_result = create_omi_conversation(
                text=processed["transcript"],
                started_at=processed.get("started_at"),
                finished_at=processed.get("finished_at"),
                language="en",
                source_file=filepath.name
            )
            
            if api_result.get("error"):
                results["errors"] += 1
                processed["api_error"] = api_result["error"]
                print(f"    API ERROR: {api_result['error']}")
            else:
                results["imported"] += 1
                processed["conversation_id"] = api_result.get("id")
                if verbose:
                    print(f"    Created: {api_result.get('id')}")
            
            # Rate limiting
            time.sleep(DELAY_BETWEEN_REQUESTS)
        else:
            # Dry run - count as would-be-imported
            results["imported"] += 1
        
        # Don't store full transcript in results (too large)
        processed.pop("transcript", None)
        results["details"].append(processed)
    
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
        verbose=args.verbose
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
    print(f"Total files found:    {results['total_found']}")
    print(f"Files processed:      {results['processed']}")
    print(f"Successfully imported:{results['imported']}" + (" (would import)" if dry_run else ""))
    print(f"Skipped (empty):      {results['skipped']}")
    print(f"Errors:               {results['errors']}")
    
    # Save results
    if args.output:
        with open(args.output, 'w', encoding='utf-8') as f:
            json.dump(results, f, indent=2, ensure_ascii=False)
        print(f"\nResults saved to: {args.output}")
    
    # Exit code
    sys.exit(0 if results['errors'] == 0 else 1)


if __name__ == '__main__':
    main()
