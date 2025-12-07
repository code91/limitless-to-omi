# Limitless to Omi Migration Tool

Migrate your Limitless AI pendant lifelogs to [Omi](https://omi.me) after Meta's acquisition shut down service in the EU.

> **Background**: On December 5, 2025, Meta acquired Limitless and immediately terminated service for EU users. This tool helps you preserve your data by importing it into Omi, an open-source alternative.

## Features

- Recursively processes all markdown files from your Limitless export
- Preserves timestamps and speaker diarization
- Rate-limited to respect Omi's API limits
- Dry-run mode to preview before importing
- Date filtering to import specific time ranges
- Detailed progress and error reporting

## Prerequisites

- Python 3.8+
- An [Omi](https://omi.me) account with the Omi app installed
- Your Limitless data export (download before December 19, 2025!)

## Installation

1. **Clone this repository**
   ```bash
   git clone https://github.com/code91/limitless-to-omi.git
   cd limitless-to-omi
   ```

2. **Install dependencies**
   ```bash
   pip install -r requirements.txt
   ```

3. **Get your Omi Developer API Key**
   - Open the Omi app on your phone
   - Go to **Settings** → **Developer**
   - Under "Developer API Keys", tap **Create Key**
   - Copy the key (starts with `omi_dev_`)

4. **Set your API key as an environment variable**
   ```bash
   # Linux/macOS
   export OMI_API_KEY='omi_dev_your_key_here'

   # Windows (PowerShell)
   $env:OMI_API_KEY='omi_dev_your_key_here'

   # Windows (CMD)
   set OMI_API_KEY=omi_dev_your_key_here
   ```

## Exporting Your Limitless Data

1. Go to [my.limitless.ai](https://my.limitless.ai)
2. Navigate to Settings → Export Data
3. Download your data (deadline: **December 19, 2025**)
4. Extract the ZIP file - you'll have a `lifelogs/` folder with structure:
   ```
   lifelogs/
   ├── 2024/
   │   ├── 01/
   │   │   ├── 15/
   │   │   │   ├── 2024-01-15_09h30m00s_Meeting-with-team.md
   │   │   │   └── 2024-01-15_14h00m00s_Call-with-client.md
   │   │   └── ...
   │   └── ...
   └── 2025/
       └── ...
   ```

## Usage

### Preview (Dry Run)

First, run in dry-run mode to see what would be imported:

```bash
python limitless_to_omi.py /path/to/lifelogs
```

### Import All Data

Once you're satisfied with the preview, import with `--chronological` to import oldest conversations first (recommended for correct relative ordering):

```bash
python limitless_to_omi.py /path/to/lifelogs --execute --chronological
```

### Import with Options

```bash
# Import only first 10 files (for testing)
python limitless_to_omi.py /path/to/lifelogs --execute --chronological --limit 10

# Import only files from 2025
python limitless_to_omi.py /path/to/lifelogs --execute --chronological --start-date 2025-01-01

# Import files from a specific date range
python limitless_to_omi.py /path/to/lifelogs --execute --chronological --start-date 2025-01-01 --end-date 2025-06-30

# Verbose output with detailed progress
python limitless_to_omi.py /path/to/lifelogs --execute --verbose

# Save results to a JSON file
python limitless_to_omi.py /path/to/lifelogs --execute --output results.json
```

### Command Line Options

| Option | Description |
|--------|-------------|
| `lifelogs_dir` | Path to your Limitless lifelogs directory (required) |
| `--execute` | Actually import to Omi (default is dry-run) |
| `--chronological` | Import oldest files first (recommended for correct ordering) |
| `--limit N` | Process only the first N files |
| `--start-date YYYY-MM-DD` | Only import files from this date onwards |
| `--end-date YYYY-MM-DD` | Only import files up to this date |
| `--verbose`, `-v` | Show detailed progress |
| `--output`, `-o FILE` | Save results to JSON file |

## How It Works

1. **Scans** your lifelogs directory recursively for `.md` files
2. **Parses** each file to extract:
   - Timestamp from filename (e.g., `2025-05-08_05h46m33s_Title.md`)
   - Start/end times from content timestamps
   - Speaker-attributed transcript
3. **Imports** each conversation to Omi via the Developer API
4. **Rate limits** requests to stay within Omi's limits (60/minute)

## Limitless File Format

The tool expects Limitless markdown files with this structure:

```markdown
# Meeting Title

> [0](#startMs=1715150793000&endMs=1715150800000): Hello, welcome to the meeting.
> [1](#startMs=1715150801000&endMs=1715150810000): Thanks for having me.
```

Where:
- `[0]`, `[1]` are speaker IDs
- `startMs` and `endMs` are timestamps in milliseconds

## Known Limitations

### Conversation Sort Order in Omi App

The Omi API preserves your original conversation timestamps in `started_at` and `finished_at` fields. However, Omi sets `created_at` to the import time (not overridable via API).

**What this means:**
- Your imported conversations will appear at the **top** of your conversation list (sorted by import date)
- The original timestamps are preserved in the conversation details
- This is a limitation of the Omi API, not this tool

**Workaround:** If this is important to you, consider:
1. Importing conversations in chronological order (oldest first) so newer ones appear at top
2. Contacting Omi support to request a `created_at` override for import scenarios

## Troubleshooting

### "OMI_API_KEY environment variable is not set"

Make sure you've exported the API key in your current terminal session:
```bash
export OMI_API_KEY='omi_dev_your_key_here'
```

### "Rate limited" errors

The script automatically rate-limits requests, but if you see this error:
- Wait a few minutes before retrying
- Use `--limit` to process fewer files at a time

### Empty transcripts being skipped

Files with no extractable dialogue are skipped. This is normal for files that only contain metadata or headers.

### API connection errors

- Check your internet connection
- Verify your API key is correct
- Try again later (Omi servers may be temporarily unavailable)

## Data Privacy

- Your data is sent directly to Omi's API
- No data is stored or transmitted anywhere else
- Your API key is only used locally and never logged

## Contributing

Contributions welcome! Please open an issue or PR.

## License

MIT License - see [LICENSE](LICENSE) file.

## Acknowledgments

- [Omi](https://omi.me) for providing an open alternative
- The Limitless community for sharing export format details

---

**Note**: This tool is not affiliated with Limitless, Meta, or Omi. Use at your own risk.
