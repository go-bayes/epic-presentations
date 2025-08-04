# EPIC-lab Presentations

Static hosting for EPIC-lab research presentations. Fast, lightweight, no build process.

## Structure

```
epic-presentations/
├── index.html          # Main presentations index
├── archive.json        # Metadata for archived presentations
├── assets/            # Shared CSS and resources
├── 2025/              # Current year presentations
│   └── apa-bier/      # Each presentation in its own folder
│       └── index.html # Self-contained HTML presentation
└── 2024/              # Previous years...
```

## Adding a New Presentation

1. Create a folder: `YYYY/presentation-name/`
2. Add your HTML file as `index.html`
3. Update the main `index.html` to include your presentation
4. Commit and push - deploys automatically via GitHub Pages

## Archiving Policy

After 6 months, presentations are moved to Dropbox to conserve space:

1. Upload presentation to Dropbox
2. Add entry to `archive.json`
3. Remove from GitHub repository
4. The index page will automatically show the Dropbox link

## Deployment

This repository uses GitHub Pages:
- Push to main branch → Live in minutes
- No build process required
- Direct HTML serving for speed

## Size Management

- Keep active presentations under 100MB each
- Archive older presentations to Dropbox
- Monitor repository size (aim to stay under 1GB total)

## Local Preview

Simply open `index.html` in a browser. No server required.