# EPIC-lab Presentations

Static hosting for EPIC-lab research presentations and course slides via GitHub Pages.

## Branches

- **`main`**: source files only (index.html, assets, archive.json, this README). Lightweight.
- **`gh-pages`**: deployed content. Force-pushed on each deploy so no history accumulates. GitHub Pages serves from this branch.

## Structure (on gh-pages)

```
epic-presentations/
├── index.html              # presentations index
├── archive.json            # metadata for archived presentations
├── assets/                 # shared CSS
├── 2026/
│   └── psyc-434-w01/       # course slides by week
│       └── index.html      # self-contained revealjs HTML
├── 2025/
│   └── apa-bier/           # research presentation
│       └── index.html
└── .nojekyll
```

## Course slides workflow (PSYC 434)

QMD sources live in the course repo (`26-434`). Rendered slides are deployed here. The render script handles everything:

```bash
# from the 26-434 repo
./scripts/render-slides.sh /path/to/NN-slides.qmd NN
```

This command:
1. Renders the QMD to self-contained revealjs HTML with the catppuccin mocha theme
2. Switches this repo to the `gh-pages` branch
3. Copies the rendered HTML to `2026/psyc-434-wNN/index.html`
4. Force-pushes `gh-pages` (no history accumulation)
5. Switches back to `main`

Slides are then live at `https://go-bayes.github.io/epic-presentations/2026/psyc-434-wNN/`.

### Theme

The revealjs theme is `26-434/theme/catppuccin-mocha-revealjs.scss`, adapted from `GIT/templates/css/catppuccin-mocha.css` with SCSS layer boundaries added for Quarto compatibility.

## Adding a research presentation

1. Switch to `gh-pages`: `git checkout gh-pages`
2. Create a folder: `YYYY/presentation-name/`
3. Add your self-contained HTML as `index.html`
4. Update the root `index.html` to include a card for the presentation
5. Commit and force-push: `git push --force origin gh-pages`
6. Switch back: `git checkout main`

## Archiving policy

After 6 months, presentations can be moved to Dropbox to save space:

1. Upload the presentation HTML to Dropbox
2. Add an entry to `archive.json`
3. Remove from the `gh-pages` branch
4. The index page loads archived entries from `archive.json` automatically

## Local preview

Open any `index.html` in a browser. No server required.
