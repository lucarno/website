# Lucas M. Novaes — Academic Website (Quarto)

## Quick Start

### 1. Install Quarto
Download from: https://quarto.org/docs/get-started/
(If you use RStudio ≥ 2022.07, Quarto is already included.)

### 2. Add your files
- Place your **photo** at `files/photo.jpg`
- Place your **CV PDF** at `files/cv.pdf`

### 3. Preview locally
```bash
cd lucasmnovaes
quarto preview
```
This opens your site at http://localhost:4200 with live reload.

### 4. Edit content
All pages are `.qmd` files (Markdown). Just edit them in any text editor, VS Code, or RStudio.

- `index.qmd` → Home page (bio, photo, links)
- `research.qmd` → Publications and working papers
- `cv.qmd` → CV download
- `teaching.qmd` → Course list
- `_quarto.yml` → Site configuration (navbar, theme, title)
- `custom.css` → Visual tweaks

### 5. Deploy to GitHub Pages (free)

#### First time setup:
```bash
# Install git if you don't have it
# https://git-scm.com/downloads

# Create a GitHub account if you don't have one
# https://github.com

# Create a new repository on GitHub (e.g., "lucasmnovaes.github.io" or "website")
# Do NOT initialize with README

# In your project folder:
git init
git add .
git commit -m "Initial website"
git remote add origin https://github.com/YOUR_USERNAME/YOUR_REPO.git
git push -u origin main

# Publish to GitHub Pages:
quarto publish gh-pages
```

#### Subsequent updates:
```bash
# After editing any .qmd file:
quarto publish gh-pages
```
That's it. One command to update your site.

### 6. Connect your custom domain (lucasmnovaes.com)

1. In your GitHub repo: **Settings → Pages → Custom domain** → type `lucasmnovaes.com`
2. At your domain registrar (wherever you bought lucasmnovaes.com), update DNS:
   - If using an **apex domain** (lucasmnovaes.com):
     Add A records pointing to:
     ```
     185.199.108.153
     185.199.109.153
     185.199.110.153
     185.199.111.153
     ```
   - If using **www.lucasmnovaes.com**:
     Add a CNAME record: `www` → `YOUR_USERNAME.github.io`
3. Back in GitHub Pages settings, check **Enforce HTTPS**
4. DNS propagation takes 5 min to 48 hours (usually under 1 hour).

### 7. Cancel Weebly
Once your new site is live and the domain is pointing to GitHub, you can cancel your Weebly subscription.

---

## Optional: Auto-generate publications from BibTeX

If you maintain a `.bib` file, you can have Quarto auto-render your publication list.

1. Place your `references.bib` in the project root.
2. In `research.qmd`, add to the YAML header:
```yaml
---
title: "Research"
bibliography: references.bib
nocite: '@*'
csl: apa.csl
---
```
3. Download an APA (or other) CSL file from https://www.zotero.org/styles and place it in your project.

This will automatically format all entries in your .bib file on the research page.

---

## Customization

### Change theme
In `_quarto.yml`, replace `cosmo` with any Bootswatch theme:
`cosmo`, `flatly`, `lux`, `simplex`, `litera`, `minty`, `journal`, `sandstone`

Preview them at: https://bootswatch.com

### Change about page layout
In `index.qmd`, replace `trestles` with: `jolla`, `solana`, `marquee`, or `broadside`.

Preview them at: https://quarto.org/docs/websites/website-about.html
