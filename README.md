# JSON-Driven Layout Optimizer

## Overview
This project implements a layout optimizer that:
- Parses layout specifications from JSON.
- Validates constraints and components.
- Optimizes placement of elements into a 12-column grid.
- Generates both a textual summary and an HTML export.
- Supports multiple CSS styles (`minimal`, `classic`, `modern`).

The project is implemented in **Haskell** and built with `cabal`.

---

## Usage

### Build the project
```bash
cabal build
```

### Run with a JSON input file
```bash
cabal run project -- layout_with_images.json
```

### Command-line options
- `--out FILE.html` — export optimized layout to an HTML file.
- `--css style.css` — choose stylesheet for the HTML export.

Example:
```bash
cabal run project -- layout_complex.json --out complex.html --css style-modern.css
```

---

## Input format
Example JSON:
```json
{
  "config": { "gridCols": 12 },
  "constraints": ["HeaderAtTop", "FooterAtBottom", "ImagesNeedTextNeighbor"],
  "components": [
    { "id": "hdr", "kind": "Header", "content": "Welcome to Our Page" },
    { "id": "img1", "kind": "Image", "src": "https://example.com/image1.jpg" },
    { "id": "txt1", "kind": "TextBlock", "content": "Our team is dedicated..." },
    { "id": "ftr", "kind": "Footer", "content": "Contact us: info@example.com" }
  ]
}
```

---

## Output
- **Console summary** (optimized layout as text).
- **HTML file** (when `--out` is specified).
- **CSS styles**: `style-minimal.css`, `style-classic.css`, `style-modern.css`.

---

## Project Structure
- `Parser.hs` — JSON parsing.
- `Layout.hs` — layout optimization.
- `HtmlExport.hs` — HTML rendering.
- `Main.hs` — entry point, CLI options.
- `style-*.css` — multiple styles for export.
