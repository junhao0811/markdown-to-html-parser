# 📝 Markdown → HTML Converter (Haskell + TypeScript)

A live Markdown editor that converts Markdown into clean, pretty-printed HTML as you type. The conversion engine is a **Markdown parser written from scratch in Haskell with parser combinators**, served to a reactive **TypeScript + RxJS** front end.

---

## ✨ Features

**Supported Markdown**
- Headings (`#` and underline style), **bold**, *italic*, ~~strikethrough~~ and `inline code`
- Links, images with captions, footnotes and footnote references
- Blockquotes and fenced code blocks with a language tag
- Ordered lists with nested sub-lists
- Tables

**Editor**
- **Live preview**: HTML updates on every keystroke
- Syntax highlighting for code (JavaScript, Python, Haskell)
- Save the generated HTML to a timestamped file

---

## 🏗️ How it works

```
┌─────────────────────────┐   POST /api/convertMD   ┌──────────────────────────────┐
│  Browser editor         │ ──────────────────────▶ │  Haskell server (Scotty)     │
│  TypeScript + RxJS      │                         │  Markdown ─▶ ADT ─▶ HTML     │
│  input$ → state → view  │ ◀────────────────────── │  parser combinators          │
└─────────────────────────┘        HTML (JSON)      └──────────────────────────────┘
```

- **Parsing**: small, composable parsers (`parseBold`, `parseLink`, `parseTable`…) are combined into a full `markdownParser` that turns raw text into an **algebraic data type (ADT)** describing the document
- **Rendering**: the ADT is pretty-printed into correctly indented HTML
- **Front end**: user input is an RxJS stream folded into immutable state, keeping side effects at the edges

| Path | Contents |
|---|---|
| `Haskell/src/Parser.hs` | Parser-combinator building blocks |
| `Haskell/src/MarkdownParser.hs` | Markdown grammar, ADT and HTML pretty-printer |
| `Haskell/app/Main.hs` | Scotty web server |
| `Haskell/examples/` | Sample Markdown inputs with expected HTML output |
| `JS/src/` | Reactive TypeScript editor |

---

## 🚀 Run it

**Requires** [Stack](https://docs.haskellstack.org/) and Node.js.

```bash
# 1. Start the Haskell server (port 3000)
cd Haskell
stack run

# 2. In another terminal, start the editor
cd JS
npm install
npm run dev
```

Run the parser tests against the example files:
```bash
cd Haskell
stack test
```

## 🛠️ Tech
Haskell · parser combinators · Scotty · TypeScript · RxJS · highlight.js · Vite
