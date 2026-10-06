import { EditorView, keymap, lineNumbers, drawSelection, Decoration, MatchDecorator, ViewPlugin } from "@codemirror/view";
import { EditorState, StateField, StateEffect } from "@codemirror/state";
import { defaultKeymap, history, historyKeymap, indentWithTab, toggleLineComment } from "@codemirror/commands";
import { bracketMatching, StreamLanguage, syntaxHighlighting, HighlightStyle, foldService, foldGutter, codeFolding, foldKeymap } from "@codemirror/language";
import { haskell } from "@codemirror/legacy-modes/mode/haskell";
import { tags as t } from "@lezer/highlight";

const STORE = "limulus.buffer";

// Calypso's phosphor palette, by role rather than by token.
const style = HighlightStyle.define([
  { tag: t.keyword, color: "var(--fg-bright)" },
  { tag: [t.string, t.special(t.string)], color: "var(--amber)" },
  { tag: t.number, color: "var(--cyan)" },
  { tag: [t.lineComment, t.blockComment, t.comment], color: "var(--fg-dim)", fontStyle: "italic" },
  { tag: [t.operator, t.punctuation], color: "var(--fg)" },
  { tag: [t.variableName, t.typeName], color: "var(--fg)" },
]);

// CodeMirror's base theme is light; this tells it otherwise, so its own
// gutter and selection colours give way to the page's.
const dark = EditorView.theme({
  "&": { backgroundColor: "var(--bg)", color: "var(--fg)" },
  ".cm-gutters": { backgroundColor: "var(--bg)", color: "var(--fg-dim)", borderRight: "1px solid var(--fg-rule)" },
  ".cm-activeLineGutter": { backgroundColor: "var(--bg-elev)" },
}, { dark: true });

// A line's head word, when it addresses the rig rather than Tidal (`drums $`,
// `odonus $`, `vetula $`, a card `v3 $`), shown inverted: phosphor on black is
// Tidal, black on phosphor is the rig's own language (Limulus.Engine's
// machineLine and the stage blocks). Inversion rather than a colour, since the
// palette is already as green as it should be.
const rigHead = new MatchDecorator({
  regexp: /(?<=^\s*)(?:drums|odonus|vetula|conspicillum|balistes|selene|route|v\d+)(?=\s*\$)/g,
  decoration: Decoration.mark({ class: "cm-rig-head" }),
});
const rigHeads = ViewPlugin.fromClass(class {
  constructor(view) { this.decorations = rigHead.createDeco(view); }
  update(u) { this.decorations = rigHead.updateDeco(u, this.decorations); }
}, { decorations: (v) => v.decorations });

// Folding, by the buffer's shape (Tidal has no syntax tree here):
// - a block (a run of non-blank lines) of several lines folds to its first;
// - a block of comments only is a heading: it folds to its first line,
//   hiding everything after it to the next heading. So `-- drums` over some
//   lines makes a section.
const blank = (doc, n) => doc.line(n).text.trim() === "";
const isComment = (text) => /^\s*--/.test(text);
const blockEnd = (doc, n) => { let m = n; while (m < doc.lines && !blank(doc, m + 1)) m++; return m; };
const startsBlock = (doc, n) => !blank(doc, n) && (n === 1 || blank(doc, n - 1));
const isHeading = (doc, n) => {
  if (!startsBlock(doc, n)) return false;
  for (let m = n; m <= blockEnd(doc, n); m++) if (!isComment(doc.line(m).text)) return false;
  return true;
};
const folds = foldService.of((state, lineStart) => {
  const doc = state.doc;
  const n = doc.lineAt(lineStart).number;
  if (!startsBlock(doc, n)) return null;
  const end = blockEnd(doc, n);
  if (isHeading(doc, n)) {
    let last = end, m = end + 1;
    while (m <= doc.lines && !isHeading(doc, m)) { if (!blank(doc, m)) last = m; m++; }
    // folds to its first line, the section's title
    return last > end ? { from: doc.line(n).to, to: doc.line(last).to } : null;
  }
  return end > n ? { from: doc.line(n).to, to: doc.line(end).to } : null;
});

// The block that was just sent lights up briefly, as in Tidal's editors.
const flash = StateEffect.define();
const flashField = StateField.define({
  create: () => Decoration.none,
  update(deco, tr) {
    deco = deco.map(tr.changes);
    for (const e of tr.effects) {
      if (e.is(flash)) {
        deco = e.value
          ? Decoration.set([Decoration.mark({ class: "cm-flash" }).range(e.value.from, e.value.to)])
          : Decoration.none;
      }
    }
    return deco;
  },
  provide: (f) => EditorView.decorations.from(f),
});

// The selection if there is one, else the run of non-blank lines around the
// cursor: a block, the unit Tidal's editors evaluate.
const blockAt = (state) => {
  const sel = state.selection.main;
  if (!sel.empty) return { from: sel.from, to: sel.to };
  const doc = state.doc;
  const here = doc.lineAt(sel.head);
  if (here.text.trim() === "") return null;
  let first = here.number;
  let last = here.number;
  while (first > 1 && doc.line(first - 1).text.trim() !== "") first--;
  while (last < doc.lines && doc.line(last + 1).text.trim() !== "") last++;
  return { from: doc.line(first).from, to: doc.line(last).to };
};

// Vetula's voices are lettered P..W (2026-10-06): a block headed `v3 $` from
// before is renamed once, as Limulus now finds it by its letter.
const VOICES = "PQRSTUVW";
const relabel = (text) => text.replace(/^v([1-8])(\s*\$)/gm, (_, n, rest) => VOICES[n - 1] + rest);
const load = (fallback) => {
  try { const t = localStorage.getItem(STORE); return t == null ? fallback : relabel(t); } catch { return fallback; }
};
const save = (text) => {
  try { localStorage.setItem(STORE, text); } catch { /* private window: fine */ }
};

export const _create = (parent, initial, handlers) => {
  const evaluate = (view) => {
    const range = blockAt(view.state);
    if (!range) return true;
    view.dispatch({ effects: flash.of(range) });
    setTimeout(() => view.dispatch({ effects: flash.of(null) }), 220);
    handlers.onEval({ text: view.state.sliceDoc(range.from, range.to), from: range.from, to: range.to })();
    return true;
  };
  const hush = () => { handlers.onHush(); return true; };

  const view = new EditorView({
    parent,
    state: EditorState.create({
      doc: load(initial),
      extensions: [
        keymap.of([
          { key: "Mod-Enter", run: evaluate },
          { key: "Shift-Enter", run: evaluate },
          { key: "Mod-.", run: hush },
          { key: "Mod-/", run: toggleLineComment },
          indentWithTab,
          ...foldKeymap,
          ...historyKeymap,
          ...defaultKeymap,
        ]),
        lineNumbers(),
        history(),
        drawSelection(),
        bracketMatching(),
        StreamLanguage.define(haskell),
        syntaxHighlighting(style),
        dark,
        flashField,
        rigHeads,
        folds,
        codeFolding({ placeholderText: "…" }),
        foldGutter({ openText: "▾", closedText: "▸" }),
        // in a machine's panel the width is the page's to give: wrap
        ...(document.documentElement.classList.contains("embedded") ? [EditorView.lineWrapping] : []),
        EditorView.updateListener.of((u) => { if (u.docChanged) save(u.state.doc.toString()); }),
      ],
    }),
  });
  // One buffer, more than one Limulus: the tab, and the panel on a machine's
  // page (same origin, same key). Every edit is saved at once and only one
  // can be typed in at a time, so the stored buffer is always the latest:
  // take it on focus, and live from another one's edits while not focused.
  const adopt = (text) => {
    if (text === null || text === view.state.doc.toString()) return;
    const head = Math.min(view.state.selection.main.head, text.length);
    view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: text }, selection: { anchor: head } });
  };
  window.addEventListener("focus", () => {
    adopt(load(null));
    // the page handed its frame the keyboard: give it to the editor
    if (embedded() && !view.hasFocus) view.focus();
  });
  window.addEventListener("storage", (e) => { if (e.key === STORE && !document.hasFocus() && e.newValue != null) adopt(relabel(e.newValue)); });
  return view;
};

// Embedded in a page, Limulus does not take the keyboard on loading: the
// page's own keys (Space, b, c …) stay the page's until Limulus is clicked
// into, or the page gives its frame the focus (see the window focus below).
const embedded = () => document.documentElement.classList.contains("embedded");
export const _focus = (view) => { if (!embedded()) view.focus(); };

// The block (run of non-blank lines) whose first line starts with `head`
// followed by `$` (`v3 $ …`), as {from, to, text}, or null.
export const _findBlock = (view, head) => {
  const doc = view.state.doc;
  const re = new RegExp("^\\s*" + head.replace(/[.*+?^${}()|[\]\\]/g, "\\$&") + "\\s*\\$");
  for (let n = 1; n <= doc.lines; n++) {
    const line = doc.line(n);
    const prevBlank = n === 1 || doc.line(n - 1).text.trim() === "";
    if (prevBlank && re.test(line.text)) {
      let last = n;
      while (last < doc.lines && doc.line(last + 1).text.trim() !== "") last++;
      const to = doc.line(last).to;
      return { from: line.from, to, text: view.state.sliceDoc(line.from, to) };
    }
  }
  return null;
};

export const _replace = (view, from, to, text) => {
  view.dispatch({ changes: { from, to, insert: text } });
};

// Add a block at the end of the buffer, after a blank line, and show it.
export const _append = (view, text) => {
  const doc = view.state.doc;
  const tail = doc.toString().endsWith("\n\n") ? "" : doc.toString().endsWith("\n") ? "\n" : "\n\n";
  const from = doc.length + tail.length;
  view.dispatch({ changes: { from: doc.length, insert: tail + text + "\n" } });
  _reveal(view, from, from + text.length);
};

// Select a range and scroll it into view.
export const _reveal = (view, from, to) => {
  view.dispatch({ selection: { anchor: from, head: to }, scrollIntoView: true });
};
