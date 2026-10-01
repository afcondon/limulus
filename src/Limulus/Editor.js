import { EditorView, keymap, lineNumbers, drawSelection, Decoration } from "@codemirror/view";
import { EditorState, StateField, StateEffect } from "@codemirror/state";
import { defaultKeymap, history, historyKeymap, indentWithTab, toggleLineComment } from "@codemirror/commands";
import { bracketMatching, StreamLanguage, syntaxHighlighting, HighlightStyle } from "@codemirror/language";
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

const load = (fallback) => {
  try { return localStorage.getItem(STORE) ?? fallback; } catch { return fallback; }
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
    handlers.onEval(view.state.sliceDoc(range.from, range.to))();
    return true;
  };
  const hush = () => { handlers.onHush(); return true; };

  return new EditorView({
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
        EditorView.updateListener.of((u) => { if (u.docChanged) save(u.state.doc.toString()); }),
      ],
    }),
  });
};

export const _focus = (view) => view.focus();
