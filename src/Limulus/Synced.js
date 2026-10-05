// What each stage object's block last agreed with the stage on, kept beside
// the buffer (same origin, so every Limulus shares it). A block that still
// says this is behind the stage, not an edit in hand, even after a reload.
const KEY = "limulus.synced";
export const load = () => {
  try { const o = JSON.parse(localStorage.getItem(KEY) || "{}"); return o && typeof o === "object" ? o : {}; }
  catch (_) { return {}; }
};
export const save = (o) => () => { try { localStorage.setItem(KEY, JSON.stringify(o)); } catch (_) {} };
