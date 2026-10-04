// The engine Limulus sends Tidal to, kept where the Dashboard sets it: the
// origin's shared storage (`atlantis/limulus-engine`), since Limulus and the
// Dashboard are both served from :3023. A change in either is heard by the
// other through the storage event.
const KEY = "atlantis/limulus-engine";
export const load = () => { try { return localStorage.getItem(KEY) || ""; } catch (_) { return ""; } };
export const save = (v) => () => { try { localStorage.setItem(KEY, v); } catch (_) {} };
export const onChange = (cb) => () => {
  window.addEventListener("storage", (e) => { if (e.key === KEY && e.newValue) cb(e.newValue)(); });
};
