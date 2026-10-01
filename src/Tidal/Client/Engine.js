const post = (path, text) =>
  fetch(path, { method: "POST", body: text }).then((r) => r.json());

export const _ghciEval = (text) => () => post("/api/ghci/eval", text);
export const _ghciRestart = () => post("/api/ghci/restart", "");
export const _ghciStatus = () =>
  fetch("/api/ghci/status").then((r) => r.json()).then((s) => s.state, () => "unreachable");

// purerl-tidal's WebSocket. Text frames only; a frame sent before the
// handshake is dropped, which the page shows as the socket being down.
export const _connect = (url, cb) => {
  const ws = new WebSocket(url);
  ws.addEventListener("open", () => cb.onOpen());
  ws.addEventListener("message", (ev) => { if (typeof ev.data === "string") cb.onMessage(ev.data)(); });
  ws.addEventListener("close", () => cb.onClose());
  return ws;
};

export const _send = (ws, text) => {
  if (ws.readyState !== 1) return false;
  ws.send(text);
  return true;
};
