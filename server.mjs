// Limulus's server: the page, and Haskell Tidal behind it.
//
// The browser cannot start GHCi, so this does, the way Tidal's own editors
// do: one long-lived `ghci` booted with BootTidal.hs, and each block the page
// sends typed into it. purerl-tidal needs no server here; the page speaks to
// its WebSocket directly.
//
// Node, no dependencies. PORT (default 3036); TIDAL_GHCI to use another GHCi.

import { createServer } from "node:http";
import { spawn } from "node:child_process";
import { readFile } from "node:fs/promises";
import { extname, join, normalize } from "node:path";
import { fileURLToPath } from "node:url";

const here = fileURLToPath(new URL(".", import.meta.url));
const PORT = Number(process.env.PORT) || 3036;
const GHCI = process.env.TIDAL_GHCI || join(here, "ghc-tidal/bin/ghci");
const BOOT = join(here, "boot/BootTidal.hs");
const PROMPT = "<<tidal>>\n";

// ---------------------------------------------------------------- GHCi

let ghci = null;     // the child process, while it lives
let state = "off";   // off | booting | ready
let out = "";        // stdout and stderr since the last prompt
let waiting = [];    // resolvers, one per prompt still to come
let queue = Promise.resolve();

function start() {
  if (ghci) return;
  state = "booting";
  out = "";
  ghci = spawn(GHCI, ["-ghci-script", BOOT], { cwd: here });
  const take = (chunk) => {
    out += chunk.toString();
    let i;
    while ((i = out.indexOf(PROMPT)) >= 0) {
      const text = out.slice(0, i);
      out = out.slice(i + PROMPT.length);
      const next = waiting.shift();
      if (next) next(text);
    }
  };
  ghci.stdout.on("data", take);
  ghci.stderr.on("data", take);
  ghci.on("exit", (code) => {
    console.log(`ghci exited (${code})`);
    ghci = null;
    state = "off";
    for (const w of waiting) w(out || `ghci exited (${code})`);
    waiting = [];
  });
  // The boot script ends by setting the prompt, so its first appearance
  // means Tidal is up.
  const booted = new Promise((resolve) => waiting.push(resolve));
  queue = booted.then((text) => {
    if (ghci) state = "ready";
    console.log(text.trim());
    return text;
  });
}

function stop() {
  if (ghci) ghci.kill();
}

// One block at a time, as GHCi reads them: wrapped in :{ :} so a block
// spread over several lines is one statement, like tidal.el sends it.
function evaluate(block) {
  start();
  const run = queue.then(() => new Promise((resolve) => {
    if (!ghci) return resolve({ ok: false, out: "ghci is not running" });
    waiting.push((text) => resolve({ ok: !/: error[:\s]|\*\*\* Exception/.test(text), out: text.trim() }));
    ghci.stdin.write(`:{\n${block}\n:}\n`);
  }));
  queue = run.catch(() => {});
  return run;
}

// ---------------------------------------------------------------- HTTP

const types = { ".html": "text/html", ".js": "text/javascript", ".css": "text/css", ".svg": "image/svg+xml" };

const body = (req) => new Promise((resolve) => {
  let s = "";
  req.on("data", (c) => (s += c));
  req.on("end", () => resolve(s));
});

const json = (res, value) => {
  res.writeHead(200, { "content-type": "application/json" });
  res.end(JSON.stringify(value));
};

createServer(async (req, res) => {
  const url = new URL(req.url, "http://x");
  // Served on its own port and, through :3023's proxy, at /limulus/ on the
  // Atlantis origin, where the tab bus reaches it; the page asks by relative
  // path, so both work.
  if (url.pathname === "/limulus") { res.writeHead(301, { Location: "/limulus/" }); res.end(); return; }
  url.pathname = url.pathname.replace(/^\/limulus(?=\/)/, "");
  if (req.method === "POST" && url.pathname === "/api/ghci/eval") {
    return json(res, await evaluate(await body(req)));
  }
  if (req.method === "POST" && url.pathname === "/api/ghci/restart") {
    stop();
    setTimeout(start, 200);
    return json(res, { ok: true });
  }
  if (url.pathname === "/api/ghci/status") {
    return json(res, { state });
  }
  const path = normalize(url.pathname === "/" ? "/index.html" : url.pathname);
  try {
    const file = await readFile(join(here, "public", path));
    res.writeHead(200, { "content-type": types[extname(path)] || "application/octet-stream", "cache-control": "no-cache" });
    res.end(file);
  } catch {
    res.writeHead(404);
    res.end("not found");
  }
}).listen(PORT, () => {
  console.log(`limulus on :${PORT}`);
  start(); // Tidal takes seconds to boot; do it before the first block.
});

for (const sig of ["SIGINT", "SIGTERM"]) process.on(sig, () => { stop(); process.exit(0); });
