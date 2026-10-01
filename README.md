# tidal-client

Text in, sound out. One editor in front of two engines, **Haskell Tidal**
(GHCi) and **purerl-tidal**, to show that they are the same. Plan and
reasoning: `docs/kb/plans/tidal-client.md`.

Cmd-Enter (or Shift-Enter) sends the block under the cursor, as Tidal's own
editors do; Cmd-. hushes both engines. Panic from any Atlantis page hushes
them too.

## Run

```
nix build --impure --expr '(builtins.getFlake "nixpkgs").legacyPackages.aarch64-darwin.haskellPackages.ghcWithPackages (p: [p.tidal])' -o ghc-tidal
npm install
npm run bundle      # spago bundle → public/app.js (add --minify)
node server.mjs     # :3036, boots GHCi with boot/BootTidal.hs
```

GHCi with Tidal 1.10.1 comes prebuilt from the nix cache (nothing compiles)
and holds about 120 MB. Its boot file is Tidal's own, with Link switched on so
it keeps the rig's tempo. Both engines play SuperDirt on :57120.

purerl-tidal is reached directly on `ws://localhost:3012/ws`. Until it has the
`d1`..`d16` verbs (step 2 of the plan) it refuses Tidal lines by name.
