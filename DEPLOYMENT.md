# Deployment — Play Call Tracker Demo

Target: a public URL on Posit Connect Cloud that anyone can open from
the portfolio, no login required.

---

## 1. Create the GitHub repo

1. On [github.com/new](https://github.com/new), create a public repo:
   - Name: `playcall-tracker-demo`
   - Visibility: **Public**
   - Don't initialize with README/LICENSE/.gitignore (I already have them)
2. **Leave the setup page open** — you'll need the clone URL in the next
   step.

## 2. Push the demo code to the repo

Open Git Bash from the folder where this demo was staged (adjust the
`cd` to wherever you dropped the files):

```bash
cd "<path to the playcall_tracker_demo folder>"
git init
git add .
git commit -m "Initial demo release — in-memory sessions, generic playbook"
git branch -M main
git remote add origin https://github.com/LuisRodCast/playcall-tracker-demo.git
git push -u origin main
```

If the push 404s, double-check the repo name and that it's public.

## 3. Deploy to Posit Connect Cloud

1. Go to [connect.posit.cloud](https://connect.posit.cloud/) and sign
   in with your GitHub account.
2. **Content → Publish → From GitHub**.
3. Select the `LuisRodCast/playcall-tracker-demo` repo.
4. Branch: `main`. App file: `app.R`.
5. **Access setting: Public** (anyone with the URL can open it).
6. Click Deploy. The first build takes 3–5 minutes while Posit resolves
   the package tree.

**No environment variables needed** — this build has no `TURSO_URL` /
`TURSO_TOKEN` dependency. The in-memory per-visitor store (R/db_demo.R)
runs with zero credentials.

## 4. Get the public URL

Once the deployment is green, Posit gives you a URL that looks like:

```
https://playcall-tracker-demo-<hash>.posit.app/
```

Copy it — this is what the portfolio story page links to.

## 5. Smoke test

Open the URL in an incognito/private window (so your Posit login
doesn't apply) and verify:

- [ ] App loads with the "DEMO" amber badge in the navbar
- [ ] Tracker tab shows a non-empty play dropdown
- [ ] History tab shows **2 preloaded sessions**: "Demo Practice —
      Film Review Day" and "Demo Game — vs North Rivals"
- [ ] Trends tab's PRACTICE vs GAME table has rows with Δ PPP + 95% CI
- [ ] CATEGORY ANALYSES section populates (the Game session has
      categories tagged)
- [ ] Logging a new possession on Tracker and refreshing the page
      wipes it (confirms per-visitor fresh-state behavior)

## 6. (Optional) Pin to the Posit "showcase" area

Settings → Share → copy a short display name so the Posit public URL
header reads nicely.

---

## Updating the demo later

Any push to `main` on the GitHub repo re-triggers the Posit build
automatically. Typical loop:

```bash
# edit files...
git add .
git commit -m "<what changed>"
git push
```

Within ~2 minutes the live URL serves the new version.

## Reverting

If a push breaks the demo, revert the last commit:

```bash
git revert HEAD
git push
```

Posit rebuilds on the revert push.
