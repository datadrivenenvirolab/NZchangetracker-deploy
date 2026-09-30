# Target Change Tracker — UNC CloudApps deployment

Deployment copy of [NZchangetracker](https://github.com/datadrivenenvirolab/NZchangetracker),
built to run on the UNC ITS
[R Shiny Server image for OpenShift](https://sc.unc.edu/middleware/ocp-rshiny-server).

This repository is public because the OpenShift build has to clone it. It
therefore holds **only** what the pod needs, and deliberately **omits the Franz
Sans font files**, which are a commercial typeface. The app falls back to the
system sans-serif and is otherwise identical.

| File | Why it is here |
|---|---|
| `app.R` | The app. Shiny Server serves the repository root, so this must stay at the top level |
| `R/tracker.R` | The analysis logic; `shiny::runApp()` sources `R/` automatically |
| `www/zerotracker.css`, `www/logo.svg` | Styling and logo (the `@font-face` rules are stripped) |
| `cache/panel.rds` | Pre-built panel, ~176 KB, so the app opens with data already loaded and needs no outbound internet |
| `requirements.txt` | R packages the build installs, one per line |
| `template.yml` | The UNC `R Shiny Server` OpenShift template, imported once per project |

## Deploying

1. In [CloudApps](https://console.apps.cloudapps.unc.edu/topology/ns/dept-data-driven-envirolab),
   **+Add → Import YAML**, paste `template.yml`, **Create**.
2. **+Add → Developer Catalog**, filter for `shiny`, pick **R Shiny Server**, **Instantiate Template**.
3. Fill in:
   - **Name**: `nzchangetracker`
   - **Git Repository URL**: `https://github.com/datadrivenenvirolab/NZchangetracker-deploy.git`
   - **Git Reference/Branch**: `main`
   - **Context Directory**: `/`
   - **Application Hostname**: leave blank
4. The build takes a while (every package is compiled from source). When the pod
   is ready, open the **Route** URL from the Topology view.

## Updating

Push to `main`, then start a new build in OpenShift (**Builds → nzchangetracker →
Start Build**). The pod redeploys itself when the build finishes.

To refresh the data, either use *Download latest export* in the running app — the
pod's copy of the cache lasts until it restarts — or rebuild `cache/panel.rds`
from the main repository and push it here.
