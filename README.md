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
| `cache/panel.rds` | Pre-built panel, ~63 KB, trimmed to the analysed scope (companies, 2024 onward) so the app opens with data loaded, needs no outbound internet, and keeps its memory footprint inside the pod limit |
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

Push to `main`. If the GitHub webhook is in place a build starts by itself;
otherwise start one in OpenShift (**Builds → nzchangetracker → Start Build**) or
run `oc start-build nzchangetracker`. The pod redeploys itself when the build
finishes, using the Recreate strategy — the old pod stops before the new one
starts, which is what fits the project's memory quota, at the cost of a few
seconds of downtime per deploy.

The webhook payload URL is on the BuildConfig page in the console under
**Webhooks → GitHub → Copy URL with Secret**. It embeds a trigger secret, so it
does not belong in this repository. Note that it only works if GitHub can reach
the cluster API server from the public internet.

To refresh the data, either use *Download latest export* in the running app — the
pod's copy of the cache lasts until it restarts — or rebuild `cache/panel.rds`
from the main repository and push it here.

The committed cache is trimmed to companies from `ANALYSIS_START` on, which is
exactly what `compute_changes()` keeps after its own filters, so the trim cannot
change a reported change event; it is verified by re-running the tracker on both
the full and trimmed panels and comparing. Note that *Download latest export*
still pulls the whole export and holds it in memory, so it is the one path that
can still push the pod against its memory limit.
