Team Pipe Dream: Harsh Rathi, Mason Mitchell, Charlie Bupp

This repo holds a Quarto report on video game film adaptations, looking at box office, critic
scores, audience scores, publisher size, and franchise trends. The report lives in
[`index.qmd`](index.qmd) and renders to [`index.pdf`](index.pdf).

Presentation: [PowerPoint version](https://pennstateoffice365-my.sharepoint.com/:p:/r/personal/mcm6619_psu_edu/Documents/Video%20Game%20Film%20Adaptations.pptx?d=wf0886230c368453c993fea5b3ee4ce2b&csf=1&web=1&e=uWTQJU).
There is also an [alternate version on Gamma](https://gamma.app/docs/Video-Game-Film-Adaptations-Factors-in-Box-Office-and-Critical-Re-r5kytgeqzkf6lsm).

The rest of this file walks you through rebuilding the PDF on a clean machine. 
It assumes you have never installed R or Quarto. Follow it top to bottom and you should end up with the same
`index.pdf` that is checked into the repo.

---

## What's in the repo

`index.qmd` is the report, and it renders to `index.pdf`.

The `images/` folder holds the figures the report shows. These are already-rendered PNG
files. The report does not regenerate them when it renders; it just embeds them with
`![](images/...)`.

`dev/render_figures.R` is the script that produces those PNGs. You only need it if you want
to rebuild the figures from scratch.

`.github/workflows/render-pdf.yml` is the GitHub Actions config that rebuilds the PDF in the
cloud on every push. You can ignore it when reproducing the report locally.

Two things to know before you start:

1. You need an internet connection every time you render. The report does not ship with a
   data file. It downloads the TidyTuesday `game_films.csv` from GitHub and scrapes a
   Wikipedia page (largest video game companies by revenue, used in Q2) while it renders.
   If you are offline, the render fails.
3. Rendering the report and rebuilding the figures are two separate jobs. Rendering
   `index.qmd` reuses the PNGs already sitting in `images/`. If you want fresh figures, run
   `dev/render_figures.R` first. See the last section.

---

## Step 0: Get the project onto your computer

If you do not already have the files, clone the repo. You need Git for this. If you do not
have Git, grab it from <https://git-scm.com/downloads> and accept the default options.

```bash
git clone https://github.com/xyzmr114/stat_184_summer_26.git
cd stat_184_summer_26
```

---

**( run these commands in your terminal equivalent app, one line at a time. )**

If you would rather not bother with Git, open the repo page, click the green **Code** button,
choose **Download ZIP**, and unzip it somewhere you can find. Then open a terminal in that
folder.

---

## Step 1: Install R

R is the language the analysis runs in. Quarto needs it to run the code in the report.

On Windows, go to <https://cran.r-project.org/bin/windows/base/>, download the installer, run
it, and click Next through every screen. The defaults are fine.

On macOS, go to <https://cran.r-project.org/bin/macosx/> and download the `.pkg` that matches
your chip. Apple Silicon (M1, M2, M3, M4) uses the arm64 build, and older Intel Macs use the
Intel build. If you are not sure which you have, open the Apple menu, click About This Mac,
and look at "Chip" or "Processor."

On Linux (Debian or Ubuntu), run `sudo apt-get update && sudo apt-get install r-base`. For
other distros, see <https://cran.r-project.org/>.

The version matters. The GitHub Actions build uses R 4.4.0, and anything 4.4.x or newer is
fine. Do not use R 3.x. The code uses the native pipe `|>` and other things older R does not
understand.

To check it worked, open a terminal (Command Prompt or PowerShell on Windows, Terminal on Mac
or Linux) and run:

```bash
R --version
```

You should see something like `R version 4.4.x`. If you get "command not found," the
installer did not add R to your PATH. The easy fix on Windows is to use RStudio (the next
step), which finds R on its own.

---

## Step 2: Install RStudio (optional, but it helps)

RStudio is a friendly editor for R and Quarto. You do not strictly need it, since you can
render from the command line, but it makes things easier and gives you a one-click **Render**
button.

Download it from <https://posit.co/download/rstudio-desktop/>, install with the defaults, and
open it once so it can find your R install.

If you only plan to render from the terminal, skip this and go to Step 3.

---

## Step 3: Install Quarto

Quarto is what turns `index.qmd` into a PDF. It runs the R code, stitches in the text and
figures, and hands the result off to LaTeX.

Download the installer for your OS from <https://quarto.org/docs/get-started/> and run it with
the default options. If you already installed RStudio, a copy of Quarto came bundled with it,
but installing the standalone version too is fine and gives you the `quarto` command in your
terminal.

To check it worked:

```bash
quarto --version
```

You should see a version number. Anything 1.4 or newer is plenty.

---

## Step 4: Install a LaTeX engine for the PDF

The report comes out as a PDF, so it needs a LaTeX install. The easiest path is to let Quarto
set up a small self-contained one called TinyTeX. Run this once:

```bash
quarto install tinytex
```
---

## Step 5: Install the R packages the report uses

The report loads a fixed set of packages, and they do not install on their own. You do this
once. Open R (type `R` in a terminal, or use the RStudio console) and paste this in:

```r
install.packages(c(
  "tidyverse", "readr", "janitor", "scales",
  "rvest", "lubridate", "ggcorrplot", "patchwork",
  "ggridges", "kableExtra", "viridis"
), repos = "https://cloud.r-project.org")
```

A few notes if this is your first time:

This is the slow step. `tidyverse` alone pulls in a lot, so give it several minutes on a fresh
machine.

If R asks "Do you want to install from sources the package which needs compilation?", answer
no. That is the safe choice and avoids needing a compiler.

On Linux, some of these packages need system libraries to build. If the install fails with
errors mentioning `curl`, `xml2`, `fontconfig`, `harfbuzz`, `freetype`, or `png`, install the
dev libraries first. On Debian or Ubuntu:

```bash
sudo apt-get update && sudo apt-get install -y \
  libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev \
  libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev \
  libtiff5-dev libjpeg-dev
```

Then rerun the `install.packages(...)` line. Windows and macOS get prebuilt packages, so they
do not need this.

You only do this step once per machine, not once per render. When R finishes, type `q()` and
press Enter to quit. Answer `n` if it asks about saving the workspace.

---

## Step 6: Render the report

Make sure your terminal is in the project folder, the one that contains `index.qmd`. Then run:

```bash
quarto render index.qmd --to pdf
```

If you are in RStudio, open `index.qmd` and click the **Render** button at the top of the
editor instead.

When it finishes you will have a fresh `index.pdf` in the project folder. Open it and check
that it looks like the report. That is the whole thing.

The first render is the slowest, since Quarto has to download the dataset and scrape
Wikipedia. Later renders are quicker.

---

## When something breaks

If you see `quarto: command not found`, Quarto is not on your PATH. Reopen your terminal after
installing, or render from inside RStudio.

If the render fails with a download error or "could not resolve host," you are offline or
GitHub or Wikipedia is unreachable. The report pulls its data live, so get back online and try
again.

If you get a Wikipedia scrape error or "subscript out of bounds" pointing at `TablesRedun[[2]]`,
the Wikipedia page layout changed or the page did not load. Run it again. If it keeps failing,
the table index in the scrape probably needs updating.

If you hit PDF or LaTeX errors, or it cannot find `pdflatex`, LaTeX is missing. Run
`quarto install tinytex` from Step 4 and render again.

If you see "there is no package called '...'", that package is not installed. Rerun the
`install.packages(...)` block from Step 5.

If you get "could not find function '|>'" or other syntax errors, your R is too old. Install
R 4.4 or newer from Step 1.

---

## Rebuilding the figures from scratch (optional)

The PNGs in `images/` are committed to the repo, so a normal render reuses them and you do not
need this. Only run it if you changed the analysis and want new figures.

[`dev/render_figures.R`](dev/render_figures.R) writes its output to `../images`, so you have to
run it from inside the `dev/` folder for the paths to work:

```bash
cd dev
Rscript render_figures.R
cd ..
```

This re-downloads the data, runs the full cleaning pipeline again (currency conversion, CPI
inflation adjustment, theatrical-only filter, CinemaScore conversion), and overwrites every
`fig-*.png` in `images/`. It uses the same packages from Step 5, so there is no extra setup.
When it finishes, render the report (Step 6) to pull the new figures in.

---

## How the automated build works (optional)

Every push to `main` triggers
[`.github/workflows/render-pdf.yml`](.github/workflows/render-pdf.yml). It spins up a clean
Ubuntu machine, installs R 4.4.0, Quarto, the system libraries, the R packages, and TinyTeX,
renders `index.qmd`, uploads the PDF as a build artifact, and commits the rendered `index.pdf`
back to the repo. It is basically Steps 1 through 6 run for you, and it is a handy reference if
you ever want to see exactly what a clean environment needs.

---

## TLDR

| Task | Command |
| --- | --- |
| Install the R packages (once) | the `install.packages(...)` block in Step 5 |
| Install LaTeX (once) | `quarto install tinytex` |
| Build the PDF | `quarto render index.qmd --to pdf` |
| Rebuild all figures | `cd dev && Rscript render_figures.R && cd ..` |

What you need: R 4.4 or newer, Quarto 1.4 or newer, TinyTeX (or any LaTeX), the 11 R packages
listed in Step 5 and the index.qmd file, and an internet connection when you render.
