# Contributing to hdx-signals

This document outlines how to contribute to the HDX Signals code repository.

## Simple text changes

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you’ve found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).
See the Tidyverse guide on [how to create a great issue](https://code-review.tidyverse.org/issues/) for more advice.

## Overall flow

### Developer setup

Before you start, follow these steps to setup the repository for development:

1. Clone GitHub repository.
2. Install R version 4.3.3. You might find the R installation manager [rig](https://github.com/r-lib/rig) helpful in managing your installation versions.
3. Install `{renv}` with `install.packages("renv").
4. Install project dependencies using `renv::init()`.
5. Ensure you have the necessary environment variables defined. See [ENVIRONMENT.md](ENVIRONMENT.md) for more details.

### Pull request process

Create a pull request to address a specific issue.

*   Pull the latest version of `main` and install all dependencies with `renv::restore()`.
*   Create a Git branch for your pull request (PR). You could use `usethis::pr_init("brief-description-of-change")`.
*   Make your changes, commit to git, and then create a PR in GitHub. For convenience you can do this by running `usethis::pr_push()`, and following the prompts in your browser.
    Ensure that `CHANGES.md` is updated as described below.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

### Update `CHANGES.md`

Add a bullet point to the `CHANGES.md` file to describe the changes in your PR.

*   All PR changes need to be reflected in `CHANGES.md`. Add a bullet to the top
of `NEWS.md` (i.e. just below the first header). Follow the style described
in <https://style.tidyverse.org/news.html>.
*   Since there is only a single branch `main`, every PR will need to increment
the version number as described in the next section.
*  `CHANGES.md` is automatically parsed to create a public-facing `NEWS.md` in
the methods documentation. This is done by ensuring `CHANGES.md` is formatted
like so:

```
## version number

### Section
*   Change 1
*   Change 2
```

`Section` should follow the below guide to ensure proper parsing to `NEWS.md`.

Public facing:

*   `indicator_id`: (e.g. `ipc_food_insecurity` or `jrc_agricultural_hotspots`) to
indicate any change to code or emails specific to that indicator code that impacts
methods for scanning the data, analyzing and visualizing it, or generating email.
Does not include code improvements or other changes that are not necessary to
report to users of Signals.
*   `Email`: changes to the email, including plots, maps, or text, that are not
specific to a single indicator.
*   `Data`: changes to the output dataset `hdx_signals.csv`.
*   `Metadata`: changes to the locations metadata data `hdx_signals_locations_metadata.csv`.

Non-public facing:

*   `System`: changes in the `src` code that are not necessary to reflect publicly,
which can include improvements or other changes in the indicators folder.
*   `Assets`: changes to the `src-static` code to generate static assets.
*   `Testing`: changes to the `test` folder.
*   `Monitoring`: changes to the `.github` actions files that change how we monitor.

Feel free to propose additional categories, but those need to be reflected here for
consistency.

### Versioning

Update the version in `.signals-version`. We follow general semantic versioning
guidelines with the following small adjustment. Beyond `major.minor.patch` we
add a fourth level for small changes to the system that have no impact on runs:
code cleanup, documentation, tools for code control like precommit, etc. This
ensures we do not constantly iterate the `major.minor.patch` while we put together
small changes, but still all changes to the codebase are reflected in versioning.

So, for all PRs:

* Use `major.minor.patch.micro`
* Increment the version in `.signals-version`
* Add a section in `CHANGES.md`

### Coding practices

*   New code should follow the tidyverse [style guide](https://style.tidyverse.org). 
    You can use the [styler](https://CRAN.R-project.org/package=styler) package to apply these styles, but please don't restyle code that has nothing to do with your PR.  

*   We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*   We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.
   
*   We use [box](https://cran.r-project.org/package=box) for module and
library management for importing local modules and R libraries.
Use only the `box::use()` syntax with `library$function()` and
never `library(library)` or `library::function()`.

*   We use `{renv}` for package management. Any new dependencies need to be
added to the lockfile. You can easily do this by running
`renv::snapshot()` in your branch once you've finished coding, and new detected
dependencies will be add automatically. Use `renv::restore()` in any branch to
bring your cache up-to-date with the lockfile in there.

*   All environment variables required in the system need to be documented in
    [ENVIRONMENT.md](/ENVIRONMENT.md)
