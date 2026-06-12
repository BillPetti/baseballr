# Getting Started with baseballr

Welcome folks,

I’m Saiem Gilani, one of the
[authors](https://billpetti.github.io/baseballr/authors.html "Authors and contributors to baseballr")
of [`baseballr`](https://billpetti.github.io/baseballr/), and I hope to
give the community a high-quality resource for accessing men’s baseball
data for statistical analysis, baseball research, and more. I am excited
to show you some of what you can do with this edition of the package.

### **Installing R and RStudio**

1.  Head to <https://cran.r-project.org>
2.  Select the appropriate link for your operating system (Windows, Mac
    OS X, or Linux)

- **Windows** - Select base and download the most recent version
- **Mac OS X** - Select Latest Release, but check to make sure your OS
  is the correct version. Look through Binaries for Legacy OS X Systems
  if you are on an older release
- **Linux** - Select the appropriate distro and follow the installation
  instructions

3.  Head to
    [RStudio.com](https://posit.co/download/rstudio-desktop/#download "Download the appropriate version of RStudio (Free) for your operating system to use with R")
4.  Follow the associated download and installation instructions for
    RStudio.
5.  Start peering over the [RStudio IDE
    Cheatsheet](https://raw.githubusercontent.com/rstudio/cheatsheets/main/rstudio-ide.pdf).
    *An IDE is an integrated development environment.*
6.  For **Windows** users: I recommend you install
    [Rtools](https://cran.r-project.org/bin/windows/Rtools/). This is
    not an R package! It is “a collection of resources for building
    packages for R under Microsoft Windows, or for building R itself”.
    Go to <https://cran.r-project.org/bin/windows/Rtools/> and follow
    the directions for installation.

## **Install** [**`baseballr`**](https://billpetti.github.io/baseballr/)

``` r

# You can install using the pak package using the following code:
if (!requireNamespace('pak', quietly = TRUE)){
  install.packages('pak')
}
pak::pak("billpetti/baseballr")
```

### **The Data**

There are generally speaking **nine** men’s baseball data sources
accessible from this package:

- [`baseballr-data`
  repo](https://github.com/sportsdataverse/baseballr-data)
  (@SportsDataverse)
- [MLB Stats API](https://www.mlb.com/)
- [ESPN (MLB)](https://www.espn.com/mlb/)
- [Baseball Savant’s Statcast](https://baseballsavant.mlb.com/)
- [Chadwick Bureau’s Public Register of Baseball
  Players](https://github.com/chadwickbureau/register/)
- [Baseball Reference](https://www.baseball-reference.com/)
- [FanGraphs](https://www.fangraphs.com/)
- [Retrosheet](https://www.retrosheet.org/)
- [NCAA](https://stats.ncaa.org/)

#### **Function names indicate the data source**

As of [baseballr
v1.0.0](https://billpetti.github.io/baseballr/news/index.html#baseballr-1.0.0),
a function naming convention was implemented to have the data source
indicator appear at the start of the function name:

- Functions that use the [`baseballr-data`
  repository](https://github.com/sportsdataverse/baseballr-data) will
  contain `load_` or `update_` in the function name and would be
  considered loading functions for the play-by-play data, team box
  scores, and player box scores.

- Functions that use the MLB Stats API start with `mlb_` by convention
  and should be assumed as `get` functions. As of `baseballr` version
  2.0.0, the package exports ~88 functions covering the MLB Stats API.

- Functions that use [ESPN](https://www.espn.com/mlb/)’s MLB endpoints
  start with `espn_mlb_` by convention and should be assumed as `get`
  functions. They cover scoreboards and schedules, play-by-play, team
  and player box scores, game rosters, standings, athletes, coaches,
  drafts, leaders, and more — including baseball-specific extractors for
  probable starting pitchers
  ([`espn_mlb_game_probables()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_probables.md))
  and the umpire crew / venue
  ([`espn_mlb_game_info()`](https://billpetti.github.io/baseballr/reference/espn_mlb_game_info.md)).
  As of `baseballr` version 2.0.0, the package exports ~107 functions
  covering ESPN’s MLB endpoints. Each returns a clean, tidy
  `baseballr_data` tibble; box scores are returned wide (one row per
  team / per athlete-side). Live ESPN tests are gated behind
  `ESPN_MLB_TESTS=1`.

- Functions that use one of [Baseball Savant’s
  Statcast](https://baseballsavant.mlb.com/) APIs start with `statcast_`
  by convention and should be assumed as `get` functions. These
  functions allow for live access to Statcast data for the MLB games
  in-progress. As of `baseballr` version 2.0.0, the package exports ~5
  Statcast-related functions.

- Functions that use [Chadwick Bureau’s Public Register of Baseball
  Players](https://github.com/chadwickbureau/register/) start with
  `chadwick_`, `playerid_`, or `playername_` by convention and should be
  assumed as `get` functions. These functions allow for access to the
  Bureau’s public register of baseball players. As of `baseballr`
  version 2.0.0, the package exports 3 functions sourced using the
  Chadwick Bureau’s public register of baseball players.

- Functions that use Baseball Reference’s website start with `bref_` by
  convention and should be assumed as `get` functions. As of `baseballr`
  version 2.0.0, the package exports ~4 functions covering [Baseball
  Reference](https://www.baseball-reference.com/).

- Functions that use FanGraphs’s baseball website start with `fg_` by
  convention and should be assumed as `get` functions. As of `baseballr`
  version 2.0.0, the package exports ~11 functions covering
  [FanGraphs.com](https://www.fangraphs.com/).

- Functions that use Retrosheet’s baseball data start with `retrosheet_`
  by convention and should be assumed as `get` functions. As of
  `baseballr` version 2.0.0, the package exports 1 function for
  [Retrosheet Data](https://www.retrosheet.org/).

- Functions that use the NCAA website start with `ncaa_` by convention
  and should be assumed as `get` functions. As of `baseballr` version
  2.0.0, the package exports ~8 function covering the [NCAA Stats
  portal](https://stats.ncaa.org/).

## Follow the SportsDataverse (@SportsDataverse) on Twitter and star this repo

[![GitHub
stars](https://img.shields.io/github/stars/billpetti/baseballr.svg?color=eee&logo=github&style=for-the-badge&label=Star%20baseballr&maxAge=2592000)](https://github.com/billpetti/baseballr/stargazers/)

## **Our Authors**

- Bill Petti (@BillPetti)  
  [![@BillPetti](https://img.shields.io/github/followers/BillPetti?color=eee&logo=Github&style=for-the-badge)](https://github.com/BillPetti)

- Saiem Gilani (@saiemgilani)  
  [![@saiemgilani](https://img.shields.io/github/followers/saiemgilani?color=eee&logo=Github&style=for-the-badge)](https://github.com/saiemgilani)
