
cols <- c(
  "Season",
  "team_name",
  "SeasonMin",
  "SeasonMax",
  "Pos",
  "G",
  "GS",
  "Inn",
  "PO",
  "A",
  "E",
  "FE",
  "TE",
  "DP",
  "DPS",
  "DPT",
  "DPF",
  "Scp",
  "SB",
  "CS",
  "PB",
  "WP",
  "FP",
  "rSB",
  "rGDP",
  "rARM",
  "rGFP",
  "rPM",
  "rSZ",
  "rTS",
  "rCERA",
  "DRS",
  "BIZ",
  "Plays",
  "RZR",
  "OOZ",
  "ARM",
  "DPR",
  "RngR",
  "ErrR",
  "UZR",
  "UZR_150",
  "Defense",
  "CStrikes",
  "CFraming",
  "OAA",
  "rFRP",
  "Q",
  "TInn",
  "teamid",
  "team_name_abb"
)

test_that("FanGraphs Team Fielder Leaders", {
  skip_on_cran()
  
  x <- fg_team_fielder(startseason = 2023, endseason = 2023, qual = 0)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
