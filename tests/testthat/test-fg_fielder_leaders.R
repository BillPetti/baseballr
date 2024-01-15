
cols <- c(
  "Season",
  "team_name",
  "xMLBAMID",
  "PlayerNameRoute",
  "PlayerName",
  "playerid",
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
  "SB",
  "CS",
  "PB",
  "WP",
  "FP",
  "rSB",
  "rGFP",
  "rSZ",
  "rCERA",
  "DRS",
  "Defense",
  "CStrikes",
  "CFraming",
  "Q",
  "TInn",
  "teamid",
  "team_name_abb",
  "rARM",
  "rPM",
  "BIZ",
  "Plays",
  "RZR",
  "OOZ",
  "ARM",
  "RngR",
  "ErrR",
  "UZR",
  "UZR_150",
  "OAA",
  "rFRP",
  "rGDP",
  "DPR",
  "Scp"
)

test_that("FanGraphs Fielder Leaders", {
  skip_on_cran()
  
  x <- fg_fielder_leaders(startseason = 2023, endseason = 2023, qual = 0)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
