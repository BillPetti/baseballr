cols <- c(
  "Season",
  "team_name",
  "SeasonMin",
  "SeasonMax",
  "W",
  "L",
  "ERA",
  "G",
  "GS",
  "CG",
  "ShO",
  "SV",
  "BS",
  "IP",
  "TBF",
  "H",
  "R",
  "ER",
  "HR",
  "BB",
  "IBB",
  "HBP",
  "WP",
  "BK",
  "SO",
  "GB",
  "FB",
  "LD",
  "IFFB",
  "Pitches",
  "Balls",
  "Strikes",
  "RS",
  "IFH",
  "BU",
  "BUH",
  "K_9",
  "BB_9",
  "K_BB",
  "H_9",
  "HR_9",
  "AVG",
  "WHIP",
  "BABIP",
  "LOB_pct",
  "FIP",
  "GB_FB",
  "LD_pct",
  "GB_pct",
  "FB_pct",
  "IFFB_pct",
  "HR_FB",
  "IFH_pct",
  "BUH_pct",
  "TTO_pct",
  "CFraming",
  "Starting",
  "Start_IP",
  "Relieving",
  "Relief_IP",
  "RAR",
  "WAR",
  "Dollars",
  "RA9-Wins",
  "LOB-Wins",
  "BIP-Wins",
  "BS-Wins",
  "tERA",
  "xFIP",
  "WPA",
  "WPA_minus",
  "WPA_plus",
  "RE24",
  "REW",
  "pLI",
  "inLI",
  "gmLI",
  "exLI",
  "Pulls",
  "Games",
  "WPA_LI",
  "Clutch",
  "FBall_pct",
  "FBv",
  "SL_pct",
  "SLv",
  "CT_pct",
  "CTv",
  "CB_pct",
  "CBv",
  "CH_pct",
  "CHv",
  "SF_pct",
  "SFv",
  "XX_pct",
  "wFB",
  "wSL",
  "wCT",
  "wCB",
  "wCH",
  "wSF",
  "wFB_C",
  "wSL_C",
  "wCT_C",
  "wCB_C",
  "wCH_C",
  "wSF_C",
  "O-Swing_pct",
  "Z-Swing_pct",
  "Swing_pct",
  "O-Contact_pct",
  "Z-Contact_pct",
  "Contact_pct",
  "Zone_pct",
  "F-Strike_pct",
  "SwStr_pct",
  "CStr_pct",
  "C+SwStr_pct",
  "HLD",
  "SD",
  "MD",
  "ERA-",
  "FIP-",
  "xFIP-",
  "K_pct",
  "BB_pct",
  "K-BB_pct",
  "SIERA",
  "kwERA",
  "RS_9",
  "E-F",
  "Pull",
  "Cent",
  "Oppo",
  "Soft",
  "Med",
  "Hard",
  "bipCount",
  "Pull_pct",
  "Cent_pct",
  "Oppo_pct",
  "Soft_pct",
  "Med_pct",
  "Hard_pct",
  "K_9+",
  "BB_9+",
  "K_BB+",
  "H_9+",
  "HR_9+",
  "AVG+",
  "WHIP+",
  "BABIP+",
  "LOB_pct+",
  "K_pct+",
  "BB_pct+",
  "LD_pct+",
  "GB_pct+",
  "FB_pct+",
  "HRFB_pct+",
  "Pull_pct+",
  "Cent_pct+",
  "Oppo_pct+",
  "Soft_pct+",
  "Med_pct+",
  "Hard_pct+",
  "rFTeamV",
  "rBTeamV",
  "rTV",
  "pfx_FA_pct",
  "pfx_FT_pct",
  "pfx_FC_pct",
  "pfx_FS_pct",
  "pfx_SI_pct",
  "pfx_SL_pct",
  "pfx_CU_pct",
  "pfx_KC_pct",
  "pfx_CH_pct",
  "pfx_vFA",
  "pfx_vFT",
  "pfx_vFC",
  "pfx_vFS",
  "pfx_vSI",
  "pfx_vSL",
  "pfx_vCU",
  "pfx_vKC",
  "pfx_vCH",
  "pfx_FA-X",
  "pfx_FT-X",
  "pfx_FC-X",
  "pfx_FS-X",
  "pfx_SI-X",
  "pfx_SL-X",
  "pfx_CU-X",
  "pfx_KC-X",
  "pfx_CH-X",
  "pfx_FA-Z",
  "pfx_FT-Z",
  "pfx_FC-Z",
  "pfx_FS-Z",
  "pfx_SI-Z",
  "pfx_SL-Z",
  "pfx_CU-Z",
  "pfx_KC-Z",
  "pfx_CH-Z",
  "pfx_wFA",
  "pfx_wFT",
  "pfx_wFC",
  "pfx_wFS",
  "pfx_wSI",
  "pfx_wSL",
  "pfx_wCU",
  "pfx_wKC",
  "pfx_wCH",
  "pfx_wFA_C",
  "pfx_wFT_C",
  "pfx_wFC_C",
  "pfx_wFS_C",
  "pfx_wSI_C",
  "pfx_wSL_C",
  "pfx_wCU_C",
  "pfx_wKC_C",
  "pfx_wCH_C",
  "pfx_O-Swing_pct",
  "pfx_Z-Swing_pct",
  "pfx_Swing_pct",
  "pfx_O-Contact_pct",
  "pfx_Z-Contact_pct",
  "pfx_Contact_pct",
  "pfx_Zone_pct",
  "pfx_Pace",
  "pi_CH_pct",
  "pi_CU_pct",
  "pi_FA_pct",
  "pi_FC_pct",
  "pi_FS_pct",
  "pi_SI_pct",
  "pi_SL_pct",
  "pi_XX_pct",
  "pi_vCH",
  "pi_vCU",
  "pi_vFA",
  "pi_vFC",
  "pi_vFS",
  "pi_vSI",
  "pi_vSL",
  "pi_vXX",
  "pi_CH-X",
  "pi_CU-X",
  "pi_FA-X",
  "pi_FC-X",
  "pi_FS-X",
  "pi_SI-X",
  "pi_SL-X",
  "pi_XX-X",
  "pi_CH-Z",
  "pi_CU-Z",
  "pi_FA-Z",
  "pi_FC-Z",
  "pi_FS-Z",
  "pi_SI-Z",
  "pi_SL-Z",
  "pi_XX-Z",
  "pi_wCH",
  "pi_wCU",
  "pi_wFA",
  "pi_wFC",
  "pi_wFS",
  "pi_wSI",
  "pi_wSL",
  "pi_wXX",
  "pi_wCH_C",
  "pi_wCU_C",
  "pi_wFA_C",
  "pi_wFC_C",
  "pi_wFS_C",
  "pi_wSI_C",
  "pi_wSL_C",
  "pi_wXX_C",
  "pi_O-Swing_pct",
  "pi_Z-Swing_pct",
  "pi_Swing_pct",
  "pi_O-Contact_pct",
  "pi_Z-Contact_pct",
  "pi_Contact_pct",
  "pi_Zone_pct",
  "pi_Pace",
  "Events",
  "EV",
  "LA",
  "Barrels",
  "Barrel_pct",
  "maxEV",
  "HardHit",
  "HardHit_pct",
  "Q",
  "TG",
  "TIP",
  "team_name_abb",
  "teamid",
  "pfx_EP_pct",
  "pfx_vEP",
  "pfx_EP-X",
  "pfx_EP-Z",
  "pfx_wEP",
  "pfx_wEP_C",
  "pi_SB_pct",
  "pi_vSB",
  "pi_SB-X",
  "pi_SB-Z",
  "pi_wSB",
  "pi_wSB_C",
  "pi_CS_pct",
  "pi_vCS",
  "pi_CS-X",
  "pi_CS-Z",
  "pi_wCS",
  "pi_wCS_C",
  "pi_KN_pct",
  "pi_vKN",
  "pi_KN-X",
  "pi_KN-Z",
  "pi_wKN",
  "pi_wKN_C",
  "KN_pct",
  "KNv",
  "wKN",
  "wKN_C",
  "pfx_KN_pct",
  "pfx_vKN",
  "pfx_KN-X",
  "pfx_KN-Z",
  "pfx_wKN",
  "pfx_wKN_C",
  "pfx_SC_pct",
  "pfx_vSC",
  "pfx_SC-X",
  "pfx_SC-Z",
  "pfx_wSC",
  "pfx_wSC_C",
  "pfx_FO_pct",
  "pfx_vFO",
  "pfx_FO-X",
  "pfx_FO-Z",
  "pfx_wFO",
  "pfx_wFO_C"
)

test_that("FanGraphs Team Pitching", {
  skip_on_cran()
  
  x <- fg_team_pitcher(startseason = 2015, endseason = 2015, qual = 150)
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, "data.frame")
})
