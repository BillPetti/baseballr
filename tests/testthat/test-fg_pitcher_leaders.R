
cols <- c(
  "Season",
  "team_name",
  "Throws",
  "xMLBAMID",
  "PlayerNameRoute",
  "PlayerName",
  "playerid",
  "Age",
  "AgeRng",
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
  "FB_pct1",
  "FBv",
  "SL_pct",
  "SLv",
  "CT_pct",
  "CTv",
  "CB_pct",
  "CBv",
  "SF_pct",
  "SFv",
  "XX_pct",
  "wFB",
  "wSL",
  "wCT",
  "wCB",
  "wSF",
  "wFB_C",
  "wSL_C",
  "wCT_C",
  "wCB_C",
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
  "xERA",
  "pb_o_CH",
  "pb_s_CH",
  "pb_c_CH",
  "pb_o_CU",
  "pb_s_CU",
  "pb_c_CU",
  "pb_o_FF",
  "pb_s_FF",
  "pb_c_FF",
  "pb_o_SI",
  "pb_s_SI",
  "pb_c_SI",
  "pb_o_SL",
  "pb_s_SL",
  "pb_c_SL",
  "pb_overall",
  "pb_stuff",
  "pb_command",
  "pb_xRV100",
  "pb_ERA",
  "sp_s_CH",
  "sp_l_CH",
  "sp_p_CH",
  "sp_s_CU",
  "sp_l_CU",
  "sp_p_CU",
  "sp_s_FF",
  "sp_l_FF",
  "sp_p_FF",
  "sp_s_SI",
  "sp_l_SI",
  "sp_p_SI",
  "sp_s_SL",
  "sp_l_SL",
  "sp_p_SL",
  "sp_stuff",
  "sp_location",
  "sp_pitching",
  "PPTV",
  "CPTV",
  "BPTV",
  "DSV",
  "DGV",
  "BTV",
  "rPPTV",
  "rBPTV",
  "EBV",
  "ESV",
  "rFTeamV",
  "rBTeamV",
  "rTV",
  "pfx_FA_pct",
  "pfx_SI_pct",
  "pfx_SL_pct",
  "pfx_CU_pct",
  "pfx_CH_pct",
  "pfx_vFA",
  "pfx_vSI",
  "pfx_vSL",
  "pfx_vCU",
  "pfx_vCH",
  "pfx_FA-X",
  "pfx_SI-X",
  "pfx_SL-X",
  "pfx_CU-X",
  "pfx_CH-X",
  "pfx_FA-Z",
  "pfx_SI-Z",
  "pfx_SL-Z",
  "pfx_CU-Z",
  "pfx_CH-Z",
  "pfx_wFA",
  "pfx_wSI",
  "pfx_wSL",
  "pfx_wCU",
  "pfx_wCH",
  "pfx_wFA_C",
  "pfx_wSI_C",
  "pfx_wSL_C",
  "pfx_wCU_C",
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
  "pi_SI_pct",
  "pi_SL_pct",
  "pi_vCH",
  "pi_vCU",
  "pi_vFA",
  "pi_vSI",
  "pi_vSL",
  "pi_CH-X",
  "pi_CU-X",
  "pi_FA-X",
  "pi_SI-X",
  "pi_SL-X",
  "pi_CH-Z",
  "pi_CU-Z",
  "pi_FA-Z",
  "pi_SI-Z",
  "pi_SL-Z",
  "pi_wCH",
  "pi_wCU",
  "pi_wFA",
  "pi_wSI",
  "pi_wSL",
  "pi_wCH_C",
  "pi_wCU_C",
  "pi_wFA_C",
  "pi_wSI_C",
  "pi_wSL_C",
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
  "CH_pct",
  "CHv",
  "wCH",
  "wCH_C",
  "pb_o_FS",
  "pb_s_FS",
  "pb_c_FS",
  "sp_s_FS",
  "sp_l_FS",
  "sp_p_FS",
  "pfx_FS_pct",
  "pfx_vFS",
  "pfx_FS-X",
  "pfx_FS-Z",
  "pfx_wFS",
  "pfx_wFS_C",
  "pi_FS_pct",
  "pi_vFS",
  "pi_FS-X",
  "pi_FS-Z",
  "pi_wFS",
  "pi_wFS_C",
  "pb_o_FC",
  "pb_s_FC",
  "pb_c_FC",
  "sp_s_FC",
  "sp_l_FC",
  "sp_p_FC",
  "pfx_FC_pct",
  "pfx_vFC",
  "pfx_FC-X",
  "pfx_FC-Z",
  "pfx_wFC",
  "pfx_wFC_C",
  "pi_FC_pct",
  "pi_vFC",
  "pi_FC-X",
  "pi_FC-Z",
  "pi_wFC",
  "pi_wFC_C",
  "pb_o_KC",
  "pb_s_KC",
  "pb_c_KC",
  "sp_s_KC",
  "sp_l_KC",
  "sp_p_KC",
  "rBTV",
  "pfx_KC_pct",
  "pfx_vKC",
  "pfx_KC-X",
  "pfx_KC-Z",
  "pfx_wKC",
  "pfx_wKC_C",
  "rCPTV",
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
  "pi_KN_pct",
  "pi_XX_pct",
  "pi_vKN",
  "pi_vXX",
  "pi_KN-X",
  "pi_XX-X",
  "pi_KN-Z",
  "pi_XX-Z",
  "pi_wKN",
  "pi_wXX",
  "pi_wKN_C",
  "pi_wXX_C",
  "sp_s_FO",
  "sp_l_FO",
  "sp_p_FO",
  "pfx_FO_pct",
  "pfx_vFO",
  "pfx_FO-X",
  "pfx_FO-Z",
  "pfx_wFO",
  "pfx_wFO_C",
  "rDGV",
  "pi_CS_pct",
  "pi_vCS",
  "pi_CS-X",
  "pi_CS-Z",
  "pi_wCS",
  "pi_wCS_C",
  "Relieving",
  "Relief_IP",
  "rDSV",
  "pfx_EP_pct",
  "pfx_vEP",
  "pfx_EP-X",
  "pfx_EP-Z",
  "pfx_wEP",
  "pfx_wEP_C",
  "pfx_SC_pct",
  "pfx_vSC",
  "pfx_SC-X",
  "pfx_SC-Z",
  "pfx_wSC",
  "pfx_wSC_C",
  "pi_SB_pct",
  "pi_vSB",
  "pi_SB-X",
  "pi_SB-Z",
  "pi_wSB",
  "pi_wSB_C"
)

test_that("FanGraphs Pitching Leaders", {
  skip_on_cran()
  
  x <- fg_pitcher_leaders(startseason = 2023, endseason = 2023, qual = 0)
  
  expect_in(sort(cols), sort(colnames(x)))
  expect_s3_class(x, "data.frame")
})
