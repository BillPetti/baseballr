#' Internal function to clean the raw pbp that is pulled in the intial stages of ncaa_get_pbp
#'
#' 
#'
#' @param season True or False, NCAA PBP by Season or Individual Teams.
#' @param division Select what division of college baseball 
#' @param teamid The numerical ID that the NCAA website uses to identify a team
#' @param conf Select a conference, naming convention can be found in master_ncaa_team_lu
#' @keywords baseball, NCAA, college, internal
#' @import dplyr
#' @import stringr
#' @export .ncaa_parse_pbp
#' @examples
#' 
#'  \dontrun{
#' ncaa_parse_pbp(temp_df)
#' }

.ncaa_parse_pbp <- function(pbp_data_frame){
  pbp_data_frame=pbp_data_frame%>%
    mutate(
      tmp_text=paste(away_text, home_text),
      #  # 
      sub_fl=case_when(
        str_detect(tmp_text, '(singled|doubled|tripled|homered|walked|reached|struck out|grounded|flied|lined|popped| hit|infield fly|infield fly|out|double play|triple play)')==TRUE & str_detect(tmp_text, c('pinch hit'))==FALSE ~ 0,
        str_detect(tmp_text, c('to (p|c|1b|2b|3b|ss|lf|rf|cf|dh)'))==TRUE ~ 1,
        str_detect(tmp_text, c('pinch hit'))==TRUE ~ 1,
        str_detect(tmp_text, c('pinch ran'))==TRUE ~ 1,
        TRUE ~ 0),
      
      # Split the text up
      bat_text=gsub('(;|3a|:).*$','', tmp_text),
      
      r1_text=case_when(
        str_detect(tmp_text, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',tmp_text)),
        TRUE~''),
      
      r2_text=case_when(
        str_detect(r1_text, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',r1_text)),
        TRUE~''),
      
      r3_text=case_when(
        str_detect(r2_text, '(;|3a|:)')==TRUE ~ stripwhite(gsub('^.*?(;|3a|:)','',r2_text)),
        TRUE~''),
      
      r2_text=stripwhite(gsub('(;|3a|:).*$','',r2_text)),
      
      r1_text=stripwhite(gsub('(;|3a|:).*$','',r1_text)),
      
      # Event code: same as retrosheet
      event_cd=case_when(
        sub_fl==1 ~ 1,
        str_sub(stripwhite(tmp_text),1,1)=='(' ~ 1,
        str_detect(tmp_text, '(hitting out of turn| for |No play|halted|delay|postponed|ejected|suspended|coach|sunny|review|challenged|HC|\\*\\*)') == TRUE ~ 1,
        str_detect(tmp_text,'struck out') == TRUE ~ 3,
        str_detect(tmp_text,'stole') == TRUE ~ 4,
        (str_detect(tmp_text,'(caught stealing|out at second c to|out at third c to)') == TRUE) & (str_detect(tmp_text,'(bunt|grounded)') == FALSE) ~ 6,
        str_detect(tmp_text,'picked off') == TRUE ~ 8,
        str_detect(tmp_text,'wild pitch') == TRUE ~ 9,
        str_detect(tmp_text,'passed ball') == TRUE ~ 10,
        str_detect(tmp_text,'balk') == TRUE ~ 11,
        str_detect(tmp_text,'Dropped foul') == TRUE ~ 13,
        str_detect(tmp_text,'walked') == TRUE ~ 14,
        str_detect(tmp_text,'hit by pitch') == TRUE ~ 16,
        str_detect(tmp_text,'interference') == TRUE ~ 17,
        str_detect(tmp_text,'error') == TRUE ~ 18,
        str_detect(tmp_text,'muffed') == TRUE ~ 18,
        str_detect(tmp_text,'dropped') == TRUE ~ 18,
        str_detect(tmp_text,'fielder\'s choice') == TRUE ~ 19,
        str_detect(tmp_text,'singled') == TRUE ~ 20,
        str_detect(tmp_text,'doubled') == TRUE ~ 21,
        str_detect(tmp_text,'tripled') == TRUE ~ 22,
        str_detect(tmp_text,'homered') == TRUE ~ 23,
        str_detect(tmp_text, '(flied out|grounded out|popped|fouled out|lined out| infield fly|double play|triple play|out at (first|second|third|home))') == TRUE ~ 2,
        str_detect(tmp_text, 'advanced') == TRUE ~ 12,
        TRUE ~ 0),
      
      
      # Bat name
      bat_name= case_when(
        event_cd %in% c(0,1)~'',
        str_detect(bat_text, '(Batter|Runner\'s interference)')==TRUE ~'',
        str_detect(bat_text, '(walked|singled|doubled|tripled|reached|struck out|grounded out)')==FALSE & str_detect(bat_text, '(advanced|caught stealing|stole|picked off|out at (first|second|third|home)|tagged out)')==TRUE ~ '',
        str_detect(bat_text, '(singled|doubled|tripled|homered|walked|reached|struck out|grounded|flied|lined|popped|hit | out |fouled out|pinch hit|infield fly|intentionally walked|was intentionally walked|fouled into double play)')==TRUE ~ gsub('((singled|doubled|tripled|homered|walked|reached|struck out|grounded|flied|lined|popped|hit | out |fouled out|pinch hit|infield fly|intentionally walked|was intentionally walked|fouled into double play).*$)', '', bat_text),
        str_detect(stripwhite(r1_text), 'caught stealing  c to (2b|3b), double play.')==TRUE ~ bat_text,
        TRUE ~ ''),
      
      #  Sub in
      sub_in= case_when(
        sub_fl==1&str_detect(bat_text, 'to (p|c|1b|2b|3b|ss|lf|rf|cf|dh)')==TRUE ~ stripwhite(gsub('(to (p|c|1b|2b|3b|ss|lf|rf|cf|dh).*$)', '', bat_text)),
        sub_fl==1&str_detect(bat_text, 'pinch ran for')==TRUE ~ stripwhite(gsub('pinch ran for.*$', '', bat_text)),
        sub_fl==1&str_detect(bat_text, 'pinch hit for')==TRUE ~ stripwhite(gsub('pinch hit for.*$', '', bat_text)),
        TRUE ~ ''),
      
      # Sub out
      sub_out= case_when(
        sub_fl==1&str_detect(bat_text, 'to (p|c|1b|2b|3b|ss|lf|rf|cf|dh) for')==TRUE ~ gsub('^.*to (p|c|1b|2b|3b|ss|lf|rf|cf|dh) for', '', bat_text),
        sub_fl==1&str_detect(bat_text, 'pinch ran for')==TRUE ~ gsub('^.*pinch ran for', '', bat_text),
        sub_fl==1&str_detect(bat_text, 'pinch hit')==TRUE ~ gsub('^.*pinch hit for', '', bat_text),
        TRUE ~ ''),
      # Clean sub out
      sub_out=strip_punc(sub_out),
      
      
      # Game end
      game_end = game_end(game_id),
      
      # New game
      new_game=new_game(game_end),
      
      # Top inning
      top_inning=ifelse(away_text=='', 0,1),
      # End of inning
      inn_end = inn_end(top_inning),
      # Runner names
      r1_name=r1_name(bat_text, bat_name, r1_text, r1_name, inn_end, game_end, sub_in, sub_out),
      r2_name =r2_name(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, inn_end, game_end, sub_in, sub_out),
      r3_name =r3_name(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, r3_text, r3_name, inn_end, game_end, sub_in, sub_out),
      # Clean runner names
      r1_name=replace(r1_name,is.na(r1_name),''),
      r2_name=replace(r2_name,is.na(r2_name),''),
      r3_name=replace(r3_name,is.na(r3_name),''),
      
      
      
      
      
      
      # Fix repeat bat names
      bat_name=case_when(
        bat_name!='' & stripwhite(bat_name)==stripwhite(r1_name)~ '',
        bat_name!='' & stripwhite(bat_name)==stripwhite(r2_name)~ '',
        bat_name!='' & stripwhite(bat_name)==stripwhite(r3_name)~ '',
        TRUE ~ bat_name),
      
      # 
      outs_on_play=case_when(
        event_cd %in% c(0,1) ~ 0,
        str_count(bat_text, 'triple play') == 1 ~ 3,
        str_count(bat_text, 'double play') == 1 ~ 2,
        (str_detect(bat_text, '( out|popped)') == TRUE) &  (str_detect(bat_text, '(reached)') == TRUE) ~ 0,
        # 1 out
        ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==TRUE)) ~ 1,
        #  2 outs
        ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==TRUE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==TRUE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == FALSE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==TRUE)) ~ 2,
        # 3 outs 
        ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==FALSE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==FALSE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==TRUE)) |
          ((str_detect(bat_text, '( out |popped|infield fly)') == TRUE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==FALSE) &(str_detect(r3_text, '( out |popped)')==TRUE)) |
          ((str_detect(bat_text, '( out |popped)') == FALSE) & (str_detect(r1_text, '( out |popped)')==TRUE) & (str_detect(r2_text, '( out |popped)')==TRUE) &(str_detect(r3_text, '( out |popped)')==TRUE)) ~ 3,
        TRUE ~ 0),
      
      # New inning
      new_inn=new_inn(inn_end),
      # Outs before
      outs_before=outs_before(outs_on_play, new_game, new_inn),
      # Outs after
      outs_after=outs_before+outs_on_play,
      
      # Base code
      base_cd_before=case_when(
        stripwhite(r1_name)!='' & r2_name=='' & r3_name=='' ~ 1,
        r1_name=='' & r2_name!='' & r3_name=='' ~ 2,
        r1_name!='' & r2_name!='' & r3_name=='' ~ 3,
        r1_name=='' & r2_name=='' & r3_name!='' ~ 4,
        r1_name!='' & r2_name=='' & r3_name!='' ~ 5,
        r1_name=='' & r2_name!='' & r3_name!='' ~ 6,
        r1_name!='' & r2_name!='' & r3_name!='' ~ 7,
        TRUE~0),
      
      # Batting order
      bat_order=bat_order_id(new_game, top_inning, bat_name),
      
      # Hit type
      hit_type=case_when(
        event_cd==3 ~ 'K',
        str_detect(bat_text,'(bunt)')==TRUE ~ 'B',
        str_detect(bat_text, '(bunt)')==FALSE & str_detect(bat_text, '(SAC)')==TRUE & str_detect(bat_text, '(flied|popped)')==FALSE ~ 'B',
        str_detect(bat_text,'(grounded out|(p|3b|2b|ss|1b) to (p|3b|2b|ss|1b|c))')==TRUE ~ 'GO',
        str_detect(bat_text,'(flied|fouled out to (lf|rf))')==TRUE ~ 'FO',
        str_detect(bat_text,'(lined)')==TRUE ~ 'LO',
        str_detect(bat_text,'(popped|infield fly|fouled out to (p|3b|2b|ss|1b|c))')==TRUE ~ 'PO',
        TRUE ~ ''   ),
      
      # Runs on play
      runs_on_play=(as.numeric(str_count(tmp_text, '(advanced to home)'))+as.numeric(str_count(tmp_text, '(scored)')) + as.numeric(str_count(tmp_text, '(homered)')) + as.numeric(str_count(tmp_text, '(stole home)'))-as.numeric(str_count(tmp_text, '(scored, scored)'))),
      
      # Away score
      away_score=score_before(new_game, runs_on_play, top_inning, home_team=0),
      
      # Home score
      home_score=score_before(new_game, runs_on_play, top_inning, home_team=1),
      
      # #  Away score after
      away_score_after=case_when(
        top_inning==1 ~away_score+ runs_on_play,
        TRUE ~ away_score),
      
      # # Home score after
      home_score_after=case_when(
        top_inning==0 ~home_score+ runs_on_play,
        TRUE ~ home_score),
      
      # Runs this inning
      runs_this_inn=runs_this_inn(inn_end, runs_on_play),
      
      # Runs rest of inning
      runs_roi=runs_rest_of_inn(inn_end,runs_on_play, runs_this_inn),
      
      # Intentional walk 
      int_bb_fl=case_when(
        str_detect(tmp_text,'intentionally ') == TRUE ~ 1,
        TRUE ~ 0
      ),
      
      # Sac bunts
      sh_fl=case_when(
        str_detect(bat_text, '(SAC)')==TRUE & str_detect(bat_text, '(flied|popped)')==FALSE ~ 1,
        TRUE~0),
      
      # Sac flys
      sf_fl=case_when(
        str_detect(bat_text, '(SAC)')==TRUE & str_detect(bat_text, '(flied|popped)')==TRUE ~ 1,
        str_detect(bat_text, '(SAC)')==FALSE & str_detect(bat_text, '(flied|popped)')==TRUE & str_detect(bat_text, '(RBI)')==TRUE~1,
        TRUE~0 )
    )
  
  
  pbp_data_frame=pbp_data_frame%>%
    mutate(bat_order=bat_order_fill(bat_order, game_end))%>%
    select(year,game_id,away_team,home_team,inning,top_inning,away_score,home_score,away_text,home_text,bat_order,bat_name,r1_name,r2_name,r3_name,sub_fl,sub_in,sub_out,
           base_cd_before,outs_before,event_cd,hit_type,outs_on_play,outs_after,runs_this_inn, runs_roi,runs_on_play, away_score_after,home_score_after,new_inn,inn_end,new_game,game_end,everything())%>%
    select(-tmp_text, -bat_text, -r1_text, -r2_text, -r3_text)
  
  return(pbp_data_frame)
}


### Helper Functions

stripwhite <- function(x) gsub("\\s*$", "", gsub("^\\s*", "", x))

strip_punc <- function(x){ 
  x=stripwhite(x)
  x=ifelse(str_sub(x,-1)=='.',gsub("\\.", "", x),x)
  return(x)}


inn_end = function(top_inn){
  m=length(top_inn)
  inn_end=integer(m)
  for (i in 1:(m-1)){
    inn_end[i]=ifelse(top_inn[i]!=top_inn[i+1], 1,0)
  }
  inn_end[m]=1
  return(inn_end)
}


game_end = function(game_id){
  m=length(game_id)
  game_end=integer(m)
  for (i in 2:m){
    if (game_id[i]!=game_id[i-1]){
      game_end[i-1]=1
    }
    game_end[m]=1
  }
  return(game_end)
}




runs_on_play= function(a_txt, h_txt, a_score,h_score){
  m=length(a_txt)
  runs_on_play=integer(m)
  runs_on_play[1]=a_score[1]
  for (i in 2:m){
    runs_on_play[i]=case_when(
      a_txt[i]=='' ~ as.integer(h_score[i]-h_score[i-1]),
      a_txt[i]!='' ~ as.integer(a_score[i]-a_score[i-1])
    )
  }
  return(runs_on_play)
}


r1_name = function(bat_text, bat_name, r1_text, r1_name, inn_end, game_end, sub_in, sub_out){
  m=length(bat_text)
  r1_name= character(m)
  for (i in 2:m){
    if (inn_end[i-1]==0 & game_end[i-1]==0){
      r1_name[i]=case_when(
        sub_out[i-1]!=''&sub_out[i-1]==stripwhite(r1_name[i-1])~sub_in[i-1],
        (str_detect(bat_text[i-1], '(singled|walked|hit by pitch|reached)') == TRUE) & (str_detect(bat_text[i-1], '(doubled|tripled|homered|advanced|scored|out|stole)') == FALSE) ~ bat_name[i-1],
        (str_detect(bat_text[i-1], '(reached first)') == TRUE) & (str_detect(bat_text[i-1], '(struck out)') == TRUE) ~ bat_name[i-1],
        (r1_text[i-1]==''|(str_detect(r1_text[i-1], '(advanced to second|stole second|advanced to third|stole third|scored|out)') == FALSE)) & (str_detect(bat_text[i-1], '(double play|advanced to second|stole second|advanced to third|stole third|scored|caught stealing|picked off|homered)') == FALSE)  ~ r1_name[i-1],
        (str_detect(bat_text[i-1], '(singled|doubled|tripled|advanced to second|stole second|advanced to third|stole third|scored|homered|out at second c to)') == FALSE) & (str_detect(r1_text[i-1], '(advanced to third|stole third|scored|out at third)') == TRUE) & stripwhite(gsub('((advanced to second|stole second|stole third|advanced to third|scored|out).*$)', '', r1_text[i-1]))!=stripwhite(gsub('((singled|reached).*$)', '', r1_name[i-1])) ~ r1_name[i-1],
        r1_text[i-1]=='' & stripwhite(gsub('((advanced to second|stole second|stole third|advanced to third|scored|out|failed|Failed|picked off).*$)', '', bat_text[i-1]))!=stripwhite(r1_name[i-1]) ~ r1_name[i-1]
      )}}
  return(stripwhite(r1_name))
}


r2_name = function(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name,  inn_end, game_end, sub_in, sub_out){
  m=length(bat_text)
  r2_name= character(m)
  for (i in 2:m){
    if (inn_end[i-1]==0 & game_end[i-1]==0){
      r2_name[i]=case_when(
        sub_out[i-1]!=''&sub_out[i-1]==stripwhite(r2_name[i-1])~sub_in[i-1],
        ((str_detect(bat_text[i-1], '(doubled|advanced to second|stole second)') == TRUE) & (str_detect(bat_text[i-1], '(advanced to third|scored|out|stole third)') == FALSE)) ~ stripwhite(gsub('((doubled|advanced to second|stole second).*$)', '', bat_text[i-1])),
        ((str_detect(r1_text[i-1], '(advanced to second|stole second)') == TRUE) & (str_detect(r1_text[i-1], '(advanced to third|scored|out|stole third)') == FALSE)) ~ stripwhite(gsub('((advanced to second|stole second).*$)', '', r1_text[i-1])),
        r2_text[i-1]=='' & stripwhite(gsub('((stole third|advanced to third|scored|out).*$)', '', r1_text[i-1]))!=stripwhite(r2_name[i-1]) & (str_detect(bat_text[i-1], '(advanced to third|stole third|scored|picked off|caught stealing)') == FALSE) ~ r2_name[i-1],
        r2_text[i-1]=='' & stripwhite(gsub('((out on the play).*$)', '', r1_text[i-1]))!=stripwhite(r2_name[i-1]) & (str_detect(bat_text[i-1], '(double play)') == TRUE) ~ r2_name[i-1],
        r1_text[i-1]=='' & (str_detect(bat_text[i-1], '(stole third|advanced to third|scored|picked off|homered|caught stealing)') == FALSE) ~ r2_name[i-1],
        sub_out[i-1]!=''&sub_out[i-1]==stripwhite(r2_name[i-1])~sub_in[i-1]
      )
      r2_name[i]=stripwhite(gsub('((singled|reached).*$)', '', r2_name[i]))
    }
  }
  return(stripwhite(r2_name))
}


r3_name = function(bat_text, bat_name, r1_text, r1_name, r2_text, r2_name, r3_text, r3_name, inn_end, game_end, sub_in, sub_out){
  m=length(bat_text)
  r3_name= character(m)
  for (i in 2:m){
    if (inn_end[i-1]==0 & game_end[i-1]==0){
      r3_name[i]=case_when( 
        sub_out[i-1]!=''&sub_out[i-1]==stripwhite(r3_name[i-1])~sub_in[i-1],
        ((str_detect(bat_text[i-1], '(tripled|advanced to third|stole third)') == TRUE) & (str_detect(bat_text[i-1], '(scored|out)') == FALSE)) ~ stripwhite(gsub('((tripled|advanced to third|stole third).*$)', '', bat_text[i-1])),
        ((str_detect(r1_text[i-1], '(advanced to third|stole third)') == TRUE) & (str_detect(r1_text[i-1], '(scored|out)') == FALSE)) ~ stripwhite(gsub('((advanced to third|stole third).*$)', '', r1_text[i-1])),
        ((str_detect(r2_text[i-1], '(advanced to third|stole third)') == TRUE) & (str_detect(r2_text[i-1], '(scored|out)') == FALSE)) ~ stripwhite(gsub('((advanced to third|stole third).*$)', '', r2_text[i-1])),
        r1_text[i-1]=='' & (str_detect(bat_text[i-1], '(scored|stole home|homered)') == FALSE) ~ r3_name[i-1],
        r2_text[i-1]=='' & stripwhite(gsub('((scored|stole home|out).*$)', '', r1_text[i-1]))!=stripwhite(r3_name[i-1]) & (str_detect(bat_text[i-1], '(scored|stole home)') == FALSE) ~ r3_name[i-1],
        r3_text[i-1]=='' & (str_detect(r2_text[i-1], '(scored|stole home|out)') == FALSE) & (str_detect(r1_text[i-1], '(scored|stole home|out)') == FALSE) & (str_detect(bat_text[i-1], '(scored|stole home)') == FALSE) ~ r3_name[i-1])
      r3_name[i]=stripwhite(gsub('((singled|doubled|reached|advanced|stole|failed|Failed|picked off).*$)', '', r3_name[i]))
    }
  }
  return(stripwhite(r3_name))
}


new_game=function(game_end){
  m = length(game_end)
  new_game=integer(m)
  new_game[1]=1
  for (i in 2:m){
    new_game[i]=game_end[i-1]
  }
  return(new_game)
}

new_inn=function(inn_end){
  m = length(inn_end)
  new_inn=integer(m)
  new_inn[1]=1
  for (i in 2:m){
    new_inn[i]=inn_end[i-1]
  }
  return(new_inn)
}


outs_before= function(outs_on_play, new_game, new_inn){
  m=length(outs_on_play)
  inn_outs=integer(m)
  for (i in 2:m){
    if (new_game[i]==0 & new_inn[i]==0){
      inn_outs[i]=((inn_outs[i-1]+outs_on_play[i-1]) %% 3)
    }
  }
  return(inn_outs)
}


score_before=function(new_game, runs_on_play, top_inning, home_team=1){
  m=length(new_game)
  home_score_before=integer(m)
  away_score_before=integer(m)
  for (i in 2:m){
    home_score_before[i]= case_when(
      new_game[i]==0 & top_inning[i-1]==0 ~ as.numeric(home_score_before[i-1]+runs_on_play[i-1]),
      new_game[i]==0 & top_inning[i-1]==1 ~ as.numeric(home_score_before[i-1]),
      TRUE ~ 0)
    
    away_score_before[i]= case_when(
      new_game[i]==0 & top_inning[i-1]==1 ~ as.numeric(away_score_before[i-1]+runs_on_play[i-1]),
      new_game[i]==0 & top_inning[i-1]==0 ~ as.numeric(away_score_before[i-1]),
      TRUE ~ 0)
  }
  if(home_team==1){
    return(home_score_before)  
  }
  else{return(away_score_before)}
  
}

runs_play=function(home_score, away_score, home_score_before, away_score_before, top_inn){
  n=length(homescore)
  runs_play=integer(n)
  for (i in 2:n){
    case_when(top_inn[i]==0 ~ homescore[i]-homescore_before[i])
    if (top_inn[i]==0){
      runs_play[i]=homescore[i]-homescore_before[i]
    } else{
      runs_play[i]=roadscore[i]-roadscore_before[i]
    }
  }
  return(runs_play)
}


runs_this_inn=function(end_inn, runs_on_play){
  m=length(end_inn)
  runs=integer(m)
  endinnloc=c(0,grep(1,end_inn))
  numinns=length(endinnloc)
  
  
  for (j in 2:numinns){
    for (k in (endinnloc[j-1]+1):endinnloc[j]){
      runs[k]=sum(runs_on_play[(endinnloc[j-1]+1):endinnloc[j]])
    }
  }
  return(runs)
}


runs_rest_of_inn=function(end_inn, runs_on_play, runs_this_inn){
  m=length(end_inn)
  runs=integer(m)
  
  endinnloc=c(0,grep(1,end_inn))
  numinns=length(endinnloc)
  
  for (j in 2:numinns){
    for (k in (endinnloc[j-1]+1):endinnloc[j]){
      runs[k]=runs_this_inn[k]-sum(runs_on_play[(endinnloc[j-1]+1):(k)])
    }
  }
  runs=runs+runs_on_play
  return(runs)
}


bat_order_id=function(new_game, top_inn, bat_name){
  m=length(top_inn)
  
  batorder=integer(m)
  
  newgameloc=c(grep(1,new_game),(m+1))
  numgames=length(newgameloc)
  
  for (j in 2:(numgames)){  
    kk=0
    jj=0
    for (i in (newgameloc[j-1]):(newgameloc[j]-1)){
      
      if (top_inn[i]==1 & bat_name[i]!=''){
        batorder[i]=(kk %% 9)+1
        kk=kk+1
        
      }
      else if (top_inn[i]==0 & bat_name[i]!='') {
        batorder[i]=(jj %% 9)+1
        jj=jj+1
      }
      else {batorder[i]=''}
    }
  }
  return(batorder)
}


bat_order_fill=function(bat_order, end_game){
  m=length(bat_order)
  for (i in (m):2){
    if(is.na(bat_order[i-1])==TRUE & end_game[i-1]==0){
      bat_order[i-1]=bat_order[i]
    }
  }
  
  for (i in 2:m){
    if(is.na(bat_order[i])==TRUE){
      bat_order[i]=bat_order[i-1]
    }
  }
  return(bat_order)
}