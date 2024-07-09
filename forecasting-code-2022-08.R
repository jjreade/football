getTeams <- function(season=2023) {
  if (season==2022) {
    EPLteams <- c("Arsenal","Aston Villa","Bournemouth","Brentford","Brighton","Chelsea","Crystal palace",
                  "Everton","Fulham","Leeds","Leicester","Liverpool","Man City","Man Utd",
                  "Newcastle","Nottm Forest","Southampton","Tottenham","West Ham","Wolves")
    EFLCteams <- c("Birmingham","Blackburn","Blackpool",
                   "Bristol C","Burnley","Cardiff","Coventry","Huddersfield","Hull",
                   "Luton","Middlesbrough","Millwall","Norwich",
                   "Preston","Qpr","Reading","Rotherham","Sheff Utd",
                   "Stoke","Sunderland","Swansea","Watford","West Brom","Wigan")
    EFL1teams <- c("Accrington","Barnsley","Bolton","Bristol R","Burton","Cambridge U","Charlton",
                   "Cheltenham",
                   "Derby","Doncaster","Exeter","Fleetwood","Forest Green","Ipswich",
                   "Lincoln","Morecambe","Mk Dons","Oxford","Peterborough","Plymouth",
                   "Portsmouth","Sheff Wed","Shrewsbury","Wycombe")
    EFL2teams <- c("AFC W’bledon","Barrow","Bradford","Carlisle",
                   "Colchester","Crawley","Crewe","Gillingham","Grimsby",
                   "Harrogate","Hartlepool","Leyton Orient","Mansfield","Northampton",
                   "Newport Co","Port Vale","Rochdale","Salford",
                   "Stevenage","Stockport","Sutton Utd","Swindon","Tranmere","Walsall")
    NLteams <- c("Aldershot","Altrincham","Barnet","Boreham W","Bromley","Chesterfield",
                 "Dag &Amp; Red","Dorking W","Eastleigh","Gateshead","Halifax",
                 "Maidenhead","Maidstone","Notts Co","Oldham","Scunthorpe","Solihull M","Southend",
                 "Torquay","Wealdstone","Woking","Wrexham","Yeovil","York")
    
  }
  if (season==2023) {
    EPLteams <- c("Arsenal","Aston Villa","Bournemouth","Brentford","Brighton","Burnley",
                  "Chelsea","C Palace",
                  "Everton","Fulham","Liverpool","Luton","Man City","Man Utd",
                  "Newcastle","Nottm Forest","Sheff Utd","Tottenham","West Ham","Wolves")
    EFLCteams <- c("Birmingham","Blackburn",
                   "Bristol C","Cardiff","Coventry","Huddersfield","Hull","Ipswich",
                   "Leeds","Leicester",
                   "Middlesbrough","Millwall","Norwich","Plymouth",
                   "Preston","Qpr","Rotherham","Southampton",
                   "Stoke","Sunderland","Swansea","Watford","West Brom")
    EFL1teams <- c("Barnsley","Blackpool","Bolton","Bristol R","Burton","Cambridge U","Carlisle",
                    "Charlton","Cheltenham",
                    "Derby","Doncaster","Exeter","Fleetwood","Leyton Orient",
                    "Lincoln","Northampton","Oxford","Peterborough",
                    "Portsmouth","Reading","Sheff Wed","Shrewsbury","Stevenage","Wigan","Wycombe")
    EFL2teams <- c("Accrington","AFC W’bledon","Barrow","Bradford",
                    "Colchester","Crawley","Crewe","Forest Green","Gillingham","Grimsby",
                    "Harrogate","Mansfield","Mk Dons","Morecambe",
                    "Newport Co","Notts Co","Port Vale","Salford",
                    "Stockport","Sutton Utd","Swindon","Tranmere","Walsall","Wrexham")
    NLteams <- c("Aldershot","Altrincham","Barnet","Boreham W","Bromley","Chesterfield",
                  "Dag &Amp; Red","Dorking W","Eastleigh","Ebbsfleet","Fylde","Gateshead",
                  "Halifax","Hartlepool","Kidderminster","Maidenhead","Oldham","Oxford C",
                  "Rochdale","Solihull M","Southend","Wealdstone","Woking","York")
    NLNteams <- c("King’s Lynn","Chester","Brackley","Alfreton","Gloucester","Scunthorpe",
                   "Scarborough A","Spennymoor","Darlington","Buxton","Chorley","Curzon A",
                   "Peterborough Sports","Boston Utd","Hereford","Banbury","Southport",
                   "Blyth Sptns","Farsley AFC","Bishops Stortford","Rushall Oly.",
                   "Tamworth","Warrington Town")
    NLSteams <- c("Torquay","Yeovil","Maidstone","Dartford","Worthing","Chelmsford","St Albans",
                   "Braintree","Eastbourne","Tonbridge","Havant &amp; W","Bath City","Farnborough",
                   "Chippenham","Taunton Town","Hemel","Welling","Hampton &amp; R","Slough",
                   "Weymouth","Dover","Aveley","Weston S-M.","Truro City")
  }

  allteams <- list("EPLteams"=EPLteams,"EFLCteams"=EFLCteams,"EFL1teams"=EFL1teams,
                   "EFL2teams"=EFL2teams,"NLteams"=NLteams,"NLNteams"=NLNteams,"NLSteams"=NLSteams)
  return(allteams)
}

load.transfers <- function() {
  transfers <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/transfers-sb.csv",stringsAsFactors = FALSE)
  
  transfers$team <- tolower(gsub("^(.*?)_(\\d+)[.]html$","\\1",transfers$filename))
  # transfers$season <- paste0(1870+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",transfers$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",transfers$filename))>145),"-",
  #                            1871+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",transfers$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",transfers$filename))>145))
  
  transfers$date <- as.Date(transfers$start.date,"%d %b, %Y")
  transfers$date[is.na(transfers$date)] <- as.Date(transfers$end[is.na(transfers$date)],"%d %b, %Y")

  transfers$season <- as.numeric(format(transfers$date,"%Y")) - as.numeric(as.numeric(format(transfers$date,"%m"))<7)
  transfers$season[format(transfers$date,"%Y")=="2020" & format(transfers$date,"%m") %in% c("06","07","08")] <- 2019
  
  transfers$players.in <- as.numeric(transfers$type=="in")
  transfers$players.out <- as.numeric(transfers$type=="out")
  transfers$loan.in <- as.numeric(transfers$type=="loaned.in")
  transfers$loan.out <- as.numeric(transfers$type=="loaned.out")
  transfers$releases <- as.numeric(transfers$type=="released")

  transfers$free.in <- as.numeric(transfers$type=="in" & transfers$fee=="Free")
  transfers$free.out <- as.numeric(transfers$type=="out" & transfers$fee=="Free")
  transfers$undisc.in <- as.numeric(transfers$type=="in" & transfers$fee=="Undisc.")
  transfers$undisc.out <- as.numeric(transfers$type=="out" & transfers$fee=="Undisc.")
  
  transfers$fee2 <- as.numeric(gsub(",","",gsub("£","",gsub("&pound;","",transfers$fee))))
  transfers$fee2[is.na(transfers$fee2)==TRUE] <- 0
  transfers$fees.paid <- as.numeric(transfers$players.in==1)*transfers$fee2
  transfers$fees.received <- as.numeric(transfers$players.out==1)*transfers$fee2
  
  transfers <- transfers[order(transfers$date),]
  
  transfers$team[regexpr("afc w",transfers$team)>-1] <- "afc wimbledon"
  transfers$team[regexpr("middlesbro",transfers$team)>-1] <- "middlesbrough"
  transfers$team[regexpr("palace",transfers$team)>-1] <- "crystal palace"
  transfers$team[regexpr("erzegbirge a",transfers$team)>-1] <- "erzegbirge a"
  return(transfers)
}

results.transfers <- function(results.data = res.data,transfer.data = transfers) {
  tr.teams <- sort(unique(transfers$team))
  
  res.transfer.m <- data.frame(stringsAsFactors = FALSE)
  res.transfer.j <- data.frame(stringsAsFactors = FALSE)
  for(tt in tr.teams) {
    #print(tt)
    team.tr.dates <- data.frame("date"=seq.Date(from=min(transfers$date[transfers$team==tt]),
                                                to=max(transfers$date[transfers$team==tt]),
                                                by="days"))
    team.tr.dates$season <- as.numeric(format(team.tr.dates$date,"%Y")) - as.numeric(as.numeric(format(team.tr.dates$date,"%m"))<7)
    team.tr.dates$team <- tt
    team.tr.dates <- merge(team.tr.dates,
                           transfers[transfers$team==tt,c("date","players.in","players.out",
                                                          "loan.in","loan.out","releases",
                                                          "fees.paid","fees.received")],
                           by=c("date"),all.x=TRUE)
    #  team.tr.dates <- merge(team.tr.dates,
    #                          transfers[transfers$team==tt,c("to","coach","coach.id")],
    #                          by.x=c("date"),by.y=c("to"),all.x=TRUE,suffixes=c(".from",".to"))
    
    team.matches.h <- results.data[tolower(results.data$team1)==tt,c("match_id","date","team2","goals1","goals2","outcome","div_id")]
    colnames(team.matches.h)[3] <- "opp" #need to ensure team2 is third column
    team.matches.a <- results.data[tolower(results.data$team2)==tt,c("match_id","date","team1","goals1","goals2","outcome","div_id")]
    team.matches.a$outcome <- 1 - team.matches.a$outcome
    temp <- team.matches.a$goals1
    team.matches.a$goals1 <- team.matches.a$goals2
    team.matches.a$goals2 <- temp
    colnames(team.matches.a)[3] <- "opp" #need to ensure team1 is third column
    team.matches <- rbind(team.matches.h,team.matches.a)
    team.matches$date <- as.Date(team.matches$date)
    team.tr.dates <- merge(team.tr.dates,team.matches[,c("date","match_id")],by=c("date"),all.x=TRUE)
    
    if(any(!is.na(team.tr.dates$match_id))==TRUE) {
      team.tr.dates <- team.tr.dates[order(team.tr.dates$date),]
      
      ##now to get for all transfers since start of previous June 
      team.tr.dates$players.in[is.na(team.tr.dates$players.in)] <- 0
      team.tr.dates$players.out[is.na(team.tr.dates$players.out)] <- 0
      team.tr.dates$loan.in[is.na(team.tr.dates$loan.in)] <- 0
      team.tr.dates$loan.out[is.na(team.tr.dates$loan.out)] <- 0
      team.tr.dates$releases[is.na(team.tr.dates$releases)] <- 0
      team.tr.dates$fees.paid[is.na(team.tr.dates$fees.paid)] <- 0
      team.tr.dates$fees.received[is.na(team.tr.dates$fees.received)] <- 0
      ##collapse so that one observation per day, summing up multiple transfers registered on a single day
      team.tr.dates1 <- aggregate(team.tr.dates[,c("players.in","players.out",
                                                   "loan.in","loan.out","releases",
                                                   "fees.paid","fees.received")],
                                  by=list(team.tr.dates$date),FUN=sum)
      team.tr.dates1$season <- as.numeric(format(team.tr.dates1$Group.1,"%Y")) - as.numeric(as.numeric(format(team.tr.dates1$Group.1,"%m"))<7)
      #count cumulative transfers since start of june (rather than start of july the usual season "start")
      team.tr.dates1$cplayers.in <- ave(team.tr.dates1$players.in,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cplayers.out <- ave(team.tr.dates1$players.out,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cloan.in <- ave(team.tr.dates1$loan.in,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cloan.out <- ave(team.tr.dates1$loan.out,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$creleases <- ave(team.tr.dates1$releases,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cfees.paid <- ave(team.tr.dates1$fees.paid,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cfees.received <- ave(team.tr.dates1$fees.received,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$team <- tt
      res.transfer.j <- rbind(res.transfer.j,team.tr.dates1)
      
      ##now want transfer activity since last match
      team.tr.dates <- team.tr.dates[order(-as.numeric(team.tr.dates$date)),]
      team.tr.dates$match_id <- na.locf(team.tr.dates$match_id,na.rm = FALSE)
      team.tr.dates <- team.tr.dates[order(team.tr.dates$date),]
      
      team.tr.last.m <- aggregate(team.tr.dates[,c("players.in","players.out",
                                                   "loan.in","loan.out","releases",
                                                   "fees.paid","fees.received")],
                                  by=list(team.tr.dates$match_id),FUN=sum,na.rm=TRUE)
      team.tr.last.m$team <- tt
      res.transfer.m <- rbind(res.transfer.m,team.tr.last.m)
    }
  }
  res.transfer.m$total.in <- res.transfer.m$players.in + res.transfer.m$loan.in
  res.transfer.m$total.out <- res.transfer.m$players.out + res.transfer.m$loan.out + res.transfer.m$releases
  res.transfer.j$jtotal.in <- res.transfer.j$cplayers.in + res.transfer.j$cloan.in
  res.transfer.j$jtotal.out <- res.transfer.j$cplayers.out + res.transfer.j$cloan.out + res.transfer.j$creleases
  colnames(res.transfer.j) <- gsub("cfees","jfees",colnames(res.transfer.j))
  results.data$team1.lc <- tolower(results.data$team1)
  results.data$team2.lc <- tolower(results.data$team2)
  results.data$date <- as.Date(results.data$date)
  results.data <- merge(results.data,res.transfer.m[,c("team","Group.1","total.in","total.out","fees.paid","fees.received")],
                        by.x=c("team1.lc","match_id"),by.y=c("team","Group.1"),all.x=TRUE)
  results.data <- merge(results.data,res.transfer.m[,c("team","Group.1","total.in","total.out","fees.paid","fees.received")],
                        by.x=c("team2.lc","match_id"),by.y=c("team","Group.1"),suffixes=c(".H",".A"),all.x=TRUE)
  results.data <- merge(results.data,res.transfer.j[,c("team","Group.1","jtotal.in","jtotal.out","jfees.paid","jfees.received")],
                        by.x=c("team1.lc","date"),by.y=c("team","Group.1"),all.x=TRUE)
  results.data <- merge(results.data,res.transfer.j[,c("team","Group.1","jtotal.in","jtotal.out","jfees.paid","jfees.received")],
                        by.x=c("team2.lc","date"),by.y=c("team","Group.1"),suffixes=c(".H",".A"),all.x=TRUE)
  results.data$team1.lc <- NULL
  results.data$team2.lc <- NULL
  return(results.data)
}

load.managers <- function() {
  #manloc <- "/Volumes/11330730-dp/Correct-score/data/soccerbase/"
  manloc <- "/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/"
  managers <- read.csv(paste0(manloc,"tenures.csv"),stringsAsFactors = FALSE)
  
  #sort out dates
  managers$from <- as.Date(managers$from,"%d %b, %Y")
  managers$to[managers$to=="Present"] <- format(Sys.Date(),"%d %b, %Y")#"21 Jan, 2015"
  managers$to <- as.Date(managers$to,"%d %b, %Y")
  managers$temp[managers$to<managers$from] <- managers$to[managers$to<managers$from]
  managers$to[managers$to<managers$from] <- managers$from[managers$to<managers$from]
  managers$from[managers$to<managers$from] <- managers$temp[managers$to<managers$from]
  managers$temp <- NULL
  
  ##teams
  managers$team <- tolower(gsub("[.]html","",managers$team))
  managers <- managers[managers$team!="afc fylde",]
  managers <- managers[managers$team!="afc w'don",]
  managers <- managers[managers$team!="afc wimbledon",]
  managers <- managers[managers$team!="c palace",]
  managers <- managers[managers$team!="chester fc",]
  managers <- managers[managers$team!="e. frankfurt",]
  managers <- managers[managers$team!="erzegbirge aue",]
  managers <- managers[managers$team!="gfc ajaccio",]
  managers <- managers[managers$team!="harrogate t",]
  managers <- managers[managers$team!="hereford fc",]
  managers <- managers[managers$team!="hungerford t",]
  managers <- managers[managers$team!="mgladbach",]
  managers <- managers[managers$team!="paris st-g.",]
  managers <- managers[managers$team!="solihull moors",]
  managers <- managers[managers$team!="sv darmstadt 98",]
  managers$team[regexpr("afc w",managers$team)>-1] <- "afc wimbledon"
  managers$to[managers$team=="northampton" & managers$coach=="Keith Curle"] <- as.Date("2021-02-10")
  managers$from[managers$team=="northampton" & managers$coach=="Jon Brady"] <- as.Date("2021-02-10")

  managers$from[managers$team=="stockport" & managers$coach=="David Ashworth"] <- as.Date("1914-05-01")
  managers$to[managers$team=="oldham" & managers$coach=="David Ashworth"] <- as.Date("1914-05-01")
  managers$to[managers$team=="oldham" & managers$coach=="David Unsworth"] <- as.Date("2023-09-17")
  # managers$to[managers$team=="oldham" & managers$coach=="John Sheridan" & managers$to==Sys.Date()] <- as.Date("2022-09-17")
#  managers <- rbind(managers,c("oldham","1924","2302","David Unsworth","2022-09-20",as.character(Sys.Date())))
  managers$coach[managers$coach=="John Eyres"]="John Sheridan"
  managers$coach[managers$coach=="Billy Ursom"]="John Sheridan"
  
  managers$Duration <- managers$to - managers$from
  managers$Date.Started <- managers$from
  managers$team <- gsub("afc wimbledon","afc w'bledon",managers$team)

  return(managers)
}

results.managers <- function(results.data = res0.att,manager.data = managers) {
  if("soccerbase.team.1" %in% colnames(results.data)) {
    results.data$old.team1 <- results.data$team1
    results.data$team1 <- results.data$soccerbase.team.1
  }
  if("soccerbase.team.2" %in% colnames(results.data)) {
    results.data$old.team2 <- results.data$team2
    results.data$team2 <- results.data$soccerbase.team.2
  }
  results.data$team1 <- tolower(results.data$team1)
  results.data$team2 <- tolower(results.data$team2)
  results.data$outcome <- as.numeric(results.data$goals1>results.data$goals2) + 0.5*as.numeric(results.data$goals1==results.data$goals2)
  
  manager.data$team <- gsub("crystal ","c ",manager.data$team)
  
  man.teams0 <- sort(unique(manager.data$team))
  man.teams1 <- sort(unique(c(results.data$team1,results.data$team2)))
  man.teams <- man.teams1[man.teams1 %in% man.teams0]
  
  res.manager <- data.frame(stringsAsFactors = FALSE)
  for(tt in man.teams) {
    print(tt)
    team.man.dates <- data.frame("date"=seq.Date(from=min(manager.data$from[manager.data$team==tt]),
                                                 to=max(manager.data$to[manager.data$team==tt]),
                                                 by="days"))
    team.man.dates$team <- tt
    team.man.dates <- merge(team.man.dates,
                            manager.data[manager.data$team==tt,c("from","coach","coach.id","Date.Started","Duration")],
                            by.x=c("date"),by.y=c("from"),all.x=TRUE)
    #  team.man.dates <- merge(team.man.dates,
    #                          manager.data[manager.data$team==tt,c("to","coach","coach.id")],
    #                          by.x=c("date"),by.y=c("to"),all.x=TRUE,suffixes=c(".from",".to"))
    team.man.dates$coach <- na.locf(team.man.dates$coach)
    team.man.dates$coach.id <- na.locf(team.man.dates$coach.id)
    team.man.dates$tenure.days <- sequence(rle(team.man.dates$coach.id)$lengths)
    team.man.dates$Duration <- na.locf(team.man.dates$Duration)
    team.man.dates$Date.Started <- na.locf(team.man.dates$Date.Started)
    
    team.matches.h <- results.data[is.na(results.data$team1)==FALSE & tolower(results.data$team1)==tt,c("match_id","date","team2","goals1","goals2","outcome","div_id")]
    colnames(team.matches.h) <- c("match_id","date","opp","goals","oppgoals","outcome","div_id")
    team.matches.a <- results.data[is.na(results.data$team2)==FALSE & tolower(results.data$team2)==tt,c("match_id","date","team1","goals2","goals1","outcome","div_id")]
    colnames(team.matches.a) <- c("match_id","date","opp","goals","oppgoals","outcome","div_id")
    
    if(NROW(team.matches.h)>0) {
      team.matches.h$venue <- "home"
    }
    if(NROW(team.matches.a)>0) {
      team.matches.a$outcome <- 1 - team.matches.a$outcome
      #      team.matches.a$elopredict <- 1 - team.matches.a$elopredict
      team.matches.a$venue <- "away"
    }
    team.matches <- rbind(team.matches.h,team.matches.a)
    team.matches$date <- as.Date(team.matches$date)
    team.man.dates <- merge(team.man.dates,team.matches,by=c("date"))
    
    if(NROW(team.man.dates)>0) {
      ##manager lagged
      team.man.dates$coach.id.1 <- c(NA,team.man.dates$coach.id[-NROW(team.man.dates)])
      team.man.dates$new.coach <- as.numeric(team.man.dates$coach.id.1!=team.man.dates$coach.id)
      team.man.dates$new.coach[1] <- 1
      
      team.man.dates$coach.id.p1 <- c(team.man.dates$coach.id[-1],NA)
      team.man.dates$old.coach <- as.numeric(team.man.dates$coach.id.p1!=team.man.dates$coach.id)
      
      team.man.dates$played <- 1
      team.man.dates$won <- as.numeric(team.man.dates$outcome==1)
      team.man.dates$drawn <- as.numeric(team.man.dates$outcome==0.5)
      team.man.dates$lost <- as.numeric(team.man.dates$outcome==0)
      team.man.dates$cplayed <- ave(team.man.dates$played, team.man.dates$coach.id, FUN=cumsum)
      team.man.dates$cwon <- ave(team.man.dates$won, team.man.dates$coach.id, FUN=cumsum)
      team.man.dates$cdrawn <- ave(team.man.dates$drawn, team.man.dates$coach.id, FUN=cumsum)
      team.man.dates$cpoints <- 3*team.man.dates$cwon + team.man.dates$cdrawn
      team.man.dates$win.pc <- 100*team.man.dates$cwon/team.man.dates$cplayed
      team.man.dates$points.per.game <- team.man.dates$cpoints/team.man.dates$cplayed
      
      team.man.dates$won.first.game <- as.numeric(team.man.dates$cplayed==1 & team.man.dates$won==1)
      team.man.dates$lost.first.game <- as.numeric(team.man.dates$cplayed==1 & team.man.dates$lost==1)
      res.manager <- rbind(res.manager,team.man.dates)
    }
  }
  res.manager <- res.manager[order(res.manager$coach.id,res.manager$date),]
  res.manager$total.experience <- sequence(rle(res.manager$coach.id)$lengths)
  #  res.manager <- res.manager[order(res.manager$date),]
  res.manager <- res.manager[order(res.manager$team,res.manager$date),]
  res.manager$team.coach <- paste0(res.manager$team,"-",res.manager$coach)
  res.manager$spell.length <- sequence(rle(res.manager$team.coach)$lengths)
  res.manager$spell0 <- paste0(res.manager$coach.id,"-",res.manager$Date.Started,"-",substr(res.manager$team,1,3))
  res.manager$spell <- paste0(res.manager$coach,"-",res.manager$Date.Started)
  res.manager$new.manager <- as.numeric(res.manager$spell.length==1)
  #number of home matches and first home match for a manager
  res.manager$spell.length.H[res.manager$venue=="home"] <- sequence(rle(res.manager$team.coach[res.manager$venue=="home"])$lengths)
  res.manager$new.manager.H <- as.numeric(res.manager$spell.length.H==1)
  #nearness to end of tenure for manager
  res.manager <- res.manager[order(res.manager$team,res.manager$date,decreasing = TRUE),]
  res.manager$spell.length.to.end <- sequence(rle(res.manager$team.coach)$lengths)
  
  res.manager <- res.manager[order(res.manager$team,res.manager$date),]

  res.manager$season <- as.numeric(format(res.manager$date,"%Y")) - as.numeric(as.numeric(format(res.manager$date,"%m"))<7)
  res.manager$season[format(res.manager$date,"%Y")=="2022" & format(res.manager$date,"%m")=="07"] <- 2022
  
  res.manager$goals <- as.numeric(res.manager$goals)
  res.manager$oppgoals <- as.numeric(res.manager$oppgoals)
  
  return(res.manager)
}

updatedata2022 <- function(dbloc="/Volumes/11330730-dp/Correct-score/") {
  ##### load data up to current season #####
  #dbloc="/Volumes/11330730-dp/Correct-score/"
  dbloc="/Users/jjreade/Dropbox/Research/Sport/Correct-score"
  require(zoo)
  require(xtable)
  elorank10 <- read.csv(paste0(dbloc,"/data/elorank-2022-06-01.csv"),stringsAsFactors = FALSE)
  full.data0 <- read.csv(paste0(dbloc,"/data/res0-2022-06-01.csv"),stringsAsFactors = FALSE, encoding = "UTF-8")
  full.data0$date <- as.Date(full.data0$date)
  # full.data0 <- res0
  
  ##### load data updates #####
  update.files <- list.files("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/",pattern="^historical_results_202.*?.csv$")
  update.files <- sort(update.files[update.files>="historical_results_2022-06-01-2020.csv"])
  
  res.update <- data.frame()
  for(ff in update.files) {
    print(ff)
    temp <- read.csv(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/",ff),stringsAsFactors = FALSE)
    temp$division[temp$division==""] <- NA
    temp$div_id[temp$div_id=="n/a"] <- "-99"
    temp$div_id <- as.numeric(temp$div_id)
    temp$division <- na.locf(temp$division)
    temp$div_id <- na.locf(temp$div_id)
    temp$goals1 <- as.numeric(gsub("^.*?<em>(\\d+)</em>.*?<em>(\\d+)</em>.*?$","\\1",temp$goals1))
    temp$goals2 <- as.numeric(gsub("^.*?<em>(\\d+)</em>.*?<em>(\\d+)</em>.*?$","\\2",temp$goals2))
    res.update <- rbind(res.update,temp)
    temp <- NULL
  }
  res.update$date <- as.Date(res.update$date)
  res.update$team1 <- tolower(res.update$team1)
  res.update$team2 <- tolower(res.update$team2)
  
  ##### remove duplicate entries #####
  res.update <- res.update[duplicated(res.update)==FALSE,]
  
  ##### get rid of any matches that haven't occurred before today (i.e. have NAs in the score) #####
  res.update <- res.update[!(is.na(res.update$goals1)==TRUE & res.update$date<Sys.Date()),]
  
  ##### get rid of matches that are "winner matches" #####
  #res <- res[regexpr("winner",tolower(res$team1))>-1,]
  
  res.update$outcome <- 0.5*(res.update$goals1==res.update$goals2) + (res.update$goals1>res.update$goals2)
  
  res.update <- res.update[order(res.update$date),]
  res.update$elostrength10.1 <- NA
  res.update$elostrength10.2 <- NA
  res.update$elo10predict <- NA
  lg.tab.colnames <- c("pts1","pts2","wins1","wins2","draws1","draws2","pld1","pld2","gs1",
                       "gs2","gd1","gd2","pos1","pos2","form1","form2")
  
  ##### current league table calculation #####
  res.tab.2022 <- data.frame()
  
  league.ids <- c(1:25,47:48,56,75,76,80,82,111:118,122:134,170:171,194,205:311,353)
  for( ll in league.ids) {
    print(ll)
    matches <- res.update[res.update$div_id==ll,c("match_id","date","team1","goals1","goals2","team2")]
    matches <- matches[is.na(matches$match_id)==FALSE,]
    if(NROW(matches)>10) {
      lge.all <- league.tab(matches$match_id,as.Date(matches$date),matches$team1,
                            matches$goals1,matches$goals2,matches$team2,3)
      lge <- lge.all$matches
      
      colnames(lge)[grep("game_id",colnames(lge))] <- "match_id"
      colnames(lge)[grep("g1",colnames(lge))] <- "goals1"
      colnames(lge)[grep("g2",colnames(lge))] <- "goals2"
      lge$season <- 2021
      lge$div_id <- ll
      res.tab.2022 <- rbind(res.tab.2022,lge)
    }
  }
  
  
  res.tab.2022 <- res.tab.2022[duplicated(res.tab.2022$match_id)==F,]
  res.tab.2022 <- res.tab.2022[order(res.tab.2022$match_id),]
  res.update <- res.update[order(res.update$date),] #order by date
  res.update <- res.update[duplicated(res.update$match_id)==FALSE,] #remove any duplicates such that most imminent match remains
  res.update <- res.update[order(res.update$match_id),] #order to now fill in with league tab data
  res.update <- merge(res.update,res.tab.2022[,c("match_id",lg.tab.colnames)],by=c("match_id"),all.x=TRUE)
  
  ##### merging previous seasons and current season data #####
  full.data0 <- full.data0[full.data0$division!="catalan league",]
  full.data0 <- full.data0[full.data0$division!="central spanish league",]
  full.data0$team1 <- tolower(full.data0$team1)
  full.data0$team2 <- tolower(full.data0$team2)
  res0.1 <- rbind(full.data0[,c("match_id","date","team1","team1_id","goals1","goals2","team2","team2_id",
                                "elostrength10.1","elostrength10.2","elo10predict","outcome","div_id","division",lg.tab.colnames)],
                  res.update[,c("match_id","date","team1","team1_id","goals1","goals2","team2","team2_id",
                                "elostrength10.1","elostrength10.2","elo10predict","outcome","div_id","division",lg.tab.colnames)])
  res0.1 <- res0.1[duplicated(res0.1[,c("match_id","date","team1","goals1","goals2","team2")])==FALSE,]
  res0.1 <- res0.1[!(is.na(res0.1$goals1)==TRUE & res0.1$date<Sys.Date()),]
  
  res0.1$season <- as.numeric(format(res0.1$date,"%Y")) - as.numeric(as.numeric(format(res0.1$date,"%m"))<7)
  
  ##team IDs from English top five divisions, top two in France, Spain, Italy and Germany
  wantedlges <- c(1:11,22,225,194,21,19,195,20,170,70,58) 
  
  ##saving file for scraping important team details each week
  #teamIDs <- res0.1[res0.1$div_id==70,c("team1","team1_id")]
  #colnames(teamIDs) <- c("team2","team2_id")
  #teamIDs <- rbind(teamIDs,res0.1[res0.1$div_id==70,c("team2","team2_id")])
  teamIDs <- res0.1[res0.1$div_id %in% wantedlges,c("team1","team1_id")]
  teamIDs <- teamIDs[duplicated(teamIDs)==FALSE,]
  #teamIDs <- teamIDs[regexpr("u\\d+",teamIDs$team2)>-1,]
  write.csv(teamIDs[order(teamIDs$team1_id),],"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/wanted-team-ids.csv")
  #write.csv(teamIDs[order(teamIDs$team2_id),],"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/wanted-u-team-ids.csv")
  
  ##FULL PYRAMID LEAGUE POSITION
  div.tot.teams0 <- res0.1[is.na(res0.1$div_id)==FALSE & duplicated(res0.1[,c("team1","div_id","season")])==FALSE & (res0.1$div_id<12 | res0.1$div_id==225 | res0.1$div_id==226) & res0.1$div_id>0,c("team1","div_id","season")]
  div.tot.teams0$tier <- div.tot.teams0$div_id
  div.tot.teams0$tier[div.tot.teams0$tier==5] <- 1
  div.tot.teams0$tier[div.tot.teams0$tier==6] <- 2
  div.tot.teams0$tier[div.tot.teams0$tier==7] <- 3
  div.tot.teams0$tier[div.tot.teams0$tier==8] <- 4
  div.tot.teams0$tier[div.tot.teams0$tier==9] <- 5
  div.tot.teams0$tier[div.tot.teams0$tier==225] <- 6.1
  div.tot.teams0$tier[div.tot.teams0$tier==226] <- 6.2
  div.tot.teams0$tier[div.tot.teams0$tier==80] <- 7
  div.tot.teams0$tier[div.tot.teams0$tier==75] <- 7
  div.tot.teams0$tier[div.tot.teams0$tier==56] <- 7
  div.tot.teams0$tier[div.tot.teams0$tier==10] <- 3.1
  div.tot.teams0$tier[div.tot.teams0$tier==11] <- 3.2
  
  div.tot.teams0$const <- 1
  
  div.tot.teams <- aggregate(div.tot.teams0$const,by=list(div.tot.teams0$tier,div.tot.teams0$season),FUN=sum)
  
  div.tot.teams1 <- div.tot.teams[div.tot.teams$Group.1==1,]
  div.tot.teams2 <- div.tot.teams[div.tot.teams$Group.1==2,]
  div.tot.teams3 <- div.tot.teams[div.tot.teams$Group.1==3,]
  div.tot.teams3.1 <- div.tot.teams[div.tot.teams$Group.1==3.1,]
  div.tot.teams3.2 <- div.tot.teams[div.tot.teams$Group.1==3.2,]
  div.tot.teams4 <- div.tot.teams[div.tot.teams$Group.1==4,]
  div.tot.teams5 <- div.tot.teams[div.tot.teams$Group.1==5,]
  div.tot.teams6.1 <- div.tot.teams[div.tot.teams$Group.1==6.1,]
  div.tot.teams6.2 <- div.tot.teams[div.tot.teams$Group.1==6.2,]
  
  div.tots <- merge(div.tot.teams1[c("Group.2","x")],
                    div.tot.teams2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".1",".2"))
  div.tots <- merge(div.tots,
                    div.tot.teams3[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams4[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3",".4"))
  div.tots <- merge(div.tots,
                    div.tot.teams3.1[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams3.2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3n",".3s"))
  div.tots <- merge(div.tots,
                    div.tot.teams5[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams6.1[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".5",".6n"))
  div.tots <- merge(div.tots,
                    div.tot.teams6.2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  
  div.tots$cum.1 <- 0
  div.tots$cum.2 <- div.tots$x.1
  div.tots$cum.3 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.1 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.2 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.4 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3
  div.tots$cum.5 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4
  div.tots$cum.6.1 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4 + div.tots$x.5
  div.tots$cum.6.2 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4 + div.tots$x.5
  div.tots$cum.7 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4 + div.tots$x.5 + (div.tots$x.6n + div.tots$x)/2
  
  div.tots$lg.total.n <- rowSums(div.tots[,c("x.1","x.2","x.3n","x.3","x.4")],na.rm=TRUE)
  div.tots$lg.total.s <- rowSums(div.tots[,c("x.1","x.2","x.3s","x.3","x.4")],na.rm=TRUE)
  
  divs <- c(1,2,3,3.1,3.2,4,5,6.1,6.2,7)
  teams.above <- data.frame(stringsAsFactors = FALSE)
  
  for(dd in divs) {
    temp <- div.tots[,c("Group.2",paste0("cum.",dd))]
    colnames(temp) <- c("season","teams.above")
    temp$tier <- dd
    teams.above <- rbind(teams.above,temp)
  }
  
  res0.1$tier <- NA
  res0.1$tier[res0.1$div_id==1] <- 1
  res0.1$tier[res0.1$div_id==2] <- 2
  res0.1$tier[res0.1$div_id==3] <- 3
  res0.1$tier[res0.1$div_id==4] <- 4
  res0.1$tier[res0.1$div_id==5] <- 1
  res0.1$tier[res0.1$div_id==6] <- 2
  res0.1$tier[res0.1$div_id==7] <- 3
  res0.1$tier[res0.1$div_id==8] <- 4
  res0.1$tier[res0.1$div_id==9] <- 5
  res0.1$tier[res0.1$div_id==10] <- 3.1
  res0.1$tier[res0.1$div_id==11] <- 3.2
  res0.1$tier[res0.1$div_id==225] <- 6.1
  res0.1$tier[res0.1$div_id==226] <- 6.2
  res0.1$tier[res0.1$div_id==80] <- 7
  res0.1$tier[res0.1$div_id==75] <- 7
  res0.1$tier[res0.1$div_id==56] <- 7
  
  
  res0.1 <- merge(res0.1,teams.above,by=c("season","tier"),all.x=TRUE)
  
  res0.1$full.pos1 <- res0.1$pos1 + res0.1$teams.above
  res0.1$full.pos2 <- res0.1$pos2 + res0.1$teams.above
  
  
  ##UPDATE ELO
  res0.1 <- res0.1[is.na(res0.1$date)==FALSE,]
  res0.1 <- res0.1[order(res0.1$date),]
  elo.date.start <- min(c(1:NROW(res0.1))[res0.1$date=="2022-06-01"])
  elo.weight=10
  for(mm in c(elo.date.start:NROW(res0.1))) {
    print(100*mm/NROW(res0.1))
    if(!(paste0("id",res0.1$team1_id[mm]) %in% names(elorank10))) {
      elorank10[[paste0("id",res0.1$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",res0.1$team2_id[mm]) %in% names(elorank10))) {
      elorank10[[paste0("id",res0.1$team2_id[mm])]] <- 1000
    }
    res0.1$elostrength10.1[mm] <- elorank10[[paste0("id",res0.1$team1_id[mm])]]
    res0.1$elostrength10.2[mm] <- elorank10[[paste0("id",res0.1$team2_id[mm])]]
    res0.1$elo10predict[mm] <- 1/(1+(10^((elorank10[[paste0("id",res0.1$team2_id[mm])]]-elorank10[[paste0("id",res0.1$team1_id[mm])]])/400)))
    adjustment <- res0.1$outcome[mm] - res0.1$elo10predict[mm]
    if(is.na(adjustment)==FALSE) {
      elorank10[paste0("id",res0.1$team1_id[mm])] <- elorank10[[paste0("id",res0.1$team1_id[mm])]] + elo.weight*adjustment
      elorank10[paste0("id",res0.1$team2_id[mm])] <- elorank10[[paste0("id",res0.1$team2_id[mm])]] - elo.weight*adjustment
    }
  }
  
  res0.1$team1 <- tolower(res0.1$team1)
  res0.1$team2 <- tolower(res0.1$team2)
  res0.1$team1[regexpr("afc w",res0.1$team1)>-1] <- "afc wimbledon"
  res0.1$team1[regexpr("middlesbro",res0.1$team1)>-1] <- "middlesbrough"
  res0.1$team1[regexpr("palace",res0.1$team1)>-1] <- "crystal palace"
  res0.1$team1[regexpr("erzegbirge a",res0.1$team1)>-1] <- "erzegbirge a"
  res0.1$team2[regexpr("afc w",res0.1$team2)>-1] <- "afc wimbledon"
  res0.1$team2[regexpr("middlesbro",res0.1$team2)>-1] <- "middlesbrough"
  res0.1$team2[regexpr("palace",res0.1$team2)>-1] <- "crystal palace"
  res0.1$team2[regexpr("erzegbirge a",res0.1$team2)>-1] <- "erzegbirge a"

  returnlist <- list("full.data"=res0.1,"elorank"=elorank,"league.tables"=res.tab.2020)
  return <- returnlist
}

updateresdata <- function() {
  ##### load data up to current season #####
  #dbloc="/Volumes/11330730-dp/Correct-score/"
  dbloc="/Users/jjreade/Dropbox/Research/Sport/Correct-score"
  require(zoo)
  require(xtable)
  elorank10 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/",stringsAsFactors = FALSE)
  full.data0 <- read.csv(paste0(dbloc,"/data/res0-2022-06-01.csv"),stringsAsFactors = FALSE, encoding = "UTF-8")
  full.data0$date <- as.Date(full.data0$date)
  # full.data0 <- res0

  ##### load data updates #####
  update.files <- list.files("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/",pattern="^historical_results_202.*?.csv$")
  update.files <- sort(update.files[update.files>="historical_results_2022-06-01-2020.csv"])
  
  res.update <- data.frame()
  for(ff in update.files) {
    print(ff)
    temp <- read.csv(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/",ff),stringsAsFactors = FALSE)
    temp$division[temp$division==""] <- NA
    temp$div_id[temp$div_id=="n/a"] <- "-99"
    temp$div_id <- as.numeric(temp$div_id)
    temp$division <- na.locf(temp$division)
    temp$div_id <- na.locf(temp$div_id)
    temp$goals1 <- as.numeric(gsub("^.*?<em>(\\d+)</em>.*?<em>(\\d+)</em>.*?$","\\1",temp$goals1))
    temp$goals2 <- as.numeric(gsub("^.*?<em>(\\d+)</em>.*?<em>(\\d+)</em>.*?$","\\2",temp$goals2))
    res.update <- rbind(res.update,temp)
    temp <- NULL
  }
  res.update$date <- as.Date(res.update$date)
  res.update$team1 <- tolower(res.update$team1)
  res.update$team2 <- tolower(res.update$team2)
  
  ##### remove duplicate entries #####
  res.update <- res.update[duplicated(res.update)==FALSE,]
  
  ##### get rid of any matches that haven't occurred before today (i.e. have NAs in the score) #####
  res.update <- res.update[!(is.na(res.update$goals1)==TRUE & res.update$date<Sys.Date()),]
  
  ##### get rid of matches that are "winner matches" #####
  #res <- res[regexpr("winner",tolower(res$team1))>-1,]
  
  res.update$outcome <- 0.5*(res.update$goals1==res.update$goals2) + (res.update$goals1>res.update$goals2)
  
  res.update <- res.update[order(res.update$date),]
  res.update$elostrength10.1 <- NA
  res.update$elostrength10.2 <- NA
  res.update$elo10predict <- NA
  lg.tab.colnames <- c("pts1","pts2","wins1","wins2","draws1","draws2","pld1","pld2","gs1",
                       "gs2","gd1","gd2","pos1","pos2","form1","form2")

  ##### current league table calculation #####
  res.tab.2022 <- data.frame()
  
  league.ids <- c(1:25,47:48,56,75,76,80,82,111:118,122:134,170:171,194,205:311,353)
  for( ll in league.ids) {
    print(ll)
    matches <- res.update[res.update$div_id==ll,c("match_id","date","team1","goals1","goals2","team2")]
    matches <- matches[is.na(matches$match_id)==FALSE,]
    if(NROW(matches)>10) {
      lge.all <- league.tab(matches$match_id,as.Date(matches$date),matches$team1,
                            matches$goals1,matches$goals2,matches$team2,3)
      lge <- lge.all$matches
      
      colnames(lge)[grep("game_id",colnames(lge))] <- "match_id"
      colnames(lge)[grep("g1",colnames(lge))] <- "goals1"
      colnames(lge)[grep("g2",colnames(lge))] <- "goals2"
      lge$season <- 2021
      lge$div_id <- ll
      res.tab.2022 <- rbind(res.tab.2022,lge)
    }
  }
  
  
  res.tab.2022 <- res.tab.2022[duplicated(res.tab.2022$match_id)==F,]
  res.tab.2022 <- res.tab.2022[order(res.tab.2022$match_id),]
  res.update <- res.update[order(res.update$date),] #order by date
  res.update <- res.update[duplicated(res.update$match_id)==FALSE,] #remove any duplicates such that most imminent match remains
  res.update <- res.update[order(res.update$match_id),] #order to now fill in with league tab data
  res.update <- merge(res.update,res.tab.2022[,c("match_id",lg.tab.colnames)],by=c("match_id"),all.x=TRUE)
  
  ##### merging previous seasons and current season data #####
  full.data0 <- full.data0[full.data0$division!="catalan league",]
  full.data0 <- full.data0[full.data0$division!="central spanish league",]
  full.data0$team1 <- tolower(full.data0$team1)
  full.data0$team2 <- tolower(full.data0$team2)
  res0.1 <- rbind(full.data0[,c("match_id","date","team1","team1_id","goals1","goals2","team2","team2_id",
                                "elostrength10.1","elostrength10.2","elo10predict","outcome","div_id","division",lg.tab.colnames)],
                    res.update[,c("match_id","date","team1","team1_id","goals1","goals2","team2","team2_id",
                            "elostrength10.1","elostrength10.2","elo10predict","outcome","div_id","division",lg.tab.colnames)])
  res0.1 <- res0.1[duplicated(res0.1[,c("match_id","date","team1","goals1","goals2","team2")])==FALSE,]
  res0.1 <- res0.1[!(is.na(res0.1$goals1)==TRUE & res0.1$date<Sys.Date()),]
  
  res0.1$season <- as.numeric(format(res0.1$date,"%Y")) - as.numeric(as.numeric(format(res0.1$date,"%m"))<7)

  ##team IDs from English top five divisions, top two in France, Spain, Italy and Germany
  wantedlges <- c(1:11,22,225,194,21,19,195,20,170,70,58) 
  
  ##saving file for scraping important team details each week
  #teamIDs <- res0.1[res0.1$div_id==70,c("team1","team1_id")]
  #colnames(teamIDs) <- c("team2","team2_id")
  #teamIDs <- rbind(teamIDs,res0.1[res0.1$div_id==70,c("team2","team2_id")])
  teamIDs <- res0.1[res0.1$div_id %in% wantedlges,c("team1","team1_id")]
  teamIDs <- teamIDs[duplicated(teamIDs)==FALSE,]
  #teamIDs <- teamIDs[regexpr("u\\d+",teamIDs$team2)>-1,]
  write.csv(teamIDs[order(teamIDs$team1_id),],"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/wanted-team-ids.csv")
  #write.csv(teamIDs[order(teamIDs$team2_id),],"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/wanted-u-team-ids.csv")
  
  ##FULL PYRAMID LEAGUE POSITION
  div.tot.teams0 <- res0.1[is.na(res0.1$div_id)==FALSE & duplicated(res0.1[,c("team1","div_id","season")])==FALSE & (res0.1$div_id<12 | res0.1$div_id==225 | res0.1$div_id==226) & res0.1$div_id>0,c("team1","div_id","season")]
  div.tot.teams0$tier <- div.tot.teams0$div_id
  div.tot.teams0$tier[div.tot.teams0$tier==5] <- 1
  div.tot.teams0$tier[div.tot.teams0$tier==6] <- 2
  div.tot.teams0$tier[div.tot.teams0$tier==7] <- 3
  div.tot.teams0$tier[div.tot.teams0$tier==8] <- 4
  div.tot.teams0$tier[div.tot.teams0$tier==9] <- 5
  div.tot.teams0$tier[div.tot.teams0$tier==225] <- 6.1
  div.tot.teams0$tier[div.tot.teams0$tier==226] <- 6.2
  div.tot.teams0$tier[div.tot.teams0$tier==80] <- 7
  div.tot.teams0$tier[div.tot.teams0$tier==75] <- 7
  div.tot.teams0$tier[div.tot.teams0$tier==56] <- 7
  div.tot.teams0$tier[div.tot.teams0$tier==10] <- 3.1
  div.tot.teams0$tier[div.tot.teams0$tier==11] <- 3.2
  
  div.tot.teams0$const <- 1
  
  div.tot.teams <- aggregate(div.tot.teams0$const,by=list(div.tot.teams0$tier,div.tot.teams0$season),FUN=sum)
  
  div.tot.teams1 <- div.tot.teams[div.tot.teams$Group.1==1,]
  div.tot.teams2 <- div.tot.teams[div.tot.teams$Group.1==2,]
  div.tot.teams3 <- div.tot.teams[div.tot.teams$Group.1==3,]
  div.tot.teams3.1 <- div.tot.teams[div.tot.teams$Group.1==3.1,]
  div.tot.teams3.2 <- div.tot.teams[div.tot.teams$Group.1==3.2,]
  div.tot.teams4 <- div.tot.teams[div.tot.teams$Group.1==4,]
  div.tot.teams5 <- div.tot.teams[div.tot.teams$Group.1==5,]
  div.tot.teams6.1 <- div.tot.teams[div.tot.teams$Group.1==6.1,]
  div.tot.teams6.2 <- div.tot.teams[div.tot.teams$Group.1==6.2,]
  
  div.tots <- merge(div.tot.teams1[c("Group.2","x")],
                    div.tot.teams2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".1",".2"))
  div.tots <- merge(div.tots,
                    div.tot.teams3[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams4[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3",".4"))
  div.tots <- merge(div.tots,
                    div.tot.teams3.1[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams3.2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3n",".3s"))
  div.tots <- merge(div.tots,
                    div.tot.teams5[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams6.1[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".5",".6n"))
  div.tots <- merge(div.tots,
                    div.tot.teams6.2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  
  div.tots$cum.1 <- 0
  div.tots$cum.2 <- div.tots$x.1
  div.tots$cum.3 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.1 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.2 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.4 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3
  div.tots$cum.5 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4
  div.tots$cum.6.1 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4 + div.tots$x.5
  div.tots$cum.6.2 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4 + div.tots$x.5
  div.tots$cum.7 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4 + div.tots$x.5 + (div.tots$x.6n + div.tots$x)/2
  
  div.tots$lg.total.n <- rowSums(div.tots[,c("x.1","x.2","x.3n","x.3","x.4")],na.rm=TRUE)
  div.tots$lg.total.s <- rowSums(div.tots[,c("x.1","x.2","x.3s","x.3","x.4")],na.rm=TRUE)
  
  divs <- c(1,2,3,3.1,3.2,4,5,6.1,6.2,7)
  teams.above <- data.frame(stringsAsFactors = FALSE)
  
  for(dd in divs) {
    temp <- div.tots[,c("Group.2",paste0("cum.",dd))]
    colnames(temp) <- c("season","teams.above")
    temp$tier <- dd
    teams.above <- rbind(teams.above,temp)
  }
  
  res0.1$tier <- NA
  res0.1$tier[res0.1$div_id==1] <- 1
  res0.1$tier[res0.1$div_id==2] <- 2
  res0.1$tier[res0.1$div_id==3] <- 3
  res0.1$tier[res0.1$div_id==4] <- 4
  res0.1$tier[res0.1$div_id==5] <- 1
  res0.1$tier[res0.1$div_id==6] <- 2
  res0.1$tier[res0.1$div_id==7] <- 3
  res0.1$tier[res0.1$div_id==8] <- 4
  res0.1$tier[res0.1$div_id==9] <- 5
  res0.1$tier[res0.1$div_id==10] <- 3.1
  res0.1$tier[res0.1$div_id==11] <- 3.2
  res0.1$tier[res0.1$div_id==225] <- 6.1
  res0.1$tier[res0.1$div_id==226] <- 6.2
  res0.1$tier[res0.1$div_id==80] <- 7
  res0.1$tier[res0.1$div_id==75] <- 7
  res0.1$tier[res0.1$div_id==56] <- 7
  
  
  res0.1 <- merge(res0.1,teams.above,by=c("season","tier"),all.x=TRUE)
  
  res0.1$full.pos1 <- res0.1$pos1 + res0.1$teams.above
  res0.1$full.pos2 <- res0.1$pos2 + res0.1$teams.above
  
  
  ##UPDATE ELO
  res0.1 <- res0.1[is.na(res0.1$date)==FALSE,]
  res0.1 <- res0.1[order(res0.1$date),]
  elo.date.start <- min(c(1:NROW(res0.1))[res0.1$date=="2022-06-01"])
  elo.weight=10
  for(mm in c(elo.date.start:NROW(res0.1))) {
    print(100*mm/NROW(res0.1))
    if(!(paste0("id",res0.1$team1_id[mm]) %in% names(elorank10))) {
      elorank10[[paste0("id",res0.1$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",res0.1$team2_id[mm]) %in% names(elorank10))) {
      elorank10[[paste0("id",res0.1$team2_id[mm])]] <- 1000
    }
    res0.1$elostrength10.1[mm] <- elorank10[[paste0("id",res0.1$team1_id[mm])]]
    res0.1$elostrength10.2[mm] <- elorank10[[paste0("id",res0.1$team2_id[mm])]]
    res0.1$elo10predict[mm] <- 1/(1+(10^((elorank10[[paste0("id",res0.1$team2_id[mm])]]-elorank10[[paste0("id",res0.1$team1_id[mm])]])/400)))
    adjustment <- res0.1$outcome[mm] - res0.1$elo10predict[mm]
    if(is.na(adjustment)==FALSE) {
      elorank10[paste0("id",res0.1$team1_id[mm])] <- elorank10[[paste0("id",res0.1$team1_id[mm])]] + elo.weight*adjustment
      elorank10[paste0("id",res0.1$team2_id[mm])] <- elorank10[[paste0("id",res0.1$team2_id[mm])]] - elo.weight*adjustment
    }
  }
  
  res0.1$team1 <- tolower(res0.1$team1)
  res0.1$team2 <- tolower(res0.1$team2)
  res0.1$team1[regexpr("afc w",res0.1$team1)>-1] <- "afc wimbledon"
  res0.1$team1[regexpr("middlesbro",res0.1$team1)>-1] <- "middlesbrough"
  res0.1$team1[regexpr("palace",res0.1$team1)>-1] <- "crystal palace"
  res0.1$team1[regexpr("erzegbirge a",res0.1$team1)>-1] <- "erzegbirge a"
  res0.1$team2[regexpr("afc w",res0.1$team2)>-1] <- "afc wimbledon"
  res0.1$team2[regexpr("middlesbro",res0.1$team2)>-1] <- "middlesbrough"
  res0.1$team2[regexpr("palace",res0.1$team2)>-1] <- "crystal palace"
  res0.1$team2[regexpr("erzegbirge a",res0.1$team2)>-1] <- "erzegbirge a"
  
  ##now want to merge in co-ordinates of teams to calculate distances
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  corsw <- corr[,c("soccerbase.team","worldfootball.net.team")]
  corsw <- corsw[duplicated(corsw)==FALSE,]
  corsw <- corsw[corsw$worldfootball.net.team!="",]
  corsw <- corsw[duplicated(corsw$worldfootball.net.team)==FALSE,]

  stads <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Deadly Games/Data/football-stadia-wf.csv",stringsAsFactors = FALSE)

  #colnames(stads) <- c("stadium","capacity","country","teams","teams2","location1","location2")
  stads$stadium <- gsub("[.]html","",stads$stadium)
  stads$country <- gsub("^.*?/>(.*?)$","\\1",stads$country)
  # stads[stads$stadium=="globe-arena-morecambe",c("teams1","location1","location2")] <- c("morecambe",54.061563,-2.867247)
  # stads[stads$stadium=="alexandra-stadium-crewe",c("location1","location2")] <- c(53.087475,-2.435768)
  # stads[stads$stadium=="dean-court-old-bournemouth",c("location1","location2")] <- c(50.73524,-1.838298)
  # stads[stads$stadium=="madejski-stadium-reading",c("location1","location2")] <- c(51.42234,-0.9827102)
  # stads[stads$stadium=="the-hive-london",c("location1","location2")] <- c(51.602599,-0.291785)
  # stads[stads$stadium=="victoria-road-dagenham",c("teams1","location1","location2")] <- c("dagenham-and-redbridge",51.547801,0.159868)
  stads <- merge(stads,corsw,by.x=c("teams1"),by.y=c("worldfootball.net.team"))
  stads$soccerbase.team <- tolower(stads$soccerbase.team)
  res0.1 <- merge(res0.1,stads[c("soccerbase.team","location1","location2")],by.x=c("team1"),
                  by.y="soccerbase.team",all.x=TRUE)
  res0.1 <- merge(res0.1,stads[c("soccerbase.team","location1","location2")],by.x=c("team2"),
                  by.y="soccerbase.team",all.x=TRUE,suffixes=c("H","A"))
  res0.1$location1H[res0.1$team1=="dag &amp; red"] <- "51.547801"
  res0.1$location2H[res0.1$team1=="dag &amp; red"] <- "0.159868"
  res0.1$location1A[res0.1$team2=="dag &amp; red"] <- "51.547801"
  res0.1$location2A[res0.1$team2=="dag &amp; red"] <- "0.159868"
  res0.1$location1H[res0.1$team1=="dorking w"] <- "51.23452"
  res0.1$location2H[res0.1$team1=="dorking w"] <- "-0.33347"
  res0.1$location1A[res0.1$team2=="dorking w"] <- "51.23452"
  res0.1$location2A[res0.1$team2=="dorking w"] <- "-0.33347"
  res0.1$location1H[res0.1$team1=="barnet"] <- "51.602599"
  res0.1$location2H[res0.1$team1=="barnet"] <- "-0.291785"
  res0.1$location1A[res0.1$team2=="barnet"] <- "51.602599"
  res0.1$location2A[res0.1$team2=="barnet"] <- "-0.291785"
  
  res0.1$location1H <- as.numeric(res0.1$location1H)
  res0.1$location1A <- as.numeric(res0.1$location1A)
  res0.1$location2H <- as.numeric(res0.1$location2H)
  res0.1$location2A <- as.numeric(res0.1$location2A)
  
  radius = 3958.8 # Radius of the Earth in miles
  res0.1$rlat1 = res0.1$location1H * (pi/180); # Convert degrees to radians
  res0.1$rlat2 = res0.1$location1A * (pi/180); # Convert degrees to radians
  res0.1$difflat = res0.1$rlat2-res0.1$rlat1; # Radian difference (latitudes)
  res0.1$difflon = (res0.1$location2H-res0.1$location2A) * (pi/180) # Radian difference (longitudes)
  
  res0.1$distance = 2 * radius * asin(sqrt(sin(res0.1$difflat/2)*sin(res0.1$difflat/2)+cos(res0.1$rlat1)*cos(res0.1$rlat2)*sin(res0.1$difflon/2)*sin(res0.1$difflon/2)))
  
  res0.1 <- res0.1[order(res0.1$date,res0.1$team1),]
  nl.fixtures.2022 <- res0.1[res0.1$div_id==9 & res0.1$season==2022,
         c("team1","team2","date","goals1","goals2","elo10predict","distance")]
  lg1.fixtures.2022 <- res0.1[res0.1$div_id==3 & res0.1$season==2022,
                             c("team1","team2","date","goals1","goals2","elo10predict","distance")]
  lg2.fixtures.2022 <- res0.1[res0.1$div_id==4 & res0.1$season==2022,
                              c("team1","team2","date","goals1","goals2","elo10predict","distance")]
  lgc.fixtures.2022 <- res0.1[res0.1$div_id==2 & res0.1$season==2022,
                              c("team1","team2","date","goals1","goals2","elo10predict","distance")]
  epl.fixtures.2022 <- res0.1[res0.1$div_id==1 & res0.1$season==2022,
                              c("team1","team2","date","goals1","goals2","elo10predict","distance")]
  summary(epl.fixtures.2022$distance)
  summary(lgc.fixtures.2022$distance)
  summary(lg1.fixtures.2022$distance)
  summary(lg2.fixtures.2022$distance)
  summary(nl.fixtures.2022$distance)
  
  #quick OAFC fixture/results list for 21/22
  oafc.list.h <- res0.1[res0.1$team1=="oldham" & res0.1$season==2022,
                      c("team2","date","goals1","goals2","elo10predict","distance")]
  colnames(oafc.list.h)[1] <- "opp"
  oafc.list.h$venue <- "H"
  oafc.list.h$goals <- oafc.list.h$goals1
  oafc.list.h$opp.goals <- oafc.list.h$goals2
  oafc.list.a <- res0.1[res0.1$team2=="oldham" & res0.1$season==2022,
                        c("team1","date","goals1","goals2","elo10predict","distance")]
  colnames(oafc.list.a)[1] <- "opp"
  oafc.list.a$venue <- "A"
  oafc.list.a$goals <- oafc.list.a$goals2
  oafc.list.a$opp.goals <- oafc.list.a$goals1
  oafc.list.a$elo10predict <- 1-oafc.list.a$elo10predict
  oafc.list <- rbind(oafc.list.h[c("date","opp","venue","goals","opp.goals","elo10predict","distance")],
                     oafc.list.a[c("date","opp","venue","goals","opp.goals","elo10predict","distance")])
  oafc.list <- oafc.list[order(oafc.list$date),]
  oafc.list$date <- as.character(oafc.list$date)
  
  print(xtable(oafc.list,digits = c(0,0,0,0,0,0,2,1)), type = "latex", 
        floating = FALSE,
        include.rownames=FALSE, file=paste0(dbloc,"/data/oldham/fixures-results-2223-",Sys.Date(),".tex"))
  
  fileConn<-file(paste0(dbloc,"/data/oldham/fixures-results-2223-",Sys.Date(),"-tab.tex"))

  writeLines(c("\\documentclass{standalone}","\\begin{document}",paste0("\\input{",dbloc,"/data/oldham/fixures-results-2223-",Sys.Date(),".tex}"),"\\end{document}"), fileConn)
  close(fileConn)

  cmds <- c(paste0("cd ",dbloc,"/data/oldham/"), 
            paste0("pdflatex fixures-results-2223-",Sys.Date(),"-tab.tex"), 
            paste0("convert -density 300 fixures-results-2223-",Sys.Date(),"-tab.pdf /Users/jjreade/Dropbox/Research/Sport/OAFC/fixures-results-2223-",Sys.Date(),"-tab.jpg"), 
            paste0("convert -density 300 fixures-results-2223-",Sys.Date(),"-tab.pdf /Users/jjreade/Dropbox/Research/Sport/OAFC/fixures-results-2223-latest-tab.jpg"));
  system(paste(cmds, collapse=";"))

  # ##NL Elos
  # #opening day
  # elo1.t1 <- res0.1[res0.1$date=="2022-08-06" & res0.1$div_id==9,c("team1","elostrength10.1")]
  # colnames(elo1.t1) <- c("team","elo")
  # elo1.t2 <- res0.1[res0.1$date=="2022-08-06" & res0.1$div_id==9,c("team2","elostrength10.2")]
  # colnames(elo1.t2) <- c("team","elo")
  # elo1 <- rbind(elo1.t1,elo1.t2)
  # elo1 <- elo1[order(elo1$elo,decreasing = TRUE),]
  # 
  # #after seven games
  # elo7.t1 <- res0.1[res0.1$date=="2022-09-03" & res0.1$div_id==9,c("team1","elostrength10.1")]
  # colnames(elo7.t1) <- c("team","elo")
  # elo7.t2 <- res0.1[res0.1$date=="2022-09-03" & res0.1$div_id==9,c("team2","elostrength10.2")]
  # colnames(elo7.t2) <- c("team","elo")
  # elo7 <- rbind(elo7.t1,elo7.t2)
  # elo7 <- elo7[order(elo7$elo,decreasing = TRUE),]
  # 
  # #after Tuesday (14 games, 2022-10-25)
  # elo14.t1 <- res0.1[res0.1$date=="2022-10-25" & res0.1$div_id==9,c("team1","elostrength10.1")]
  # colnames(elo14.t1) <- c("team","elo")
  # elo14.t2 <- res0.1[res0.1$date=="2022-10-25" & res0.1$div_id==9,c("team2","elostrength10.2")]
  # colnames(elo14.t2) <- c("team","elo")
  # elo14 <- rbind(elo14.t1,elo14.t2)
  # elo14 <- elo14[order(elo14$elo,decreasing = TRUE),]

  ##load in transfer information - transfers since last match, total since previous June
  transfers <- load.transfers()

  res.transfers <- transfers.results(results.data=res0.1,transfer.data=transfers)
  
  ##load in manager information
  managers <- load.managers()
  #write.csv(managers,"/Users/jjreade/Dropbox/Research/Sport/OAFC/managers-all.csv")
  
  res.manager <- manager.results(res0.1,managers)
  
  res.manager$season <- as.numeric(format(res.manager$date,"%Y")) - as.numeric(as.numeric(format(res.manager$date,"%m"))<7)
  res.manager$summer <- as.numeric(as.numeric(format(res.manager$date,"%m"))>5 & as.numeric(format(res.manager$date,"%m"))<9)
  res.manager$autumn <- as.numeric(as.numeric(format(res.manager$date,"%m"))>8 & as.numeric(format(res.manager$date,"%m"))<12)
  res.manager$winter <- as.numeric(as.numeric(format(res.manager$date,"%m"))>11 & as.numeric(format(res.manager$date,"%m"))<3)
  res.manager$spring <- as.numeric(as.numeric(format(res.manager$date,"%m"))>2 & as.numeric(format(res.manager$date,"%m"))<6)
  club.season.changes <- aggregate(res.manager$new.manager,by=list(res.manager$season,res.manager$team),FUN=sum)
  res.manager$coach[res.manager$coach=="John Eyres"]="John Sheridan"
  res.manager$coach[res.manager$coach=="Billy Ursom"]="John Sheridan"
  res.manager <- res.manager[duplicated(res.manager[,c("team","match_id")])==FALSE,]
  res0.1 <- merge(res0.1,
                res.manager[,c("match_id","coach","team","tenure.days","new.coach","cplayed","win.pc","points.per.game","won.first.game","lost.first.game","total.experience")],
                by.x=c("team1","match_id"),by.y=c("team","match_id"),all.x=TRUE)
  res0.1 <- merge(res0.1,
                res.manager[,c("match_id","coach","team","tenure.days","new.coach","cplayed","win.pc","points.per.game","won.first.game","lost.first.game","total.experience")],
                by.x=c("team2","match_id"),by.y=c("team","match_id"),all.x=TRUE,suffixes=c(".H",".A"))

  res.manager$const <- 1

  res.manager.record <- aggregate(res.manager[is.na(res.manager$goals)==FALSE,c("const","won","drawn","lost","goals","oppgoals","won.first.game","lost.first.game")],by=list(res.manager$team[is.na(res.manager$goals)==FALSE],res.manager$spell[is.na(res.manager$goals)==FALSE]),FUN=sum,na.rm=TRUE)
  res.manager.record$coach <- gsub("^(.*?)-(\\d{4}-\\d{2}-\\d{2})$","\\1",res.manager.record$Group.2)
  res.manager.record$start.date <- gsub("^(.*?)-(\\d{4}-\\d{2}-\\d{2})$","\\2",res.manager.record$Group.2)
  res.manager.record <- res.manager.record[order(res.manager.record$Group.1,res.manager.record$coach),]
  res.manager.record$coach.club <- paste0(res.manager.record$coach,"-",res.manager.record$Group.1)
  res.manager.record$tenures <- sequence(rle(res.manager.record$coach.club)$lengths)
  res.manager.record$win.pc <- res.manager.record$won/res.manager.record$const
  
  colnames(res.manager.record)[1:3] <- c("team","manager.start","total.played")
  write.csv(res.manager.record[res.manager.record$Group.1=="oldham",],
            "/Users/jjreade/Dropbox/Research/Sport/OAFC/latics-managers.csv")
  
  res.manager.record2 <- aggregate(res.manager[,c("const","won","drawn","lost","goals","oppgoals")],by=list(res.manager$team,res.manager$coach),FUN=sum,na.rm=TRUE)
  colnames(res.manager.record2)[1:3] <- c("team","manager","played")
  res.manager.record2$played[res.manager.record2$team=="oldham" & res.manager.record2$manager=="John Sheridan"] <- 251
  res.manager.record2$won[res.manager.record2$team=="oldham" & res.manager.record2$manager=="John Sheridan"] <- 91
  res.manager.record2$goals[res.manager.record2$team=="oldham" & res.manager.record2$manager=="John Sheridan"] <- 312
  res.manager.record2$oppgoals[res.manager.record2$team=="oldham" & res.manager.record2$manager=="John Sheridan"] <- 302
  res.manager.record2$win.pc <- res.manager.record2$won/res.manager.record2$played
  
  res.manager <- res.manager[order(res.manager$team,res.manager$date),]
  res.manager$points <- 3*as.numeric(res.manager$outcome==1) + as.numeric(res.manager$outcome==0.5)
  res.manager$points.next.6 <- zoo::rollsum(res.manager$points, 6,fill=NA,align="left")
  res.manager$points.last.6 <- c(NA,zoo::rollsum(res.manager$points, 6,fill=NA,align="right")[-NROW(res.manager)])
  
  res.man.bounce <- res.manager[is.na(res.manager$new.coach)==FALSE & res.manager$new.coach==1,c("spell","team","points.next.6","points.last.6")]
  res.man.bounce$bounce <- res.man.bounce$points.next.6 - res.man.bounce$points.last.6
  res.man.bounce <- res.man.bounce[order(res.man.bounce$bounce,decreasing = TRUE),]
  
  write.csv(res.manager,"Dropbox/Research/Sport/OAFC/manager-histories.csv")
  write.csv(res.man.bounce,"Dropbox/Research/Sport/OAFC/manager-bounce.csv")
  
  # res.man.6 <- res.manager[res.manager$cplayed<=6,c("spell","team","Date.Started","outcome","cpoints","cplayed","spell0")]
  # res.man.6$cplayed <- sequence(rle(res.man.6$spell0)$lengths)
  # res.man.6$cplayed[res.man.6$spell=="Jack Fairbrother-1952-08-01-pet"] <- 1:5
  # res.man.6$cplayed[res.man.6$spell=="Roy Hodgson-1997-06-01-int"] <- 1:6
  # res.man.6$cplayed[res.man.6$spell=="Tommy Docherty-1968-11-01-qpr"] <- 1:6
  # res.man.6$cplayed[res.man.6$spell=="Tommy Docherty-1968-12-01-ast"] <- 1:6
  # res.man.6$cplayed[res.man.6$spell=="Filippo Inzaghi-2016-07-01-uni"] <- 1:4
  # res.man.6$cplayed[res.man.6$spell=="Franco Lerda-2016-03-15-vic"] <- 1:2
  # res.man.6$cplayed[res.man.6$spell=="Jose Manuel Aira-2014-07-10-mur"] <- 1:6
  # res.man.6$cplayed[res.man.6$spell=="Jose Manuel Aira-2016-07-01-alb"] <- 1:6
  # res.man.6$points <- 3*as.numeric(res.man.6$outcome==1) + as.numeric(res.man.6$outcome==0.5)
  # res.man.6$cpoints <- ave(res.man.6$points, res.man.6$spell, FUN=cumsum)
  # res.man.6$outcomeWDL[res.man.6$outcome==1] <- "W"
  # res.man.6$outcomeWDL[res.man.6$outcome==0.5] <- "D"
  # res.man.6$outcomeWDL[res.man.6$outcome==0] <- "L"
  # spells <- sort(unique(res.man.6$spell0))
  # res.man.6.wide <- data.frame("spell"=spells,"team"=NA,"Game1"=NA,"Game2"=NA,"Game3"=NA,"Game4"=NA,"Game5"=NA,"Game6"=NA,"points"=NA,"coach"=NA,"Date.Started"=NA,stringsAsFactors = FALSE)
  # for(ss in spells) {
  #   res.man.6.wide[res.man.6.wide$spell==ss,3:c(3+NROW(res.man.6$outcomeWDL[res.man.6$spell0==ss])-1)] <- res.man.6$outcomeWDL[res.man.6$spell0==ss]
  #   res.man.6.wide[res.man.6.wide$spell==ss,"points"] <- res.man.6$cpoints[res.man.6$spell0==ss & res.man.6$cplayed==max(1,NROW(res.man.6$outcomeWDL[res.man.6$spell0==ss])-1)]
  #   res.man.6.wide[res.man.6.wide$spell==ss,"team"] <- unique(res.man.6$team[res.man.6$spell0==ss])
  # }

  manager.tenures <- aggregate(res.manager.record$const[res.manager.record$start.date>="1960-01-01"],by=list(as.numeric(format(as.Date(res.manager.record$start.date[res.manager.record$start.date>="1960-01-01"]),"%Y"))),FUN=mean,na.rm=TRUE)  
  manager.tenure.days0 <- aggregate(res.manager$tenure.days[res.manager$Date.Started>="1960-01-01"],by=list(res.manager$spell[res.manager$Date.Started>="1960-01-01"]),FUN=max,na.rm=TRUE)
  manager.tenure.days0$date <- as.Date(gsub("^.*?-(\\d+)-(\\d+)-(\\d+)$","\\1-\\2-\\3",manager.tenure.days0$Group.1))
  manager.tenure.days <- aggregate(manager.tenure.days0$x,by=list(as.numeric(format(manager.tenure.days0$date,"%Y"))),FUN=mean,na.rm=TRUE)
  
  
  jpeg(paste0("/Users/jjreade/Dropbox/Teaching/EC126/Notes-Slides/managerial-tenure-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(manager.tenures$Group.1[manager.tenures$Group.1<c(as.numeric(format(Sys.Date(),"%Y"))-2)],
       manager.tenures$x[manager.tenures$Group.1<c(as.numeric(format(Sys.Date(),"%Y"))-2)],
       type="l",main="Length of managerial tenures in football",
       ylab="Number of matches",xlab="Date of start of tenure",
       sub="black=matches (left scale), red=days (right scale)")
  par(new=TRUE)
  plot(manager.tenure.days$Group.1[manager.tenure.days$Group.1<c(as.numeric(format(Sys.Date(),"%Y"))-2)],
       manager.tenure.days$x[manager.tenure.days$Group.1<c(as.numeric(format(Sys.Date(),"%Y"))-2)],
       type="l",yaxt="n",xaxt="n",ylab="",xlab="",col=2)
  axis(side=4)
  dev.off()
  
  ##load in squad info
  squads <- read.csv("/Volumes/11330730-dp/Correct-score/data/soccerbase/squads-sb.csv",stringsAsFactors = FALSE)
  
  
  squads$team <- tolower(gsub("^(.*?)_(\\d+)[.]html$","\\1",squads$filename))
  squads$filename <- gsub("2021-08-02","154",squads$filename)
  squads$season <- 1870+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))>145)
  squads <- squads[order(squads$season),]
  
  squads$player.position <- gsub("^(.*?) [(](.*?)[)].*\\s*","\\2",squads$player.name)
  squads$player.position[squads$player.position=="GK"] <- "G"
  squads$player.position[nchar(squads$player.position)>1] <- NA
  
  squads$surname <- gsub("^(.*?) (.*?) [(](.*?)[)]","\\2",squads$player.name)
  
  squads$league.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$league.apps))
  squads$league.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$league.apps))
  squads$facup.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$facup.apps))
  squads$facup.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$facup.apps))
  squads$lgecup.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$lgecup.apps))
  squads$lgecup.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$lgecup.apps))
  squads$other.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$other.apps))
  squads$other.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$other.apps))
  
  squads$total.apps <- squads$league.apps + squads$facup.apps + squads$lgecup.apps + squads$other.apps
  squads$total.sub.apps <- squads$league.sub.apps + squads$facup.sub.apps + squads$lgecup.sub.apps + squads$other.sub.apps
  squads$total.total.apps <- squads$total.apps + squads$total.sub.apps
  
  squads$total.goals <- squads$league.goals + squads$facup.goals + squads$lgecup.goals + squads$other.goals
  
  squad.totals <- aggregate(squads[,c("total.total.apps","total.goals")],
                            by=list(squads$team,squads$season),FUN=sum,na.rm=TRUE)
  
  res0.1 <- merge(res0.1,squad.totals,by.x=c("team1","season"),by.y=c("Group.1","Group.2"),all.x=TRUE)
  res0.1 <- merge(res0.1,squad.totals,by.x=c("team2","season"),by.y=c("Group.1","Group.2"),all.x=TRUE,suffixes=c(".H",".A"))
  
  ##promotion/relegation previous season
  comps <- sort(res0.1$div_id[duplicated(res0.1$div_id)==FALSE])
  comps <- data.frame("div_id"=comps,
                      "tier"=c(NA,1,2,3,4,1,2,3,4,5,3,3,1,2,3,4,1,2,3,1,1,1,1,1,1,1,1,1,1,
                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,7,1,NA,7,5,
                               NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                               NA,NA,NA,2,1,NA,NA,2,1,1,1,1,1,6,6,1,2,2,1,NA,5,NA,NA,NA,1,1),
                      stringsAsFactors = FALSE)
  ##league IDs
  league.ids <- c(1:25,47:48,56,75,76,80,82,111:118,122:134,170:171,194,205:311,353)
  
  res0.1 <- merge(res0.1,comps[,c("div_id","tier")],by=c("div_id"),all.x=TRUE,suffixes = c("",".H"))
  res0.1 <- merge(res0.1,comps[,c("div_id","tier")],by=c("div_id"),all.x=TRUE,suffixes = c("",".A"))
  
  club.season.comp0 <- res0.1[res0.1$div_id %in% league.ids,c("team1","season","div_id")]
  colnames(club.season.comp0) <- c("team","season","div_id")
  club.season.comp <- res0.1[res0.1$div_id %in% league.ids,c("team2","season","div_id")]
  colnames(club.season.comp) <- c("team","season","div_id")
  club.season.comp <- rbind(club.season.comp,club.season.comp0)
  rm(club.season.comp0)
  club.season.comp <- club.season.comp[duplicated(club.season.comp)==FALSE,]
  club.season.comp <- club.season.comp[duplicated(club.season.comp[,c("team","season")])==FALSE,]
  
  club.season.comp <- club.season.comp[order(club.season.comp$team,club.season.comp$season),]
  club.season.comp$team.1 <- c(-1000,club.season.comp$team[-NROW(club.season.comp)])
  club.season.comp$div_id.1 <- c(NA,club.season.comp$div_id[-NROW(club.season.comp)])
  club.season.comp$div_id.1[club.season.comp$team!=club.season.comp$team.1] <- NA
  club.season.comp$promoted <- as.numeric(club.season.comp$div_id<club.season.comp$div_id.1)
  club.season.comp$relegated <- as.numeric(club.season.comp$div_id>club.season.comp$div_id.1)
  
  res0.1 <- merge(res0.1,club.season.comp[,c("team","season","promoted","relegated")],
                by.x=c("team1","season"),by.y=c("team","season"),all.x=TRUE)
  res0.1 <- merge(res0.1,club.season.comp[,c("team","season","promoted","relegated")],
                by.x=c("team2","season"),by.y=c("team","season"),all.x=TRUE,suffixes=c(".H",".A"))
  
  ##copy/paste into all future matches and fill in gaps (from league table creation) using na.locf
  res0.1 <- res0.1[order(res0.1$date),]
  all.teams <- sort(unique(c(res0.1$team1,res0.1$team2)))
  Hcols.to.do <- colnames(res0.1)[regexpr("[.]H$|\\w1$",colnames(res0.1))>-1]
  Acols.to.do <- colnames(res0.1)[regexpr("[.]A$|\\w2$",colnames(res0.1))>-1]
  for(tt in all.teams) {
    print(tt)
    fill.res0.10 <- res0.1[res0.1$team1==tt,Hcols.to.do[-1:-2]]
    if(NROW(fill.res0.10)>0) {
      fill.res0.10$home <- 1
      colnames(fill.res0.10) <- gsub(".H|1","",colnames(fill.res0.10))
    }
    fill.res0.1 <- res0.1[res0.1$team2==tt,Acols.to.do[-1:-2]]
    if(NROW(fill.res0.1)>0) {
      fill.res0.1$home <- 0
      colnames(fill.res0.1) <- gsub(".A|2","",colnames(fill.res0.1))
    }
    fill.res0.1 <- rbind(fill.res0.10,fill.res0.1)
    fill.res0.1 <- na.locf(fill.res0.1,na.rm=FALSE)
    
    res0.1[res0.1$team1==tt,Hcols.to.do[-1:-2]] <- fill.res0.1[fill.res0.1$home==1,-NCOL(fill.res0.1)]
    res0.1[res0.1$team2==tt,Acols.to.do[-1:-2]] <- fill.res0.1[fill.res0.1$home==0,-NCOL(fill.res0.1)]
  }
  
  res0.1 <- res0.1[order(res0.1$team1,res0.1$date),]
  
  res0.1$tier.diff <- res0.1$tier.H - res0.1$tier.A
  res0.1$promoted.diff <- res0.1$promoted.H - res0.1$promoted.A
  res0.1$relegated.diff <- res0.1$relegated.H - res0.1$relegated.A
  res0.1$win.pc.diff <- res0.1$win.pc.H - res0.1$win.pc.A
  res0.1$cplayed.diff <- res0.1$cplayed.H - res0.1$cplayed.A
  res0.1$new.coach.diff <- res0.1$new.coach.H - res0.1$new.coach.A
  
  ##creating info based on match results
  res0.1$team1.1 <- c(NA,res0.1$team1[-NROW(res0.1)])
  res0.1$team1.2 <- c(NA,res0.1$team1.1[-NROW(res0.1)])
  res0.1$goals1.1 <- c(NA,res0.1$goals1[-NROW(res0.1)])
  res0.1$goals1.1[res0.1$team1.1!=res0.1$team1] <- NA
  
  ##info from last match by team
  res0.1$scored.1.1 <- NA
  res0.1$conceded.1.1 <- NA
  res0.1$outcome.1.1 <- NA
  res0.1$scored.1.1.6 <- NA
  res0.1$conceded.1.1.6 <- NA
  res0.1$outcome.1.1.6 <- NA
  res0.1$scored.2.1 <- NA
  res0.1$conceded.2.1 <- NA
  res0.1$outcome.2.1 <- NA
  res0.1$scored.2.1.6 <- NA
  res0.1$conceded.2.1.6 <- NA
  res0.1$outcome.2.1.6 <- NA
  teams <- sort(unique(c(res0.1$team1,res0.1$team2)))
  res0.1 <- res0.1[order(res0.1$date),]
  for(tt in teams) {
    print(tt)
    t1 <- res0.1[res0.1$team1==tt,c("date","goals1","goals2","outcome")]
    colnames(t1) <- c("date","scored","conceded","outcome")
    t1$home = rep(1,NROW(t1))
    t2 <- res0.1[res0.1$team2==tt,c("date","goals2","goals1","outcome")]
    t2$outcome <- 1-t2$outcome
    colnames(t2) <- c("date","scored","conceded","outcome")
    t2$home <- rep(0,NROW(t2))
    team.res0.1 <- rbind(t1,t2)
    team.res0.1 <- team.res0.1[order(team.res0.1$date),]
    if(NROW(team.res0.1)>6) {
      team.res0.1$scored.1 <- c(NA, team.res0.1$scored[-NROW(team.res0.1)])
      team.res0.1$conceded.1 <- c(NA, team.res0.1$conceded[-NROW(team.res0.1)])
      team.res0.1$outcome.1 <- c(NA, team.res0.1$outcome[-NROW(team.res0.1)])
      team.res0.1$cscored <- cumsum(team.res0.1$scored)
      team.res0.1$cconceded <- cumsum(team.res0.1$conceded)
      team.res0.1$coutcome <- cumsum(team.res0.1$outcome)
      team.res0.1$cscored.1 <- c(0, team.res0.1$cscored[-NROW(team.res0.1)])
      team.res0.1$cconceded.1 <- c(0, team.res0.1$cconceded[-NROW(team.res0.1)])
      team.res0.1$coutcome.1 <- c(0, team.res0.1$coutcome[-NROW(team.res0.1)])
      team.res0.1$cscored.7 <- c(rep(0,7), team.res0.1$cscored[-seq(NROW(team.res0.1)-6,NROW(team.res0.1))])
      team.res0.1$cconceded.7 <- c(rep(0,7), team.res0.1$cconceded[-seq(NROW(team.res0.1)-6,NROW(team.res0.1))])
      team.res0.1$coutcome.7 <- c(rep(0,7), team.res0.1$coutcome[-seq(NROW(team.res0.1)-6,NROW(team.res0.1))])
      team.res0.1$scored.1.6 <- team.res0.1$cscored.1 - team.res0.1$cscored.7
      team.res0.1$conceded.1.6 <- team.res0.1$cconceded.1 - team.res0.1$cconceded.7
      team.res0.1$outcome.1.6 <- team.res0.1$coutcome.1 - team.res0.1$coutcome.7
    } else {
      team.res0.1$scored.1 <- rep(NA,NROW(team.res0.1))
      team.res0.1$conceded.1 <- rep(NA,NROW(team.res0.1))
      team.res0.1$outcome.1 <- rep(NA,NROW(team.res0.1))
      team.res0.1$scored.1.6 <- rep(NA,NROW(team.res0.1))
      team.res0.1$conceded.1.6 <- rep(NA,NROW(team.res0.1))
      team.res0.1$outcome.1.6 <- rep(NA,NROW(team.res0.1))
      
    }
    
    ##put back into full.res0.1
    res0.1[res0.1$team1==tt,c("scored.1.1","conceded.1.1","outcome.1.1","scored.1.1.6","conceded.1.1.6","outcome.1.1.6")] <- team.res0.1[team.res0.1$home==1,c("scored.1","conceded.1","outcome.1","scored.1.6","conceded.1.6","outcome.1.6")]
    res0.1[res0.1$team2==tt,c("scored.2.1","conceded.2.1","outcome.2.1","scored.2.1.6","conceded.2.1.6","outcome.2.1.6")] <- team.res0.1[team.res0.1$home==0,c("scored.1","conceded.1","outcome.1","scored.1.6","conceded.1.6","outcome.1.6")]
  }
  
  ##lagged goals scored by away team in home team's previous home matches
  res0.1 <- res0.1[order(res0.1$team1,res0.1$date),]
  res0.1$goals2.1.1 <- c(NA,res0.1$goals2[-NROW(res0.1)])
  res0.1$goals2.1.1[res0.1$team1.1!=res0.1$team1] <- NA
  
  ##ditto for away team
  res0.1 <- res0.1[order(res0.1$team2,res0.1$date),]
  res0.1$team2.1 <- c(NA,res0.1$team2[-NROW(res0.1)])
  res0.1$team2.2 <- c(NA,res0.1$team2.1[-NROW(res0.1)])
  res0.1$goals2.1 <- c(NA,res0.1$goals2[-NROW(res0.1)])
  res0.1$goals2.1[res0.1$team2.1!=res0.1$team2] <- NA
  
  ##lagged goals scored by home team in away team's previous away matches
  res0.1$goals1.2.1 <- c(NA,res0.1$goals1[-NROW(res0.1)])
  res0.1$goals1.2.1[res0.1$team2.1!=res0.1$team2] <- NA
  
  
  res0.1 <- res0.1[order(res0.1$date),]
  
  write(res0.1,paste0("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/all-results-sb",Sys.Date(),".csv"))
  
  returnlist <- list("full.data"=res0.1,"elorank"=elorank,"league.tables"=res.tab.2020)
  return <- returnlist
}

eloorderings <- function() {
  full.data0 <- read.csv(paste0(dbloc,"/data/res0-2022-07-17.csv"),stringsAsFactors = FALSE)
  full.data0$date <- as.Date(full.data0$date)
  
  ##### make long dataset with elo ratings for teams and their league levels #####
  elo1 <- full.data0[,c("team1","elostrength10.1","full.pos1","season","date","tier1")]
  colnames(elo1) <- c("team","elo","pos","season","date","tier")
  elo2 <- full.data0[,c("team2","elostrength10.2","full.pos2","season","date","tier2")]
  colnames(elo2) <- c("team","elo","pos","season","date","tier")
  all.elos <- rbind(elo1,elo2)
  
  ##### want end of season Elo to rank #####
  all.elos <- all.elos[order(all.elos$team,all.elos$season,all.elos$date),]
  all.elos$season.l1 <- c(all.elos$season[-1],3000)
  # all.elos <- all.elos[regexpr("Cup|Play|Fri",all.elos$division)==-1,]
  all.elos.end <- all.elos[all.elos$season!=all.elos$season.l1,]

  ##now order by elo per year to see where best SL team was
  all.elos.end <- all.elos.end[order(all.elos.end$season,all.elos.end$elo,decreasing = TRUE),]
  
  end.of.season.top.20 <- data.frame("rank"=1:50,stringsAsFactors = FALSE)
  for(yy in 1890:1925) {
    sranks <- all.elos.end[all.elos.end$season==yy & all.elos.end$tier %in% c(1,2,2.5),c("team","elo","tier")]
    end.of.season.top.20[[paste0("s",yy,"team")]] <- sranks$team[1:50]
    end.of.season.top.20[[paste0("s",yy,"elo")]] <- sranks$elo[1:50]
    sranks$tier2[sranks$tier==1] <- "FL1"
    sranks$tier2[sranks$tier==2] <- "FL2"
    sranks$tier2[sranks$tier==2.5] <- "SL1"
    end.of.season.top.20[[paste0("s",yy,"tier")]] <- sranks$tier2[1:50]
  }
  write.csv(end.of.season.top.20,"/Users/jjreade/Dropbox/Research/Sport/RFC-150/end-of-season-elo-1890-1925.csv")
  
  all.elos <- all.elos[order(all.elos$date),]
  
  all.elos.div <- aggregate(all.elos$elo,by=list(all.elos$division,all.elos$season),FUN=mean)
  all.elos.div.max <- aggregate(all.elos$elo,by=list(all.elos$division,all.elos$season),FUN=max)
  all.elos.div.min <- aggregate(all.elos$elo,by=list(all.elos$division,all.elos$season),FUN=min)
  
}

runsimregression <- function() {
  
  data <- read.csv("/Volumes/11330730-dp/Correct-score/data/full.data-2020-09-08.csv",stringsAsFactors = FALSE)
  data$date <- as.Date(data$date)
  
  summary(m1d.sim <- glm(goals1 ~ elostrength1 + elostrength2, family="poisson", data=data[data$date>="2000-01-01",], na.action=na.exclude))
  summary(m2d.sim <- glm(goals2 ~ elostrength1 + elostrength2, family="poisson", data=data[data$date>="2000-01-01",], na.action=na.exclude))
  
  #  summary(m1d <- glm(goals1 ~ goals1.1 + goals2.1.1 + elostrength1 + elostrength2, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  #  summary(m2d <- glm(goals2 ~ goals2.1 + goals1.2.1 + elostrength1 + elostrength2, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  
  data$goals1d.hat <- NA
  data$goals2d.hat <- NA
  data$goals1d.hat[data$date>="2000-01-01"] <- predict(m1d.sim,type="response")
  data$goals2d.hat[data$date>="2000-01-01"] <- predict(m2d.sim,type="response")
  
  
  
  update.list <- list("m1d"=m1d.sim,"m2d"=m2d.sim)
  return = update.list
}  


runbasicregression <- function(data = res,reg.start.date="2010-01-01",date0=Sys.Date()) {
  
  summary(m1d <- glm(goals1 ~ scored.1.1 + scored.2.1 + conceded.1.1 + conceded.2.1
                     + outcome.1.1 + outcome.2.1 + scored.1.1.6 + scored.2.1.6
                     + conceded.1.1.6 + conceded.2.1.6 + outcome.1.1.6 + outcome.2.1.6
                     + elostrength1 + elostrength2
                     + tier.diff + promoted.diff + relegated.diff
                     + cplayed.diff + new.coach.diff, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  summary(m2d <- glm(goals2 ~ scored.1.1 + scored.2.1 + conceded.1.1 + conceded.2.1
                     + outcome.1.1 + outcome.2.1 + scored.1.1.6 + scored.2.1.6
                     + conceded.1.1.6 + conceded.2.1.6 + outcome.1.1.6 + outcome.2.1.6
                     + elostrength1 + elostrength2
                     + tier.diff + promoted.diff + relegated.diff
                     + cplayed.diff + new.coach.diff, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  
  #  summary(m1d <- glm(goals1 ~ goals1.1 + goals2.1.1 + elostrength1 + elostrength2, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  #  summary(m2d <- glm(goals2 ~ goals2.1 + goals1.2.1 + elostrength1 + elostrength2, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  
  data$goals1d.hat <- NA
  data$goals2d.hat <- NA
  data$goals1d.hat[data$date>=reg.start.date & data$date<date0] <- predict(m1d,type="response")
  data$goals2d.hat[data$date>=reg.start.date & data$date<date0] <- predict(m2d,type="response")
  
  
  
  update.list <- list("res"=data,"m1d"=m1d,"m2d"=m2d)
  return = update.list
}  


loadbookies <- function(date0 = Sys.Date(),loc00 = "/Users/jjreade/") {
  ###oddsportal for forecast matches (outcomes)
  #date0 = "2019-11-30"
  
  
  bk.odds <- read.csv(paste0(loc00,"Dropbox/Research/Sport/Correct-score/data/oddsportal-odds-",date0,".csv"),stringsAsFactors = FALSE)
  
  bk.odds$team1 <- gsub("^(.*?) - (.*?)$","\\1",bk.odds$teams)
  bk.odds$team2 <- gsub("^(.*?) - (.*?)$","\\2",bk.odds$teams)
  
  bk.odds <- bk.odds[is.na(bk.odds$odds1)==FALSE,]
  bk.odds <- bk.odds[is.na(bk.odds$odds2)==FALSE,]
  bk.odds <- bk.odds[is.na(bk.odds$odds3)==FALSE,]
  
  if(any(regexpr("/",bk.odds$odds1)>-1)) {
    bk.odds$odds1[regexpr("/",bk.odds$odds1)>-1] <- as.numeric(gsub("^(\\d+)/(\\d+)$","\\1",bk.odds$odds1[regexpr("/",bk.odds$odds1)>-1]))/as.numeric(gsub("^(\\d+)/(\\d+)$","\\2",bk.odds$odds1[regexpr("/",bk.odds$odds1)>-1]))+1
  }
  if(any(regexpr("/",bk.odds$odds2)>-1)) {
    bk.odds$odds2[regexpr("/",bk.odds$odds2)>-1] <- as.numeric(gsub("^(\\d+)/(\\d+)$","\\1",bk.odds$odds2[regexpr("/",bk.odds$odds2)>-1]))/as.numeric(gsub("^(\\d+)/(\\d+)$","\\2",bk.odds$odds2[regexpr("/",bk.odds$odds2)>-1]))+1
  }
  if(any(regexpr("/",bk.odds$odds3)>-1)) {
    bk.odds$odds3[regexpr("/",bk.odds$odds3)>-1] <- as.numeric(gsub("^(\\d+)/(\\d+)$","\\1",bk.odds$odds3[regexpr("/",bk.odds$odds3)>-1]))/as.numeric(gsub("^(\\d+)/(\\d+)$","\\2",bk.odds$odds3[regexpr("/",bk.odds$odds3)>-1]))+1
  }
  
  bk.odds$pH0 <- 1/as.numeric(bk.odds$odds1)
  bk.odds$pD0 <- 1/as.numeric(bk.odds$odds2)
  bk.odds$pA0 <- 1/as.numeric(bk.odds$odds3)
  bk.odds$overround <- bk.odds$pH0 + bk.odds$pD0 + bk.odds$pA0
  bk.odds$pH <- bk.odds$pH0/bk.odds$overround
  bk.odds$pA <- bk.odds$pA0/bk.odds$overround
  
  correspondence <- read.csv(paste0(loc00,"Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv"),stringsAsFactors = FALSE)
  corr.op.sb <- correspondence[is.na(correspondence$oddsportal.team)==FALSE,c("oddsportal.team","soccerbase.team","soccerbase.id")]
  corr.op.sb <- corr.op.sb[duplicated(corr.op.sb)==FALSE,]
  corr.op.sb <- corr.op.sb[corr.op.sb$oddsportal.team!="",]
  corr.op.sb <- corr.op.sb[is.na(corr.op.sb$soccerbase.id)==FALSE,]
  
  bk.odds <- merge(bk.odds,corr.op.sb,by.x=c("team1"),by.y=c("oddsportal.team"),all.x=TRUE)
  bk.odds <- merge(bk.odds,corr.op.sb,by.x=c("team2"),by.y=c("oddsportal.team"),all.x=TRUE,suffixes=c("1","2"))
  
  bk.odds0 <- aggregate(bk.odds[,c("pH","pA")],by=list(bk.odds$soccerbase.team1,bk.odds$soccerbase.team2),
                        FUN=mean)
  return(bk.odds0)
}

createforecasts <- function(data = res,
                            home.g.mod = m1d,
                            away.g.mod = m2d,
                            bk.odds = bk.odds0,
                            date0 = Sys.Date(),
                            days.ahead=3,
                            dbloc="/Volumes/11330730-dp/Correct-score/",
                            save=TRUE,wanteddiv_id = c("EPL"),
                            closed.door=FALSE) {  
  #data = simdata[simdata$div_id==4 & simdata$date<="2020-09-14",]
  # data$scored.2.1.6[is.na(data$scored.2.1.6)] <- 5
  # data$conceded.2.1.6[is.na(data$conceded.2.1.6)] <- 5
  # data$outcome.2.1.6[is.na(data$outcome.2.1.6)] <- 2
  # home.g.mod = list0$m1d
  # away.g.mod = list0$m2d
  # date0=Sys.Date()
  # days.ahead=4
  # wanteddiv_id = c("EFLC")
  require(xtable)
  
  ##create variable with possible scores, assuming a team will score max 8 goals (!)
  poss.scores <- cbind(rep(0:8,9)[order(rep(0:8,9))],rep(0:8,9))
  home.win.scores <- paste0(poss.scores[poss.scores[,1]>poss.scores[,2],1],"-",poss.scores[poss.scores[,1]>poss.scores[,2],2])
  away.win.scores <- paste0(poss.scores[poss.scores[,1]<poss.scores[,2],1],"-",poss.scores[poss.scores[,1]<poss.scores[,2],2])
  draw.scores <- paste0(poss.scores[poss.scores[,1]==poss.scores[,2],1],"-",poss.scores[poss.scores[,1]==poss.scores[,2],2])
  ##string variables for each score
  scores <- paste0(poss.scores[,1],"-",poss.scores[,2])
  
  ####create forecasts
  
  #res$wins1 <- NA
  #res$wins2 <- NA
  forecast.matches <- data[data$date>=date0 & data$date<=c(date0+days.ahead), ]
  
  if(NROW(forecast.matches)>0) {
    forecast.matches$lambda1.hat <- predict(home.g.mod, newdata = forecast.matches,type="response")
    forecast.matches$lambda2.hat <- predict(away.g.mod, newdata = forecast.matches,type="response")
    
    for(cc in scores) { forecast.matches[[paste0("score.",cc)]] <- NA}
    
    forecast.matches$most.likely <- NA
    forecast.matches$p.most.likely <- NA
    forecast.matches$c.most.likely <- NA
    forecast.matches$p.c.most.likely <- NA
    forecast.matches$p.home <- NA
    forecast.matches$p.away <- NA
    forecast.matches$p.draw <- NA
    forecast.matches$entropy <- NA
    
    entropy.limit = 1.085
    
    for(ii in 1:NROW(forecast.matches)) {
      probs <- as.vector(t(dpois(0:8,forecast.matches$lambda1.hat[ii]-as.numeric(closed.door==TRUE)*0.21)%*%t(dpois(0:8,forecast.matches$lambda2.hat[ii]+as.numeric(closed.door==TRUE)*0.08))))
      forecast.matches[ii,grep("^score[.]\\d-\\d$",colnames(forecast.matches))] <- probs
      if(is.na(forecast.matches$lambda1.hat[ii])==FALSE & is.na(forecast.matches$lambda2.hat[ii])==FALSE & is.na(forecast.matches$lambda1.hat[ii])==FALSE ) {
        p.home <- sum(probs[poss.scores[,1]>poss.scores[,2]])
        p.away <- sum(probs[poss.scores[,1]<poss.scores[,2]])
        p.draw <- 1-p.away-p.home
        
        entropy <- -p.home*log(p.home)-p.away*log(p.away)-p.draw*log(p.draw)
        prob.most.likely <- max(probs)
        if(p.home>p.away & p.home>p.draw & entropy<entropy.limit) {
          cond.probs <- probs[which(poss.scores[,1]>poss.scores[,2])]
          cond.most.likely.score <- home.win.scores[which(cond.probs==max(cond.probs))]
          cond.prob.most.likely <- cond.probs[which(cond.probs==max(cond.probs))]
        } else if (p.away>p.home & p.away>p.draw & entropy<entropy.limit) {
          cond.probs <- probs[which(poss.scores[,1]<poss.scores[,2])]
          cond.most.likely.score <- away.win.scores[which(cond.probs==max(cond.probs))]
          cond.prob.most.likely <- cond.probs[which(cond.probs==max(cond.probs))]
        } else {
          cond.probs <- probs[which(poss.scores[,1]==poss.scores[,2])]
          cond.most.likely.score <- draw.scores[which(cond.probs==max(cond.probs))]
          cond.prob.most.likely <- cond.probs[which(cond.probs==max(cond.probs))]
        }
        forecast.matches$p.home[ii] <- p.home
        forecast.matches$p.away[ii] <- p.away
        forecast.matches$p.draw[ii] <- p.draw
        forecast.matches$most.likely[ii] <- scores[which(probs==max(probs))]
        forecast.matches$p.most.likely[ii] <- probs[which(probs==max(probs))]
        forecast.matches$c.most.likely[ii] <- cond.most.likely.score
        forecast.matches$p.c.most.likely[ii] <- cond.prob.most.likely
        forecast.matches$entropy[ii] <- entropy
      }
    }
    #    forecast.matches <- forecast.matches[is.na(forecast.matches$goals1.1)==FALSE,]
    
    
    ############################################################
    ###### SAVING FORECASTS                               ######
    ############################################################
    
    if(save==TRUE) {
      if(NROW(forecast.matches)>0) {
        if(!dir.exists(paste0(dbloc,"/data/",wanteddiv_id))) {
          dir.create(paste0(dbloc,"/data/",wanteddiv_id))
        }
        if(closed.door==FALSE) {
          write.csv(forecast.matches,paste0(dbloc,"/data/",wanteddiv_id,"/",wanteddiv_id,"-2122-",date0,".csv"))
        } else {
          write.csv(forecast.matches,paste0(dbloc,"/data/",wanteddiv_id,"/",wanteddiv_id,"-2122-",date0,"-closed-door.csv"))
        }
        bk.odds0$Group.1 <- gsub("afc wimbledon","afc w'bledon",bk.odds0$Group.1)
        bk.odds0$Group.2 <- gsub("afc wimbledon","afc w'bledon",bk.odds0$Group.2)
        forecast.matches0.display <- forecast.matches[order(forecast.matches$date,forecast.matches$team1),c("team1","team2","lambda1.hat","lambda2.hat","p.home","p.away","most.likely","c.most.likely","score.2-0","score.2-1","score.1-0","score.0-0","score.1-1","score.2-2","score.0-1","score.1-2","score.0-2")]
        tw.forc <- forecast.matches[order(forecast.matches$date,forecast.matches$team1),c("team1","team2","p.home","p.away")]
        print(toString(paste0("#",toupper(substr(tw.forc$team1,1,3)),"v",toupper(substr(tw.forc$team2,1,3)),
                              " P(#",toupper(substr(tw.forc$team1,1,3)),") ",round(tw.forc$p.home,2)*100,
                              "% P(#",toupper(substr(tw.forc$team2,1,3)),") ",round(tw.forc$p.away,2)*100,"%")))
        forecast.matches0.display$lambda1.hat <- forecast.matches0.display$lambda1.hat-as.numeric(closed.door==TRUE)*0.21
        forecast.matches0.display$lambda2.hat <- forecast.matches0.display$lambda2.hat+as.numeric(closed.door==TRUE)*0.08
        forecast.matches0.display <- forecast.matches0.display[forecast.matches0.display$team1!="Bury" & forecast.matches0.display$team2!="Bury",]##chop out bury
        forecast.matches0.display <- merge(forecast.matches0.display,bk.odds0,
                                           by.x=c("team1","team2"),by.y=c("Group.1","Group.2"),all.x=TRUE)
        forecast.matches0.display$team1 <- gsub("afc wimbledon","afc w'bledon",gsub("bristol c","bristol city",gsub("bristol r","bristol rovers",to3(forecast.matches0.display$team1))))
        forecast.matches0.display$team2 <- gsub("afc wimbledon","afc w'bledon",gsub("bristol c","bristol city",gsub("bristol r","bristol rovers",to3(forecast.matches0.display$team2))))
        forecast.matches0.display[,c(5:6,9:19)] <- round(100*forecast.matches0.display[,c(5:6,9:19)],1)
        forecast.matches0.displayx <- xtable(forecast.matches0.display,digits = c(0,0,0,2,2,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1))
        #names(forecast.matches0.displayx) <- c("","","EG1","EG2","pH","pA","ML","CML","p2-0","p2-1","p1-0","p0-0","p1-1","p2-2","p0-1","p1-2","p0-2")
        align(forecast.matches0.displayx) <- "lll|rr|rr|rr|rrr|rrr|rrr|rr"
        addtorow <- list()
        addtorow$pos <- list(0, 0)
        addtorow$command <- c("& & \\multicolumn{2}{|c}{Expected Goals}& \\multicolumn{2}{|c}{Outcome Probs} & \\multicolumn{2}{|c}{Score Picks} & \\multicolumn{3}{|c}{Home wins} & \\multicolumn{3}{|c}{Draws} & \\multicolumn{3}{|c}{Away wins} & \\multicolumn{2}{|c}{Mean Odds} \\\\\n",
                              paste0("\\multicolumn{2}{l|}{",wanteddiv_id,"} & Home & Away & Home & Away & Most & Cond & 2-0 & 2-1 & 1-0 & 0-0 & 1-1 & 2-2 & 0-1 & 1-2 & 0-2 & Home & Away \\\\\n"))
        print(xtable(forecast.matches0.displayx), add.to.row = addtorow, include.colnames = FALSE)
        print(forecast.matches0.displayx, add.to.row = addtorow, include.colnames = FALSE, type = "latex", 
              floating = FALSE,
              include.rownames=FALSE, file=paste0(dbloc,"/data/",wanteddiv_id,"/",wanteddiv_id,"-2122-",date0,".tex"))
        
        if(closed.door==FALSE) {
          fileConn<-file(paste0(dbloc,"/data/",wanteddiv_id,"/",wanteddiv_id,"-2122-",date0,"-comp.tex"))
        } else {
          fileConn<-file(paste0(dbloc,"/data/",wanteddiv_id,"/",wanteddiv_id,"-2122-",date0,"-comp-closed-door.tex"))
        }
        writeLines(c("\\documentclass{standalone}","\\begin{document}",paste0("\\input{",dbloc,"/data/",wanteddiv_id,"/",wanteddiv_id,"-2122-",date0,".tex}"),"\\end{document}"), fileConn)
        close(fileConn)
        
        if(closed.door==FALSE) {
          cmds <- c(paste0("cd ",dbloc,"/data/",wanteddiv_id,"/"), 
                    paste0("pdflatex ",wanteddiv_id,"-2122-",date0,"-comp.tex"), 
                    paste0("convert -density 300 ",wanteddiv_id,"-2122-",date0,"-comp.pdf ",wanteddiv_id,"-2122-",date0,"-comp.jpg"));
        } else {
          cmds <- c(paste0("cd ",dbloc,"/data/",wanteddiv_id,"/"), 
                    paste0("pdflatex ",wanteddiv_id,"-2122-",date0,"-comp-closed-door.tex"), 
                    paste0("convert -density 300 ",wanteddiv_id,"-2122-",date0,"-comp-closed-door.pdf ",wanteddiv_id,"-2122-",date0,"-comp-closed-door.jpg"));
        }
        system(paste(cmds, collapse=";"))
      }
    }    
  } else {
    forecast.matches <- NULL
  }
  
  return = forecast.matches
}


##updating in the simulation
quickupdate <- function(data=res,elorank.u=elorank0,results=forecasts,elo.weight=10,ids=FALSE) {
  if(is.null(results)==FALSE) {
    data$goals1[data$match_id %in% forecasts$match_id] <- forecasts$goals1
    data$goals2[data$match_id %in% forecasts$match_id] <- forecasts$goals2
    data$outcome[data$match_id %in% forecasts$match_id] <- forecasts$outcome
    
    #update elo
    adjustment <- elo.weight*(as.numeric(data$outcome[data$match_id %in% forecasts$match_id]) - as.numeric(data$elopredict[data$match_id %in% forecasts$match_id]))
    if(ids==TRUE) {
      elorank.u[paste0("id",data$team1_id[data$match_id %in% forecasts$match_id])] <- unlist(elorank.u[paste0("id",data$team1_id[data$match_id %in% forecasts$match_id])]) + adjustment
      elorank.u[paste0("id",data$team2_id[data$match_id %in% forecasts$match_id])] <- unlist(elorank.u[paste0("id",data$team2_id[data$match_id %in% forecasts$match_id])]) - adjustment
    } else {
      elorank.u[data$team1[data$match_id %in% forecasts$match_id]] <- unlist(elorank.u[data$team1[data$match_id %in% forecasts$match_id]]) + adjustment
      elorank.u[data$team2[data$match_id %in% forecasts$match_id]] <- unlist(elorank.u[data$team2[data$match_id %in% forecasts$match_id]]) - adjustment
    }
    
    #update future matches for all teams
    if(ids==TRUE) {
      comp.teams <- unique(paste0("id",c(data$team1_id,data$team2_id)))[unique(paste0("id",c(data$team1_id,data$team2_id))) %in% names(elorank.u)]
      for(tt in comp.teams) {
        ##elo
        data$elostrength1[data$team1_id==gsub("id","",tt) & data$date>unique(forecasts$date)] <- elorank.u[[tt]]
        data$elostrength2[data$team2_id==gsub("id","",tt) & data$date>unique(forecasts$date)] <- elorank.u[[tt]]
        ##goals in last matches
        if(NROW(forecasts$goals1[forecasts$team1==tt])>0) {
          data$goals1.1[data$team1_id==gsub("id","",tt) & data$date>unique(forecasts$date)] <- forecasts$goals1[forecasts$team1_id==gsub("id","",tt)]
        }
        if(NROW(forecasts$goals2[forecasts$team2==tt])) {
          data$goals2.1[data$team2_id==gsub("id","",tt) & data$date>unique(forecasts$date)] <- forecasts$goals2[forecasts$team2_id==gsub("id","",tt)]
        }
        if(NROW(forecasts$goals2[forecasts$team1==tt])) {
          data$goals2.1.1[data$team1_id==gsub("id","",tt) & data$date>unique(forecasts$date)] <- forecasts$goals2[forecasts$team1_id==gsub("id","",tt)]
        }
        if(NROW(forecasts$goals1[forecasts$team2==tt])) {
          data$goals1.2.1[data$team2_id==gsub("id","",tt) & data$date>unique(forecasts$date)] <- forecasts$goals1[forecasts$team2_id==gsub("id","",tt)]
        }
      }
    } else {
      comp.teams <- unique(c(data$team1,data$team2))[unique(c(data$team1,data$team2)) %in% names(elorank.u)]
      for(tt in comp.teams) {
        ##elo
        data$elostrength1[data$team1==tt & data$date>unique(forecasts$date)] <- elorank.u[[tt]]
        data$elostrength2[data$team2==tt & data$date>unique(forecasts$date)] <- elorank.u[[tt]]
        ##goals in last matches
        if(NROW(forecasts$goals1[forecasts$team1==tt])>0) {
          data$goals1.1[data$team1==tt & data$date>unique(forecasts$date)] <- forecasts$goals1[forecasts$team1==tt]
        }
        if(NROW(forecasts$goals2[forecasts$team2==tt])) {
          data$goals2.1[data$team2==tt & data$date>unique(forecasts$date)] <- forecasts$goals2[forecasts$team2==tt]
        }
        if(NROW(forecasts$goals2[forecasts$team1==tt])) {
          data$goals2.1.1[data$team1==tt & data$date>unique(forecasts$date)] <- forecasts$goals2[forecasts$team1==tt]
        }
        if(NROW(forecasts$goals1[forecasts$team2==tt])) {
          data$goals1.2.1[data$team2==tt & data$date>unique(forecasts$date)] <- forecasts$goals1[forecasts$team2==tt]
        }
      }
    }
  }
  
  updatelist = list("elorank"=elorank.u,"res"=data)
  return = updatelist
}




datasetup2022 <- function(data) {##adding in and creating information per team per match
  data=res0 ##for now while testing
  data$team1 <- tolower(data$team1)
  data$team2 <- tolower(data$team2)
  data$team1[regexpr("afc w",data$team1)>-1] <- "afc wimbledon"
  data$team1[regexpr("middlesbro",data$team1)>-1] <- "middlesbrough"
  data$team1[regexpr("palace",data$team1)>-1] <- "crystal palace"
  data$team1[regexpr("erzegbirge a",data$team1)>-1] <- "erzegbirge a"
  data$team2[regexpr("afc w",data$team2)>-1] <- "afc wimbledon"
  data$team2[regexpr("middlesbro",data$team2)>-1] <- "middlesbrough"
  data$team2[regexpr("palace",data$team2)>-1] <- "crystal palace"
  data$team2[regexpr("erzegbirge a",data$team2)>-1] <- "erzegbirge a"
  
  ##load in transfer information - transfers since last match, total since previous June
  transfers <- read.csv("/Volumes/11330730-dp/Correct-score/data/soccerbase/transfers-sb.csv",stringsAsFactors = FALSE)
  
  transfers$team <- tolower(gsub("^(.*?)_(\\d+)[.]html$","\\1",transfers$filename))
  transfers$season <- paste0(1870+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",transfers$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",transfers$filename))>145),"-",
                             1871+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",transfers$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",transfers$filename))>145))
  
  transfers$date <- as.Date(transfers$start.date,"%d %b, %Y")
  transfers$date[is.na(transfers$date)] <- as.Date(transfers$end[is.na(transfers$date)],"%d %b, %Y")
  
  transfers$players.in <- as.numeric(transfers$type=="in")
  transfers$players.out <- as.numeric(transfers$type=="out")
  transfers$loan.in <- as.numeric(transfers$type=="loaned.in")
  transfers$loan.out <- as.numeric(transfers$type=="loaned.out")
  transfers$releases <- as.numeric(transfers$type=="released")
  
  transfers$fee2 <- as.numeric(gsub(",","",gsub("£","",gsub("&pound;","",transfers$fee))))
  transfers$fee2[is.na(transfers$fee2)==TRUE] <- 0
  transfers$fees.paid <- as.numeric(transfers$players.in==1)*transfers$fee2
  transfers$fees.received <- as.numeric(transfers$players.out==1)*transfers$fee2
  
  transfers <- transfers[order(transfers$date),]
  
  transfers$team[regexpr("afc w",transfers$team)>-1] <- "afc wimbledon"
  transfers$team[regexpr("middlesbro",transfers$team)>-1] <- "middlesbrough"
  transfers$team[regexpr("palace",transfers$team)>-1] <- "crystal palace"
  transfers$team[regexpr("erzegbirge a",transfers$team)>-1] <- "erzegbirge a"
  tr.teams <- sort(unique(transfers$team))
  
  res.transfer.m <- data.frame(stringsAsFactors = FALSE)
  res.transfer.j <- data.frame(stringsAsFactors = FALSE)
  for(tt in tr.teams) {
    #print(tt)
    team.tr.dates <- data.frame("date"=seq.Date(from=min(transfers$date[transfers$team==tt]),
                                                to=max(transfers$date[transfers$team==tt]),
                                                by="days"))
    team.tr.dates$season <- as.numeric(format(team.tr.dates$date,"%Y")) - as.numeric(as.numeric(format(team.tr.dates$date,"%m"))<7)
    team.tr.dates$team <- tt
    team.tr.dates <- merge(team.tr.dates,
                           transfers[transfers$team==tt,c("date","players.in","players.out",
                                                          "loan.in","loan.out","releases",
                                                          "fees.paid","fees.received")],
                           by=c("date"),all.x=TRUE)
    #  team.tr.dates <- merge(team.tr.dates,
    #                          transfers[transfers$team==tt,c("to","coach","coach.id")],
    #                          by.x=c("date"),by.y=c("to"),all.x=TRUE,suffixes=c(".from",".to"))
    
    team.matches.h <- data[tolower(data$team1)==tt,c("match_id","date","team2","goals1","goals2","outcome","div_id")]
    colnames(team.matches.h)[3] <- "opp"
    team.matches.a <- data[tolower(data$team2)==tt,c("match_id","date","team1","goals1","goals2","outcome","div_id")]
    team.matches.a$outcome <- 1 - team.matches.a$outcome
    temp <- team.matches.a$goals1
    team.matches.a$goals1 <- team.matches.a$goals2
    team.matches.a$goals2 <- temp
    colnames(team.matches.a)[3] <- "opp"
    team.matches <- rbind(team.matches.h,team.matches.a)
    team.tr.dates <- merge(team.tr.dates,team.matches[,c("date","match_id")],by=c("date"),all.x=TRUE)
    
    if(any(!is.na(team.tr.dates$match_id))==TRUE) {
      team.tr.dates <- team.tr.dates[order(team.tr.dates$date),]
      
      ##now to get for all transfers since previous June 
      team.tr.dates$players.in[is.na(team.tr.dates$players.in)] <- 0
      team.tr.dates$players.out[is.na(team.tr.dates$players.out)] <- 0
      team.tr.dates$loan.in[is.na(team.tr.dates$loan.in)] <- 0
      team.tr.dates$loan.out[is.na(team.tr.dates$loan.out)] <- 0
      team.tr.dates$releases[is.na(team.tr.dates$releases)] <- 0
      team.tr.dates$fees.paid[is.na(team.tr.dates$fees.paid)] <- 0
      team.tr.dates$fees.received[is.na(team.tr.dates$fees.received)] <- 0
      team.tr.dates1 <- aggregate(team.tr.dates[,c("players.in","players.out",
                                                   "loan.in","loan.out","releases",
                                                   "fees.paid","fees.received")],
                                  by=list(team.tr.dates$date),FUN=sum)
      team.tr.dates1$season <- as.numeric(format(team.tr.dates1$Group.1,"%Y")) - as.numeric(as.numeric(format(team.tr.dates1$Group.1,"%m"))<7)
      team.tr.dates1$cplayers.in <- ave(team.tr.dates1$players.in,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cplayers.out <- ave(team.tr.dates1$players.out,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cloan.in <- ave(team.tr.dates1$loan.in,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cloan.out <- ave(team.tr.dates1$loan.out,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$creleases <- ave(team.tr.dates1$releases,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cfees.paid <- ave(team.tr.dates1$fees.paid,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$cfees.received <- ave(team.tr.dates1$fees.received,team.tr.dates1$season,FUN=cumsum)
      team.tr.dates1$team <- tt
      res.transfer.j <- rbind(res.transfer.j,team.tr.dates1)
      
      team.tr.dates <- team.tr.dates[order(-as.numeric(team.tr.dates$date)),]
      team.tr.dates$match_id <- na.locf(team.tr.dates$match_id,na.rm = FALSE)
      team.tr.dates <- team.tr.dates[order(team.tr.dates$date),]
      
      team.tr.last.m <- aggregate(team.tr.dates[,c("players.in","players.out",
                                                   "loan.in","loan.out","releases",
                                                   "fees.paid","fees.received")],
                                  by=list(team.tr.dates$match_id),FUN=sum,na.rm=TRUE)
      team.tr.last.m$team <- tt
      res.transfer.m <- rbind(res.transfer.m,team.tr.last.m)
    }
  }
  res.transfer.m$total.in <- res.transfer.m$players.in + res.transfer.m$loan.in
  res.transfer.m$total.out <- res.transfer.m$players.out + res.transfer.m$loan.out + res.transfer.m$releases
  res.transfer.j$jtotal.in <- res.transfer.j$cplayers.in + res.transfer.j$cloan.in
  res.transfer.j$jtotal.out <- res.transfer.j$cplayers.out + res.transfer.j$cloan.out + res.transfer.j$creleases
  colnames(res.transfer.j) <- gsub("cfees","jfees",colnames(res.transfer.j))
  data <- merge(data,res.transfer.m[,c("team","Group.1","total.in","total.out","fees.paid","fees.received")],
                by.x=c("team1","match_id"),by.y=c("team","Group.1"),all.x=TRUE)
  data <- merge(data,res.transfer.m[,c("team","Group.1","total.in","total.out","fees.paid","fees.received")],
                by.x=c("team2","match_id"),by.y=c("team","Group.1"),suffixes=c(".H",".A"),all.x=TRUE)
  data <- merge(data,res.transfer.j[,c("team","Group.1","jtotal.in","jtotal.out","jfees.paid","jfees.received")],
                by.x=c("team1","date"),by.y=c("team","Group.1"),all.x=TRUE)
  data <- merge(data,res.transfer.j[,c("team","Group.1","jtotal.in","jtotal.out","jfees.paid","jfees.received")],
                by.x=c("team2","date"),by.y=c("team","Group.1"),suffixes=c(".H",".A"),all.x=TRUE)
  
  ##load in manager information
  loc <- "/Volumes/11330730-dp/Correct-score/data/soccerbase/"
  managers <- read.csv(paste0(loc,"tenures.csv"),stringsAsFactors = FALSE)
  
  #sort out dates
  managers$from <- as.Date(managers$from,"%d %b, %Y")
  managers$to[managers$to=="Present"] <- format(Sys.Date()+350,"%d %b, %Y")#"21 Jan, 2015"
  managers$to <- as.Date(managers$to,"%d %b, %Y")
  managers$temp[managers$to<managers$from] <- managers$to[managers$to<managers$from]
  managers$to[managers$to<managers$from] <- managers$from[managers$to<managers$from]
  managers$from[managers$to<managers$from] <- managers$temp[managers$to<managers$from]
  managers$temp <- NULL
  managers$Duration <- managers$to - managers$from
  
  ##teams
  managers$team <- tolower(gsub("[.]html","",managers$team))
  managers <- managers[managers$team!="afc fylde",]
  managers <- managers[managers$team!="afc w'don",]
  managers <- managers[managers$team!="afc wimbledon",]
  managers <- managers[managers$team!="c palace",]
  managers <- managers[managers$team!="chester fc",]
  managers <- managers[managers$team!="e. frankfurt",]
  managers <- managers[managers$team!="erzegbirge aue",]
  managers <- managers[managers$team!="gfc ajaccio",]
  managers <- managers[managers$team!="harrogate t",]
  managers <- managers[managers$team!="hereford fc",]
  managers <- managers[managers$team!="hungerford t",]
  managers <- managers[managers$team!="mgladbach",]
  managers <- managers[managers$team!="paris st-g.",]
  managers <- managers[managers$team!="solihull moors",]
  managers <- managers[managers$team!="sv darmstadt 98",]
  managers$team[regexpr("afc w",managers$team)>-1] <- "afc wimbledon"
  man.teams <- sort(unique(managers$team))
  
  res.manager <- data.frame(stringsAsFactors = FALSE)
  for(tt in man.teams) {
    print(tt)
    team.man.dates <- data.frame("date"=seq.Date(from=min(managers$from[managers$team==tt]),
                                                 to=max(managers$to[managers$team==tt]),
                                                 by="days"))
    team.man.dates$team <- tt
    team.man.dates <- merge(team.man.dates,
                            managers[managers$team==tt,c("from","coach","coach.id","Duration")],
                            by.x=c("date"),by.y=c("from"),all.x=TRUE)
    #  team.man.dates <- merge(team.man.dates,
    #                          managers[managers$team==tt,c("to","coach","coach.id")],
    #                          by.x=c("date"),by.y=c("to"),all.x=TRUE,suffixes=c(".from",".to"))
    team.man.dates$coach <- na.locf(team.man.dates$coach)
    team.man.dates$coach.id <- na.locf(team.man.dates$coach.id)
    team.man.dates$tenure.days <- sequence(rle(team.man.dates$coach.id)$lengths)
    team.man.dates$Duration <- na.locf(team.man.dates$Duration)
    
    team.matches.h <- data[tolower(data$team1)==tt,c("match_id","date","team2","goals1","goals2","outcome","div_id")]
    colnames(team.matches.h)[3] <- "opp"
    team.matches.a <- data[tolower(data$team2)==tt,c("match_id","date","team1","goals1","goals2","outcome","div_id")]
    team.matches.a$outcome <- 1 - team.matches.a$outcome
    temp <- team.matches.a$goals1
    team.matches.a$goals1 <- team.matches.a$goals2
    team.matches.a$goals2 <- temp
    colnames(team.matches.a)[3] <- "opp"
    team.matches <- rbind(team.matches.h,team.matches.a)
    team.man.dates <- merge(team.man.dates,team.matches,by=c("date"))
    
    if(NROW(team.man.dates)>0) {
      ##manager lagged
      team.man.dates$coach.id.1 <- c(NA,team.man.dates$coach.id[-NROW(team.man.dates)])
      team.man.dates$new.coach <- as.numeric(team.man.dates$coach.id.1!=team.man.dates$coach.id)
      
      team.man.dates$coach.id.p1 <- c(team.man.dates$coach.id[-1],NA)
      team.man.dates$old.coach <- as.numeric(team.man.dates$coach.id.p1!=team.man.dates$coach.id)
      
      team.man.dates$played <- 1
      team.man.dates$won <- as.numeric(team.man.dates$outcome==1)
      team.man.dates$drawn <- as.numeric(team.man.dates$outcome==0.5)
      team.man.dates$lost <- as.numeric(team.man.dates$outcome==0)
      team.man.dates$cplayed <- ave(team.man.dates$played, team.man.dates$coach.id, FUN=cumsum)
      team.man.dates$cwon <- ave(team.man.dates$won, team.man.dates$coach.id, FUN=cumsum)
      team.man.dates$cdrawn <- ave(team.man.dates$drawn, team.man.dates$coach.id, FUN=cumsum)
      team.man.dates$cpoints <- 3*team.man.dates$cwon + team.man.dates$cdrawn
      team.man.dates$win.pc <- 100*team.man.dates$cwon/team.man.dates$cplayed
      team.man.dates$points.per.game <- team.man.dates$cpoints/team.man.dates$cplayed
      
      team.man.dates$won.first.game <- as.numeric(team.man.dates$cplayed==1 & team.man.dates$won==1)
      team.man.dates$lost.first.game <- as.numeric(team.man.dates$cplayed==1 & team.man.dates$lost==1)
      res.manager <- rbind(res.manager,team.man.dates)
    }
  }
  
  res.manager <- res.manager[duplicated(res.manager[,c("team","match_id")])==FALSE,]
  data <- merge(data,
                res.manager[,c("match_id","team","tenure.days","new.coach","cplayed","win.pc","points.per.game","won.first.game","lost.first.game")],
                by.x=c("team1","match_id"),by.y=c("team","match_id"),all.x=TRUE)
  data <- merge(data,
                res.manager[,c("match_id","team","tenure.days","new.coach","cplayed","win.pc","points.per.game","won.first.game","lost.first.game")],
                by.x=c("team2","match_id"),by.y=c("team","match_id"),all.x=TRUE,suffixes=c(".H",".A"))
  
  
  ##load in squad info
  squads <- read.csv("/Volumes/11330730-dp/Correct-score/data/soccerbase/squads-sb.csv",stringsAsFactors = FALSE)
  
  
  squads$team <- tolower(gsub("^(.*?)_(\\d+)[.]html$","\\1",squads$filename))
  squads$filename <- gsub("2021-08-02","154",squads$filename)
  squads$season <- 1870+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))>145)
  squads <- squads[order(squads$season),]
  
  squads$player.position <- gsub("^(.*?) [(](.*?)[)].*\\s*","\\2",squads$player.name)
  squads$player.position[squads$player.position=="GK"] <- "G"
  squads$player.position[nchar(squads$player.position)>1] <- NA
  
  squads$surname <- gsub("^(.*?) (.*?) [(](.*?)[)]","\\2",squads$player.name)
  
  squads$league.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$league.apps))
  squads$league.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$league.apps))
  squads$facup.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$facup.apps))
  squads$facup.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$facup.apps))
  squads$lgecup.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$lgecup.apps))
  squads$lgecup.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$lgecup.apps))
  squads$other.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$other.apps))
  squads$other.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$other.apps))
  
  squads$total.apps <- squads$league.apps + squads$facup.apps + squads$lgecup.apps + squads$other.apps
  squads$total.sub.apps <- squads$league.sub.apps + squads$facup.sub.apps + squads$lgecup.sub.apps + squads$other.sub.apps
  squads$total.total.apps <- squads$total.apps + squads$total.sub.apps
  
  squads$total.goals <- squads$league.goals + squads$facup.goals + squads$lgecup.goals + squads$other.goals
  
  squad.totals <- aggregate(squads[,c("total.total.apps","total.goals")],
                            by=list(squads$team,squads$season),FUN=sum,na.rm=TRUE)
  data$season <- as.numeric(format(data$date,"%Y")) - as.numeric(as.numeric(format(data$date,"%m"))<7)
  
  data <- merge(data,squad.totals,by.x=c("team1","season"),by.y=c("Group.1","Group.2"),all.x=TRUE)
  data <- merge(data,squad.totals,by.x=c("team2","season"),by.y=c("Group.1","Group.2"),all.x=TRUE,suffixes=c(".H",".A"))
  
  ##promotion/relegation previous season
  comps <- data[duplicated(data$div_id)==FALSE,c("division","div_id")]
  comps <- comps[order(comps$div_id),]
  comps$tier <- c(NA,1,2,3,4,1,2,3,4,5,3,3,1,2,3,4,1,2,3,1,1,1,1,1,1,1,1,1,1,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,7,1,NA,7,5,
                  NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                  NA,NA,NA,2,1,NA,NA,2,1,1,1,1,6,6,1,2,2,1,NA,5,NA,NA)
  ##league IDs
  league.ids <- c(1:25,47:48,56,75,76,80,82,111:118,122:134,170:171,194,205:311,353)
  
  club.season.comp0 <- data[data$div_id %in% league.ids,c("team1","season","div_id")]
  colnames(club.season.comp0) <- c("team","season","div_id")
  club.season.comp <- data[data$div_id %in% league.ids,c("team2","season","div_id")]
  colnames(club.season.comp) <- c("team","season","div_id")
  club.season.comp <- rbind(club.season.comp,club.season.comp0)
  rm(club.season.comp0)
  club.season.comp <- club.season.comp[duplicated(club.season.comp)==FALSE,]
  club.season.comp <- club.season.comp[duplicated(club.season.comp[,c("team","season")])==FALSE,]
  
  club.season.comp <- club.season.comp[order(club.season.comp$team,club.season.comp$season),]
  club.season.comp$team.1 <- c(-1000,club.season.comp$team[-NROW(club.season.comp)])
  club.season.comp$div_id.1 <- c(NA,club.season.comp$div_id[-NROW(club.season.comp)])
  club.season.comp$div_id.1[club.season.comp$team!=club.season.comp$team.1] <- NA
  club.season.comp$promoted <- as.numeric(club.season.comp$div_id<club.season.comp$div_id.1)
  club.season.comp$relegated <- as.numeric(club.season.comp$div_id>club.season.comp$div_id.1)
  
  data <- merge(data,club.season.comp[,c("team","season","promoted","relegated")],
                by.x=c("team1","season"),by.y=c("team","season"),all.x=TRUE)
  data <- merge(data,club.season.comp[,c("team","season","promoted","relegated")],
                by.x=c("team2","season"),by.y=c("team","season"),all.x=TRUE,suffixes=c(".H",".A"))
  
  ##league tables
  res.tab <- data.frame()
  res.final.tabs <- data.frame()
  seasons <- unique(data$season)
  seasons <- seasons[order(seasons)]
  
  for( ss in seasons) {
    print(ss)
    
    for( ll in league.ids) {
      #if(ss==2019 & ll<5) { next }
      #print(ll)
      matches <- res[data$season==ss & data$div_id==ll,c("match_id","date","team1","goals1","goals2","team2")]
      matches <- matches[is.na(matches$match_id)==F,]
      if(NROW(matches)>10) {
        if(var(matches$date)==0) {
          matches$date <- matches$date + 1:NROW(matches)
        }
        lge.all <- league.tab(matches$match_id,as.Date(matches$date),matches$team1,
                              matches$goals1,matches$goals2,matches$team2,
                              2+as.numeric( (ss>1980 & ll %in% 1:11 ) | (ss>1994 & !(ll %in% 1:11) ) ))
        lge <- lge.all$matches
        
        final.lge.tab <- lge.all$final.table
        final.lge.tab$season <- ss
        final.lge.tab$div_id <- ll
        
        colnames(lge)[grep("game_id",colnames(lge))] <- "match_id"
        colnames(lge)[grep("g1",colnames(lge))] <- "goals1"
        colnames(lge)[grep("g2",colnames(lge))] <- "goals2"
        lge$season <- ss
        lge$div_id <- ll
        res.tab <- rbind(res.tab,lge)
        res.final.tabs <- rbind(res.final.tabs,final.lge.tab)
      }
    }
  }
  
  res.tab <- res.tab[duplicated(res.tab$match_id)==F,]
  data <- data[duplicated(data$match_id)==FALSE,]
  data <- merge(data,res.tab[,c(1,7:22)],by=c("match_id"),all.x=T)
  
  ##save league tables up to end of previous season
  write.csv(res.tab,paste0(dbloc,"/data/league-tabs-1888-2020.csv"))
  
  ##save final league tables up to end of previous season
  res.final.tabs <- res.final.tabs[duplicated(res.final.tabs$match_id)==F,]
  write.csv(res.final.tabs,paste0(dbloc,"/data/final-league-tabs-1888-2020.csv"))
  res.final.tabs <- read.csv(paste0(dbloc,"/data/final-league-tabs-1888-2020.csv"),stringsAsFactors = FALSE)
  data <- merge(data,comps[,c("div_id","tier")],by=c("div_id"),all.x=TRUE)
  data <- merge(data,comps[,c("div_id","tier")],by=c("div_id"),all.x=TRUE,suffixes = c(".H",".A"))
  
  ##copy/paste into all future matches and fill in gaps (from league table creation) using na.locf
  data <- data[order(data$date),]
  all.teams <- sort(unique(c(data$team1,data$team2)))
  Hcols.to.do <- colnames(data)[regexpr("[.]H$|\\w1$",colnames(data))>-1]
  Acols.to.do <- colnames(data)[regexpr("[.]A$|\\w2$",colnames(data))>-1]
  for(tt in all.teams) {
    print(tt)
    fill.data0 <- data[data$team1==tt,Hcols.to.do[-1:-2]]
    if(NROW(fill.data0)>0) {
      fill.data0$home <- 1
      colnames(fill.data0) <- gsub(".H|1","",colnames(fill.data0))
    }
    fill.data <- data[data$team2==tt,Acols.to.do[-1:-2]]
    if(NROW(fill.data)>0) {
      fill.data$home <- 0
      colnames(fill.data) <- gsub(".A|2","",colnames(fill.data))
    }
    fill.data <- rbind(fill.data0,fill.data)
    fill.data <- na.locf(fill.data,na.rm=FALSE)
    
    data[data$team1==tt,Hcols.to.do[-1:-2]] <- fill.data[fill.data$home==1,-NCOL(fill.data)]
    data[data$team2==tt,Acols.to.do[-1:-2]] <- fill.data[fill.data$home==0,-NCOL(fill.data)]
  }
  
  data <- data[order(data$team1,data$date),]
  
  data$tier.diff <- data$tier.H - data$tier.A
  data$promoted.diff <- data$promoted.H - data$promoted.A
  data$relegated.diff <- data$relegated.H - data$relegated.A
  data$win.pc.diff <- data$win.pc.H - data$win.pc.A
  data$cplayed.diff <- data$cplayed.H - data$cplayed.A
  data$new.coach.diff <- data$new.coach.H - data$new.coach.A
  
  ##creating info based on match results
  data$team1.1 <- c(NA,data$team1[-NROW(data)])
  data$team1.2 <- c(NA,data$team1.1[-NROW(data)])
  data$goals1.1 <- c(NA,data$goals1[-NROW(data)])
  data$goals1.1[data$team1.1!=data$team1] <- NA
  
  ##info from last match by team
  data$scored.1.1 <- NA
  data$conceded.1.1 <- NA
  data$outcome.1.1 <- NA
  data$scored.1.1.6 <- NA
  data$conceded.1.1.6 <- NA
  data$outcome.1.1.6 <- NA
  data$scored.2.1 <- NA
  data$conceded.2.1 <- NA
  data$outcome.2.1 <- NA
  data$scored.2.1.6 <- NA
  data$conceded.2.1.6 <- NA
  data$outcome.2.1.6 <- NA
  teams <- sort(unique(c(data$team1,data$team2)))
  data <- data[order(data$date),]
  for(tt in teams) {
    #print(tt)
    t1 <- data[data$team1==tt,c("date","goals1","goals2","outcome")]
    colnames(t1) <- c("date","scored","conceded","outcome")
    t1$home = rep(1,NROW(t1))
    t2 <- data[data$team2==tt,c("date","goals2","goals1","outcome")]
    t2$outcome <- 1-t2$outcome
    colnames(t2) <- c("date","scored","conceded","outcome")
    t2$home <- rep(0,NROW(t2))
    team.data <- rbind(t1,t2)
    team.data <- team.data[order(team.data$date),]
    if(NROW(team.data)>6) {
      team.data$scored.1 <- c(NA, team.data$scored[-NROW(team.data)])
      team.data$conceded.1 <- c(NA, team.data$conceded[-NROW(team.data)])
      team.data$outcome.1 <- c(NA, team.data$outcome[-NROW(team.data)])
      team.data$cscored <- cumsum(team.data$scored)
      team.data$cconceded <- cumsum(team.data$conceded)
      team.data$coutcome <- cumsum(team.data$outcome)
      team.data$cscored.1 <- c(0, team.data$cscored[-NROW(team.data)])
      team.data$cconceded.1 <- c(0, team.data$cconceded[-NROW(team.data)])
      team.data$coutcome.1 <- c(0, team.data$coutcome[-NROW(team.data)])
      team.data$cscored.7 <- c(rep(0,7), team.data$cscored[-seq(NROW(team.data)-6,NROW(team.data))])
      team.data$cconceded.7 <- c(rep(0,7), team.data$cconceded[-seq(NROW(team.data)-6,NROW(team.data))])
      team.data$coutcome.7 <- c(rep(0,7), team.data$coutcome[-seq(NROW(team.data)-6,NROW(team.data))])
      team.data$scored.1.6 <- team.data$cscored.1 - team.data$cscored.7
      team.data$conceded.1.6 <- team.data$cconceded.1 - team.data$cconceded.7
      team.data$outcome.1.6 <- team.data$coutcome.1 - team.data$coutcome.7
    } else {
      team.data$scored.1 <- rep(NA,NROW(team.data))
      team.data$conceded.1 <- rep(NA,NROW(team.data))
      team.data$outcome.1 <- rep(NA,NROW(team.data))
      team.data$scored.1.6 <- rep(NA,NROW(team.data))
      team.data$conceded.1.6 <- rep(NA,NROW(team.data))
      team.data$outcome.1.6 <- rep(NA,NROW(team.data))
      
    }
    
    ##put back into full.data
    data[data$team1==tt,c("scored.1.1","conceded.1.1","outcome.1.1","scored.1.1.6","conceded.1.1.6","outcome.1.1.6")] <- team.data[team.data$home==1,c("scored.1","conceded.1","outcome.1","scored.1.6","conceded.1.6","outcome.1.6")]
    data[data$team2==tt,c("scored.2.1","conceded.2.1","outcome.2.1","scored.2.1.6","conceded.2.1.6","outcome.2.1.6")] <- team.data[team.data$home==0,c("scored.1","conceded.1","outcome.1","scored.1.6","conceded.1.6","outcome.1.6")]
  }
  
  ##lagged goals scored by away team in home team's previous home matches
  data$goals2.1.1 <- c(NA,data$goals2[-NROW(data)])
  data$goals2.1.1[data$team1.1!=data$team1] <- NA
  
  ##ditto for away team
  data <- data[order(data$team2,data$date),]
  data$team2.1 <- c(NA,data$team2[-NROW(data)])
  data$team2.2 <- c(NA,data$team2.1[-NROW(data)])
  data$goals2.1 <- c(NA,data$goals2[-NROW(data)])
  data$goals2.1[data$team2.1!=data$team2] <- NA
  
  ##lagged goals scored by home team in away team's previous away matches
  data$goals1.2.1 <- c(NA,data$goals1[-NROW(data)])
  data$goals1.2.1[data$team2.1!=data$team2] <- NA
  
  
  data <- data[order(data$date),]
  return = data
  
}

to3comp <- function(compname0) {
  correspondence <- data.frame("comp.long"=c("Premier League","English Premier","Premiership","English Div 1 (old)",
                                             "Championship","Football League Div 1","English Div 2 (old)","Football League Championship","English Championship",
                                             "League One","English League One","Football League Div 2","English Div 3 (old)",
                                             "League Two","English League Two","Football League Div 3","English Div 4 (old)","Division 4",
                                             "FA Cup","English FA Cup",
                                             "EFL Cup","Capital One Cup","Carling Cup","English League Cup","League Cup",
                                             "Football League Trophy","Football League Trop","JP Trophy",
                                             "Champions League","European Cup",
                                             "Europa League","UEFA Cup"),
                               "comp.short"=c("EPL","EPL","EPL","EPL",
                                              "EFLC","EFLC","EFLC","EFLC","EFLC",
                                              "EFL1","EFL1","EFL1","EFL1",
                                              "EFL2","EFL2","EFL2","EFL2","EFL2",
                                              "FAC","FAC",
                                              "LGC","LGC","LGC","LGC","LGC",
                                              "LGT","LGT","LGT",
                                              "CHL","CHL",
                                              "EUL","EUL"),
                               stringsAsFactors = FALSE)
  compname0 <- data.frame("comp.long" = compname0,stringsAsFactors = FALSE)
  compname0$comp.long <- gsub(" Play-Off","",compname0$comp.long)
  compname0$order <- 1:NROW(compname0)
  compname1 <- merge(compname0,correspondence,by="comp.long")
  return ( compname1$comp.short[order(compname1$order)] )
}


to3 <- function(teamnames0,soccerbase=TRUE) {
  ##first load up Reuters acronyms
  acronyms <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/acronyms-file.csv",stringsAsFactors = FALSE)
  teamnames.original <- teamnames0
  
  ##correct for common problems
  
  if(soccerbase==TRUE) {
    acronyms$Soccerbase <- tolower(acronyms$Soccerbase)
    teamnames0 <- tolower(teamnames0)
    teamnames0 <- gsub("w'bledon","wimbledon",teamnames0)
    teamnames0 <- data.frame("Soccerbase"=teamnames0,stringsAsFactors = FALSE)
    teamnames0$order <- 1:NROW(teamnames0)
    ##try to convert using these
    teamnames0 <- merge(teamnames0,acronyms[,c("Soccerbase","Code")],by=c("Soccerbase"),all.x=TRUE,sort=FALSE)
    teamnames0 <- teamnames0[order(teamnames0$order),]
    teamnames0$Soccerbase[is.na(teamnames0$Code)] <- gsub("[.]","",gsub(" AFC|AFC ","",teamnames0$Soccerbase[is.na(teamnames0$Code)]))
    teamnames0$Code[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Soccerbase)>-1] <- paste0(substr(toupper(teamnames0$Soccerbase[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Soccerbase)>-1]),1,2),substr(toupper(teamnames0$Soccerbase[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Soccerbase)>-1]),regexpr(" ",teamnames0$Soccerbase[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Soccerbase)>-1])+1,regexpr(" ",teamnames0$Soccerbase[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Soccerbase)>-1])+1))
    teamnames0$Code[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Soccerbase)==-1] <- substr(toupper(teamnames0$Soccerbase[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Soccerbase)==-1]),1,3)
  } else {
    teamnames0 <- data.frame("Team"=teamnames0,stringsAsFactors = FALSE)
    teamnames0$order <- 1:NROW(teamnames0)
    ##try to convert using these
    teamnames0 <- merge(teamnames0,acronyms[acronyms$Team!="" & duplicated(acronyms$Team)==FALSE,c("Team","Code")],by=c("Team"),all.x=TRUE,sort=FALSE)
    teamnames0 <- teamnames0[order(teamnames0$order),]
    teamnames0$Team[is.na(teamnames0$Code)] <- gsub("[.]","",gsub(" AFC|AFC ","",teamnames0$Team[is.na(teamnames0$Code)]))
    teamnames0$Code[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Team)>-1] <- paste0(substr(toupper(teamnames0$Team[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Team)>-1]),1,2),substr(toupper(teamnames0$Team[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Team)>-1]),regexpr(" ",teamnames0$Team[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Team)>-1])+1,regexpr(" ",teamnames0$Team[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Team)>-1])+1))
    teamnames0$Code[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Team)==-1] <- substr(toupper(teamnames0$Team[is.na(teamnames0$Code) & regexpr(" ",teamnames0$Team)==-1]),1,3)
  }
  
  return(teamnames0$Code)
}

narrative.tab <- function(forecast.matches = forecasts1,
                          n.team="oldham",type="normal",bookie.odds = bk.odds0,
                          tables = res.tab.2020, tab.goals=3,
                          dbloc="/Volumes/11330730-dp/Correct-score/") {
  divisions <- c("Premier League","Championship","League One","League Two","National League")
  ##need team id first off
  team.ids0 <- forecast.matches[duplicated(forecast.matches$team1_id)==FALSE,c("team1","team1_id")]
  colnames(team.ids0) <- c("team2","team2_id")
  team.ids <- rbind(team.ids0,forecast.matches[duplicated(forecast.matches$team2_id)==FALSE,c("team2","team2_id")])
  colnames(team.ids) <- c("team","team_id")
  n.team.id = team.ids$team_id[team.ids$team == n.team]
  
  match.details <- forecast.matches[forecast.matches$team1==n.team | forecast.matches$team2==n.team,]

  opp.team = toString(match.details[c("team1","team2")][which(match.details[c("team1","team2")] != n.team)])
  opp.team.id = toString(match.details[c("team1_id","team2_id")][which(match.details[c("team1_id","team2_id")] != n.team.id)])
  n.team.at.home <- n.team==match.details$team1
  match.details <- merge(match.details,bookie.odds,by.x=c("team1","team2"),by.y=c("Group.1","Group.2"),all.x=TRUE)

  if(type=="normal") {
    match.details <- merge(match.details,tables[c(1,grep("content|points",colnames(tables)))],by=c("match_id"),all.x=TRUE)
    
    ##get end-of-season probabilities
    div.name <- c("EPL","EFLC","EFL1","EFL2")[match.details$div_id]
    #end-of-season-EFL2-2020-10-28-summary.csv
    eos.file.name <- tail(sort(list.files(paste0(dbloc,"data/end-of-season/"),pattern=paste0("^end-of-season-",div.name,"-\\d*-\\d*-\\d*[.]csv$"))),1)
    eos <- read.csv(paste0(dbloc,"data/end-of-season/",eos.file.name),stringsAsFactors = FALSE)
    eos.prom <- eos[list(1,1:2,1:2,1:3)[[match.details$div_id]],]
    eos.playoffs <- eos[list(1:4,3:6,3:6,4:7)[[match.details$div_id]],]
    eos.releg <- eos[list(18:20,22:24,21:23,23:24)[[match.details$div_id]],]
    if(n.team.at.home) {
      home.team = n.team
      away.team = opp.team
    } else {
      home.team = opp.team
      away.team = n.team
    }
  } else {
    ##need each team's league record
    ##keep most recent one
    tables <- tables[order(tables$date),]
    n.team.lge.details <- tail(tables[tables$team1==n.team | tables$team2==n.team,],1)
    ##team home or away?
    if(n.team.lge.details$team1==n.team) {
      n.team.lge.details <- n.team.lge.details[,c("team1","div_id","pos1","pts1","wins1","gs1","gd1")]
    } else {
      n.team.lge.details <- n.team.lge.details[,c("team2","div_id","pos2","pts2","wins2","gs2","gd2")]
    }
    colnames(n.team.lge.details) <- gsub("1$|2$","",colnames(n.team.lge.details))
    
    ##for opp team
    ##need each team's league record
    ##first get all matches
    opp.team.lge.details <- tail(tables[tables$team1==opp.team | tables$team2==opp.team,],1)
    ##team home or away?
    if(opp.team.lge.details$team1==opp.team) {
      opp.team.lge.details <- opp.team.lge.details[,c("team1","div_id","pos1","pts1","wins1","gs1","gd1")]
    } else {
      opp.team.lge.details <- opp.team.lge.details[,c("team2","div_id","pos2","pts2","wins2","gs2","gd2")]
    }
    colnames(opp.team.lge.details) <- gsub("1$|2$","",colnames(opp.team.lge.details))
    
    ##now merge in. first delete NA entries
    # match.details[,c("pos1","pos2","pts1","pts2","wins1","wins2","gd1","gd2","gs1","gs2")] <- NULL
    # match.details$comp_id <- match.details$div_id
    # match.details$div_id <- NULL
    # if(n.team.at.home) {
    #   home.team = n.team
    #   away.team = opp.team
    #   match.details <- merge(match.details,n.team.lge.details,by.x=c("team1"),by.y=c("team"),all.x=TRUE)
    #   match.details <- merge(match.details,opp.team.lge.details,by.x=c("team2"),by.y=c("team"),all.x=TRUE,
    #                          suffixes= c("1","2"))
    # } else {
    #   home.team = opp.team
    #   away.team = n.team
    #   match.details <- merge(match.details,n.team.lge.details,by.x=c("team2"),by.y=c("team"),all.x=TRUE)
    #   match.details <- merge(match.details,opp.team.lge.details,by.x=c("team1"),by.y=c("team"),all.x=TRUE,
    #                          suffixes= c("1","2"))
    # }
    # match.details$division1 <- divisions[match.details$div_id1]
    # match.details$division2 <- divisions[match.details$div_id2]
  }
  res0.1 <- res0.1[order(res0.1$date),]
  res0.1$score <- paste0(res0.1$goals1,"-",res0.1$goals2)
  ##form for team
  n.team.form <- tail(res0.1[(tolower(res0.1$team1)==n.team | tolower(res0.1$team2)==n.team) & is.na(res0.1$goals1)==FALSE,c("team1","score","team2")],6)
  
  ##form for opposition
  opp.team.form <- tail(res0.1[(tolower(res0.1$team1)==opp.team | tolower(res0.1$team2)==opp.team) & is.na(res0.1$goals1)==FALSE,c("team1","score","team2","division")],6)
  opp.team.form <- opp.team.form[,c("team1","score","team2")]
  
  ##all matches between the two teams
  all.history <- res0.1[(tolower(res0.1$team1)==opp.team & tolower(res0.1$team2)==n.team) | (tolower(res0.1$team1)==n.team & tolower(res0.1$team2)==opp.team),c("team1","team2","goals1","goals2","date","elostrength1","elostrength2","full.pos1","full.pos2")]
  all.history$score <- paste0(all.history$goals1,"-",all.history$goals2)
  
  all.history[is.na(all.history$goals1)==FALSE,c("date","team1","score","team2")]
  all.history$full.pos.diff[all.history$team1=="oldham"] <- all.history$full.pos1[all.history$team1=="oldham"]-all.history$full.pos2[all.history$team1=="oldham"]
  all.history$full.pos.diff[all.history$team1!="oldham"] <- all.history$full.pos2[all.history$team1!="oldham"]-all.history$full.pos1[all.history$team1!="oldham"]
  hist.tab <- data.frame("Played" = c(NROW(all.history[is.na(all.history$goals1)==FALSE,]),
                                      NROW(all.history[is.na(all.history$goals1)==FALSE & all.history$team1==n.team,]),
                                      NROW(all.history[is.na(all.history$goals1)==FALSE & all.history$team1==opp.team,])))
  hist.tab[[to3(n.team)]] <- c(sum(c(all.history$goals1>all.history$goals2 & all.history$team1==n.team,all.history$goals1<all.history$goals2 & all.history$team2==n.team),na.rm=TRUE),
                               sum(c(all.history$goals1>all.history$goals2 & all.history$team1==n.team),na.rm=TRUE),
                               sum(c(all.history$goals1<all.history$goals2 & all.history$team2==n.team),na.rm=TRUE))
  hist.tab$Draws <- c(sum(all.history$goals1==all.history$goals2,na.rm=TRUE),
                      sum(all.history$goals1==all.history$goals2 & all.history$team1==n.team,na.rm=TRUE),
                      sum(all.history$goals1==all.history$goals2 & all.history$team2==n.team,na.rm=TRUE))
  hist.tab[[to3(opp.team)]] <- c(sum(c(all.history$goals1>all.history$goals2 & all.history$team2==n.team,all.history$goals1<all.history$goals2 & all.history$team1==n.team),na.rm=TRUE),
                                 sum(c(all.history$goals1<all.history$goals2 & all.history$team1==n.team),na.rm=TRUE),
                                 sum(c(all.history$goals1>all.history$goals2 & all.history$team2==n.team),na.rm=TRUE))
  hist.tab[[paste0(to3(n.team)," goals")]] <- round(c(sum(c(all.history$goals1[all.history$team1==n.team],all.history$goals2[all.history$team2==n.team]),na.rm=TRUE),
                                                sum(c(all.history$goals1[all.history$team1==n.team]),na.rm=TRUE),
                                                sum(c(all.history$goals2[all.history$team2==n.team]),na.rm=TRUE)))
  hist.tab[[paste0(to3(opp.team)," goals")]] <- round(c(sum(c(all.history$goals1[all.history$team1==opp.team],all.history$goals2[all.history$team2==opp.team]),na.rm=TRUE),
                                                  sum(c(all.history$goals2[all.history$team1==n.team]),na.rm=TRUE),
                                                  sum(c(all.history$goals1[all.history$team2==n.team]),na.rm=TRUE)))
  rownames(hist.tab) <- c("All",paste0("At ",to3(n.team)),paste0("At ",to3(opp.team)))
  
  n.team.record <- paste0("P",NROW(all.history[is.na(all.history$goals1)==FALSE,]),
                          " W",sum(c(all.history$goals1>all.history$goals2 & all.history$team1==n.team,all.history$goals1<all.history$goals2 & all.history$team2==n.team),na.rm=TRUE),
                          " D",sum(all.history$goals1==all.history$goals2,na.rm=TRUE),
                          " L",sum(c(all.history$goals1>all.history$goals2 & all.history$team2==n.team,all.history$goals1<all.history$goals2 & all.history$team1==n.team),na.rm=TRUE),
                          " For ",sum(c(all.history$goals1[all.history$team1==n.team],all.history$goals2[all.history$team2==n.team]),na.rm=TRUE),
                          " Against ",sum(c(all.history$goals2[all.history$team1==n.team],all.history$goals1[all.history$team2==n.team]),na.rm=TRUE))
  print(n.team.record)
  n.h.team.record <- paste0("P",NROW(all.history[is.na(all.history$goals1)==FALSE & all.history$team1==n.team,]),
                            " W",sum(c(all.history$goals1>all.history$goals2 & all.history$team1==n.team),na.rm=TRUE),
                            " D",sum(all.history$goals1==all.history$goals2 & all.history$team1==n.team,na.rm=TRUE),
                            " L",sum(c(all.history$goals1<all.history$goals2 & all.history$team1==n.team),na.rm=TRUE),
                            " For ",sum(c(all.history$goals1[all.history$team1==n.team]),na.rm=TRUE),
                            " Against ",sum(c(all.history$goals2[all.history$team1==n.team]),na.rm=TRUE))
  print(n.h.team.record)
  n.a.team.record <- paste0("P",NROW(all.history[is.na(all.history$goals1)==FALSE & all.history$team2==n.team,]),
                            " W",sum(c(all.history$goals1<all.history$goals2 & all.history$team2==n.team),na.rm=TRUE),
                            " D",sum(all.history$goals1==all.history$goals2 & all.history$team2==n.team,na.rm=TRUE),
                            " L",sum(c(all.history$goals1>all.history$goals2 & all.history$team2==n.team),na.rm=TRUE),
                            " For ",sum(c(all.history$goals2[all.history$team2==n.team]),na.rm=TRUE),
                            " Against ",sum(c(all.history$goals1[all.history$team2==n.team]),na.rm=TRUE))
  print(n.a.team.record)
  ##twitter ready
  #  print(paste0("#",toupper(substr(all.history$team1,1,3))," ",all.history$goals1,"-",all.history$goals2," ",toupper(substr(all.history$team2,1,3))," ",all.history$date))
  
  ##get all elo history for team
  all.elo.team0 <- res0.1[res0.1$team1_id==n.team.id,c("elostrength1","date","full.pos1")]#
  colnames(all.elo.team0) <- c("elo","date","pos")#
  all.elo.team1 <- res0.1[res0.1$team2_id==n.team.id,c("elostrength2","date","full.pos2")]#
  colnames(all.elo.team1) <- c("elo","date","pos")#
  all.elo.team <- rbind(all.elo.team0,
                        all.elo.team1)
  all.elo.team <- all.elo.team[order(all.elo.team$date),]
  
  ##get all elo history for opponents
  all.elo.opp0 <- res0.1[res0.1$team1_id==opp.team.id,c("elostrength1","date","full.pos1")]#
  colnames(all.elo.opp0) <- c("elo","date","pos") #
  all.elo.opp1 <- res0.1[res0.1$team2_id==opp.team.id,c("elostrength2","date","full.pos2")]#
  colnames(all.elo.opp1) <- c("elo","date","pos") #
  all.elo.opp <- rbind(all.elo.opp0,
                       all.elo.opp1)
  all.elo.opp <- all.elo.opp[order(all.elo.opp$date),]
  
  recents <- tail(all.history[is.na(all.history$goals1)==FALSE,c("team1","score","team2","date")],6)
  #recents$division <- gsub("English ","",recents$division)
  #recents$division[recents$division=="Premier"] <- "Premier League"
  
  ##make the main stats table --- want P(promotion) etc in there too
  if(type=="normal") {
    table0 <- as.matrix(rbind(as.character(round(match.details[,c("pos1","pos2")],0)),
                              as.character(c(sum(eos.prom[,to3(home.team)]),sum(eos.prom[,to3(away.team)]))),
                              as.character(c(sum(eos.playoffs[,to3(home.team)]),sum(eos.playoffs[,to3(away.team)]))),
                              as.character(c(sum(eos.releg[,to3(home.team)]),sum(eos.releg[,to3(away.team)]))),
                              as.character(round(match.details[,c("wins1","wins2")],0)),
                              as.character(round(match.details[,c("pts1","pts2")],0)),
                              as.character(round(match.details[,c("gs1","gs2")],0)),
                              as.character(round(match.details[,c("gs1","gs2")]-match.details[,c("gd1","gd2")],0)),
                              as.character(round(match.details[,c("elostrength1","elostrength2")],0)),
                              as.character(round(match.details[,c("lambda1.hat","lambda2.hat")],2)),
                              as.character(round(100*as.numeric(match.details[,c("p.home","p.away")]),0)),
                              as.character(round(100*as.numeric(match.details[,c("pH","pA")]),0))))
    table0 <- data.frame(table0,stringsAsFactors = FALSE)
    colnames(table0) <- match.details[,c("team1","team2")]
    rownames(table0) <- c("Position","Promotion (Probability %)","Play-offs (Probability %)",
                          "Relegation (Probability %)","Wins","Points","Goals Scored",
                          "Goals Conceded","Elo strength","Expected Goals",
                          "Model Probabilities (to win, %)",
                          "Bookmaker Probabilities (to win, %)")
  } else {
    table0 <- as.matrix(rbind(as.character(round(match.details[,c("pos1","pos2")],0)),
                              match.details[,c("tier.H","tier.A")],
                              as.character(round(match.details[,c("wins1","wins2")],0)),
                              as.character(round(match.details[,c("pts1","pts2")],0)),
                              as.character(round(match.details[,c("gs1","gs2")],0)),
                              as.character(round(match.details[,c("gs1","gs2")]-match.details[,c("gd1","gd2")],0)),
                              as.character(round(match.details[,c("elostrength1","elostrength2")],0)),
                              as.character(round(match.details[,c("lambda1.hat","lambda2.hat")],2)),
                              as.character(round(100*as.numeric(match.details[,c("p.home","p.away")]),0)),
                              as.character(round(100*as.numeric(match.details[,c("pH","pA")]),0))))
    table0 <- data.frame(table0,stringsAsFactors = FALSE)
    colnames(table0) <- match.details[,c("team1","team2")]
    rownames(table0) <- c("Position","Division","Wins","Points","Goals Scored","Goals Conceded",
                          "Elo strength","Expected Goals",
                          "Model Probabilities (to win, %)",
                          "Bookmaker Probabilities (to win, %)")
  } 
  
  tab.gen <- as.matrix(rbind(as.character(round(match.details[,c("jtotal.in.H","jtotal.in.A")],0)),
                             as.character(round(match.details[,c("jtotal.out.H","jtotal.out.A")],0)),
                             as.character(match.details[,c("coach.H","coach.A")]),
                             as.character(round(match.details[,c("cplayed.H","cplayed.A")],0)),
                            as.character(round(match.details[,c("win.pc.H","win.pc.A")],0)),
                            as.character(round(match.details[,c("total.experience.H","total.experience.A")],0)),
                            as.character(round(match.details[,c("total.total.apps.H","total.total.apps.A")],0)),
                            as.character(round(match.details[,c("total.goals.H","total.goals.A")],0))))
  tab.gen <- data.frame(tab.gen,stringsAsFactors = FALSE)
  colnames(tab.gen) <- match.details[,c("team1","team2")]
  rownames(tab.gen) <- c("Players In","Players Out","Manager","Manager Matches at Club",
                        "Manager Win %","Total Manager Experience","Appearances per Player","Goals per Player")
  
  ##creating directory for club if it's first time for this club
  mainDir <- paste0(dbloc,"data")
  subDir <- gsub(" ","-",n.team)
  ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
  
  tab0 <- xtable(table0,digits = c(0,0,0))
  align(tab0) <- "r|rr"
  print(tab0, type = "latex", 
        floating = FALSE,
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab0.tex"))
  print(tab0, type = "html", 
        floating = FALSE,
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab0.html"))
  
  tabg0 <- xtable(tab.gen,digits = c(0,0,0))
  align(tabg0) <- "r|rr"
  print(tabg0, type = "latex", 
        floating = FALSE,
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab-gen.tex"))
  print(tabg0, type = "html", 
        floating = FALSE,
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab-gen.html"))

  table1 <- recents[recents$score!="NA-NA",]
  colnames(table1) <- c("Home","H-A","Away","Date") #,"Comp"
  table1$Home <- to3(table1$Home)
  table1$Away <- to3(table1$Away)
  #table1$Comp <- to3comp(table1$Comp)
  # table1$Date <- as.character(table1$Date)
  table1$Season <- as.numeric(format(as.Date(table1$Date),"%Y")) - as.numeric(as.numeric(format(as.Date(table1$Date),"%m"))<7 | (format(as.Date(table1$Date),"%Y")=="2020" & as.numeric(format(as.Date(table1$Date),"%m"))<9))
  table1$Season <- as.character(paste0(substring(table1$Season,3),"-",substring(table1$Season+1,3)))
  tab1 <- xtable(table1[,-4])
  print(tab1, type = "latex", 
        floating = FALSE,include.rownames=FALSE, 
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1.tex"))
  print(tab1, type = "html", 
        floating = FALSE,include.rownames=FALSE, 
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1.html"))
  
  hist.tab2 <- xtable(hist.tab,digits = c(0,0,0,0,0,0,0))
  print(hist.tab2, type = "latex", 
        floating = FALSE,
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-hist-tab.tex"))
  print(hist.tab2, type = "html", 
        floating = FALSE,
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-hist-tab.html"))
  
  ##make team form table
  table1form <- n.team.form
  colnames(table1form) <- c("Home","H-A","Away")
  table1form$Home <- to3(table1form$Home)
  table1form$Away <- to3(table1form$Away)
  tab1form <- xtable(table1form)
  print(tab1form, type = "latex", 
        floating = FALSE,include.rownames=FALSE, 
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1form.tex"))
  print(tab1form, type = "html", 
        floating = FALSE,include.rownames=FALSE, 
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1form.html"))
  
  ##make opp form table
  table1oppform <- opp.team.form
  colnames(table1oppform) <- c("Home","H-A","Away")
  table1oppform$Home <- to3(table1oppform$Home)
  table1oppform$Away <- to3(table1oppform$Away)
  tab1oppform <- xtable(table1oppform)
  if(n.team==match.details$team1) {
    print(tab1form, type = "latex", 
          floating = FALSE,include.rownames=FALSE, 
          file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1hform.tex"))
    print(tab1form, type = "html", 
          floating = FALSE,include.rownames=FALSE, 
          file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1hform.html"))
    print(tab1oppform, type = "latex", 
          floating = FALSE,include.rownames=FALSE, 
          file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1aform.tex"))
    print(tab1oppform, type = "html", 
          floating = FALSE,include.rownames=FALSE, 
          file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1aform.html"))
  } else {
    print(tab1form, type = "latex", 
          floating = FALSE,include.rownames=FALSE, 
          file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1aform.tex"))
    print(tab1form, type = "html", 
          floating = FALSE,include.rownames=FALSE, 
          file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1aform.html"))
    print(tab1oppform, type = "latex", 
          floating = FALSE,include.rownames=FALSE, 
          file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1hform.tex"))
    print(tab1oppform, type = "html", 
          floating = FALSE,include.rownames=FALSE, 
          file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1hform.html"))
  }
  
  table2 <- c()
  for(g1 in 0:tab.goals) {
    table2 <- rbind(table2,100*as.numeric(match.details[paste0("score.",c(0:tab.goals),"-",g1)]))
  }
  
  colnames(table2) <- c(0:tab.goals)
  rownames(table2) <- c(0:tab.goals)
  addtorow <- list()
  addtorow$pos <- list(0, 0)
  tab.headers <- paste(paste0("& ",0:tab.goals,""),collapse=" ")
  addtorow$command <- c(paste0("& \\multicolumn{",tab.goals+1,"}{|c}{Home Goals} \\\\\n"),
                        paste0("Away ",tab.headers," \\\\\n"))
  
  tab2 <- xtable(table2,digits = c(0,rep(1,tab.goals+1)))
  align(tab2) <- paste0("r|",paste(rep("r",tab.goals+1),collapse=""))
  print(tab2, add.to.row = addtorow, include.colnames = FALSE, type = "latex", 
        floating = FALSE,
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab2.tex"))
  print(tab2, add.to.row = addtorow, include.colnames = FALSE, type = "html", 
        floating = FALSE,
        file=paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab2.html"))
  
  team.colours <- read.csv(paste0(dbloc,"/data/team-colours.csv"),stringsAsFactors = FALSE)  
  n.col <- team.colours$col1[tolower(team.colours$soccerbase.team)==n.team]
#  n.col="red"
  opp.col <- team.colours$col1[tolower(team.colours$soccerbase.team)==opp.team]
  #opp.col="yellow4"
  #opp.col <- "blue"
  if(opp.col=="#ffffff" | opp.col=="#fbfafa") {
    opp.col <- team.colours$col2[tolower(team.colours$soccerbase.team)==opp.team]
  }
  history.date <- min(all.history$date,as.Date("1950-01-01"))
  jpeg(paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-elo.jpg"),
       width = 8, height = 5, units = "in", res=300)
  par(mfrow = c(1,2))
  plot(as.Date(all.elo.team$date[all.elo.team$date>c(Sys.Date()-2*365.25) & all.elo.team$date<Sys.Date()]),
       smooth(all.elo.team$elo[all.elo.team$date>c(Sys.Date()-2*365.25) & all.elo.team$date<Sys.Date()]),type="l",
       main="Elo Rating Histories of Teams In the Last Two Years",
       col=n.col,
       ylim=range(c(all.elo.team$elo[all.elo.team$date>c(Sys.Date()-2*365.25) & all.elo.team$date<Sys.Date()],
                    all.elo.opp$elo[all.elo.opp$date>c(Sys.Date()-2*365.25) & all.elo.opp$date<Sys.Date()])),
       lwd=3,ylab="Elo strength",xlab="Date")
  lines(as.Date(all.elo.opp$date[all.elo.opp$date>c(Sys.Date()-2*365.25) & all.elo.opp$date<Sys.Date()]),
        smooth(all.elo.opp$elo[all.elo.opp$date>c(Sys.Date()-2*365.25) & all.elo.opp$date<Sys.Date()]),
        type="l",
        col=opp.col,
        lwd=3)
  legend("top",col=c(n.col,opp.col),lty=1,legend=c(n.team,opp.team),bty="n",ncol = 2,lwd=3)
  lines(all.history$date,rep(min(smooth(all.elo.team$elo[all.elo.team$date>history.date & all.elo.team$date<Sys.Date()]),smooth(all.elo.opp$elo[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date()])),NROW(all.history$date)),pch="|",type="p",cex=1)
  plot(as.Date(all.elo.team$date[all.elo.team$date>history.date & all.elo.team$date<Sys.Date()]),
       smooth(all.elo.team$elo[all.elo.team$date>history.date & all.elo.team$date<Sys.Date()]),type="l",
       main=paste0("Elo Rating Histories of Teams Since ",format(as.Date(history.date),"%Y")),
       col=n.col,
       ylim=range(c(all.elo.team$elo[all.elo.team$date>history.date & all.elo.team$date<Sys.Date()],
                    all.elo.opp$elo[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date()])),
       lwd=3,ylab="Elo strength",xlab="Date")
  lines(as.Date(all.elo.opp$date[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date()]),
        smooth(all.elo.opp$elo[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date()]),
        type="l",
        col=opp.col,
        lwd=3)
  legend("top",col=c(n.col,opp.col),lty=1,legend=c(n.team,opp.team),bty="n",ncol = 2,lwd=3)
  lines(all.history$date,rep(min(smooth(all.elo.team$elo[all.elo.team$date>history.date & all.elo.team$date<Sys.Date()]),smooth(all.elo.opp$elo[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date()])),NROW(all.history$date)),pch="|",type="p",cex=1)
  dev.off()
  
  jpeg(paste0(dbloc,"/data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-lge.jpg"),
       width = 8, height = 5, units = "in", res=300)
  par(mfrow = c(1,2))
  plot(all.elo.team$date[all.elo.team$date>c(Sys.Date()-5*365.25) & all.elo.team$date<Sys.Date() & is.na(all.elo.team$pos)==FALSE],
       all.elo.team$pos[all.elo.team$date>c(Sys.Date()-5*365.25) & all.elo.team$date<Sys.Date() & is.na(all.elo.team$pos)==FALSE],type="l",
       main="League Positions in Last Five Years",
       col=n.col,
       ylim=range(c(all.elo.team$pos[all.elo.team$date>c(Sys.Date()-5*365.25) & all.elo.team$date<Sys.Date()],
                    all.elo.opp$pos[all.elo.opp$date>c(Sys.Date()-5*365.25) & all.elo.opp$date<Sys.Date()]),na.rm=TRUE),
       lwd=3,ylab="Elo strength",xlab="Date")
  lines(all.elo.opp$date[all.elo.opp$date>c(Sys.Date()-5*365.25) & all.elo.opp$date<Sys.Date() & is.na(all.elo.opp$pos)==FALSE],
        all.elo.opp$pos[all.elo.opp$date>c(Sys.Date()-5*365.25) & all.elo.opp$date<Sys.Date() & is.na(all.elo.opp$pos)==FALSE],
        type="l",
        col=opp.col,
        lwd=3)
  #legend("top",col=c(n.col,opp.col),lty=1,legend=c(n.team,opp.team),bty="n",ncol = 2,lwd=3)
  oafc.lge.pos <- aggregate(all.elo.team$pos[all.elo.team$date>history.date & all.elo.team$date<Sys.Date() & is.na(all.elo.team$pos)==FALSE],
                            by=list(format(all.elo.team$date[all.elo.team$date>history.date & all.elo.team$date<Sys.Date() & is.na(all.elo.team$pos)==FALSE],"%Y-%m")),
                            FUN=mean)
  oafc.lge.pos$date <- as.numeric(gsub("^(\\d+)-(\\d+)","\\1",oafc.lge.pos$Group.1)) + (as.numeric(gsub("^(\\d+)-(\\d+)","\\2",oafc.lge.pos$Group.1))-1)/12
  opp.lge.pos <- aggregate(all.elo.opp$pos[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date() & is.na(all.elo.opp$pos)==FALSE],
                           by=list(format(all.elo.opp$date[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date() & is.na(all.elo.opp$pos)==FALSE],"%Y-%m")),
                           FUN=mean)
  opp.lge.pos$date <- as.numeric(gsub("^(\\d+)-(\\d+)","\\1",opp.lge.pos$Group.1)) + (as.numeric(gsub("^(\\d+)-(\\d+)","\\2",opp.lge.pos$Group.1))-1)/12
  plot(oafc.lge.pos$date,
       oafc.lge.pos$x,type="l",
       main=paste0("League Positions Since ",format(history.date,"%Y")),
       col=n.col,
       ylim=range(c(all.elo.team$pos[all.elo.team$date>history.date & all.elo.team$date<Sys.Date()],
                    all.elo.opp$pos[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date()]),na.rm=TRUE),
       lwd=3,ylab="Elo strength",xlab="Date")
  lines(opp.lge.pos$date,
        opp.lge.pos$x,
        type="l",
        col=opp.col,
        lwd=3)
  legend("top",col=c(n.col,opp.col),lty=1,legend=c(n.team,opp.team),bty="n",ncol = 2,lwd=3)
  lines(all.history$date,rep(min(all.elo.team$pos[all.elo.team$date>history.date & all.elo.team$date<Sys.Date()],all.elo.opp$pos[all.elo.opp$date>history.date & all.elo.opp$date<Sys.Date()],na.rm=TRUE),NROW(all.history$date)),pch="|",type="p",cex=1)
  dev.off()
  
  fileConn<-file(paste0(dbloc,"data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-table.tex"))
  writeLines(c("\\documentclass[preview]{standalone}",
               "\\begin{document}",
               "\\begin{center}",
               paste0("\\input{",dbloc,"data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab0.tex}"),
               paste0("\\input{",dbloc,"data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab-gen.tex}"),
               "\\\\[.25cm]",
               "Previous",
               "\\\\[.25cm]",
               paste0("\\input{",dbloc,"data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1.tex}"),
               paste0("\\input{",dbloc,"data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-hist-tab.tex}"),
               "\\\\[.25cm]",
               "Form",
               "\\\\[.25cm]",
               paste0("\\input{",dbloc,"data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1hform.tex}"),
               paste0("\\input{",dbloc,"data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab1aform.tex}"),
               "\\\\[.25cm]",
               # "Scoreline Probabilities and Elo Strengths",
               "Scoreline Probabilities",
               "\\\\[.25cm]",
               # paste0("\\begin{array}{cc}\\input{",dbloc,"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab2.tex} & \\includegraphics[height=2cm]{",
               #        paste0(dbloc,"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-elo.jpg"),"}\\end{array}"),
               paste0("\\input{",dbloc,"data/",gsub(" ","-",n.team),"/",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-tab2.tex}"),
               "\\end{center}",
               "\\end{document}"), fileConn)
  close(fileConn)
  
  cmds <- c(paste0("cd ",dbloc,"data/",gsub(" ","-",n.team),"/"), 
            paste0("pdflatex ",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-table.tex"), 
            paste0("convert -density 300 ",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-table.pdf ",gsub(" ","-",match.details$team1),"-",gsub(" ","-",match.details$team2),"-",match.details$date,"-table.jpg"));
  system(paste(cmds, collapse=";"))
  return(all.history)
}

# game_id=NL.matches$match_id
# date0=as.Date(NL.matches$date)
# div_id=9
# team1=NL.matches$team1
# g1=NL.matches$goals1
# g2=NL.matches$goals2
# team2=NL.matches$team2
# game_id=comp.matches$match_id
# date0=as.Date(comp.matches$match_date)
# div_id=9
# team1=comp.matches$home_team.home_team_name
# g1=comp.matches$home_score
# g2=comp.matches$away_score
# team2=comp.matches$away_team.away_team_name
# pfw=3
# hpfw=pfw
# points.deduct = list("team"="Southend.United","points"=-10,"date"=as.Date("2023-08-23"))
# season=2023
# wc=FALSE
# game_id=matches$match_id;date0=as.Date(matches$date);team1=matches$team1;
# g1=matches$goals1;g2=matches$goals2;team2=matches$team2;div_id=ll;season=ss;pfw=3;points.deduct = NULL;wc=FALSE;
# game_id=matches$match_id;date0=as.Date(matches$date);team1=matches$team1;
# g1=matches$goals1;g2=matches$goals2;team2=matches$team2;div_id=NA;season=ss;pfw=3

# game_id=simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="D"];
# date0=as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="D"]);
# team1=simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="D"];
# g1=simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="D"];
# g2=simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="D"];
# team2=simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="D"];
# points.deduct = NA;wc=TRUE;div_id = NA; pfw=3; hpfw=pfw

league.tab <- function(game_id,date0,team1,g1,g2,team2,
                       div_id = NA,season=NA,pfw=3,points.deduct = NA, wc=FALSE, hpfw=pfw) {
  require(zoo)
  ##load up file with league qualification places for divisions with soccerbase division IDs
  if(is.na(div_id)==FALSE) {
#    qual.places <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Pedro/data/all-league-qualification-places.csv",stringsAsFactors = FALSE)
    qual.places <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/all-league-qualification-places.csv",stringsAsFactors = FALSE)
    if(!(season %in% unique(qual.places$Season[qual.places$div_id==div_id]))) {
      div_id=NA
    }
    if(!(div_id %in% unique(qual.places$div_id))) {
      div_id=NA
    }
  }

  if(NROW(points.deduct)<2) {
    if(is.na(points.deduct)==TRUE) {
      points.deduct <- NULL
    }
  }
  
  ##what teams are in league?
  teams <- c(team1,team2)[duplicated(c(team1,team2))==F]
  no.teams <- NROW(teams)
  ##how many matches each team will play (we assume simple home/away ties)
  # if(no.teams*(no.teams-1)==NROW(game_id)) {#then double-round robin schedule
  #   tot.matches <- NROW(teams)*2-2
  # } 
  
  if(2*no.teams*(no.teams-1)==NROW(game_id)) {#then quadruple-round robin schedule
    tot.matches <- 2*(NROW(teams)*2-2)
  } else if(wc==TRUE) {
    tot.matches <- 0.5*(NROW(teams)*2-2)
  } else {
    tot.matches <- NROW(teams)*2-2  
  }#if not, assume double round robin
  #note this isn't going to work if play-offs are included.
  #if playoffs included, then usually an odd number of fixtures (though not if final two legged also)
  
  
  schedule.matches <- data.frame("game_id"=game_id,"date"=date0,"team1"=team1,"team2"=team2,"g1"=as.numeric(g1),"g2"=as.numeric(g2),stringsAsFactors=F)
  schedule.matches <- schedule.matches[order(as.Date(schedule.matches$date)),]
  schedule.matches <- schedule.matches[order(schedule.matches$date),] 
  if(NROW(schedule.matches)>no.teams*(no.teams-1)) {#need to remove play-off matches
    schedule.matches <- schedule.matches[1:c(no.teams*(no.teams-1)),]
  }#note that this won't work for quadruple schedules yet - Scottish leagues
  
  date <- as.Date(date0)[duplicated(date0)==F]
  date <- date[order(date)]
  #points in match
  pts <- data.frame("date"=date)
  #number of games played
  pld <- data.frame("date"=date)
  #goals scored
  gs <- data.frame("date"=date)
  #goal diff
  gd <- data.frame("date"=date)
  #form
  form <- data.frame("date"=date)
  for (n in 1:length(teams)) {
    pts[[teams[n]]] <- rep(NA,NROW(date))
    pld[[teams[n]]] <- rep(0,NROW(date))
    form[[teams[n]]] <- rep(0,NROW(date))
    gd[[teams[n]]] <- rep(0,NROW(date))
    gs[[teams[n]]] <- rep(0,NROW(date))
  }
  for (n in 1:NROW(schedule.matches)) {
    pts[pts$date==schedule.matches$date[n],schedule.matches$team1[n]] <- hpfw*as.numeric(schedule.matches$g1[n]>schedule.matches$g2[n]) + 1*as.numeric(schedule.matches$g1[n]==schedule.matches$g2[n])
    pts[pts$date==schedule.matches$date[n],schedule.matches$team2[n]] <- pfw*as.numeric(schedule.matches$g1[n]<schedule.matches$g2[n]) + 1*as.numeric(schedule.matches$g1[n]==schedule.matches$g2[n])
    pld[pts$date==schedule.matches$date[n],schedule.matches$team1[n]] <- 1
    pld[pts$date==schedule.matches$date[n],schedule.matches$team2[n]] <- 1
    gs[pts$date==schedule.matches$date[n],schedule.matches$team1[n]] <- schedule.matches$g1[n]
    gs[pts$date==schedule.matches$date[n],schedule.matches$team2[n]] <- schedule.matches$g2[n]
    gd[pts$date==schedule.matches$date[n],schedule.matches$team1[n]] <- schedule.matches$g1[n]-schedule.matches$g2[n]
    gd[pts$date==schedule.matches$date[n],schedule.matches$team2[n]] <- schedule.matches$g2[n]-schedule.matches$g1[n]  
  }
  #want to create matrix with each match outcome per team - hence if 20 teams a 38x20 matrix
  pts.grid <- cbind(1:tot.matches,matrix(NA,tot.matches,no.teams))
  colnames(pts.grid) <- c("match.no",teams)
  for(tt in teams) {
    pts.grid[,tt] <- pts[is.na(pts[,tt])==FALSE,tt][1:tot.matches]
  }
  
  #treat matches yet to occur as goalless and pointless so that table is populated for forecasting purposes
  #probably want switch for this in routine - and only for last day (for now) of dates
  pts[is.na(pts)] <- 0
  pld[is.na(pld)] <- 0
  gs[is.na(gs)] <- 0
  gd[is.na(gd)] <- 0
  #cumulative sums to create league tables
  tpts <- data.matrix(apply(pts[,teams],2,cumsum)-pts[,teams])
  
  tpts.left <- 3*matrix(2*NROW(teams)-2,NROW(tpts),NCOL(tpts)) - tpts
  
  ##on any given day want difference between 1st and 2nd most points, 1st and 3rd, and 4th bottom to bottom
  # points.gaps <- data.frame("date"=date,
  #                           "top.to.second"=apply(tpts,1,function(x)x[maxn(1)(x)]) - apply(tpts,1,function(x)x[maxn(2)(x)]),
  #                           "top.to.third"=apply(tpts,1,function(x)x[maxn(1)(x)]) - apply(tpts,1,function(x)x[maxn(3)(x)]),
  #                           "fourth.bottom.to.bottom"=apply(tpts,1,function(x)x[maxn(NROW(teams)-4)(x)]) - apply(tpts,1,function(x)x[maxn(NROW(teams))(x)]),
  #                           "fourth.bottom.to.third.bottom"=apply(tpts,1,function(x)x[maxn(NROW(teams)-4)(x)]) - apply(tpts,1,function(x)x[maxn(NROW(teams)-3)(x)]),
  #                           stringsAsFactors = FALSE)
  ##then want to know if the gap can be overturned --- too tricky
  
  
  tpld <- data.matrix(apply(pld[,teams],2,cumsum)-pld[,teams])
  twins <- data.matrix(apply(pts[,teams]>1,2,cumsum)-matrix(as.numeric(as.matrix(pts[,teams])>1),nrow = NROW(pts)))
  tdraws <- data.matrix(apply(pts[,teams]==1,2,cumsum)-matrix(as.numeric(as.matrix(pts[,teams])==1),nrow = NROW(pts)))
  tgs <- data.matrix(apply(gs[,teams],2,cumsum)-gs[,teams])
  tgd <- data.matrix(apply(gd[,teams],2,cumsum)-gd[,teams])
  #cumulative sums for final league table
  ftpts <- data.matrix(apply(pts[,teams],2,cumsum))
  ftpld <- data.matrix(apply(pld[,teams],2,cumsum))
  ftwins <- data.matrix(apply(pts[,teams]>1,2,cumsum))
  ftdraws <- data.matrix(apply(pts[,teams]==1,2,cumsum))
  ftgs <- data.matrix(apply(gs[,teams],2,cumsum))
  ftgd <- data.matrix(apply(gd[,teams],2,cumsum))
  
  #calculate league position each day based on points then goal difference then goals scored then alphabetical order
  #  alpha <- matrix(rep(NROW(teams) - rank(teams),NROW(date)),NROW(date),NROW(teams))
  alpha <- tpts
  for(ii in 1:NROW(alpha)) { alpha[ii,] <- NROW(teams) - rank(teams) }
  tot <- data.frame(date,tpts+tgd/100+tgs/100000+alpha/10000000) #weighting goal difference and goals scored less than points
  #now apply any points deductions from the moment they were applied
  if(is.null(points.deduct)==FALSE) {
    tot[tot$date>=points.deduct$date,points.deduct$team] <- tot[tot$date>=points.deduct$date,points.deduct$team]+as.numeric(points.deduct$points)
  }
  #need column names for position calculating
  colnames(tot) <- colnames(pts)
  #league position each matchday
  pos <- data.frame(date,t(apply(-tot[,teams], 1, rank, ties.method='min')))
  
  #calculate final league position (points then goal difference then goals scored then alphabetical order)
  ftot <- data.frame(date,ftpts+ftgd/100+ftgs/100000+alpha/10000000) #weighting goal difference and goals scored less than points
  # if(goal.average==1) {
  #   ftot <- data.frame(date,ftpts+(ftgs/ftgc)/1000+alpha/10000000) #weighting goal difference and goals scored less than points
  # }
  # if(wc==1) { ##then head to head is first tie breaker
  #   ftot <- data.frame(date,ftpts+ftgd/100+ftgs/100000+alpha/10000000)
  #   schedule.matches$outcome1 <- as.numeric(schedule.matches$g1>schedule.matches$g2)
  #   schedule.matches$outcome2 <- as.numeric(schedule.matches$g1<schedule.matches$g2)
  #   if(ftpts[NROW(ftpts),1]==ftpts[NROW(ftpts),2]) {
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[1] & schedule.matches$team2==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[1] & schedule.matches$team1==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[2] & schedule.matches$team2==colnames(ftpts)[1]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[2] & schedule.matches$team1==colnames(ftpts)[1]]
  #   }
  #   if(ftpts[NROW(ftpts),1]==ftpts[NROW(ftpts),3]) {
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[1] & schedule.matches$team2==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[1] & schedule.matches$team1==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[2] & schedule.matches$team2==colnames(ftpts)[1]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[2] & schedule.matches$team1==colnames(ftpts)[1]]
  #   }
  #   if(ftpts[NROW(ftpts),1]==ftpts[NROW(ftpts),4]) {
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[1] & schedule.matches$team2==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[1] & schedule.matches$team1==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[2] & schedule.matches$team2==colnames(ftpts)[1]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[2] & schedule.matches$team1==colnames(ftpts)[1]]
  #   }
  #   if(ftpts[NROW(ftpts),2]==ftpts[NROW(ftpts),3]) {
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[1] & schedule.matches$team2==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[1] & schedule.matches$team1==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[2] & schedule.matches$team2==colnames(ftpts)[1]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[2] & schedule.matches$team1==colnames(ftpts)[1]]
  #   }
  #   if(ftpts[NROW(ftpts),2]==ftpts[NROW(ftpts),4]) {
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[1] & schedule.matches$team2==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[1] & schedule.matches$team1==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[2] & schedule.matches$team2==colnames(ftpts)[1]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[2] & schedule.matches$team1==colnames(ftpts)[1]]
  #   }
  #   if(ftpts[NROW(ftpts),3]==ftpts[NROW(ftpts),4]) {
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[1] & schedule.matches$team2==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),2] <- ftot[NROW(ftot),2] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[1] & schedule.matches$team1==colnames(ftpts)[2]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome1[schedule.matches$team1==colnames(ftpts)[2] & schedule.matches$team2==colnames(ftpts)[1]]
  #     ftot[NROW(ftot),3] <- ftot[NROW(ftot),3] + schedule.matches$outcome2[schedule.matches$team2==colnames(ftpts)[2] & schedule.matches$team1==colnames(ftpts)[1]]
  #   }
  # }
  #now apply any points deductions from the moment they were applied
  if(is.null(points.deduct)==FALSE) {
    ftot[ftot$date>=points.deduct$date,as.character(points.deduct$team)] <- ftot[ftot$date>=points.deduct$date,as.character(points.deduct$team)]+as.numeric(points.deduct$points)
  }
  #need column names for position calculating
  colnames(ftot) <- colnames(pts)
  #league position after each matchday
  fpos <- data.frame(date,t(apply(-ftot[,teams], 1, rank, ties.method='min')))
  
  
  
  final.table <- data.frame("team"=colnames(pts)[-1],"played"=ftpld[NROW(ftpld),],
                            "won"=ftwins[NROW(ftwins),],
                            "drawn"=ftdraws[NROW(ftdraws),],
                            "lost"=NA,
                            "goals.scored"=ftgs[NROW(ftgs),],
                            "goals.conceded"=NA,
                            "goal.diff" = ftgd[NROW(ftgd),],
                            "points"=ftpts[NROW(ftpts),],
                            "position"=as.numeric(fpos[NROW(pos),-1]))
  final.table$lost <- final.table$played - final.table$won - final.table$drawn
  final.table$goals.conceded <- final.table$goals.scored - final.table$goal.diff
  final.table <- final.table[order(final.table$position),]
  final.table$ISD <- sd(as.numeric(final.table$points/(pfw*tot.matches)))/(0.5/sqrt(tot.matches))
  final.table$h1 <- sum(final.table$points[1])/sum(final.table$points)
  final.table$h2 <- sum(final.table$points[1:2])/sum(final.table$points)
  final.table$h3 <- sum(final.table$points[1:3])/sum(final.table$points)
  final.table$h4 <- sum(final.table$points[1:4])/sum(final.table$points)
  final.table$HHI <- sum((final.table$points/sum(final.table$points,na.rm=TRUE))^2)
  HHIl <- 2*(2*no.teams-1)/(3*no.teams*(no.teams-1))
  HHIu <- (no.teams+1)/(3*no.teams*(no.teams-1))
  final.table$HHIn <- (final.table$HHI-HHIl)/(HHIu-HHIl)
  
  #turn into data frames for final output
  tpts <- data.frame(date,tpts)
  twins <- data.frame(date,twins)
  tdraws <- data.frame(date,tdraws)
  tgs <- data.frame(date,tgs)
  tgd <- data.frame(date,tgd)
  tpld <- data.frame(date,tpld)
  ftpts <- data.frame(date,ftpts)
  ftpld <- data.frame(date,ftpld)
  
  #we can do daily table (pos for start of day, fpos for end of day)
  # daily.tables <- list()
  if(is.na(div_id)==FALSE) {
    CLpos=qual.places$Champions.League[qual.places$div_id==div_id & qual.places$Season==season] #or auto promotion
    ELpos=qual.places$Europa.League[qual.places$div_id==div_id & qual.places$Season==season] #or play off qualification
    ECLpos=qual.places$Conf.League[qual.places$div_id==div_id & qual.places$Season==season]
    rplpos=qual.places$Relegation.Play.off[qual.places$div_id==div_id & qual.places$Season==season]
    rpos=qual.places$Relegation[qual.places$div_id==div_id & qual.places$Season==season]
    achieveCL <- data.frame()
    achieveEL <- data.frame()
    achieveECL <- data.frame()
    avoidRPL <- data.frame()
    avoidREL <- data.frame()
  } else {
    CLpos=NA
    ELpos=NA
    ECLpos=NA
    rplpos=NA
    rpos=NA
  }
  achieve1  <- data.frame()
  achieve1winsneeded <- c()
  isd <- c()
  hhi <- c()
  pos.changes <- c()
  pos.abs.changes <- c()
  
  schedule.matches$achieve1.1 <- NA
  schedule.matches$achieve1.2 <- NA
  schedule.matches$achieveCL.1 <- NA
  schedule.matches$achieveCL.2 <- NA
  schedule.matches$achieveEL.1 <- NA
  schedule.matches$achieveEL.2 <- NA
  schedule.matches$achieveECL.1 <- NA
  schedule.matches$achieveECL.2 <- NA
  schedule.matches$avoidRPL.1 <- NA
  schedule.matches$avoidRPL.2 <- NA
  schedule.matches$avoidREL.1 <- NA
  schedule.matches$avoidREL.2 <- NA
  
  last.day.positions <- 1:NCOL(fpos[,-1])
  for(dd0 in date) {
    dd = as.Date(dd0,origin="1970-01-01")
    day.positions <- as.numeric(fpos[fpos$date==dd,-1])
    day.pld <- ftpld[ftpld$date==dd,-1]
    day.pts <- ftpts[ftpts$date==dd,-1]
    day.pld <- day.pld[order(day.positions)]
    day.pts <- day.pts[order(day.positions)]
    day.pts.remaining <- (tot.matches-day.pld)*pfw
    
    #calculated ISD here
    isd <- c(isd,sd(as.numeric(day.pts/(pfw*tot.matches)))/(0.5/sqrt(tot.matches)))
    hhi <- c(hhi,sum((day.pts/sum(day.pts))^2))
    
    #rank changes for league standing effect
    pos.changes <- c(pos.changes,NROW(day.positions)-sum(as.numeric(day.positions==last.day.positions)))
    pos.abs.changes <- c(pos.abs.changes,sum(abs(day.positions-last.day.positions)))
    
    #can each team achieve 1st?
    achieve.day <- day.pts+day.pts.remaining >= as.numeric(day.pts[1])
    colnames(achieve.day) <- gsub("[.]"," ",colnames(achieve.day))
    achieve1 <- rbind(achieve1,achieve.day)
    schedule.matches$achieve1.1[schedule.matches$date==dd] <- as.numeric(achieve.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team1[schedule.matches$date==dd])),colnames(achieve.day))])
    schedule.matches$achieve1.2[schedule.matches$date==dd] <- as.numeric(achieve.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team2[schedule.matches$date==dd])),colnames(achieve.day))])
    achieve1winsneeded <- c(achieve1winsneeded,(day.pts[2]+day.pts.remaining[2] - day.pts[1])/3)
    if(is.na(CLpos)==FALSE & CLpos>0) {
      #can each team achieve CL?
      achieveCL.day <- day.pts+day.pts.remaining > as.numeric(day.pts[CLpos])
      colnames(achieveCL.day) <- gsub("[.]"," ",colnames(achieveCL.day))
      achieveCL <- rbind(achieveCL,achieveCL.day)
      schedule.matches$achieveCL.1[schedule.matches$date==dd] <- as.numeric(achieveCL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team1[schedule.matches$date==dd])),colnames(achieve.day))])
      schedule.matches$achieveCL.2[schedule.matches$date==dd] <- as.numeric(achieveCL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team2[schedule.matches$date==dd])),colnames(achieve.day))])
    }
    #can each team achieve EL?
    if(is.na(ELpos)==FALSE & ELpos>0) {
      achieveEL.day <- day.pts+day.pts.remaining > as.numeric(day.pts[ELpos])
      colnames(achieveEL.day) <- gsub("[.]"," ",colnames(achieveEL.day))
      achieveEL <- rbind(achieveEL,achieveEL.day)
      schedule.matches$achieveEL.1[schedule.matches$date==dd] <- as.numeric(achieveEL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team1[schedule.matches$date==dd])),colnames(achieve.day))])
      schedule.matches$achieveEL.2[schedule.matches$date==dd] <- as.numeric(achieveEL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team2[schedule.matches$date==dd])),colnames(achieve.day))])
    }
    
    #can each team achieve ECL?
    if(is.na(ECLpos)==FALSE & ECLpos>0) {
      achieveECL.day <- day.pts+day.pts.remaining > as.numeric(day.pts[ECLpos])
      colnames(achieveECL.day) <- gsub("[.]"," ",colnames(achieveECL.day))
      achieveECL <- rbind(achieveECL,achieveECL.day)
      schedule.matches$achieveECL.1[schedule.matches$date==dd] <- as.numeric(achieveECL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team1[schedule.matches$date==dd])),colnames(achieve.day))])
      schedule.matches$achieveECL.2[schedule.matches$date==dd] <- as.numeric(achieveECL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team2[schedule.matches$date==dd])),colnames(achieve.day))])
    }
      
    #can each team be caught by relegation play off team?
    if(is.na(rplpos)==FALSE & rplpos>0) {
      avoidRPL.day <- day.pts > max(as.numeric(day.pts.remaining[min(NCOL(day.pts),rplpos):NCOL(day.pts)])+as.numeric(day.pts[min(NCOL(day.pts),rplpos):NCOL(day.pts)]))
      colnames(avoidRPL.day) <- gsub("[.]"," ",colnames(avoidRPL.day))
      avoidRPL <- rbind(avoidRPL,avoidRPL.day)
      schedule.matches$avoidRPL.1[schedule.matches$date==dd] <- as.numeric(avoidRPL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team1[schedule.matches$date==dd])),colnames(achieve.day))])
      schedule.matches$avoidRPL.2[schedule.matches$date==dd] <- as.numeric(avoidRPL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team2[schedule.matches$date==dd])),colnames(achieve.day))])
    }
      
    #can each team be caught by relegation zone teams?
    if(is.na(rpos)==FALSE & rpos>0) {
      avoidREL.day <- day.pts > max(as.numeric(day.pts.remaining[min(NCOL(day.pts),rpos):NCOL(day.pts)])+as.numeric(day.pts[min(NCOL(day.pts),rpos):NCOL(day.pts)]))
      colnames(avoidREL.day) <- gsub("[.]"," ",colnames(avoidREL.day))
      avoidREL <- rbind(avoidREL,avoidREL.day)
      schedule.matches$avoidREL.1[schedule.matches$date==dd] <- as.numeric(avoidREL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team1[schedule.matches$date==dd])),colnames(avoidREL.day))])
      schedule.matches$avoidREL.2[schedule.matches$date==dd] <- as.numeric(avoidREL.day[match(gsub("[.]"," ",gsub("-"," ",schedule.matches$team2[schedule.matches$date==dd])),colnames(avoidREL.day))])
    }
    #daily.tables[[dd]] <- fpos[fpos$date==dd,-1]
    last.day.positions <- day.positions
  }
  #now work out when title position was secured by week
  c.achieve1 <- rowSums(achieve1)
  c.achieve1 <- data.frame("date"=date,"club.can.acheve"=c.achieve1)
  c.achieve1$club.can.acheve[c.achieve1$club.can.acheve==0] <- 1
  c.achieve1$winner.known <- as.numeric(c.achieve1$club.can.acheve==1)
  # achieve1 <- cbind(date,achieve1)
  # achieve1$date <- achieve1$date-as.numeric(format(achieve1$date,"%w"))

  if(is.na(CLpos)==FALSE & CLpos>0) {
    c.achieveCL <- rowSums(achieveCL)
    c.achieveCL <- data.frame("date"=date,"club.can.acheve"=c.achieveCL)
    c.achieveCL$club.can.acheve[c.achieveCL$club.can.acheve<CLpos+1] <- CLpos+1
    c.achieveCL$winner.known <- as.numeric(c.achieveCL$club.can.acheve==CLpos+1)
    # achieveCL <- cbind(date,achieveCL)
    # achieveCL$date <- achieveCL$date-as.numeric(format(achieveCL$date,"%w"))
  } else {
    c.achieveCL <- data.frame("date"=date,"club.can.acheve"=rep(NA,NROW(date)),
                               "winner.known"=rep(NA,NROW(date)))
  }
  if(is.na(ELpos)==FALSE & ELpos>0) {
    c.achieveEL <- rowSums(achieveEL)
    c.achieveEL <- data.frame("date"=date,"club.can.acheve"=c.achieveEL)
    c.achieveEL$club.can.acheve[c.achieveEL$club.can.acheve<ELpos+1] <- ELpos+1
    c.achieveEL$winner.known <- as.numeric(c.achieveEL$club.can.acheve==ELpos+1)
    # achieveEL <- cbind(date,achieveEL)
    # achieveEL$date <- achieveEL$date-as.numeric(format(achieveEL$date,"%w"))
  } else {
    c.achieveEL <- data.frame("date"=date,"club.can.acheve"=rep(NA,NROW(date)),
                               "winner.known"=rep(NA,NROW(date)))
  }
  if(is.na(ECLpos)==FALSE & ECLpos>0) {
      c.achieveECL <- rowSums(achieveECL)
      c.achieveECL <- data.frame("date"=date,"club.can.acheve"=c.achieveECL)
      c.achieveECL$club.can.acheve[c.achieveECL$club.can.acheve<ECLpos+1] <- ECLpos+1
      c.achieveECL$winner.known <- as.numeric(c.achieveECL$club.can.acheve==ECLpos+1)
      # achieveECL <- cbind(date,achieveECL)
      # achieveECL$date <- achieveECL$date-as.numeric(format(achieveECL$date,"%w"))
  } else {
      c.achieveECL <- data.frame("date"=date,"club.can.acheve"=rep(NA,NROW(date)),
                               "winner.known"=rep(NA,NROW(date)))
  }
    
  if(is.na(rplpos)==FALSE) {
    c.avoidRPL <- rowSums(avoidRPL)
    c.avoidRPL <- data.frame("date"=date,"club.safe.from"=c.avoidRPL)
    #c.avoidRPL$club.safe.from[c.avoidRPL$club.safe.from==0] <- 1
    c.avoidRPL$rpl.known <- as.numeric(c.avoidRPL$club.safe.from==rplpos-1)
    # avoidRPL <- cbind(date,avoidRPL)
    # avoidRPL$date <- avoidRPL$date-as.numeric(format(avoidRPL$date,"%w"))
  } else {
    c.avoidRPL <- data.frame("date"=date,"club.safe.from"=rep(NA,NROW(date)),
                             "rpl.known"=rep(NA,NROW(date)))
  }
    
  if(is.na(rpos)==FALSE) {
    c.avoidREL <- rowSums(avoidREL)
    c.avoidREL <- data.frame("date"=date,"no.safe.from"=c.avoidREL)
    #c.avoidREL$club.safe.from[c.avoidREL$club.safe.from<rpos-1] <- rpos-1
    c.avoidREL$relegation.known <- as.numeric(c.avoidREL$no.safe.from==rpos-1)
    # avoidREL <- cbind(date,avoidREL)
    # avoidREL$date <- avoidREL$date-as.numeric(format(avoidREL$date,"%w"))
  } else {
    c.avoidREL <- data.frame("date"=date,"no.safe.from"=rep(NA,NROW(date)),
                             "relegation.known"=rep(NA,NROW(date)))
  }  
  outcomesknown0 <- data.frame("date"=date,"can.title"=c.achieve1$club.can.acheve,
                               "title"=c.achieve1$winner.known,
                               "titlewinsneeded"=as.numeric(achieve1winsneeded),
                               "can.CL"=c.achieveCL$club.can.acheve,
                               "CL"=c.achieveCL$winner.known,
                               "can.EL"=c.achieveEL$club.can.acheve,
                               "EL"=c.achieveEL$winner.known,
                               "can.ECL"=c.achieveECL$club.can.acheve,
                               "ECL"=c.achieveECL$winner.known,
                               "can.RPL"=c.avoidRPL$club.safe.from,
                               "RPL"=c.avoidRPL$rpl.known,
                               "can.REL"=c.avoidREL$no.safe.from,
                               "REL"=c.avoidREL$relegation.known,
                               "ISD"=isd,"HHI"=hhi,
                               "pos.changes"=pos.changes,"pos.abs.changes"=pos.abs.changes)
  outcomesknown0$date2 <- outcomesknown0$date-as.numeric(format(outcomesknown0$date,"%w"))
  outcomesknown <- aggregate(outcomesknown0[,c("can.title","title","titlewinsneeded","can.CL","CL","can.EL","EL",
                                               "can.ECL","ECL","can.RPL","RPL","can.REL","REL","ISD","HHI",
                                               "pos.changes","pos.abs.changes")],
                             list(outcomesknown0$date2),FUN=tail,1)
  # } else {
  #   outcomesknown0 <- data.frame("date"=date,"title"=c.achieve1$winner.known,
  #                               "titlewinsneeded"=as.numeric(achieve1winsneeded),
  #                               "ISD"=isd)
  #   outcomesknown0$date2 <- outcomesknown0$date-as.numeric(format(outcomesknown0$date,"%w"))
  #   outcomesknown <- aggregate(outcomesknown0[,c("title","titlewinsneeded","ISD")],
  #                              list(outcomesknown0$date2),FUN=tail,1)
  # }

  colnames(pos) <- colnames(pts)
  colnames(tpts) <- colnames(pts)
  colnames(twins) <- colnames(pts)
  colnames(tdraws) <- colnames(pts)
  colnames(tgd) <- colnames(pts)
  colnames(tgs) <- colnames(pts)
  colnames(tpld) <- colnames(pts)
  colnames(fpos) <- colnames(pts)
  
  #form - sum of "points" in last six matches (or less at start of season)  
  for (n in 1:NROW(teams)) {
    form.temp.0 <- tot[pld[,teams[n]]==1,teams[n]]
    if(NROW(form.temp.0)>5) {
      form.temp.6 <- c(rep(0,6),form.temp.0[-seq(NROW(form.temp.0)-5,NROW(form.temp.0))])#6th lag
      form.temp <- form.temp.0 - form.temp.6
      form.temp[1:6] <- form.temp.0[1:6]      
    } else {
      form.temp <- form.temp.0[1:NROW(form.temp.0)]
    }
    form[pld[,teams[n]]==1,teams[n]] <- form.temp
    form[,teams[n]] <- na.locf(form[,teams[n]],na.rm=F)
  }
  
  #input back into matchday dataset
  schedule.matches$pts1 <- NA
  schedule.matches$pts2 <- NA
  schedule.matches$wins1 <- NA
  schedule.matches$wins2 <- NA
  schedule.matches$draws1 <- NA
  schedule.matches$draws2 <- NA
  schedule.matches$pld1 <- NA
  schedule.matches$pld2 <- NA
  schedule.matches$gs1 <- NA
  schedule.matches$gs2 <- NA
  schedule.matches$gd1 <- NA
  schedule.matches$gd2 <- NA
  schedule.matches$pos1 <- NA
  schedule.matches$pos2 <- NA
  schedule.matches$post.pos1 <- NA
  schedule.matches$post.pos2 <- NA
  schedule.matches$form1 <- NA
  schedule.matches$form2 <- NA
  if(wc==FALSE) {
    schedule.matches$contention1.1 <- NA
    schedule.matches$contention1.2 <- NA
    schedule.matches$contention2.1 <- NA
    schedule.matches$contention2.2 <- NA
    schedule.matches$contention3.1 <- NA
    schedule.matches$contention3.2 <- NA
    schedule.matches$contention4.1 <- NA
    schedule.matches$contention4.2 <- NA
    schedule.matches$contention5.1 <- NA
    schedule.matches$contention5.2 <- NA
    schedule.matches$contention6.1 <- NA
    schedule.matches$contention6.2 <- NA
    schedule.matches$contention7.1 <- NA
    schedule.matches$contention7.2 <- NA
    schedule.matches$contention8.1 <- NA
    schedule.matches$contention8.2 <- NA
    schedule.matches$contention9.1 <- NA
    schedule.matches$contention9.2 <- NA
    schedule.matches$contention16.1 <- NA
    schedule.matches$contention16.2 <- NA
    schedule.matches$contention17.1 <- NA
    schedule.matches$contention17.2 <- NA
    schedule.matches$contention18.1 <- NA
    schedule.matches$contention18.2 <- NA
    schedule.matches$contention19.1 <- NA
    schedule.matches$contention19.2 <- NA
    schedule.matches$contention20.1 <- NA
    schedule.matches$contention20.2 <- NA
    schedule.matches$contention21.1 <- NA
    schedule.matches$contention21.2 <- NA
    schedule.matches$contention22.1 <- NA
    schedule.matches$contention22.2 <- NA
    schedule.matches$contention23.1 <- NA
    schedule.matches$contention23.2 <- NA
    schedule.matches$contention24.1 <- NA
    schedule.matches$contention24.2 <- NA
    schedule.matches$points.from3.1 <- NA
    schedule.matches$points.from3.2 <- NA
    schedule.matches$points.from4.1 <- NA
    schedule.matches$points.from4.2 <- NA
    schedule.matches$points.from5.1 <- NA
    schedule.matches$points.from5.2 <- NA
    schedule.matches$points.from6.1 <- NA
    schedule.matches$points.from6.2 <- NA
    schedule.matches$points.from7.1 <- NA
    schedule.matches$points.from7.2 <- NA
    schedule.matches$points.from8.1 <- NA
    schedule.matches$points.from8.2 <- NA
    schedule.matches$points.from9.1 <- NA
    schedule.matches$points.from9.2 <- NA
  }
  for (n in 1:NROW(schedule.matches)) {
    positions <- pos[pos$date==schedule.matches$date[n],-1]
    team.in.1st <- colnames(positions)[positions==1]
    pts.in.1st <- tpts[tpts$date==schedule.matches$date[n],team.in.1st]
    team.in.2nd <- colnames(positions)[positions==2]
    pts.in.2nd <- tpts[tpts$date==schedule.matches$date[n],team.in.2nd]
    team.in.3rd <- colnames(positions)[positions==3]
    pts.in.3rd <- tpts[tpts$date==schedule.matches$date[n],team.in.3rd]
    team.in.4th <- colnames(positions)[positions==4]
    pts.in.4th <- tpts[tpts$date==schedule.matches$date[n],team.in.4th]
    team.in.5th <- colnames(positions)[positions==5]
    pts.in.5th <- tpts[tpts$date==schedule.matches$date[n],team.in.5th]
    team.in.6th <- colnames(positions)[positions==6]
    pts.in.6th <- tpts[tpts$date==schedule.matches$date[n],team.in.6th]
    team.in.7th <- colnames(positions)[positions==7]
    pts.in.7th <- tpts[tpts$date==schedule.matches$date[n],team.in.7th]
    team.in.8th <- colnames(positions)[positions==8]
    pts.in.8th <- tpts[tpts$date==schedule.matches$date[n],team.in.8th]
    team.in.9th <- colnames(positions)[positions==9]
    pts.in.9th <- tpts[tpts$date==schedule.matches$date[n],team.in.9th]
    
    team.in.16th <- colnames(positions)[positions==16]
    pts.in.16th <- tpts[tpts$date==schedule.matches$date[n],team.in.16th]
    team.in.17th <- colnames(positions)[positions==17]
    pts.in.17th <- tpts[tpts$date==schedule.matches$date[n],team.in.17th]
    team.in.18th <- colnames(positions)[positions==18]
    pts.in.18th <- tpts[tpts$date==schedule.matches$date[n],team.in.18th]
    team.in.19th <- colnames(positions)[positions==19]
    pts.in.19th <- tpts[tpts$date==schedule.matches$date[n],team.in.19th]
    team.in.20th <- colnames(positions)[positions==20]
    pts.in.20th <- tpts[tpts$date==schedule.matches$date[n],team.in.20th]
    team.in.21st <- colnames(positions)[positions==21]
    pts.in.21st <- tpts[tpts$date==schedule.matches$date[n],team.in.21st]
    team.in.22nd <- colnames(positions)[positions==22]
    pts.in.22nd <- tpts[tpts$date==schedule.matches$date[n],team.in.22nd]
    team.in.23rd <- colnames(positions)[positions==23]
    pts.in.23rd <- tpts[tpts$date==schedule.matches$date[n],team.in.23rd]
    team.in.24th <- colnames(positions)[positions==24]
    pts.in.24th <- tpts[tpts$date==schedule.matches$date[n],team.in.24th]
    
    schedule.matches$pts1[n] <- tpts[tpts$date==schedule.matches$date[n],schedule.matches$team1[n]]
    schedule.matches$pts2[n] <- tpts[tpts$date==schedule.matches$date[n],schedule.matches$team2[n]]
    schedule.matches$wins1[n] <- twins[twins$date==schedule.matches$date[n],schedule.matches$team1[n]]
    schedule.matches$wins2[n] <- twins[twins$date==schedule.matches$date[n],schedule.matches$team2[n]]
    schedule.matches$draws1[n] <- tdraws[tdraws$date==schedule.matches$date[n],schedule.matches$team1[n]]
    schedule.matches$draws2[n] <- tdraws[tdraws$date==schedule.matches$date[n],schedule.matches$team2[n]]
    schedule.matches$pld1[n] <- tpld[tpld$date==schedule.matches$date[n],schedule.matches$team1[n]]
    pts.available1 <- pfw*(tot.matches - schedule.matches$pld1[n])
    schedule.matches$pld2[n] <- tpld[tpld$date==schedule.matches$date[n],schedule.matches$team2[n]]
    pts.available2 <- pfw*(tot.matches - schedule.matches$pld2[n])
    schedule.matches$gd1[n] <- tgd[tgd$date==schedule.matches$date[n],schedule.matches$team1[n]]
    schedule.matches$gd2[n] <- tgd[tgd$date==schedule.matches$date[n],schedule.matches$team2[n]]
    schedule.matches$gs1[n] <- tgs[tgs$date==schedule.matches$date[n],schedule.matches$team1[n]]
    schedule.matches$gs2[n] <- tgs[tgs$date==schedule.matches$date[n],schedule.matches$team2[n]]
    schedule.matches$pos1[n] <- pos[pos$date==schedule.matches$date[n],schedule.matches$team1[n]]
    schedule.matches$pos2[n] <- pos[pos$date==schedule.matches$date[n],schedule.matches$team2[n]]
    schedule.matches$post.pos1[n] <- fpos[fpos$date==schedule.matches$date[n],schedule.matches$team1[n]]
    schedule.matches$post.pos2[n] <- fpos[fpos$date==schedule.matches$date[n],schedule.matches$team2[n]]
    schedule.matches$form1[n] <- form[form$date==schedule.matches$date[n],schedule.matches$team1[n]]
    schedule.matches$form2[n] <- form[form$date==schedule.matches$date[n],schedule.matches$team2[n]]
    if(wc==FALSE) {
      schedule.matches$contention1.1[n] <- as.numeric((pts.in.1st - schedule.matches$pts1[n]) <= pts.available1)
      schedule.matches$contention1.2[n] <- as.numeric((pts.in.1st - schedule.matches$pts2[n]) <= pts.available2)
      schedule.matches$contention2.1[n] <- as.numeric((pts.in.2nd - schedule.matches$pts1[n]) <= pts.available1)
      schedule.matches$contention2.2[n] <- as.numeric((pts.in.2nd - schedule.matches$pts2[n]) <= pts.available2)
      schedule.matches$contention3.1[n] <- as.numeric((pts.in.3rd - schedule.matches$pts1[n]) <= pts.available1)
      schedule.matches$contention3.2[n] <- as.numeric((pts.in.3rd - schedule.matches$pts2[n]) <= pts.available2)
      schedule.matches$contention4.1[n] <- as.numeric((pts.in.4th - schedule.matches$pts1[n]) <= pts.available1)
      schedule.matches$contention4.2[n] <- as.numeric((pts.in.4th - schedule.matches$pts2[n]) <= pts.available2)
      if(NROW(teams)>4) {
        schedule.matches$contention5.1[n] <- as.numeric((pts.in.5th - schedule.matches$pts1[n]) <= pts.available1)
        schedule.matches$contention5.2[n] <- as.numeric((pts.in.5th - schedule.matches$pts2[n]) <= pts.available2)
      }
      if(NROW(teams)>5) {
        schedule.matches$points.from6.1[n] <- pts.in.6th - schedule.matches$pts1[n]
        schedule.matches$points.from6.2[n] <- pts.in.6th - schedule.matches$pts2[n]
      }
      if(NROW(teams)>6) {
        schedule.matches$points.from7.1[n] <- pts.in.7th - schedule.matches$pts1[n]
        schedule.matches$points.from7.2[n] <- pts.in.7th - schedule.matches$pts2[n]
      }
      if(NROW(teams)>7) {
        schedule.matches$points.from8.1[n] <- pts.in.8th - schedule.matches$pts1[n]
        schedule.matches$points.from8.2[n] <- pts.in.8th - schedule.matches$pts2[n]
      }
      if(NROW(teams)>8) {
        schedule.matches$points.from9.1[n] <- pts.in.9th - schedule.matches$pts1[n]
        schedule.matches$points.from9.2[n] <- pts.in.9th - schedule.matches$pts2[n]
      }
      if(NROW(teams)>5) {
        schedule.matches$contention6.1[n] <- as.numeric((pts.in.6th - schedule.matches$pts1[n]) <= pts.available1)
        schedule.matches$contention6.2[n] <- as.numeric((pts.in.6th - schedule.matches$pts2[n]) <= pts.available2)
      }
      if(NROW(teams)>6) {
        schedule.matches$contention7.1[n] <- as.numeric((pts.in.7th - schedule.matches$pts1[n]) <= pts.available1)
        schedule.matches$contention7.2[n] <- as.numeric((pts.in.7th - schedule.matches$pts2[n]) <= pts.available2)
      }
      if(NROW(teams)>7) {
        schedule.matches$contention8.1[n] <- as.numeric((pts.in.8th - schedule.matches$pts1[n]) <= pts.available1)
        schedule.matches$contention8.2[n] <- as.numeric((pts.in.8th - schedule.matches$pts2[n]) <= pts.available2)
      }
      if(NROW(teams)>8) {
        schedule.matches$contention9.1[n] <- as.numeric((pts.in.9th - schedule.matches$pts1[n]) <= pts.available1)
        schedule.matches$contention9.2[n] <- as.numeric((pts.in.9th - schedule.matches$pts2[n]) <= pts.available2)
      }
      if(NROW(teams)>15) {
        schedule.matches$contention16.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.16th) <= pts.available1)
        schedule.matches$contention16.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.16th) <= pts.available2)
      }
      if(NROW(teams)>16) {
        schedule.matches$contention17.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.17th) <= pts.available1)
        schedule.matches$contention17.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.17th) <= pts.available2)
      }
      if(NROW(teams)>17) {
        schedule.matches$contention18.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.18th) <= pts.available1)
        schedule.matches$contention18.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.18th) <= pts.available2)
      }
      if(NROW(teams)>18) {
        schedule.matches$contention19.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.19th) <= pts.available1)
        schedule.matches$contention19.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.19th) <= pts.available2)
      }
      if(NROW(teams)>19) {
        schedule.matches$contention20.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.20th) <= pts.available1)
        schedule.matches$contention20.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.20th) <= pts.available2)
      }
      if(NROW(teams)>20) {
        schedule.matches$contention21.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.21st) <= pts.available1)
        schedule.matches$contention21.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.21st) <= pts.available2)
      }
      if(NROW(teams)>21) {
        schedule.matches$contention22.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.22nd) <= pts.available1)
        schedule.matches$contention22.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.22nd) <= pts.available2)
      }
      if(NROW(teams)>22) {
        schedule.matches$contention23.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.23rd) <= pts.available1)
        schedule.matches$contention23.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.23rd) <= pts.available2)
      }
      if(NROW(teams)>23) {
        schedule.matches$contention24.1[n] <- as.numeric((schedule.matches$pts1[n] - pts.in.24th) <= pts.available1)
        schedule.matches$contention24.2[n] <- as.numeric((schedule.matches$pts2[n] - pts.in.24th) <= pts.available2)
      }
    }
  }
  
  wins <- pts[,-1]
  wins[wins==1] <- 0
  wins[wins==pfw] <- 1
  draws <- pts[,-1]
  draws[draws==pfw] <- 0
  
  return(list("matches"=schedule.matches,"positions"=fpos,
              "match"=pld[,-1],
              "point"=pts[,-1],
              "points.grid"=pts.grid,
              "played"=tpld,"gs"=tgs,
              "gd"=tgd,"wins"=twins,"draws"=tdraws,
              "points"=tpts,
              #"points.gaps"=points.gaps,
              "final.table"=final.table,
              "outcomes.known"=outcomesknown,
              "outcomes.known.match"=outcomesknown0))
}



club.colours <- function() {
  EPLteams <- data.frame("team"=c("arsenal","aston villa","brighton","burnley","chelsea",
                                  "crystal palace","everton","fulham","leeds","leicester",
                                  "liverpool","man city","man utd","newcastle","sheff utd",
                                  "southampton","tottenham","west brom","west ham","wolves"),
                         "colour1"=c("red","claret","blue","claret","blue","red","blue","white",
                                     "white","blue","red","lightblue","red","black","red","red",
                                     "white","blue","claret","gold"),
                         "colour2"=c("white","blue","white","blue","yellow","blue","white","black",
                                     "yellow","white","white","white","black","black","black",
                                     "black","blue","white","blue","black"))
  EFLCteams <- c("barnsley","birmingham","blackburn","bournemouth","brentford",
                 "bristol c","cardiff","coventry","derby","huddersfield",
                 "luton","middlesbrough","millwall","norwich","nottm forest",
                 "preston","qpr","reading","rotherham","sheff wed",
                 "stoke","swansea","watford","wycombe")
  EFL1teams <- c("accrington","afc w'bledon","blackpool","bristol r","burton","charlton",
                 "crewe","doncaster","fleetwood","gillingham","hull","ipswich"    , 
                 "lincoln","mk dons","northampton","oxford","peterborough","plymouth",    
                 "portsmouth","rochdale","shrewsbury","sunderland","swindon","wigan")
  EFL2teams <- c("barrow","bolton","bradford","cambridge u","carlisle",
                 "cheltenham","colchester","crawley","exeter","forest green",
                 "grimsby","harrogate","leyton orient","mansfield","morecambe",
                 "newport co","oldham","port vale","salford","scunthorpe",
                 "southend","stevenage","tranmere","walsall")
  
}

simulate2022 <- function() {
  require(zoo)
  #######
  ##SIMULATIONS 2022
  #######
  ##the teams
  #EPLteams <- merge(EPLteams,sim.update[,c("team1","team1_id")],by.x="team",by.y="team1")
  #EPLteams <- EPLteams[duplicated(EPLteams)==FALSE,]
  # exloc <- "/Volumes/11330730-dp/Correct-score/"
  dbloc <- "/Users/jjreade/Dropbox/Research/Sport/Correct-score/"
  allteams <- getTeams(2022)  
  EPLteams <- allteams$EPLteams
  EFLCteams <- allteams$EFLCteams
  EFL1teams <- allteams$EFL1teams
  EFL2teams <- allteams$EFL2teams
  NLteams <- allteams$NLteams
  
  ##load up the data
  elorank <- read.csv(paste0(dbloc,"/data/elorank-2022-07-17.csv"),stringsAsFactors = FALSE)
  colnames(elorank) <- gsub("(\\w)[.]([a-z])","\\1'\\2",gsub("[.]of[.]"," of ",gsub("[.][.](\\w+)[.]"," (\\1)",gsub("[.][.]",". ",gsub("(\\w)[.](\\w)","\\1 \\2",colnames(elorank))))))
  
  update.files <- list.files("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/",pattern="^historical_results_202.*?.csv$")
  #  update.files <- list.files("/Volumes/11330730-dp/Correct-score-data/soccerbase-data/",pattern="^historical_results_202.*?.csv$")
  update.files <- update.files[update.files>="historical_results_2022-07-17-20202.csv"]
  #update.files <- c("historical_results_2021-06-01-20202.csv",
                    #update.files[update.files>"historical_results_2021-06-01-20202.csv"])
  
  sim.update <- data.frame()
  for(ff in update.files) {
    print(ff)
    temp <- read.csv(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/",ff),stringsAsFactors = FALSE)
    #temp <- read.csv(paste0("/Volumes/11330730-dp/Correct-score-data/soccerbase-data/",ff),stringsAsFactors = FALSE)
    temp$division[temp$division==""] <- NA
    temp$div_id[temp$div_id=="n/a"] <- "-99"
    temp$div_id <- as.numeric(temp$div_id)
    temp$division <- na.locf(temp$division)
    temp$div_id <- na.locf(temp$div_id)
    temp$goals1 <- as.numeric(gsub("^\\D+(\\d+)\\D+(\\d+)\\D+$","\\1",temp$goals1))
    temp$goals2 <- as.numeric(gsub("^\\D+(\\d+)\\D+(\\d+)\\D+$","\\2",temp$goals2))
    #need to replace matches in existing sim.update that have happened in update file
    #first remove the ones that haven't happened in sim.update that have in temp
#     if(NROW(sim.update)>0) {
#       matches.now.happened <- temp$match_id[is.na(temp$goals1)==FALSE]
#       sim.update.not.happened <- sim.update$match_id[is.na(sim.update$goals1)==TRUE]
#       update.matches <- sim.update.not.happened[sim.update.not.happened %in% matches.now.happened]
# #      new.matches <- temp$match_id[is.na(temp$goals1)==FALSE]
#       sim.update <- rbind(sim.update[!(sim.update$match_id %in% update.matches),],temp[temp$match_id %in% update.matches,])
#     } else {
      sim.update <- rbind(sim.update,temp)
    # }
    temp <- NULL
  }
  sim.update$date <- as.Date(sim.update$date)
  sim.update <- sim.update[order(sim.update$match_id,sim.update$goals1,sim.update$date,decreasing = FALSE),]
  sim.update <- sim.update[duplicated(sim.update$match_id)==FALSE,]
  sim.update <- sim.update[order(sim.update$date),]
  
  # original <- read.csv("/Volumes/11330730-dp/Correct-score-data/soccerbase-data/historical_results_2021-06-01-20202.csv",stringsAsFactors = FALSE)
  # original$division[original$division==""] <- NA
  # original$div_id[original$div_id=="n/a"] <- "-99"
  # original$div_id <- as.numeric(original$div_id)
  # original$division <- na.locf(original$division)
  # original$div_id <- na.locf(original$div_id)
  # original$goals1 <- as.numeric(gsub("^\\D+(\\d+)\\D+(\\d+)\\D+$","\\1",original$goals1))
  # original$goals2 <- as.numeric(gsub("^\\D+(\\d+)\\D+(\\d+)\\D+$","\\2",original$goals2))
  # original <- original[original$date>="2021-08-01" & original$div_id>0 & original$div_id<10,]
  # 
  # ##NEED TO ADD IN ALL MISSING MATCHES (COVID POSTPONEMENTS)
  # sim.update <- sim.update[duplicated(sim.update$match_id)==FALSE,]
  # postponed <- original[!(original$match_id %in% sim.update$match_id),]
  # remaining.dates <- seq(from=Sys.Date(),to=as.Date("2022-07-29"),by="day")
  # free.dates <- remaining.dates[!(remaining.dates %in% as.Date(unique(sim.update$date[sim.update$date>=Sys.Date() & sim.update$div_id>0 & sim.update$div_id<10])))]
  # date=1
  # for(dd in 1:NROW(postponed)) {
  #   postponed$date[dd] <- free.dates[date]
  #   date = date+1
  # }
  # postponed$date <- as.Date(as.numeric(postponed$date),origin="1970-01-01")
  # sim.update <- rbind(sim.update,postponed)
  divnames <- unique(sim.update[sim.update$div_id<=10 & sim.update$div_id>0,c("div_id","division")])
  for(div in c(1:4,9)) {
    fixture.grid <- table(sim.update$team1_id[sim.update$div_id==div],sim.update$team2_id[sim.update$div_id==div])
    team.names.ids <- unique(sim.update[sim.update$div_id==div,c("team1","team1_id")])
    remaining.dates <- seq(from=Sys.Date(),to=as.Date("2023-05-29"),by="day")
    free.dates <- remaining.dates[!(remaining.dates %in% as.Date(unique(sim.update$date[sim.update$div_id==div & sim.update$date>=Sys.Date()])))]
    date=1
    for(rows in 1:NROW(fixture.grid)) {
      for(cols in 1:NCOL(fixture.grid)) {
        if(rows!=cols) {
          if(fixture.grid[rows,cols]==0) {
            print(paste0("Adding game: ",team.names.ids$team1[team.names.ids$team1_id==dimnames(fixture.grid)[[1]][rows]]," v ",team.names.ids$team1[team.names.ids$team1_id==dimnames(fixture.grid)[[2]][cols]]))
            sim.update <- rbind(sim.update,c(paste0("tgd",100*div+date),as.character(free.dates[date]),
                                             div,divnames$division[divnames$div_id==div],
                                             dimnames(fixture.grid)[[1]][rows],
                                             team.names.ids$team1[team.names.ids$team1_id==dimnames(fixture.grid)[[1]][rows]],
                                             NA,NA,
                                             team.names.ids$team1[team.names.ids$team1_id==dimnames(fixture.grid)[[2]][cols]],
                                             dimnames(fixture.grid)[[2]][cols]))
            date=date+1
          }
        }
      }
    }
  }
  # simdata0$date <- as.Date(simdata0$date)
  #  sim.update <- sim.update[is.na(sim.update$goals1)==FALSE,]
  
  ##UPDATE ELO
  sim.update$outcome <- 0.5*as.numeric(sim.update$goals1==sim.update$goals2) + as.numeric(sim.update$goals1>sim.update$goals2)
  sim.update$elostrength1 <- NA
  sim.update$elostrength2 <- NA
  sim.update$elopredict <- NA
  
  sim.update <- sim.update[order(sim.update$date),]
  elo.weight=20
  for(mm in 1:NROW(sim.update)) {
    print(100*mm/NROW(sim.update))
    if(!(paste0("id",sim.update$team1_id[mm]) %in% names(elorank))) {
      elorank[[paste0("id",sim.update$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",sim.update$team2_id[mm]) %in% names(elorank))) {
      elorank[[paste0("id",sim.update$team2_id[mm])]] <- 1000
    }
    sim.update$elostrength1[mm] <- elorank[[paste0("id",sim.update$team1_id[mm])]]
    sim.update$elostrength2[mm] <- elorank[[paste0("id",sim.update$team2_id[mm])]]
    sim.update$elopredict[mm] <- 1/(1+(10^((elorank[[paste0("id",sim.update$team2_id[mm])]]-elorank[[paste0("id",sim.update$team1_id[mm])]])/400)))
    adjustment <- sim.update$outcome[mm] - sim.update$elopredict[mm]
    if(is.na(adjustment)==FALSE) {
      elorank[paste0("id",sim.update$team1_id[mm])] <- elorank[[paste0("id",sim.update$team1_id[mm])]] + elo.weight*adjustment
      elorank[paste0("id",sim.update$team2_id[mm])] <- elorank[[paste0("id",sim.update$team2_id[mm])]] - elo.weight*adjustment
    }
  }
  
  simdata00 <- read.csv(paste0(dbloc,"/data/res0-2022-07-17.csv"),stringsAsFactors = FALSE)

  simdata00 <- simdata00[simdata00$div_id>=0 & simdata00$div_id<=10,]
  simdata00$elopredict <- simdata00$elo10predict
  simdata00$elostrength1 <- simdata00$elostrength10.1
  simdata00$elostrength2 <- simdata00$elostrength10.2
  sim.update$div_id <- as.numeric(sim.update$div_id)
  simdata00$div_id <- as.numeric(simdata00$div_id)
  simdata0 <- rbind(simdata00[simdata00$date<"2021-08-04",c("match_id","date","div_id","division","team1","team1_id","goals1","goals2","team2","team2_id","outcome","elostrength1","elostrength2","elopredict")],
                    sim.update[sim.update$date>="2021-08-04" & sim.update$div_id>0 & sim.update$div_id<=10,c("match_id","date","div_id","division","team1","team1_id","goals1","goals2","team2","team2_id","outcome","elostrength1","elostrength2","elopredict")])
  simdata0 <- simdata0[duplicated(simdata0[,c("team1_id","team2_id","date")])==FALSE,]
  
  simdata0 <- simdata0[is.na(simdata0$date)==FALSE,]
  simdata0$date0[regexpr("-",simdata0$date)>-1] <- as.Date(simdata0$date[regexpr("-",simdata0$date)>-1])
  simdata0$date0[regexpr("-",simdata0$date)==-1] <- as.Date(as.numeric(simdata0$date[regexpr("-",simdata0$date)==-1]),origin="1970-01-01")
  simdata0$date <- as.Date(simdata0$date0)
  simdata0$date0 <- NULL 
  
  #simdata0 <- res0[res0$div_id>=0 & res0$div_id<=10,]
  
  # simdata0$goals1[is.na(simdata0$goals1.update)==FALSE] <- simdata0$goals1.update[is.na(simdata0$goals1.update)==FALSE]
  # simdata0$goals2[is.na(simdata0$goals2.update)==FALSE] <- simdata0$goals2.update[is.na(simdata0$goals2.update)==FALSE]
  # simdata0$goals1.update <- NULL
  # simdata0$goals2.update <- NULL
  
  ##add in elo ratings for all teams
  # simdata0$team1 <- tolower(simdata0$team1)
  # simdata0$team2 <- tolower(simdata0$team2)
  # simdata0$elostrength1 <- NA
  # simdata0$elostrength2 <- NA
  
  #colnames(elorank)[colnames(elorank)=="aw bledon"] <- "afc w'bledon"
  
  # for(tt in c(EPLteams,EFLCteams,EFL1teams,EFL2teams)) {
  #   simdata0$elostrength1[paste0("id",simdata0$team1_id)==tt] <- elorank[[tt]]
  #   simdata0$elostrength2[paste0("id",simdata0$team2_id)==tt] <- elorank[[tt]]
  # }
  #simdata0$elopredict <- 1/(1+(10^((simdata0$elostrength2-simdata0$elostrength1)/400)))
  
  # sim.list0 <- runsimregression()
  #sim.list0 <- list("m1d"=m1d.sim,"m2d"=m2d.sim)
  #simdata0 <- data[data$date>=Sys.Date() & data$div_id>0 & data$div_id<10,]
  simdata0$goals1 <- as.numeric(simdata0$goals1)
  summary(m1d.sim <- glm(goals1 ~ elostrength1 + elostrength2, family="poisson", data=simdata0[simdata0$date>="2010-01-01",], na.action=na.exclude))
  simdata0$goals2 <- as.numeric(simdata0$goals2)
  summary(m2d.sim <- glm(goals2 ~ elostrength1 + elostrength2, family="poisson", data=simdata0[simdata0$date>="2010-01-01",], na.action=na.exclude))
  
  #  summary(m1d <- glm(goals1 ~ goals1.1 + goals2.1.1 + elostrength1 + elostrength2, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  #  summary(m2d <- glm(goals2 ~ goals2.1 + goals1.2.1 + elostrength1 + elostrength2, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  
  simdata0$goals1d.hat <- NA
  simdata0$goals2d.hat <- NA
  simdata0$goals1d.hat[simdata0$date>="2010-01-01"] <- predict(m1d.sim,type="response")
  simdata0$goals2d.hat[simdata0$date>="2010-01-01"] <- predict(m2d.sim,type="response")
  
  all.season.tabs1 <- data.frame(stringsAsFactors = FALSE)
  all.season.tabs2 <- data.frame(stringsAsFactors = FALSE)
  all.season.tabs3 <- data.frame(stringsAsFactors = FALSE)
  all.season.tabs4 <- data.frame(stringsAsFactors = FALSE)
  all.season.tabs5 <- data.frame(stringsAsFactors = FALSE)
  all.season.results1 <- data.frame(stringsAsFactors = FALSE)
  all.season.results2 <- data.frame(stringsAsFactors = FALSE)
  all.season.results3 <- data.frame(stringsAsFactors = FALSE)
  all.season.results4 <- data.frame(stringsAsFactors = FALSE)
  all.season.results5 <- data.frame(stringsAsFactors = FALSE)
  
  
  #update data
  
  counter = 0
  start.date = Sys.Date()
  RR=1000
  match.dates <- sort(unique(simdata0$date[is.na(simdata0$goals1)==TRUE & simdata0$date>"2022-07-20"]))
  
  basedir = paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/end-of-season/endof2023/")
  fulldir1 <- paste0(basedir,"div1/sim-",Sys.Date(),"/simno-",counter,"/")
  fulldir2 <- paste0(basedir,"div2/sim-",Sys.Date(),"/simno-",counter,"/")
  fulldir3 <- paste0(basedir,"div3/sim-",Sys.Date(),"/simno-",counter,"/")
  fulldir4 <- paste0(basedir,"div4/sim-",Sys.Date(),"/simno-",counter,"/")
  fulldir5 <- paste0(basedir,"div5/sim-",Sys.Date(),"/simno-",counter,"/")
  if (!file.exists(basedir)){
    dir.create(basedir)}
  if (!file.exists(paste0(basedir,"div1"))){
    dir.create(paste0(basedir,"div1"))}
  if (!file.exists(paste0(basedir,"div1/sim-",Sys.Date(),"/"))){
    dir.create(paste0(basedir,"div1/sim-",Sys.Date(),"/"))}
  if (!file.exists(fulldir1)){
    dir.create(fulldir1)}
  if (!file.exists(paste0(basedir,"div2"))){
    dir.create(paste0(basedir,"div2"))}
  if (!file.exists(paste0(basedir,"div2/sim-",Sys.Date(),"/"))){
    dir.create(paste0(basedir,"div2/sim-",Sys.Date(),"/"))}
  if (!file.exists(fulldir2)){
    dir.create(fulldir2)}
  if (!file.exists(paste0(basedir,"div3"))){
    dir.create(paste0(basedir,"div3"))}
  if (!file.exists(paste0(basedir,"div3/sim-",Sys.Date(),"/"))){
    dir.create(paste0(basedir,"div3/sim-",Sys.Date(),"/"))}
  if (!file.exists(fulldir3)){
    dir.create(fulldir3)}
  if (!file.exists(paste0(basedir,"div4"))){
    dir.create(paste0(basedir,"div4"))}
  if (!file.exists(paste0(basedir,"div4/sim-",Sys.Date(),"/"))){
    dir.create(paste0(basedir,"div4/sim-",Sys.Date(),"/"))}
  if (!file.exists(fulldir4)){
    dir.create(fulldir4)}
  if (!file.exists(paste0(basedir,"div5"))){
    dir.create(paste0(basedir,"div5"))}
  if (!file.exists(paste0(basedir,"div5/sim-",Sys.Date(),"/"))){
    dir.create(paste0(basedir,"div5/sim-",Sys.Date(),"/"))}
  if (!file.exists(fulldir5)){
    dir.create(fulldir5)}
  
  ##manual correction nfor halifax playing twice on one day
  # simdata0$date[simdata0$match_id=="tgc833905"] <- as.Date("2022-03-10")
  
  simdata0 <- simdata0[order(simdata0$date),]
  
  ##start of simulation loop####
  for(rr in 1:RR) {
    print(rr)
    #only care about selected league for forecasts
    #
    elo.sim <- elorank
    #step two: forecast day by day in selected league
    
    simdata <- simdata0[simdata0$date>"2022-07-20",]
    simdata <- simdata[order(simdata$date),]
    
    #now loop through dates
    for(dd0 in match.dates) {
      dd = as.Date(dd0,origin="1970-01-01")
      
      #forecast
      forecast.matches0 <- simdata[simdata$date==dd & is.na(simdata$goals1)==TRUE,]
      
      if(NROW(forecast.matches0)>0) {
        forecasts <- createforecasts(data=forecast.matches0,
                                     home.g.mod = m1d.sim,
                                     away.g.mod = m2d.sim,
                                     date0=dd,
                                     days.ahead=0,save=FALSE)
        #insert outcomes
        forecasts$goals1 <- rpois(NROW(forecasts),lambda=forecasts$lambda1.hat)
        forecasts$goals2 <- rpois(NROW(forecasts),lambda=forecasts$lambda2.hat)
        forecasts$outcome <- 0.5*as.numeric(forecasts$goals1==forecasts$goals2) + as.numeric(forecasts$goals1>forecasts$goals2)
        
        #update data
        updatelist <- quickupdate(data=simdata,elorank.u=elo.sim,results=forecasts)
        simdata <- updatelist$res
        elo.sim <- updatelist$elorank
        simdata <- simdata[order(simdata$date),]
      }
    }
    ##at end of season calculate league table
    season.tab1 <- league.tab(game_id=simdata$match_id[simdata$div_id==1],
                              date0=as.Date(simdata$date[simdata$div_id==1]),
                              team1=simdata$team1[simdata$div_id==1],
                              g1=simdata$goals1[simdata$div_id==1],
                              g2=simdata$goals2[simdata$div_id==1],
                              team2=simdata$team2[simdata$div_id==1],pfw=3,
                              div_id = 1,season=2022,
                              points.deduct = NULL,wc=FALSE)
    final.lge.tab1 <- season.tab1$final.table
    final.lge.tab1$replication <- rr
    #    all.season.tabs1 <- rbind(all.season.tabs1,final.lge.tab1)
    simdata$replication <- rr
    #    all.season.results1 <- rbind(all.season.results1,simdata[simdata$div_id==1,c("match_id","goals1","goals2","replication")])
    write.csv(final.lge.tab1,
              paste0(fulldir1,"/table-rep-",rr,".csv"))
    write.csv(simdata[simdata$div_id==1,c("match_id","goals1","goals2","replication")],
              paste0(fulldir1,"/results-rep-",rr,".csv"))
    # write.csv(all.season.tabs1,
    #           paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div1-sim",Sys.Date(),
    #                  "-",counter,"-raw.csv"))
    # write.csv(all.season.results1,
    #           paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div1-sim",Sys.Date(),
    #                  "-",counter,"-all-results.csv"))
    # if(rr %in% seq(2000,84000,2000)) {
    #   all.season.tabs1 <- data.frame(stringsAsFactors = FALSE)
    #   all.season.results1 <- data.frame(stringsAsFactors = FALSE)
    #   counter = counter+1
    # }
    season.tab2 <- league.tab(game_id=simdata$match_id[simdata$div_id==2],
                              date0=as.Date(simdata$date[simdata$div_id==2]),
                              team1=simdata$team1[simdata$div_id==2],
                              g1=simdata$goals1[simdata$div_id==2],
                              g2=simdata$goals2[simdata$div_id==2],
                              team2=simdata$team2[simdata$div_id==2],pfw=3,
                              div_id = 2,season=2022,
                              points.deduct = NULL,wc=FALSE)##data.frame("team"=c("Derby","Reading"),"points"=c(21,6))
    final.lge.tab2 <- season.tab2$final.table
    final.lge.tab2$replication <- rr
    write.csv(final.lge.tab2,
              paste0(fulldir2,"/table-rep-",rr,".csv"))
    write.csv(simdata[simdata$div_id==2,c("match_id","team1","goals1","goals2","team2","replication")],
              paste0(fulldir2,"/results-rep-",rr,".csv"))
    #    all.season.tabs2 <- rbind(all.season.tabs2,final.lge.tab2)
    #    all.season.results2 <- rbind(all.season.results2,simdata[simdata$div_id==2,c("match_id","goals1","goals2","replication")])
    # write.csv(all.season.tabs2,
    #           paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div2-sim",Sys.Date(),
    #                  "-",counter,"-raw.csv"))
    # write.csv(all.season.results2,
    #           paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div2-sim",Sys.Date(),
    #                  "-",counter,"-all-results.csv"))
    # if(rr %in% seq(2000,84000,2000)) {
    #   all.season.tabs2 <- data.frame(stringsAsFactors = FALSE)
    #   all.season.results2 <- data.frame(stringsAsFactors = FALSE)
    #   counter = counter+2
    # }
    season.tab3 <- league.tab(game_id=simdata$match_id[simdata$div_id==3],
                              date0=as.Date(simdata$date[simdata$div_id==3]),
                              team1=simdata$team1[simdata$div_id==3],
                              g1=simdata$goals1[simdata$div_id==3],
                              g2=simdata$goals2[simdata$div_id==3],
                              team2=simdata$team2[simdata$div_id==3],pfw=3,
                              div_id = 3,season=2022,
                              points.deduct = NULL,wc=FALSE)##
    final.lge.tab3 <- season.tab3$final.table
    final.lge.tab3$replication <- rr
    write.csv(final.lge.tab3,
              paste0(fulldir3,"/table-rep-",rr,".csv"))
    write.csv(simdata[simdata$div_id==3,c("match_id","goals1","goals2","replication")],
              paste0(fulldir3,"/results-rep-",rr,".csv"))
    #    all.season.tabs3 <- rbind(all.season.tabs3,final.lge.tab3)
    #    all.season.results3 <- rbind(all.season.results3,simdata[simdata$div_id==3,c("match_id","goals1","goals2","replication")])
    #    write.csv(all.season.tabs3,
    #              paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div3-sim",Sys.Date(),
    #                     "-",counter,"-raw.csv"))
    # if(rr %in% seq(2000,84000,2000)) {
    #   all.season.tabs3 <- data.frame(stringsAsFactors = FALSE)
    #   all.season.results3 <- data.frame(stringsAsFactors = FALSE)
    #   counter = counter+1
    # }
    season.tab4 <- league.tab(game_id=simdata$match_id[simdata$div_id==4],
                              date0=as.Date(simdata$date[simdata$div_id==4]),
                              team1=simdata$team1[simdata$div_id==4],
                              g1=simdata$goals1[simdata$div_id==4],
                              g2=simdata$goals2[simdata$div_id==4],
                              team2=simdata$team2[simdata$div_id==4],pfw=3,
                              div_id = 4,season=2022,
                              points.deduct = NULL,wc=FALSE)##
    final.lge.tab4 <- season.tab4$final.table
    final.lge.tab4$replication <- rr
    write.csv(final.lge.tab4,
              paste0(fulldir4,"/table-rep-",rr,".csv"))
    write.csv(simdata[simdata$div_id==4,c("match_id","goals1","goals2","replication")],
              paste0(fulldir4,"/results-rep-",rr,".csv"))
    #    all.season.tabs4 <- rbind(all.season.tabs4,final.lge.tab4)
    #    all.season.results4 <- rbind(all.season.results4,simdata[simdata$div_id==4,c("match_id","goals1","goals2","replication")])
    #    write.csv(all.season.tabs4,
    #              paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div4-sim",Sys.Date(),
    #                     "-",counter,"-raw.csv"))
    #    write.csv(all.season.results4,
    #              paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div4-sim",Sys.Date(),
    #                     "-",counter,"-all-results.csv"))
    # if(rr %in% seq(2000,84000,2000)) {
    #   all.season.tabs4 <- data.frame(stringsAsFactors = FALSE)
    #   all.season.results4 <- data.frame(stringsAsFactors = FALSE)
    #   counter = counter+1
    # }
    season.tab5 <- league.tab(game_id=simdata$match_id[simdata$div_id==9],
                              date0=as.Date(simdata$date[simdata$div_id==9]),
                              team1=simdata$team1[simdata$div_id==9],
                              g1=simdata$goals1[simdata$div_id==9],
                              g2=simdata$goals2[simdata$div_id==9],
                              team2=simdata$team2[simdata$div_id==9],pfw=3,
                              div_id = 9,season=2022,
                              points.deduct = NULL,wc=FALSE)##
    final.lge.tab5 <- season.tab5$final.table
    final.lge.tab5$replication <- rr
    write.csv(final.lge.tab5,
              paste0(fulldir5,"/table-rep-",rr,".csv"))
    write.csv(simdata[simdata$div_id==5,c("match_id","goals1","goals2","replication")],
              paste0(fulldir5,"/results-rep-",rr,".csv"))
  }
}

###outputting the results
#  all.season.tabs1.0 <- read.csv("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div1-sim2020-09-10-1-raw.csv",stringsAsFactors = FALSE)
#  all.season.tabs2.0 <- read.csv("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div2-sim2020-09-10-1-raw.csv",stringsAsFactors = FALSE)
#  all.season.tabs3.0 <- read.csv("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div3-sim2020-09-10-1-raw.csv",stringsAsFactors = FALSE)
#  all.season.tabs4.0 <- read.csv("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div4-sim2020-09-10-1-raw.csv",stringsAsFactors = FALSE)
#  all.season.tabs <- rbind(all.season.tabs4.0[,-1],all.season.tabs4)
output.sim2021 <- function(date0 = Sys.Date()-1,counter=0) {
  require(xtable)
  basedir = paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/end-of-season/endof2023/")
  date0 = Sys.Date()-1
  print(date0)
  fulldir1 <- paste0(basedir,"div1/sim-",date0,"/simno-",counter,"/")
  fulldir2 <- paste0(basedir,"div2/sim-",date0,"/simno-",counter,"/")
  fulldir3 <- paste0(basedir,"div3/sim-",date0,"/simno-",counter,"/")
  fulldir4 <- paste0(basedir,"div4/sim-",date0,"/simno-",counter,"/")
  fulldir5 <- paste0(basedir,"div5/sim-",date0,"/simno-",counter,"/")
  fulldirs <- c(fulldir1,fulldir2,fulldir3,fulldir4,fulldir5)
  leagues <- c("EPL","EFLC","EFL1","EFL2","NL")
  leagues.long <- c("Premier League","Championship","League One","League Two","National League")

  ##load up full league final tables
  all.final.tabs <- read.csv("/Users/jjreade/Dropbox/Research/Sport/all-final-league-tables.csv",stringsAsFactors = FALSE)

  ##full league positions for CDF plots
  lge.releg <- c(17,21,20,22,20)
  lge.title <- c(1,1,1,1,1)
  lge.prom <- c(NA,2,2,3,1)
  lge.playoffs <- c(NA,6,6,7,7)
  lgef.releg <- c(17,41,64,90,112)
  lgef.title <- c(1,21,45,69,93)
  lgef.prom <- c(NA,22,46,71,93)
  lgef.playoffs <- c(NA,26,50,75,99)
  divids <- c(1:4,NA)
  divids.1992 <- c(5:9)
  
  dbloc = "/Users/jjreade/Dropbox/Research/Sport/Correct-score"
  
  for(ll in 1:5) {
    print(leagues[ll])
    all.season.tabs <- data.frame(stringsAsFactors = FALSE)
    all.season.res <- data.frame(stringsAsFactors = FALSE)
    divfiles <- list.files(fulldirs[ll],pattern="^table")
    #resfiles <- list.files(fulldirs[ll],pattern="^results")
    for(ff in 1:NROW(divfiles)) {
      temp <- read.csv(paste0(fulldirs[ll],divfiles[ff]),stringsAsFactors = FALSE)
      #tempres <- read.csv(paste0(fulldirs[ll],resfiles[ff]),stringsAsFactors = FALSE)
      if(ll==2) {
        #temp$points[temp$team=="Reading"] <- temp$points[temp$team=="Reading"] - 12
        #temp$points[temp$team=="Derby"] <- temp$points[temp$team=="Derby"] - 6-21
        temp <- temp[order(temp$points,temp$goal.diff,temp$goals.scored,decreasing = TRUE),]
        temp$position <- 1:24
        rownames(temp) <- 1:24
      }
      all.season.tabs <- rbind(all.season.tabs,temp)
      #all.season.res <- rbind(all.season.res,tempres)
    }
    lge.short <- leagues[ll]
    lge.long <- leagues.long[ll]
    
    # all.season.res$outcome <- as.numeric(all.season.res$goals1==all.season.res$goals2) + 3*as.numeric(all.season.res$goals1>all.season.res$goals2)
    # pts.next4 <- aggregate(all.season.res$outcome[all.season.res$match_id %in% c("tgc830501","tgc830511","tgc830356","tgc830368")],
    #                        by=list(all.season.res$replication[all.season.res$match_id %in% c("tgc830501","tgc830511","tgc830356","tgc830368")]),
    #                        FUN=sum)
    #positions if 4 defeats = 0 pts
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==0]])/135
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==1]])/266
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==2]])/177
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==3]])/361
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==4]])/391
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==5]])/172
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==6]])/214
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==7]])/167
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==8]])/40
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==9]])/56
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==10]])/13
    # table(all.season.tabs$position[all.season.tabs$team=="Oldham" & all.season.tabs$replication %in% pts.next4$Group.1[pts.next4$x==12]])/8
    
    
    # if(ll==4) {
    #   jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/end-of-season/end-of-season-oafc-",date0,"-cdf-pts.jpg"),width = 8, height = 6, units = "in", res=300)
    #   plot(ecdf(all.season.tabs$points[all.season.tabs$team=="Oldham"]),
    #        main=paste0("Probability of Oldham avoiding ",lge.long," relegation"),
    #        ylab="Probability",xlab="Points",sub="blue=oafc, black=simulated",col="blue")
    #   lines(ecdf(all.season.tabs$points[all.season.tabs$position==lge.releg[ll]]),
    #         col=1)
    #   lines(ecdf(all.season.tabs$points[all.season.tabs$team=="Oldham"]+3),lty=3,pch=1,col="blue")
    #   dev.off()
    # }
    base.year <- 1960+42*as.numeric(ll==4)
    jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/end-of-season/end-of-season-",lge.short,"-",date0,"-cdf-releg.jpg"),width = 8, height = 6, units = "in", res=300)
    plot(ecdf(all.season.tabs$points[all.season.tabs$position==lge.releg[ll]]),
         main=paste0("Probability of avoiding ",lge.long," relegation with X points"),
         ylab="Probability",xlab="Points",sub="red=actual, black=simulated")
    lines(ecdf(all.final.tabs$points[(all.final.tabs$div == divids[ll] | all.final.tabs$div == divids.1992[ll]) & all.final.tabs$full.pos==lgef.releg[ll] & all.final.tabs$season>base.year]),
          col=2)
    # abline(v=44,lty=3,col="grey")
    # abline(h=.54,lty=3,col="grey")
    # abline(v=45,lty=3,col="grey")
    # abline(h=.73,lty=3,col="grey")
    # abline(v=46,lty=3,col="grey")
    # abline(h=.88,lty=3,col="grey")
    dev.off()

    jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/end-of-season/end-of-season-",lge.short,"-",date0,"-cdf-title.jpg"),width = 8, height = 6, units = "in", res=300)
    plot(ecdf(all.season.tabs$points[all.season.tabs$position==lge.title[ll]]),
         main=paste0("Probability of achieving ",lge.long," title with X points"),
         ylab="Probability",xlab="Points",sub="red=actual, black=simulated")
    lines(ecdf(all.final.tabs$points[(all.final.tabs$div == divids[ll] | all.final.tabs$div == divids.1992[ll]) & all.final.tabs$full.pos==lgef.title[ll]]),
          col=2)
    dev.off()
    
    the.teams <- sort(unique(as.character(all.season.tabs$team)))
    final.tab.matrix <- matrix(NA,NROW(the.teams),NROW(the.teams))
    colnames(final.tab.matrix) <- the.teams
    rownames(final.tab.matrix) <- 1:NROW(the.teams)
    for(tt in the.teams) {
      for(pp in 1:NROW(the.teams)) {
        final.tab.matrix[pp,tt] <- 100*sum(all.season.tabs$position[all.season.tabs$team==tt]==pp)/(NROW(all.season.tabs)/NROW(the.teams))
      }
    }
    #sort the table
    colnames(final.tab.matrix) <- to3(the.teams)
    
    #View(round(final.tab.matrix[,order(aggregate(all.season.tabs$position,by=list(all.season.tabs$team),FUN=mean)$x)],1))
    
    #output the table
    ordered.output.tab <- final.tab.matrix[,order(aggregate(all.season.tabs$position,by=list(all.season.tabs$team),FUN=mean)$x)]
    write.csv(ordered.output.tab,paste0(dbloc,"/data/end-of-season/end-of-season-",lge.short,"-",date0,".csv"))
    ordered.output.tab.display <- xtable(ordered.output.tab,digits = rep(1,NCOL(ordered.output.tab)+1))
    align(ordered.output.tab.display) <- paste0("l|",paste(rep("r",NROW(the.teams)), collapse = ""))
    
    ##want summary for promotion/playoffs/relegation
    if(lge.short=="EPL") {
      col1 <- ordered.output.tab[1,] #title
      col2 <- colSums(ordered.output.tab[1:4,]) #top4
      col3 <- colSums(ordered.output.tab[18:20,]) #relegation
      lge.colnames <- c("Team","Title","Top 4","Relegation","Goals","Conceded")
    }
    if(lge.short=="EFLC") {
      col1 <- colSums(ordered.output.tab[1:2,]) #promotion
      col2 <- colSums(ordered.output.tab[3:6,]) #playoffs
      col3 <- colSums(ordered.output.tab[22:24,]) #relegation
      lge.colnames <- c("Team","Promotion","Play-offs","Relegation","Goals","Conceded")
    }
    if(lge.short=="EFL1") {
      col1 <- colSums(ordered.output.tab[1:2,]) #promotion
      col2 <- colSums(ordered.output.tab[3:6,]) #playoffs
      col3 <- colSums(ordered.output.tab[21:24,]) #relegation
      lge.colnames <- c("Team","Promotion","Play-offs","Relegation","Goals","Conceded")
    }
    if(lge.short=="EFL2") {
      col1 <- colSums(ordered.output.tab[1:3,]) #promotion
      col2 <- colSums(ordered.output.tab[4:7,]) #playoffs
      col3 <- colSums(ordered.output.tab[23:24,]) #relegation
      lge.colnames <- c("Team","Promotion","Play-offs","Relegation","Goals","Conceded")
    }
    if(lge.short=="NL") {
      col1 <- ordered.output.tab[1,] #promotion
      col2 <- colSums(ordered.output.tab[2:7,]) #playoffs
      col3 <- colSums(ordered.output.tab[21:23,]) #relegation
      lge.colnames <- c("Team","Promotion","Play-offs","Relegation","Goals","Conceded")
    }
    gs.col <- aggregate(all.season.tabs$goals.scored,by=list(all.season.tabs$team),FUN=mean)
    gs.col$x <- gs.col$x - mean(all.season.tabs$goals.scored)
    gs.col$Group.1 <- to3(gs.col$Group.1)
    gc.col <- aggregate(all.season.tabs$goals.conceded,by=list(all.season.tabs$team),FUN=mean)
    gc.col$x <- gc.col$x - mean(all.season.tabs$goals.conceded)
    gc.col$Group.1 <- to3(gc.col$Group.1)
    
    summ.tab <- data.frame(cbind(col1,col2,col3))
    summ.tab$team <- rownames(summ.tab)
    summ.tab <- merge(summ.tab,gs.col,by.x=c("team"),by.y=c("Group.1"),all.x=TRUE)
    summ.tab <- merge(summ.tab,gc.col,by.x=c("team"),by.y=c("Group.1"),all.x=TRUE)
    colnames(summ.tab) <- lge.colnames
    summ.tab <- summ.tab[order(-summ.tab[,2],-summ.tab[,3],summ.tab[,4],-summ.tab[,5],summ.tab[,6]),]
    write.csv(summ.tab,paste0(dbloc,"/data/end-of-season/end-of-season-",lge.short,"-",date0,"-summary.csv"))
    
    summ.tab.files <- list.files(paste0(dbloc,"/data/end-of-season/"),pattern=paste0("^end-of-season-",lge.short,"-\\d{4}-\\d{2}-\\d{2}[.]csv$"))
    
    jpeg(paste0(dbloc,"/data/end-of-season/end-of-season-",lge.short,"-",date0,"-col-summ.jpg"),width = 12, height = 12, units = "in", res=300)
    x = 1:ncol(summ.tab[,-1])
    y = 1:nrow(summ.tab)
    centers <- expand.grid(y,x)
    par(mar = c(2,7,4,2))
    
    image(x, y, t(as.matrix(summ.tab[,-1])),
          col = rev(c(heat.colors(100))),
          breaks = c(0:100),
          xaxt = 'n', 
          yaxt = 'n', 
          xlab = '', 
          ylab = '',
          ylim = c(max(y) + 0.5, min(y) - 0.5)
    )
    for(ii in 2:NCOL(summ.tab)) {
      text(ii-1, 1:NROW(summ.tab), round(summ.tab[,ii],1), col= "black")
    }
    
    mtext(colnames(summ.tab[,-1]), at=1:ncol(summ.tab[,-1]), padj = -1)
    mtext(summ.tab$Team, at=1:nrow(summ.tab), side = 2, las = 1, adj = 1.2)
    
    #add black lines
    abline(v=x + 0.5)
    dev.off()
    
    ##print out non-summary
    print(xtable(ordered.output.tab.display))
    print(ordered.output.tab.display, type = "latex", 
          floating = FALSE,file=paste0(dbloc,"/data/end-of-season/end-of-season-",lge.short,"-",date0,".tex"))
    print(ordered.output.tab.display, type = "html", 
          floating = FALSE,file=paste0(dbloc,"/data/end-of-season/end-of-season-",lge.short,"-",date0,".html"))
    
    fileConn <- file(paste0(dbloc,"/data/end-of-season/end-of-season-",lge.short,"-",date0,"-tab.tex"))
    writeLines(c("\\documentclass{standalone}","\\begin{document}",paste0("\\input{",dbloc,"/data/end-of-season/end-of-season-",lge.short,"-",date0,".tex}"),"\\end{document}"), fileConn)
    close(fileConn)
    
    cmds <- c(paste0("cd ",dbloc,"/data/end-of-season/"), 
              paste0("pdflatex end-of-season-",lge.short,"-",date0,"-tab.tex  --output-directory=",dbloc,"/data/end-of-season/"), 
              paste0("convert -density 300 end-of-season-",lge.short,"-",date0,"-tab.pdf end-of-season-",lge.short,"-",date0,"-tab.jpg"));
    system(paste(cmds, collapse=";"))
    
    x = 1:ncol(ordered.output.tab.display)
    y = 1:nrow(ordered.output.tab.display)
    centers <- expand.grid(y,x)
    
    jpeg(paste0(dbloc,"/data/end-of-season/end-of-season-",lge.short,"-",date0,"-col-tab.jpg"),width = 12, height = 12, units = "in", res=300)
    
    #make the plot margins a little bigger
    par(mar = c(2,7,4,2))
    
    image(x, y, as.matrix(ordered.output.tab),
          col = rev(c(heat.colors(100))),
          breaks = c(0:100),
          xaxt = 'n', 
          yaxt = 'n', 
          xlab = '', 
          ylab = '',
          ylim = c(max(y) + 0.5, min(y) - 0.5)
    )
    for(ii in 1:NCOL(ordered.output.tab)) {
      text(ii, 1:NROW(ordered.output.tab), round(ordered.output.tab[,ii],1), col= "black")
    }
    
    mtext(colnames(ordered.output.tab.display), at=1:ncol(ordered.output.tab.display), padj = -1)
    mtext(rownames(ordered.output.tab.display), at=1:nrow(ordered.output.tab.display), side = 2, las = 1, adj = 1.2)
    
    #add black lines
    abline(h=y + 0.5)
    if(lge.short=="NED1") {
      abline(h=y[c(2,4,15,16)] + 0.5, lwd=3)##premier league
    }
    if(lge.short=="FRA1") {
      abline(h=y[c(1,3,4,17)] + 0.5, lwd=3)##premier league
    }
    if(lge.short=="ITA1" | lge.short=="ESP1") {
      abline(h=y[c(1,4,6,17)] + 0.5, lwd=3)##Serie A
    }
    if(lge.short=="GER1") {
      abline(h=y[c(1,4,6,15,16)] + 0.5, lwd=3)##Serie A
    }
    if(lge.short=="EPL") {
      abline(h=y[c(1,4,5,17)] + 0.5, lwd=3)##premier league
    }
    if(lge.short=="EFLC") {
      abline(h=y[c(2,6,21)] + 0.5, lwd=3)##championship
    }
    if(lge.short=="EFL1") {
      abline(h=y[c(2,6,20)] + 0.5, lwd=3)##league one
    }
    if(lge.short=="EFL2") {
      abline(h=y[c(3,7,22)] + 0.5, lwd=3)##league two
    }
    if(lge.short=="NL1") {
      abline(h=y[c(1,3,7,20)] + 0.5, lwd=3)##league two
    }
    
    abline(v=x + 0.5)
    dev.off()
  }
  
  
}

load.facup.results <- function(teamnames="soccerbase") {
  #teamnames can be soccerbase and 11v11 for now. if neither specified, soccerbase defaults
  if(teamnames!="soccerbase" & teamnames!="11v11") { teamnames="soccerbase" }
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)

  fac.res1 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/facup-qualifiers-to-2000.csv",stringsAsFactors = FALSE)
  fac.res1$team1 <- gsub("<font color=#1B3332><b>","",fac.res1$team1)
  fac.res1$team2 <- gsub("<font color=#1B3332><b>","",fac.res1$team2)
  fac.res1$score <- gsub("<td bgcolor=COC26D align=center><font color=#1B3332><b>","",fac.res1$score)
  fac.res1$date <- as.Date(paste0(substr(fac.res1$date,1,10),gsub("^(\\d{4}).*?$","\\1",fac.res1$filename)),"%a %d.%m.%Y")
  
  fac.res1$team1 <- gsub(" AFC$","",gsub("[(].*?[)]","",gsub("Boro[.]$","Borough",gsub(" '90","",gsub(" '93","",gsub(" Utd[.]$"," United",gsub(" U$"," United",gsub("Ath[.]$","Athletic",gsub("^\\s+|\\s+$","",gsub("  "," ",gsub("  "," ",gsub("\\t"," ",gsub(" [(].*?[)]","",fac.res1$team1)))))))))))))
  fac.res1$team2 <- gsub(" AFC$","",gsub("[(].*?[)]","",gsub("Boro[.]$","Borough",gsub(" '90","",gsub(" '93","",gsub(" Utd[.]$"," United",gsub(" U$"," United",gsub("Ath[.]$","Athletic",gsub("^\\s+|\\s+$","",gsub("  "," ",gsub("  "," ",gsub("\\t"," ",gsub(" [(].*?[)]","",fac.res1$team2)))))))))))))
  #teamtab <- table(c(fac.res1$team1,fac.res1$team2))
  
  fac.res2 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/fa-cup-qualifiers-2000-2022.csv",stringsAsFactors = FALSE)
  fac.res2$Date[nchar(fac.res2$Date)==7] <- paste0("0",fac.res2$Date[nchar(fac.res2$Date)==7])
  colnames(fac.res2)[c(3:5)] <- c("team1","score","team2")
  fac.res2$date <- as.Date(paste0(substr(fac.res2$Date,1,6),"20",substr(fac.res2$Date,7,8)),"%d.%m.%Y")
  fac.res2$team1 <- gsub("_","",gsub(" AFC$","",gsub("[(].*?[)]","",gsub("Boro[.]$","Borough",gsub(" '90","",gsub(" '93","",gsub(" Utd[.]$"," United",gsub(" U$"," United",gsub("Ath[.]$","Athletic",gsub("^\\s+|\\s+$","",gsub("  "," ",gsub("  "," ",gsub("\\t"," ",gsub(" [(].*?[)]","",fac.res2$team1))))))))))))))
  fac.res2$team2 <- gsub("_","",gsub(" AFC$","",gsub("[(].*?[)]","",gsub("Boro[.]$","Borough",gsub(" '90","",gsub(" '93","",gsub(" Utd[.]$"," United",gsub(" U$"," United",gsub("Ath[.]$","Athletic",gsub("^\\s+|\\s+$","",gsub("  "," ",gsub("  "," ",gsub("\\t"," ",gsub(" [(].*?[)]","",fac.res2$team2))))))))))))))
  
  fac.res <- rbind(fac.res1[,c("date","team1","score","team2")],fac.res2[,c("date","team1","score","team2")])
  
  fac.res <- fac.res[fac.res$team1!="" & fac.res$team2!="",]
  
  fac.res$match_id <- paste0("fac",1:NROW(fac.res))
  fac.res$division <- "FA Cup Qualifiers"
  fac.res$div_id <- 58
  fac.res$attendance <- NA
  
  fac.res$goals1 <- as.numeric(gsub("^.*?(\\d+).*?-.*?(\\d+).*?$","\\1",fac.res$score))
  fac.res$goals2 <- as.numeric(gsub("^.*?(\\d+).*?-.*?(\\d+).*?$","\\2",fac.res$score))
  
  fac.res <- fac.res[is.na(fac.res$goals1)==FALSE & is.na(fac.res$goals2)==FALSE,]
  fac.res$season <- as.numeric(format(fac.res$date,"%Y")) - as.numeric(as.numeric(format(fac.res$date,"%m"))<7)
  
  if(teamnames=="soccerbase") {
    corsf <- corr[,c("soccerbase.team","soccerbase.id","footballwebpages.teams")]
    corsf <- corsf[duplicated(corsf)==FALSE,]
    corsf <- corsf[corsf$footballwebpages.teams!="",]
    corsf <- corsf[duplicated(corsf$footballwebpages.teams)==FALSE,]
    fac.res <- merge(fac.res,corsf,by.x="team1",by.y="footballwebpages.teams",all.x=TRUE)
    fac.res <- merge(fac.res,corsf,by.x="team2",by.y="footballwebpages.teams",all.x=TRUE,suffixes=c("1","2"))
    fac.res$team1[fac.res$soccerbase.team1!="" & is.na(fac.res$soccerbase.team1)==FALSE] <- fac.res$soccerbase.team1[fac.res$soccerbase.team1!="" & is.na(fac.res$soccerbase.team1)==FALSE]
    fac.res$team2[fac.res$soccerbase.team2!="" & is.na(fac.res$soccerbase.team2)==FALSE] <- fac.res$soccerbase.team2[fac.res$soccerbase.team2!="" & is.na(fac.res$soccerbase.team2)==FALSE]
    fac.res$team1_id[fac.res$soccerbase.team1!="" & is.na(fac.res$soccerbase.team1)==FALSE] <- fac.res$soccerbase.id1[fac.res$soccerbase.team1!="" & is.na(fac.res$soccerbase.team1)==FALSE]
    fac.res$team2_id[fac.res$soccerbase.team2!="" & is.na(fac.res$soccerbase.team2)==FALSE] <- fac.res$soccerbase.id2[fac.res$soccerbase.team2!="" & is.na(fac.res$soccerbase.team2)==FALSE]
    # fac.res$team1_id <- fac.res$soccerbase.id1
    # fac.res$team2_id <- fac.res$soccerbase.id2
    fac.return <- fac.res[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  } else if(teamnames=="11v11") {
    corsf <- corr[,c("X11v11","footballwebpages.teams")]
    corsf <- corsf[duplicated(corsf)==FALSE,]
    corsf <- corsf[corsf$footballwebpages.teams!="",]
    corsf <- corsf[duplicated(corsf$footballwebpages.teams)==FALSE,]
    fac.res <- merge(fac.res,corsf,by.x="team1",by.y="footballwebpages.teams",all.x=TRUE)
    fac.res <- merge(fac.res,corsf,by.x="team2",by.y="footballwebpages.teams",all.x=TRUE,suffixes=c("1","2"))
    fac.res$team1[fac.res$X11v111!="" & is.na(fac.res$X11v111)==FALSE] <- fac.res$X11v111[fac.res$X11v111!="" & is.na(fac.res$X11v111)==FALSE]
    fac.res$team2[fac.res$X11v112!="" & is.na(fac.res$X11v112)==FALSE] <- fac.res$X11v112[fac.res$X11v112!="" & is.na(fac.res$X11v112)==FALSE]
    fac.res$team1_id <- NA
    fac.res$team2_id <- NA
    # fac.res$soccerbase.team1 <- fac.res$team1
    # fac.res$soccerbase.team2 <- fac.res$team2
    # fac.res$team1 <- fac.res$X11v111
    # fac.res$team2 <- fac.res$X11v112
    # fac.res$team1_id <- NA
    # fac.res$team2_id <- NA
    fac.return <- fac.res[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  }
  
  return(fac.return)
  # return(fac.res[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
}

loading.old.sl.results <- function(teamnames="soccerbase") {
  #teamnames can be soccerbase and 11v11 for now. if neither specified, soccerbase defaults
  if(teamnames!="soccerbase" & teamnames!="11v11") { teamnames="soccerbase" }
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  cors <- corr[,c("soccerbase.team","soccerbase.id","X11v11","location1","location2")]
  cors <- cors[duplicated(cors)==FALSE,]
  cors <- cors[cors$soccerbase.team!="",]
  cors$soccerbase.team <- iconv(cors$soccerbase.team, from = "ISO-8859-1", to = "UTF-8")
  cors$soccerbase.team <- gsub(" [(].*?[)]","",cors$soccerbase.team)
  cors <- cors[duplicated(cors$soccerbase.team)==FALSE,]
  
  ##### Southern League #####
  sl.res <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/Southern-League-results.csv", stringsAsFactors = FALSE)
  
  sl.res$date[sl.res$date=="S33"] <- "S22"
  sl.res$date[sl.res$date=="S32"] <- "S22"
  sl.res$date[sl.res$date=="N31"] <- "N30"
  sl.res$date[sl.res$date=="L29"] <- "O29"
  sl.res$date <- gsub("[(]","O",sl.res$date)
  
  sl.res$date.month <- tolower(substr(sl.res$date,1,1))
  
  sl.res$date.day <- substring(sl.res$date,2)
  
  sl.res$date.year <- substr(sl.res$season,1,4)
  sl.res$date.year[sl.res$date.month %in% c("j","f","m","a")] <- substring(sl.res$season,6)[sl.res$date.month %in% c("j","f","m","a")]
  
  sl.res$date.month.full[sl.res$date.month=="a"] <- "Apr"
  sl.res$date.month.full[sl.res$date.month=="d"] <- "Dec"
  sl.res$date.month.full[sl.res$date.month=="f"] <- "Feb"
  sl.res$date.month.full[sl.res$date.month=="j"] <- "Jan"
  sl.res$date.month.full[sl.res$date.month=="m"] <- "Mar"
  sl.res$date.month.full[sl.res$date.month=="n"] <- "Nov"
  sl.res$date.month.full[sl.res$date.month=="o"] <- "Oct"
  sl.res$date.month.full[sl.res$date.month=="s"] <- "Sep"
  
  sl.res$date2 <- as.Date(paste(sl.res$date.year,sl.res$date.month.full,sl.res$date.day,sep="-"),
                          format="%Y-%b-%d")
  sl.res$date.dow <- format(sl.res$date2,"%u")
  
  ##playoff matches have different date format
  colnames(sl.res)[1] <- "old.season"
  sl.res$season <- as.numeric(substr(sl.res$old.season,1,4))
  
  sl.res$division <- paste0("Southern League ",sl.res$Division)
  sl.res$division[is.na(sl.res$date2)] <- "Southern League Division One Play Offs"
  
  sl.res$date2[is.na(sl.res$date2)==TRUE & regexpr("[.]",sl.res$date)>-1] <- as.Date(paste0(sl.res$date[is.na(sl.res$date2)==TRUE & regexpr("[.]",sl.res$date)>-1],".",sl.res$season[is.na(sl.res$date2)==TRUE & regexpr("[.]",sl.res$date)>-1]+1),
                                                                                     "%d.%m.%Y")
  sl.res$date2[is.na(sl.res$date2)==TRUE & regexpr("-",sl.res$date)>-1] <- as.Date(paste0(sl.res$date[is.na(sl.res$date2)==TRUE & regexpr("-",sl.res$date)>-1],"-",sl.res$season[is.na(sl.res$date2)==TRUE & regexpr("-",sl.res$date)>-1]+1),
                                                                                   "%d-%b-%Y")
  
  colnames(sl.res)[2] <- "old.date"
  colnames(sl.res)[16] <- "date"

  sl.res0 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/Southern-League-Results-Attendances.csv", stringsAsFactors = FALSE)
  
  #sl.res0$date <- gsub("[(]","O",sl.res0$date)
  
  sl.res0$date.month <- tolower(substr(sl.res0$date,1,1))
  
  sl.res0$date.day <- substring(sl.res0$date,2)
  
  sl.res0$date.year <- as.numeric(substr(sl.res0$season,1,4))
  sl.res0$date.year[sl.res0$date.month %in% c("j","f","m","a")] <- sl.res0$date.year[sl.res0$date.month %in% c("j","f","m","a")]+1
  
  sl.res0$date.month.full[sl.res0$date.month=="a"] <- "Apr"
  sl.res0$date.month.full[sl.res0$date.month=="d"] <- "Dec"
  sl.res0$date.month.full[sl.res0$date.month=="f"] <- "Feb"
  sl.res0$date.month.full[sl.res0$date.month=="j"] <- "Jan"
  sl.res0$date.month.full[sl.res0$date.month=="m"] <- "Mar"
  sl.res0$date.month.full[sl.res0$date.month=="n"] <- "Nov"
  sl.res0$date.month.full[sl.res0$date.month=="o"] <- "Oct"
  sl.res0$date.month.full[sl.res0$date.month=="s"] <- "Sep"
  
  sl.res0$date2 <- as.Date(paste(sl.res0$date.year,sl.res0$date.month.full,sl.res0$date.day,sep="-"),
                          format="%Y-%b-%d")
  sl.res0$date.dow <- format(sl.res0$date2,"%u")
  
  ##playoff matches have different date format
  colnames(sl.res0)[1] <- "old.season"
  sl.res0$season <- as.numeric(substr(sl.res0$old.season,1,4))
  
  sl.res0$division <- paste0("Southern League ",sl.res0$Division)
  sl.res0$division[is.na(sl.res0$date2)] <- "Southern League Division One Play Offs"
  
  sl.res0$date2[is.na(sl.res0$date2)==TRUE & regexpr("[.]",sl.res0$date)>-1] <- as.Date(paste0(sl.res0$date[is.na(sl.res0$date2)==TRUE & regexpr("[.]",sl.res0$date)>-1],".",sl.res0$season[is.na(sl.res0$date2)==TRUE & regexpr("[.]",sl.res0$date)>-1]+1),
                                                                                     "%d.%m.%Y")
  sl.res0$date2[is.na(sl.res0$date2)==TRUE & regexpr("-",sl.res0$date)>-1] <- as.Date(paste0(sl.res0$date[is.na(sl.res0$date2)==TRUE & regexpr("-",sl.res0$date)>-1],"-",sl.res0$season[is.na(sl.res0$date2)==TRUE & regexpr("-",sl.res0$date)>-1]+1),
                                                                                   "%d-%b-%Y")
  
  colnames(sl.res0)[2] <- "old.date"
  colnames(sl.res0)[10] <- "date"

  ##turn team names into Soccerbase ones to then use that in correspondence file to get back other team names
  sl.res$team1 <- gsub("^\\s+|\\s+$","",sl.res$team1)
  sl.res$team2 <- gsub("^\\s+|\\s+$","",sl.res$team2)
  sl.res$team1 <- gsub("P[.]A[.]","PA",sl.res$team1)
  sl.res$team2 <- gsub("P[.]A[.]","PA",sl.res$team2)
  sl.res$team1[regexpr("Aberdare",sl.res$team1)>-1] <- "Aberdare Ath."
  sl.res$team2[regexpr("Aberdare",sl.res$team2)>-1] <- "Aberdare Ath."
  sl.res$team1[regexpr("Brighton",sl.res$team1)>-1] <- "Brighton"
  sl.res$team2[regexpr("Brighton",sl.res$team2)>-1] <- "Brighton"
  sl.res$team1[regexpr("Bristol C",sl.res$team1)>-1] <- "Bristol C"
  sl.res$team2[regexpr("Bristol C",sl.res$team2)>-1] <- "Bristol C"
  sl.res$team2[regexpr("Bristol R",sl.res$team2)>-1] <- "Bristol R"
  sl.res$team1[regexpr("Bristol R",sl.res$team1)>-1] <- "Bristol R"
  sl.res$team1[regexpr("Cardiff",sl.res$team1)>-1] <- "Cardiff"
  sl.res$team2[regexpr("Cardiff",sl.res$team2)>-1] <- "Cardiff"
  sl.res$team1[regexpr("Clapton",sl.res$team1)>-1] <- "Clapton"
  sl.res$team2[regexpr("Clapton",sl.res$team2)>-1] <- "Clapton"
  sl.res$team1[regexpr("Coventry",sl.res$team1)>-1] <- "Coventry"
  sl.res$team2[regexpr("Coventry",sl.res$team2)>-1] <- "Coventry"
  sl.res$team1[regexpr("Croydon",sl.res$team1)>-1] <- "Croydon"
  sl.res$team2[regexpr("Croydon",sl.res$team2)>-1] <- "Croydon"
  sl.res$team1[regexpr("Palace",sl.res$team1)>-1] <- "C Palace"
  sl.res$team2[regexpr("Palace",sl.res$team2)>-1] <- "C Palace"
  sl.res$team1[regexpr("Exeter",sl.res$team1)>-1] <- "Exeter"
  sl.res$team2[regexpr("Exeter",sl.res$team2)>-1] <- "Exeter"
  sl.res$team1[regexpr("Luton",sl.res$team1)>-1] <- "Luton"
  sl.res$team2[regexpr("Luton",sl.res$team2)>-1] <- "Luton"
  sl.res$team1[regexpr("Millwall",sl.res$team1)>-1] <- "Millwall"
  sl.res$team2[regexpr("Millwall",sl.res$team2)>-1] <- "Millwall"
  sl.res$team1[regexpr("New Brompton",sl.res$team1)>-1] <- "Gillingham"
  sl.res$team2[regexpr("New Brompton",sl.res$team2)>-1] <- "Gillingham"
  sl.res$team1[regexpr("Norwich",sl.res$team1)>-1] <- "Norwich"
  sl.res$team2[regexpr("Norwich",sl.res$team2)>-1] <- "Norwich"
  sl.res$team1[regexpr("Thames",sl.res$team1)>-1] <- "Thames"
  sl.res$team2[regexpr("Thames",sl.res$team2)>-1] <- "Thames"
  sl.res$team1[regexpr("Swansea",sl.res$team1)>-1] <- "Swansea"
  sl.res$team2[regexpr("Swansea",sl.res$team2)>-1] <- "Swansea"
  sl.res$team1[regexpr("Swindon",sl.res$team1)>-1] <- "Swindon"
  sl.res$team2[regexpr("Swindon",sl.res$team2)>-1] <- "Swindon"
  sl.res$team1[regexpr("Southend",sl.res$team1)>-1] <- "Southend"
  sl.res$team2[regexpr("Southend",sl.res$team2)>-1] <- "Southend"
  sl.res$team1[regexpr("Sheppey",sl.res$team1)>-1] <- "Sheppey"
  sl.res$team2[regexpr("Sheppey",sl.res$team2)>-1] <- "Sheppey"
  sl.res$team1[regexpr("Tottenham",sl.res$team1)>-1] <- "Tottenham"
  sl.res$team2[regexpr("Tottenham",sl.res$team2)>-1] <- "Tottenham"
  sl.res$team1[regexpr("Kettering",sl.res$team1)>-1] <- "Kettering"
  sl.res$team2[regexpr("Kettering",sl.res$team2)>-1] <- "Kettering"
  sl.res$team1[regexpr("Newport Co",sl.res$team1)>-1] <- "Newport Co"
  sl.res$team2[regexpr("Newport Co",sl.res$team2)>-1] <- "Newport Co"
  sl.res$team1[regexpr("Wellingborough",sl.res$team1)>-1] <- "Wellingboro T"
  sl.res$team2[regexpr("Wellingborough",sl.res$team2)>-1] <- "Wellingboro T"
  sl.res$team1[regexpr("Merthyr",sl.res$team1)>-1] <- "Merthyr T"
  sl.res$team2[regexpr("Merthyr",sl.res$team2)>-1] <- "Merthyr T"
  sl.res$team1[regexpr("Grav",sl.res$team1)>-1] <- "Gravesend"
  sl.res$team2[regexpr("Grav",sl.res$team2)>-1] <- "Gravesend"
  sl.res$team1[regexpr("R[.]A[.] [(]Portsmouth[)]",sl.res$team1)>-1] <- "Royal Artillery"
  sl.res$team2[regexpr("R[.]A[.] [(]Portsmouth[)]",sl.res$team2)>-1] <- "Royal Artillery"
  sl.res$team1[regexpr("Salisbury",sl.res$team1)>-1] <- "Salisbury"
  sl.res$team2[regexpr("Salisbury",sl.res$team2)>-1] <- "Salisbury"
  sl.res$team1[regexpr("Treharris",sl.res$team1)>-1] <- "Treharris"
  sl.res$team2[regexpr("Treharris",sl.res$team2)>-1] <- "Treharris"
  sl.res$team1[regexpr("West Ham",sl.res$team1)>-1] <- "West Ham"
  sl.res$team2[regexpr("West Ham",sl.res$team2)>-1] <- "West Ham"
  
  sl.res0$team1 <- gsub("^\\s+|\\s+$","",sl.res0$team1)
  sl.res0$team2 <- gsub("^\\s+|\\s+$","",sl.res0$team2)
  sl.res0$team1 <- gsub("P[.]A[.]","PA",sl.res0$team1)
  sl.res0$team2 <- gsub("P[.]A[.]","PA",sl.res0$team2)
  sl.res0$team1[regexpr("Aberdare",sl.res0$team1)>-1] <- "Aberdare Ath."
  sl.res0$team2[regexpr("Aberdare",sl.res0$team2)>-1] <- "Aberdare Ath."
  sl.res0$team1[regexpr("Brighton",sl.res0$team1)>-1] <- "Brighton"
  sl.res0$team2[regexpr("Brighton",sl.res0$team2)>-1] <- "Brighton"
  sl.res0$team1[regexpr("Bristol C",sl.res0$team1)>-1] <- "Bristol C"
  sl.res0$team2[regexpr("Bristol C",sl.res0$team2)>-1] <- "Bristol C"
  sl.res0$team2[regexpr("Bristol R",sl.res0$team2)>-1] <- "Bristol R"
  sl.res0$team1[regexpr("Bristol R",sl.res0$team1)>-1] <- "Bristol R"
  sl.res0$team1[regexpr("Cardiff",sl.res0$team1)>-1] <- "Cardiff"
  sl.res0$team2[regexpr("Cardiff",sl.res0$team2)>-1] <- "Cardiff"
  sl.res0$team1[regexpr("Clapton",sl.res0$team1)>-1] <- "Clapton"
  sl.res0$team2[regexpr("Clapton",sl.res0$team2)>-1] <- "Clapton"
  sl.res0$team1[regexpr("Leyton",sl.res0$team1)>-1] <- "Clapton"
  sl.res0$team2[regexpr("Leyton",sl.res0$team2)>-1] <- "Clapton"
  sl.res0$team1[regexpr("Coventry",sl.res0$team1)>-1] <- "Coventry"
  sl.res0$team2[regexpr("Coventry",sl.res0$team2)>-1] <- "Coventry"
  sl.res0$team1[regexpr("Croydon",sl.res0$team1)>-1] <- "Croydon"
  sl.res0$team2[regexpr("Croydon",sl.res0$team2)>-1] <- "Croydon"
  sl.res0$team1[regexpr("Crystal",sl.res0$team1)>-1] <- "C Palace"
  sl.res0$team2[regexpr("Crystal",sl.res0$team2)>-1] <- "C Palace"
  sl.res0$team1[regexpr("Palace",sl.res0$team1)>-1] <- "C Palace"
  sl.res0$team2[regexpr("Palace",sl.res0$team2)>-1] <- "C Palace"
  sl.res0$team1[regexpr("Exeter",sl.res0$team1)>-1] <- "Exeter"
  sl.res0$team2[regexpr("Exeter",sl.res0$team2)>-1] <- "Exeter"
  sl.res0$team1[regexpr("Luton",sl.res0$team1)>-1] <- "Luton"
  sl.res0$team2[regexpr("Luton",sl.res0$team2)>-1] <- "Luton"
  sl.res0$team1[regexpr("Millwall",sl.res0$team1)>-1] <- "Millwall"
  sl.res0$team2[regexpr("Millwall",sl.res0$team2)>-1] <- "Millwall"
  sl.res0$team1[regexpr("New Brompton",sl.res0$team1)>-1] <- "Gillingham"
  sl.res0$team2[regexpr("New Brompton",sl.res0$team2)>-1] <- "Gillingham"
  sl.res0$team1[regexpr("Norwich",sl.res0$team1)>-1] <- "Norwich"
  sl.res0$team2[regexpr("Norwich",sl.res0$team2)>-1] <- "Norwich"
  sl.res0$team1[regexpr("Thames",sl.res0$team1)>-1] <- "Thames"
  sl.res0$team2[regexpr("Thames",sl.res0$team2)>-1] <- "Thames"
  sl.res0$team1[regexpr("Plymouth",sl.res0$team1)>-1] <- "Plymouth"
  sl.res0$team2[regexpr("Plymouth",sl.res0$team2)>-1] <- "Plymouth"
  sl.res0$team1[regexpr("Queen's Park Rangers",sl.res0$team1)>-1] <- "QPR"
  sl.res0$team2[regexpr("Queen's Park Rangers",sl.res0$team2)>-1] <- "QPR"
  sl.res0$team1[regexpr("Swansea",sl.res0$team1)>-1] <- "Swansea"
  sl.res0$team2[regexpr("Swansea",sl.res0$team2)>-1] <- "Swansea"
  sl.res0$team1[regexpr("Swindon",sl.res0$team1)>-1] <- "Swindon"
  sl.res0$team2[regexpr("Swindon",sl.res0$team2)>-1] <- "Swindon"
  sl.res0$team1[regexpr("Southend",sl.res0$team1)>-1] <- "Southend"
  sl.res0$team2[regexpr("Southend",sl.res0$team2)>-1] <- "Southend"
  sl.res0$team1[regexpr("Sheppey",sl.res0$team1)>-1] <- "Sheppey"
  sl.res0$team2[regexpr("Sheppey",sl.res0$team2)>-1] <- "Sheppey"
  sl.res0$team1[regexpr("Tottenham",sl.res0$team1)>-1] <- "Tottenham"
  sl.res0$team2[regexpr("Tottenham",sl.res0$team2)>-1] <- "Tottenham"
  sl.res0$team1[regexpr("Kettering",sl.res0$team1)>-1] <- "Kettering"
  sl.res0$team2[regexpr("Kettering",sl.res0$team2)>-1] <- "Kettering"
  sl.res0$team1[regexpr("Newport Co",sl.res0$team1)>-1] <- "Newport Co"
  sl.res0$team2[regexpr("Newport Co",sl.res0$team2)>-1] <- "Newport Co"
  sl.res0$team1[regexpr("Wellingborough",sl.res0$team1)>-1] <- "Wellingboro T"
  sl.res0$team2[regexpr("Wellingborough",sl.res0$team2)>-1] <- "Wellingboro T"
  sl.res0$team1[regexpr("Merthyr",sl.res0$team1)>-1] <- "Merthyr T"
  sl.res0$team2[regexpr("Merthyr",sl.res0$team2)>-1] <- "Merthyr T"
  sl.res0$team1[regexpr("Grav",sl.res0$team1)>-1] <- "Gravesend"
  sl.res0$team2[regexpr("Grav",sl.res0$team2)>-1] <- "Gravesend"
  sl.res0$team1[regexpr("R[.]A[.] [(]Portsmouth[)]",sl.res0$team1)>-1] <- "Royal Artillery"
  sl.res0$team2[regexpr("R[.]A[.] [(]Portsmouth[)]",sl.res0$team2)>-1] <- "Royal Artillery"
  sl.res0$team1[regexpr("Salisbury",sl.res0$team1)>-1] <- "Salisbury"
  sl.res0$team2[regexpr("Salisbury",sl.res0$team2)>-1] <- "Salisbury"
  sl.res0$team1[regexpr("Treharris",sl.res0$team1)>-1] <- "Treharris"
  sl.res0$team2[regexpr("Treharris",sl.res0$team2)>-1] <- "Treharris"
  sl.res0$team1[regexpr("West Ham",sl.res0$team1)>-1] <- "West Ham"
  sl.res0$team2[regexpr("West Ham",sl.res0$team2)>-1] <- "West Ham"
  
  sl.res1 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/SL-1906-1909.csv",stringsAsFactors = FALSE)
  sl.res1$X <- NULL

  sl.res <- merge(sl.res,rbind(sl.res0[,c("date","team1","team2","attendance")],
                               sl.res1[,c("date","team1","team2","attendance")]),
                  by=c("date","team1","team2"),all.x=TRUE)

  ##there are still some attendances (200-300) that aren't merging based on dates in sl.res0. NEEDS FIXING
  
  
  sl.res <- merge(sl.res,cors,by.x=c("team1"),by.y=c("soccerbase.team"),all.x=TRUE)
  sl.res <- merge(sl.res,cors,by.x=c("team2"),by.y=c("soccerbase.team"),suffixes=c("1","2"),all.x=TRUE)
  sl.res$div_id[sl.res$division=="Southern League Division One"] <- 75
  sl.res$div_id[sl.res$division=="Southern League Division One Play Offs"] <- 7501
  sl.res$div_id[sl.res$division=="Southern League Division Two"] <- 7500
  sl.res$match_id <- paste0("old.sl",1:NROW(sl.res))
  #sl.res$attendance <- NA
  
  if(teamnames=="soccerbase") {
    sl.res$team1_id <- sl.res$soccerbase.id1
    sl.res$team2_id <- sl.res$soccerbase.id2
    sl.return <- sl.res[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  } else if(teamnames=="11v11") {
    sl.res$soccerbase.team1 <- sl.res$team1
    sl.res$soccerbase.team2 <- sl.res$team2
    sl.res$team1 <- sl.res$X11v111
    sl.res$team2 <- sl.res$X11v112
    sl.res$team1_id <- NA
    sl.res$team2_id <- NA
    sl.return <- sl.res[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  }
  
  return(sl.return)
}

loading.historical.results <- function() {
  require(tools)
  
  ##### Correspondence File #####
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  corff <- corr[,c("football.data","footballwebpages.teams")]
  corff <- corff[duplicated(corff)==FALSE,]
  corff <- corff[corff$football.data!="",]
  corff <- corff[duplicated(corff$football.data)==FALSE,]
  corsf <- corr[,c("soccerbase.team","footballwebpages.teams")]
  corsf <- corsf[duplicated(corsf)==FALSE,]
  corsf <- corsf[corsf$soccerbase.team!="",]
  corsf <- corsf[duplicated(corsf$soccerbase.team)==FALSE,]
  corfs <- corr[,c("footballwebpages.teams","soccerbase.team")]
  corfs <- corfs[duplicated(corfs)==FALSE,]
  corfs <- corfs[corfs$footballwebpages.teams!="",]
  corfs <- corfs[duplicated(corfs$footballwebpages.teams)==FALSE,]
  corwf <- corr[,c("worldfootball.net.team","footballwebpages.teams")]
  corwf <- corwf[duplicated(corwf)==FALSE,]
  corwf <- corwf[corwf$worldfootball.net.team!="",]
  corwf <- corwf[duplicated(corwf$worldfootball.net.team)==FALSE,]
  cor11f <- corr[,c("X11v11","footballwebpages.teams")]
  cor11f <- cor11f[duplicated(cor11f)==FALSE,]
  cor11f <- cor11f[cor11f$X11v11!="",]
  cor11f <- cor11f[duplicated(cor11f$X11v11)==FALSE,]
  
  
  
  ##### READING FC #####
  rdg <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/rfc-oldest-results.csv",stringsAsFactors = FALSE)
  
  rdg$date <- as.Date(rdg$Date,"%d/%m/%Y")
  rdg <- rdg[order(rdg$date),]
  rdg <- rdg[duplicated(rdg$date)==FALSE,]
  rdg$match_id <- paste0("rdg",1:NROW(rdg))
  rdg$season <- as.numeric(format(rdg$date,"%Y")) - as.numeric(as.numeric(format(rdg$date,"%m"))<7)
  rdg$Opponents <- gsub("^\\s+|\\s+$","",rdg$Opponents)
  rdg$Opponents <- gsub("[(]|[)]","",rdg$Opponents)
  rdg$Opponents[regexpr("Millwall",rdg$Opponents)>-1] <- "Millwall"
  rdg$Opponents[regexpr("Preston",rdg$Opponents)>-1] <- "Preston"
  rdg$Opponents[regexpr("Royal Engineers",rdg$Opponents)>-1] <- "Royal Engineers"
  rdg$Opponents[regexpr("Swindon",rdg$Opponents)>-1] <- "Swindon"
  rdg$Opponents[regexpr("Port Vale",rdg$Opponents)>-1] <- "Port Vale"
  rdg$Opponents[regexpr("Southampton",rdg$Opponents)>-1] <- "Southampton"
  rdg$Opponents[regexpr("St Mark",rdg$Opponents)>-1] <- "St Marks Windsor"
  rdg$Opponents[regexpr("Windsor H",rdg$Opponents)>-1] <- "St Marks Windsor"
  rdg$Opponents[regexpr("Wolverton",rdg$Opponents)>-1] <- "Wolverton"
  rdg$Opponents[regexpr("Arsenal",rdg$Opponents)>-1] <- "Arsenal"
  rdg$Opponents[regexpr("Tottenham",rdg$Opponents)>-1] <- "Tottenham"
  rdg$Opponents[regexpr("Queens Park",rdg$Opponents)>-1] <- "QPR"
  rdg$Opponents[regexpr("Newbury",rdg$Opponents)>-1] <- "Newburys"
  rdg$Opponents[regexpr("Magdalen",rdg$Opponents)>-1] <- "Magdalen Oxford"
  rdg$Opponents[regexpr("Hertford C",rdg$Opponents)>-1] <- "Hertford Oxford"
  rdg$Opponents[regexpr("Bradfield C",rdg$Opponents)>-1] <- "Bradfield Coll"
  rdg$V[regexpr("^H",rdg$V)>-1] <- "H"
  rdg$V[rdg$V=="Cav"] <- "N"
  rdg$V[regexpr("\\?",rdg$V)>-1] <- "H"
  rdg$team1[rdg$V=="H"] <- "Reading"
  rdg$team2[rdg$V=="A"] <- "Reading"
  rdg$team1[rdg$V=="A"] <- rdg$Opponents[rdg$V=="A"]
  rdg$team2[rdg$V=="H"] <- rdg$Opponents[rdg$V=="H"]
  rdg$F <- gsub("\\?","",rdg$F)
  rdg$A <- gsub("\\?","",rdg$A)
  rdg$goals1[rdg$V=="A"] <- as.numeric(rdg$A[rdg$V=="A"])
  rdg$goals1[rdg$V=="H"] <- as.numeric(rdg$F[rdg$V=="H"])
  rdg$goals2[rdg$V=="A"] <- as.numeric(rdg$F[rdg$V=="A"])
  rdg$goals2[rdg$V=="H"] <- as.numeric(rdg$A[rdg$V=="H"])
  rdg <- rdg[is.na(rdg$goals1)==FALSE,]
  
  rdg$division[regexpr("BB",rdg$Cmp)>-1] <- "Berks and Bucks Cup"
  rdg$division[regexpr("^F",rdg$Cmp)>-1] <- "Friendly"
  rdg$division[regexpr("^FC",rdg$Cmp)>-1] <- "FA Cup"
  rdg$division[regexpr("^FA",rdg$Cmp)>-1] <- "FA Amateur Cup"
  rdg$division[regexpr("^S",rdg$Cmp)>-1] <- "Southern League"
  
  ##### LUTON Town #####
  ltn <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/luton-town-results.csv",stringsAsFactors = FALSE)
  ltn$date <- as.Date(ltn$Date,"%d/%m/%Y")
  ltn <- ltn[order(ltn$date),]
  ltn$match_id <- paste0("ltn",1:NROW(ltn))
  ltn$season <- as.numeric(format(ltn$date,"%Y")) - as.numeric(as.numeric(format(ltn$date,"%m"))<7)
  ltn$team1[ltn$team1==""] <- "Luton"
  ltn$team2[ltn$team2==""] <- "Luton"
  ltn$team1[regexpr("Arsenal",ltn$team1)>-1] <- "Arsenal"
  ltn$team2[regexpr("Arsenal",ltn$team2)>-1] <- "Arsenal"
  ltn$team1[regexpr("Derby",ltn$team1)>-1] <- "Derby"
  ltn$team2[regexpr("Derby",ltn$team2)>-1] <- "Derby"
  ltn$team1[regexpr("Tottenham",ltn$team1)>-1] <- "Tottenham"
  ltn$team2[regexpr("Tottenham",ltn$team2)>-1] <- "Tottenham"
  ltn$team1[regexpr("Wolverhampton",ltn$team1)>-1] <- "Wolves"
  ltn$team2[regexpr("Wolverhampton",ltn$team2)>-1] <- "Wolves"
  ltn$team1[regexpr("West Brom",ltn$team1)>-1] <- "West Brom"
  ltn$team2[regexpr("West Brom",ltn$team2)>-1] <- "West Brom"
  ltn$team1[regexpr("Millwall",ltn$team1)>-1] <- "Millwall"
  ltn$team2[regexpr("Millwall",ltn$team2)>-1] <- "Millwall"
  ltn$team1[regexpr("Forest$",ltn$team1)>-1] <- "Nottm Forest"
  ltn$team2[regexpr("Forest$",ltn$team2)>-1] <- "Nottm Forest"
  ltn$team1[regexpr("Notts County",ltn$team1)>-1] <- "Notts Co"
  ltn$team2[regexpr("Notts County",ltn$team1)>-1] <- "Notts Co"
  ltn$team1[regexpr("Watford",ltn$team1)>-1] <- "Watford"
  ltn$team2[regexpr("Watford",ltn$team1)>-1] <- "Watford"
  ltn$team1[regexpr("Wolverton",ltn$team1)>-1] <- "Wolverton"
  ltn$team2[regexpr("Wolverton",ltn$team1)>-1] <- "Wolverton"
  ltn$team1[regexpr("Unity",ltn$team1)>-1] <- "Unity"
  ltn$team2[regexpr("Unity",ltn$team1)>-1] <- "Unity"
  ltn$outcome <- gsub("^(\\w+) (\\d+) (\\d+)$","\\1",ltn$Result)
  ltn$goals1 <- as.numeric(gsub("^(\\w+) (\\d+) (\\d+)$","\\2",ltn$Result))
  ltn$goals2 <- as.numeric(gsub("^(\\w+) (\\d+) (\\d+)$","\\3",ltn$Result))
  ltn$division <- "Friendly"
  ltn$division[regexpr("^FA",ltn$Comp)>-1] <- "FA Cup"
  ltn$division[regexpr("^KCC",ltn$Comp)>-1] <- "Kettering Charity Cup"
  ltn$division[regexpr("^LCC",ltn$Comp)>-1] <- "Luton Charity Cup"
  
  ##### Sheffield Wednesday #####
  wed <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/sheff-wed.csv",stringsAsFactors = FALSE)
  wed <- wed[wed$date!="",]
  wed$date[wed$date=="31-Nov"] <- "30-Nov"
  wed$year <- na.locf(wed$year)
  wed$day <- as.numeric(gsub("^.*?(\\d{2}).*?$",'\\1',wed$date))
  wed$month <- gsub("^.*?(\\w{3}).*?$",'\\1',wed$date)
  wed$date <- as.Date(paste0(wed$day,wed$month,wed$year),"%d%b%Y")
  wed$season <- as.numeric(format(wed$date,"%Y")) - as.numeric(as.numeric(format(wed$date,"%m"))<7)
  
  wed$home <- as.numeric(wed$venue %in% c("h","Hunters Bar","Hunter's Bar","Myrtle Road","Myrtle Rpad","Highfield","Bramall Lane","Bramall Lane(West)","Olive Grove","Sheaf Hosue","Sheaf House"))
  
  wed$opponents[regexpr("Preston",wed$opponents)>-1] <- "Preston"
  wed$opponents[regexpr("Walsall",wed$opponents)>-1] <- "Walsall"
  wed$opponents[regexpr("Millwall",wed$opponents)>-1] <- "Millwall"
  wed$opponents[regexpr("Ironopolis",wed$opponents)>-1] <- "Middlsbro Irn"
  wed$opponents[regexpr("Hotspur",wed$opponents)>-1] <- "Tottenham"
  wed$opponents[regexpr("coln City",wed$opponents)>-1] <- "Lincoln"
  wed$opponents[regexpr("Grimsby T",wed$opponents)>-1] <- "Grimsby"
  wed$opponents[regexpr("Derby Co",wed$opponents)>-1] <- "Derby"
  wed$opponents[regexpr("Crewe",wed$opponents)>-1] <- "Crewe"
  wed$opponents[regexpr("Port Vale",wed$opponents)>-1] <- "Port Vale"
  wed$opponents[regexpr("Bolton",wed$opponents)>-1] <- "Bolton"
  wed$opponents[regexpr("Blackburn R",wed$opponents)>-1] <- "Blackburn"
  wed$opponents[regexpr("George",wed$opponents)>-1] <- "B'ham St George's"
  wed$opponents[tolower(wed$opponents)=="sheffield united"] <- "Sheff Utd"
  wed$opponents[tolower(wed$opponents)=="notts forest"] <- "Nottm Forest"
  wed$opponents[tolower(wed$opponents)=="notts county"] <- "Notts Co"
  wed$opponents[tolower(wed$opponents)=="middlesborough"] <- "Middlesbrough"
  wed$opponents[tolower(wed$opponents)=="darwin"] <- "Darwen"
  
  wed$score[wed$score=="W"] <- "1-0"
  wed$score <- gsub("[?]","",gsub("Jan","1",gsub("Feb","2",gsub("Mar","3",gsub("Apr","4",gsub("May","5",gsub("Jun","6",gsub("Jul","7",gsub("Aug","8",gsub("Sep","9",wed$score))))))))))
  wed$team1[wed$home==1] <- "Sheff Wed"
  wed$team2[wed$home==0] <- "Sheff Wed"
  wed$team2[wed$home==1] <- wed$opponents[wed$home==1]
  wed$team1[wed$home==0] <- wed$opponents[wed$home==0]
  wed$goals1[wed$home==1] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\1",wed$score[wed$home==1]))
  wed$goals2[wed$home==1] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\2",wed$score[wed$home==1]))
  wed$goals2[wed$home==0] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\1",wed$score[wed$home==0]))
  wed$goals1[wed$home==0] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\2",wed$score[wed$home==0]))
  wed$division <- wed$X.1
  wed$division[wed$division==""] <- "Friendlies"
  wed$match_id <- paste0("wed",1:NROW(wed))
  
  ##### Macclesfield Town #####
  mac <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/old-Macclesfield-Town.csv",stringsAsFactors = FALSE)
  mac <- mac[mac$date!="",]
  mac$year <- as.numeric(mac$year)
  mac$year <- na.locf(mac$year)
  mac$date0 <- gsub("\\s+","-",gsub("\\t","-",mac$date))
  mac$date <- as.Date(paste0(mac$year,mac$date0),"%Y%b-%d")
  mac <- mac[is.na(mac$date)==FALSE,]
  mac$opposition <- gsub("othHILL","OTHHILL",gsub("'","",mac$opposition))
  mac$home <- as.numeric(grepl("^[[:upper:]]+\\s*[[:upper:]]*\\s*[[:upper:]]*$", mac$opposition))
  mac$goals1[mac$home==1] <- gsub("^(\\d+)-(\\d+) (\\w+)","\\1",mac$result[mac$home==1])
  mac$goals2[mac$home==1] <- gsub("^(\\d+)-(\\d+) (\\w+)","\\2",mac$result[mac$home==1])
  mac$goals1[mac$home==0] <- gsub("^(\\d+)-(\\d+) (\\w+)","\\2",mac$result[mac$home==0])
  mac$goals2[mac$home==0] <- gsub("^(\\d+)-(\\d+) (\\w+)","\\1",mac$result[mac$home==0])
  
  mac$opposition <- gsub("^\\s+|\\s+$","",gsub("^(.*?)[(].*?[)](.*?)$","\\1\\2",gsub("^(.*?)\\s[a-z](\\s.*?)$","\\1",gsub("^(.*?)\\s[a-z]$","\\1",toTitleCase(tolower(mac$opposition))))))
  mac$opposition <- gsub("Hazle","Hazel",mac$opposition)
  mac$opposition <- gsub(" Alexander| Alexandra","",mac$opposition)
  mac$opposition[mac$opposition=="Manchester Ne"] <- "Manchester North End"
  mac$opposition[mac$opposition=="Newton Heath"] <- "Man Utd"
  
  mac$team1[mac$home==1] <- "Macclesfield"
  mac$team2[mac$home==0] <- "Macclesfield"
  mac$team1[mac$home==0] <- mac$opposition[mac$home==0]
  mac$team2[mac$home==1] <- mac$opposition[mac$home==1]
  
  mac$season <- as.numeric(format(mac$date,"%Y")) - as.numeric(as.numeric(format(mac$date,"%m"))<7)
  mac$match_id <- paste0("mac",1:NROW(mac))
  mac$division <- "Friendly"
  
  ##### West Brom #####
  bsc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/birmingham-senior-cup.csv",stringsAsFactors = FALSE)
  bsc$date <- as.Date(bsc$date)
  bsc$season <- as.numeric(format(bsc$date,"%Y")) - as.numeric(as.numeric(format(bsc$date,"%m"))<7)
  bsc$score <- gsub("[?]","",gsub("Jan","1",gsub("Feb","2",gsub("Mar","3",gsub("Apr","4",gsub("May","5",gsub("Jun","6",gsub("Jul","7",gsub("Aug","8",gsub("Sep","9",bsc$score))))))))))
  bsc$goals1 <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\1",bsc$score))
  bsc$goals2 <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\2",bsc$score))
  bsc$division <- "Birmingham Senior Cup"
  bsc$match_id <- paste0("bsc",1:NROW(bsc))
  
  wba <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/west-brom.csv",stringsAsFactors = FALSE)
  wba$date <- as.Date(wba$date,"%d %b %Y")
  wba$season <- as.numeric(format(wba$date,"%Y")) - as.numeric(as.numeric(format(wba$date,"%m"))<7)
  
  wba$opponent[regexpr("Blackburn R",wba$opponent)>-1] <- "Blackburn"
  wba$opponent[regexpr("Bolton",wba$opponent)>-1] <- "Bolton"
  wba$opponent[regexpr("Port Vale",wba$opponent)>-1] <- "Port Vale"
  wba$opponent[regexpr("Derby Co",wba$opponent)>-1] <- "Derby"
  wba$opponent[regexpr("Hednesford",wba$opponent)>-1] <- "Hednesford"
  wba$opponent[regexpr("Lincoln",wba$opponent)>-1] <- "Lincoln"
  wba$opponent[regexpr("Notts Co",wba$opponent)>-1] <- "Notts Co"
  wba$opponent[regexpr("Nottingham",wba$opponent)>-1] <- "Nottm Forest"
  wba$opponent[regexpr("Preston",wba$opponent)>-1] <- "Preston"
  wba$opponent[regexpr("Walsall S|Walsall T",wba$opponent)>-1] <- "Walsall"
  wba$opponent[regexpr("Wolverhampton",wba$opponent)>-1] <- "Wolves"
  wba$opponent[regexpr("Wednesbury Old Athletic",wba$opponent)>-1] <- "Wednesbury O A"
  
  wba$team1[wba$venue=="H" & wba$opponent!=""] <- "West Brom"
  wba$team1[wba$venue=="A" & wba$opponent!=""] <- wba$opponent[wba$venue=="A" & wba$opponent!=""]
  wba$team2[wba$venue=="A" & wba$opponent!=""] <- "West Brom"
  wba$team2[wba$venue=="H" & wba$opponent!=""] <- wba$opponent[wba$venue=="H" & wba$opponent!=""]
  
  wba$score <- gsub("–","-",gsub("[?]","",gsub("Jan","1",gsub("Feb","2",gsub("Mar","3",gsub("Apr","4",gsub("May","5",gsub("Jun","6",gsub("Jul","7",gsub("Aug","8",gsub("Sep","9",wba$score)))))))))))
  
  wba$goals1[wba$venue=="H"] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\1",wba$score[wba$venue=="H"]))
  wba$goals1[wba$venue=="A"] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\2",wba$score[wba$venue=="A"]))
  wba$goals2[wba$venue=="H"] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\2",wba$score[wba$venue=="H"]))
  wba$goals2[wba$venue=="A"] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\1",wba$score[wba$venue=="A"]))
  
  wba$match_id <- paste0("wba",1:NROW(wba))
  
  ##### Nottm Forest #####
  ntf <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/nottm-forest.csv",stringsAsFactors = FALSE)
  ntf$date <- as.Date(ntf$Date,"%a %b %d, %Y")
  ntf$season <- as.numeric(format(ntf$date,"%Y")) - as.numeric(as.numeric(format(ntf$date,"%m"))<7)
  
  ntf$Score <- gsub("[?]","",gsub("Jan","1",gsub("Feb","2",gsub("Mar","3",gsub("Apr","4",gsub("May","5",gsub("Jun","6",gsub("Jul","7",gsub("Aug","8",gsub("Sep","9",ntf$Score))))))))))
  ntf$goals1[ntf$H.A=="H" | ntf$H.A=="N" | ntf$H.A=="?"] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\1",ntf$Score[ntf$H.A=="H" | ntf$H.A=="N" | ntf$H.A=="?"]))
  ntf$goals1[ntf$H.A=="A"] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\2",ntf$Score[ntf$H.A=="A"]))
  ntf$goals2[ntf$H.A=="H" | ntf$H.A=="N" | ntf$H.A=="?"] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\2",ntf$Score[ntf$H.A=="H" | ntf$H.A=="N" | ntf$H.A=="?"]))
  ntf$goals2[ntf$H.A=="A"] <- as.numeric(gsub("^(\\d+)[-](\\d+)$","\\1",ntf$Score[ntf$H.A=="A"]))
  
  ntf$Opponent[regexpr("Arsenal",ntf$Opponent)>-1] <- "Arsenal"
  ntf$Opponent[regexpr("Blackburn R",ntf$Opponent)>-1] <- "Blackburn"
  ntf$Opponent[regexpr("Bolton",ntf$Opponent)>-1] <- "Bolton"
  ntf$Opponent[regexpr("Port Vale",ntf$Opponent)>-1] <- "Port Vale"
  ntf$Opponent[regexpr("Derby Co",ntf$Opponent)>-1] <- "Derby"
  ntf$Opponent[regexpr("Hednesford",ntf$Opponent)>-1] <- "Hednesford"
  ntf$Opponent[regexpr("Lincoln",ntf$Opponent)>-1] <- "Lincoln"
  ntf$Opponent[regexpr("Notts Co",ntf$Opponent)>-1] <- "Notts Co"
  ntf$Opponent[ntf$Opponent=="Notts"] <- "Notts Co"
  ntf$Opponent[regexpr("Preston",ntf$Opponent)>-1] <- "Preston"
  ntf$Opponent[regexpr("Walsall S|Walsall T",ntf$Opponent)>-1] <- "Walsall"
  ntf$Opponent[regexpr("West Brom",ntf$Opponent)>-1] <- "West Brom"
  ntf$Opponent[regexpr("Wolverhampton",ntf$Opponent)>-1] <- "Wolves"
  ntf$Opponent[regexpr("Wednesbury Old Athletic",ntf$Opponent)>-1] <- "Wednesbury O A"
  
  ntf$Opponent[regexpr("Hotspur",ntf$Opponent)>-1] <- "Tottenham"
  ntf$Opponent[regexpr("Stoke",ntf$Opponent)>-1] <- "Stoke"
  ntf$Opponent[regexpr("Swindon",ntf$Opponent)>-1] <- "Swindon"
  ntf$Opponent[regexpr("Wednesday",ntf$Opponent)>-1] <- "Sheff Wed"
  ntf$Opponent[regexpr("Birmingham C",ntf$Opponent)>-1] <- "Birmingham"
  ntf$Opponent[regexpr("Small Heath",ntf$Opponent)>-1] <- "Birmingham"
  ntf$Opponent[regexpr("Sheffield U",ntf$Opponent)>-1] <- "Sheff Utd"
  ntf$Opponent[regexpr("Sheffield H",ntf$Opponent)>-1] <- "Heeley"
  ntf$Opponent[regexpr("Sheffield C",ntf$Opponent)>-1] <- "Sheffield"
  ntf$Opponent[regexpr("Plymouth",ntf$Opponent)>-1] <- "Plymouth"
  ntf$Opponent[regexpr("Newcastle",ntf$Opponent)>-1] <- "Newcastle"
  ntf$Opponent[regexpr("Luton",ntf$Opponent)>-1] <- "Luton"
  ntf$Opponent[regexpr("Ironopolis",ntf$Opponent)>-1] <- "Middlsbro Irn"
  ntf$Opponent[regexpr("Leicester",ntf$Opponent)>-1] <- "Leicester"
  ntf$Opponent[regexpr("Hereford",ntf$Opponent)>-1] <- "Hereford"
  ntf$Opponent[regexpr("Derby Co",ntf$Opponent)>-1] <- "Derby Co"
  ntf$Opponent[regexpr("Coventry",ntf$Opponent)>-1] <- "Coventry"
  ntf$Opponent[regexpr("Crewe",ntf$Opponent)>-1] <- "Crewe"
  ntf$Opponent[regexpr("Chesterfield",ntf$Opponent)>-1] <- "Chesterfield"
  ntf$Opponent[regexpr("Port Vale",ntf$Opponent)>-1] <- "Port Vale"
  
  ntf$team1[ntf$H.A=="H" & ntf$Opponent!=""] <- "Nottm Forest"
  ntf$team1[ntf$H.A=="A" & ntf$Opponent!=""] <- ntf$Opponent[ntf$H.A=="A" & ntf$Opponent!=""]
  ntf$team2[ntf$H.A=="A" & ntf$Opponent!=""] <- "Nottm Forest"
  ntf$team2[ntf$H.A=="H" & ntf$Opponent!=""] <- ntf$Opponent[ntf$H.A=="H" & ntf$Opponent!=""]
  
  ntf$match_id <- paste0("ntf",1:NROW(ntf))
  ntf$division <- ntf$Division
  
  ##OAFC #####
  oafc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/old-OAFC-results.csv",stringsAsFactors = FALSE)
  oafc$date[regexpr("/",oafc$date)>-1] <- gsub("^(\\d+)/(\\d+)/(\\d+)$","\\3-\\2-\\1",oafc$date[regexpr("/",oafc$date)>-1])
  oafc$date <- as.Date(oafc$date)
  oafc$season <- as.numeric(format(oafc$date,"%Y")) - as.numeric(as.numeric(format(oafc$date,"%m"))<7)
  oafc$match_id <- paste0("oafc",1:NROW(oafc))
  
  
  ##### FA CUP QUALIFIERS 1888-1919 #####
  # fac.res0 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/fa-cup-qualifiers.csv",stringsAsFactors = FALSE)
  # fac.res1 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/facup-qualifiers-to-2000.csv",stringsAsFactors = FALSE)
  # fac.res1$team1 <- gsub("<font color=#1B3332><b>","",fac.res1$team1)
  # fac.res1$team2 <- gsub("<font color=#1B3332><b>","",fac.res1$team2)
  # fac.res1$score <- gsub("<td bgcolor=COC26D align=center><font color=#1B3332><b>","",fac.res1$score)
  # fac.res1$date <- as.Date(paste0(substr(fac.res1$date,1,10),gsub("^(\\d{4}).*?$","\\1",fac.res1$filename)),"%a %d.%m.%Y")
  # 
  # fac.res1$team1 <- gsub(" AFC$","",gsub("[(].*?[)]","",gsub("Boro[.]$","Borough",gsub(" '93","",gsub(" Utd[.]$"," United",gsub(" U$"," United",gsub("Ath[.]$","Athletic",gsub("^\\s+|\\s+$","",gsub("  "," ",gsub("  "," ",gsub("\\t"," ",gsub(" [(].*?[)]","",fac.res1$team1))))))))))))
  # fac.res1$team2 <- gsub(" AFC$","",gsub("[(].*?[)]","",gsub("Boro[.]$","Borough",gsub(" '93","",gsub(" Utd[.]$"," United",gsub(" U$"," United",gsub("Ath[.]$","Athletic",gsub("^\\s+|\\s+$","",gsub("  "," ",gsub("  "," ",gsub("\\t"," ",gsub(" [(].*?[)]","",fac.res1$team2))))))))))))
  # #teamtab <- table(c(fac.res1$team1,fac.res1$team2))
  # 
  # fac.res2 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/fa-cup-qualifiers-2000-2022.csv",stringsAsFactors = FALSE)
  # colnames(fac.res2)[c(3:5)] <- c("team1","score","team2")
  # fac.res2$date <- as.Date(paste0(substr(fac.res2$Date,1,6),"20",substr(fac.res2$Date,7,8)),"%d.%m.%Y")
  # fac.res2$team1 <- gsub(" AFC$","",gsub("[(].*?[)]","",gsub("Boro[.]$","Borough",gsub(" '93","",gsub(" Utd[.]$"," United",gsub(" U$"," United",gsub("Ath[.]$","Athletic",gsub("^\\s+|\\s+$","",gsub("  "," ",gsub("  "," ",gsub("\\t"," ",gsub(" [(].*?[)]","",fac.res2$team1))))))))))))
  # fac.res2$team2 <- gsub(" AFC$","",gsub("[(].*?[)]","",gsub("Boro[.]$","Borough",gsub(" '93","",gsub(" Utd[.]$"," United",gsub(" U$"," United",gsub("Ath[.]$","Athletic",gsub("^\\s+|\\s+$","",gsub("  "," ",gsub("  "," ",gsub("\\t"," ",gsub(" [(].*?[)]","",fac.res2$team2))))))))))))
  # 
  # fac.res <- rbind(fac.res1[,c("date","team1","score","team2")],fac.res2[,c("date","team1","score","team2")])
  # 
  # fac.res$team1 <- gsub("^\\s+|\\s+$","",fac.res$team1)
  # fac.res$team2 <- gsub("^\\s+|\\s+$","",fac.res$team2)
  # fac.res$team1 <- gsub(" AFC","",fac.res$team1)
  # fac.res$team2 <- gsub(" AFC","",fac.res$team2)
  # fac.res$team1 <- gsub(" FC","",fac.res$team1)
  # fac.res$team2 <- gsub(" FC","",fac.res$team2)
  # fac.res$team1 <- gsub(" Alexandra","",fac.res$team1)
  # fac.res$team2 <- gsub(" Alexandra","",fac.res$team2)
  # fac.res$team1 <- gsub(" Athletic","",fac.res$team1)
  # fac.res$team2 <- gsub(" Athletic","",fac.res$team2)
  # fac.res$team1 <- gsub(" Celtic","",fac.res$team1)
  # fac.res$team2 <- gsub(" Celtic","",fac.res$team2)
  # fac.res$team1 <- gsub("Sunderland Albion","Sunderland Alb",fac.res$team1)
  # fac.res$team2 <- gsub("Sunderland Albion","Sunderland Alb",fac.res$team2)
  # fac.res$team1 <- gsub(" Town","",fac.res$team1)
  # fac.res$team2 <- gsub(" Town","",fac.res$team2)
  # fac.res$team1 <- gsub(" Rovers","",fac.res$team1)
  # fac.res$team2 <- gsub(" Rovers","",fac.res$team2)
  # fac.res$team1 <- gsub(" Victoria","",fac.res$team1)
  # fac.res$team2 <- gsub(" Victoria","",fac.res$team2)
  # fac.res$team1 <- gsub(" Wanderers","",fac.res$team1)
  # fac.res$team2 <- gsub(" Wanderers","",fac.res$team2)
  # fac.res$team1 <- gsub(" County"," Co",fac.res$team1)
  # fac.res$team2 <- gsub(" County"," Co",fac.res$team2)
  # fac.res$team1 <- gsub(" Fosse","",fac.res$team1)
  # fac.res$team2 <- gsub(" Fosse","",fac.res$team2)
  # fac.res$team1 <- gsub(" Park Avenue","PA",fac.res$team1)
  # fac.res$team2 <- gsub(" Park Avenue","PA",fac.res$team2)
  # fac.res$team1[regexpr("Arsenal",fac.res$team1)>-1] <- "Arsenal"
  # fac.res$team2[regexpr("Arsenal",fac.res$team2)>-1] <- "Arsenal"
  # fac.res$team1[regexpr("Brighton",fac.res$team1)>-1 & regexpr("Hove",fac.res$team1)>-1 & regexpr("Albion",fac.res$team1)>-1] <- "Brighton"
  # fac.res$team2[regexpr("Brighton",fac.res$team2)>-1 & regexpr("Hove",fac.res$team2)>-1 & regexpr("Albion",fac.res$team2)>-1] <- "Brighton"
  # fac.res$team1[regexpr("Bristol C",fac.res$team1)>-1] <- "Bristol C"
  # fac.res$team2[regexpr("Bristol C",fac.res$team2)>-1] <- "Bristol C"
  # fac.res$team2[regexpr("Bristol R",fac.res$team2)>-1] <- "Bristol R"
  # fac.res$team1[regexpr("Bristol R",fac.res$team1)>-1] <- "Bristol R"
  # fac.res$team1[regexpr("Cardiff Ci",fac.res$team1)>-1] <- "Cardiff"
  # fac.res$team2[regexpr("Cardiff Ci",fac.res$team2)>-1] <- "Cardiff"
  # fac.res$team1[regexpr("Cambridge U",fac.res$team1)>-1] <- "Cambridge"
  # fac.res$team2[regexpr("Cambridge U",fac.res$team2)>-1] <- "Cambridge"
  # fac.res$team1[regexpr("Carlisle C|U",fac.res$team1)>-1] <- "Carlisle"
  # fac.res$team2[regexpr("Carlisle C|U",fac.res$team2)>-1] <- "Carlisle"
  # fac.res$team1[regexpr("Clapton",fac.res$team1)>-1] <- "Clapton"
  # fac.res$team2[regexpr("Clapton",fac.res$team2)>-1] <- "Clapton"
  # fac.res$team1[regexpr("Coventry",fac.res$team1)>-1] <- "Coventry"
  # fac.res$team2[regexpr("Coventry",fac.res$team2)>-1] <- "Coventry"
  # fac.res$team1[regexpr("Croydon",fac.res$team1)>-1] <- "Croydon"
  # fac.res$team2[regexpr("Croydon",fac.res$team2)>-1] <- "Croydon"
  # fac.res$team1[regexpr("Palace",fac.res$team1)>-1] <- "C Palace"
  # fac.res$team2[regexpr("Palace",fac.res$team2)>-1] <- "C Palace"
  # fac.res$team1[regexpr("Exeter",fac.res$team1)>-1] <- "Exeter"
  # fac.res$team2[regexpr("Exeter",fac.res$team2)>-1] <- "Exeter"
  # fac.res$team1[regexpr("Hartlepools U",fac.res$team1)>-1] <- "Hartlepool"
  # fac.res$team2[regexpr("Hartlepools U",fac.res$team2)>-1] <- "Hartlepool"
  # fac.res$team1[regexpr("Hull C",fac.res$team1)>-1] <- "Hull"
  # fac.res$team2[regexpr("Hull C",fac.res$team2)>-1] <- "Hull"
  # fac.res$team1[regexpr("Leeds U",fac.res$team1)>-1] <- "Leeds"
  # fac.res$team2[regexpr("Leeds U",fac.res$team2)>-1] <- "Leeds"
  # fac.res$team1[regexpr("Luton",fac.res$team1)>-1] <- "Luton"
  # fac.res$team2[regexpr("Luton",fac.res$team2)>-1] <- "Luton"
  # fac.res$team1[regexpr("Millwall",fac.res$team1)>-1] <- "Millwall"
  # fac.res$team2[regexpr("Millwall",fac.res$team2)>-1] <- "Millwall"
  # fac.res$team1[regexpr("Milwall",fac.res$team1)>-1] <- "Millwall"
  # fac.res$team2[regexpr("Milwall",fac.res$team2)>-1] <- "Millwall"
  # fac.res$team1[regexpr("New Brompton",fac.res$team1)>-1] <- "Gillingham"
  # fac.res$team2[regexpr("New Brompton",fac.res$team2)>-1] <- "Gillingham"
  # fac.res$team1[regexpr("Newcastle U",fac.res$team1)>-1] <- "Newcastle"
  # fac.res$team2[regexpr("Newcastle U",fac.res$team2)>-1] <- "Newcastle"
  # fac.res$team1[regexpr("Ardwick",fac.res$team1)>-1] <- "Man City"
  # fac.res$team2[regexpr("Ardwick",fac.res$team2)>-1] <- "Man City"
  # fac.res$team1[regexpr("Manchester United",fac.res$team1)>-1] <- "Man Utd"
  # fac.res$team2[regexpr("Manchester United",fac.res$team2)>-1] <- "Man Utd"
  # fac.res$team1[regexpr("Newton Heath$",fac.res$team1)>-1] <- "Man Utd"
  # fac.res$team2[regexpr("Newton Heath$",fac.res$team2)>-1] <- "Man Utd"
  # fac.res$team1[regexpr("Norwich C",fac.res$team1)>-1] <- "Norwich"
  # fac.res$team2[regexpr("Norwich C",fac.res$team2)>-1] <- "Norwich"
  # fac.res$team1[regexpr("Thames",fac.res$team1)>-1] <- "Thames"
  # fac.res$team2[regexpr("Thames",fac.res$team2)>-1] <- "Thames"
  # fac.res$team1[regexpr("Southend U",fac.res$team1)>-1] <- "Southend"
  # fac.res$team2[regexpr("Southend U",fac.res$team2)>-1] <- "Southend"
  # fac.res$team1[regexpr("Stoke",fac.res$team1)>-1] <- "Stoke"
  # fac.res$team2[regexpr("Stoke",fac.res$team2)>-1] <- "Stoke"
  # fac.res$team1[regexpr("Stockport",fac.res$team1)>-1] <- "Stockport"
  # fac.res$team2[regexpr("Stockport",fac.res$team2)>-1] <- "Stockport"
  # fac.res$team1[regexpr("Scunthorpe",fac.res$team1)>-1] <- "Scunthorpe"
  # fac.res$team2[regexpr("Scunthorpe",fac.res$team2)>-1] <- "Scunthorpe"
  # fac.res$team1[regexpr("Middlesbrough",fac.res$team1)>-1] <- "Middlesbro"
  # fac.res$team2[regexpr("Middlesbrough",fac.res$team2)>-1] <- "Middlesbro"
  # fac.res$team1[regexpr("Ironopolis",fac.res$team1)>-1] <- "Middlsbro Irn"
  # fac.res$team2[regexpr("Ironopolis",fac.res$team2)>-1] <- "Middlsbro Irn"
  # fac.res$team1[regexpr("Lincoln City",fac.res$team1)>-1] <- "Lincoln"
  # fac.res$team2[regexpr("Lincoln City",fac.res$team2)>-1] <- "Lincoln"
  # fac.res$team1[regexpr("Wednesbury O",fac.res$team1)>-1] <- "Wednesbury O"
  # fac.res$team2[regexpr("Wednesbury O",fac.res$team2)>-1] <- "Wednesbury O"
  # fac.res$team1[regexpr("West Ham U",fac.res$team1)>-1] <- "West Ham"
  # fac.res$team2[regexpr("West Ham U",fac.res$team2)>-1] <- "West Ham"
  # 
  # fac.res$division <- "FA Cup Qualifiers"
  # fac.res$pts1 <- NA
  # fac.res$pts2 <- NA
  # fac.res$pos1 <- NA
  # fac.res$pos2 <- NA
  # 
  # fac.res$date <- as.Date(fac.res$date,"%d.%m.%Y")
  # fac.res$season <- as.numeric(format(fac.res$date,"%Y"))
  # fac.res$X <- NULL
  # fac.res$X.1 <- NULL
  # fac.res$X.2 <- NULL
  # fac.res$goals1 <- as.numeric(fac.res$goals1)
  # fac.res$goals2 <- as.numeric(fac.res$goals2)
  # fac.res <- fac.res[is.na(fac.res$goals1)==FALSE,]

  fac.res <- load.facup.results()
    
  ##### Annie's files #####
  a86 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/annie-1886-87.csv",stringsAsFactors = FALSE)
  a86 <- a86[is.na(a86$H.Goals)==FALSE,]
  a86$match_id <- paste0("a86",1:NROW(a86))
  a86$season <- "1886" 
  colnames(a86)[c(1,9,2,4,5,3)] <- c("date0","division","team1","goals1","goals2","team2")
  a86$date0 <- gsub("^\\s+|\\s+$","",toTitleCase(tolower(gsub("st|th|rd|nd","",a86$date0))))
  a86$date.dow <- gsub("^(\\w+)\\s*(\\d*)\\s*(\\w+)\\s*(\\d*)$","\\1",a86$date0)
  a86$date.month <- gsub("^(\\w+)\\s*(\\d*)\\s*(\\w+)\\s*(\\d*)$","\\3",a86$date0)
  a86$date.date <- gsub("^(\\w+)\\s*(\\d*)\\s*(\\w+)\\s*(\\d*)$","\\2\\4",a86$date0)
  a86$date.year <- as.numeric(a86$season)-as.numeric(regexpr("Aug|Sep|Oct|Nov|Dec",a86$date0)>-1)
  a86$date <- as.Date(paste0(a86$date.year,"-",a86$date.month,"-",a86$date.date),"%Y-%b-%d")
  a86$attendance <- as.numeric(gsub("^.*?(\\d+).*?$","\\1",gsub(",","",a86$Attendance)))

  a87 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/annie-1887-88.csv",stringsAsFactors = FALSE)
  a87 <- a87[is.na(a87$H.goals)==FALSE,]
  a87$match_id <- paste0("a87",1:NROW(a87))
  a87$season <- "1887" 
  colnames(a87)[c(1,9,2,4,5,3)] <- c("date0","division","team1","goals1","goals2","team2")
  a87$date0 <- gsub("^\\s+|\\s+$","",toTitleCase(tolower(gsub("st|th|rd|nd","",a87$date0))))
  a87$date.dow <- gsub("^(\\w+)\\s*(\\d*)\\s*(\\w+)\\s*(\\d*)$","\\1",a87$date0)
  a87$date.month <- gsub("^(\\w+)\\s*(\\d*)\\s*(\\w+)\\s*(\\d*)$","\\3",a87$date0)
  a87$date.date <- gsub("^(\\w+)\\s*(\\d*)\\s*(\\w+)\\s*(\\d*)$","\\2\\4",a87$date0)
  a87$date.year <- as.numeric(a87$season)-as.numeric(regexpr("Aug|Sep|Oct|Nov|Dec",a87$date0)>-1)
  a87$date <- as.Date(paste0(a87$date.year,"-",a87$date.month,"-",a87$date.date),"%Y-%b-%d")
  a87$attendance <- as.numeric(gsub("^.*?(\\d+).*?$","\\1",gsub(",","",a87$Attendance)))
  
  sl.res$attendance <- NA
  fac.res$attendance <- NA
  rdg$attendance <- rdg$Att
  ltn$attendance <- NA
  mac$attendance <- NA
  ntf$attendance <- ntf$Att
  oafc$attendance <- NA
  
  ##merging all results together
  sl.res$match_id <- paste0("sl",1:NROW(sl.res))
  fac.res$match_id <- paste0("facup",1:NROW(fac.res))
  all.res <- rbind(sl.res[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   fac.res[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   rdg[rdg$division!="FA Cup" & rdg$division!="Southern League",
                       c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   ltn[ltn$division!="FA Cup",
                       c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   wba[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   wed[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   bsc[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   mac[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   ntf[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   oafc[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   a86[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")],
                   a87[,c("match_id","date","division","team1","goals1","goals2","team2","season","attendance")])
  
  all.res <- all.res[is.na(all.res$date)==FALSE,]
  all.res <- all.res[order(all.res$date,all.res$division),]
  all.res <- all.res[duplicated(all.res[,c("date","team1","goals1","goals2","team2")])==FALSE,]
  
  all.res$date[all.res$match_id=="sl4756"] <- as.Date("1911-09-13")
  
  return(all.res)  
}

load.nlm.results <- function() {
  nlm <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/nonleaguematters-data.csv",
                  stringsAsFactors = FALSE)
  nlm$date <- as.Date(gsub("^(\\w+) (\\d+)\\w+ (\\w+) (\\d+)$","\\4-\\3-\\2",nlm$date0),"%Y-%B-%d")
  nlm$div.id <- as.numeric(gsub("^divisions-(\\d+)-(\\d+)-[.]html$","\\1",nlm$file))
  nlm$season.id <- as.numeric(gsub("^divisions-(\\d+)-(\\d+)-[.]html$","\\2",nlm$file))
  
  nlmteams <- c(nlm$team1,nlm$team2)
  nlmteams <- nlmteams[duplicated(nlmteams)==FALSE]
  write.csv(nlmteams[order(nlmteams)],"/Users/jjreade/Dropbox/Research/Sport/Attendance/data/nonleaguematters-teams.csv")
  
  return(nlm[duplicated(nlm[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
               c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
}

# nlm.loc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/nlm-locations.csv",stringsAsFactors = FALSE)
# nlm.teams <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/nonleaguematters-teams.csv",stringsAsFactors = FALSE)
# nlm.teams <- merge(nlm.teams,nlm.loc,by.x='x',by.y='Club',all=TRUE)
# write.csv(nlm.teams,"/Users/jjreade/Dropbox/Research/Sport/Attendance/data/nonleaguematters-teams-with-locs.csv")

load.nl.results <- function(teamnames="soccerbase") {
  #teamnames can be soccerbase and 11v11 for now. if neither specified, soccerbase defaults
  if(teamnames!="soccerbase" & teamnames!="11v11") { teamnames="soccerbase" }
  #need correspondence for team names to soccerbase (or 11v11) names
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  if(teamnames=="soccerbase") {
    corfs <- corr[,c("soccerbase.team","soccerbase.id","thefootballarchive")]
    corfs <- corfs[duplicated(corfs)==FALSE,]
    corfs <- corfs[corfs$thefootballarchive!="",]
    corfs <- corfs[duplicated(corfs$thefootballarchive)==FALSE,]
  } else if(teamnames=="11v11") {
    corfs <- corr[,c("X11v11","thefootballarchive")]
    corfs <- corfs[duplicated(corfs)==FALSE,]
    corfs <- corfs[corfs$thefootballarchive!="",]
    corfs <- corfs[duplicated(corfs$thefootballarchive)==FALSE,]
  }
  
  nlres <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/NL/NL-results.csv",stringsAsFactors = FALSE)
  colnames(nlres)[5:6] <- c("team2","attendance")
  # nlres$team1 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub("[.]","",gsub("\t","",nlres$team1))))))
  # nlres$team2 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub("[.]","",gsub("\t","",nlres$team2))))))
  nlres$season <- nlres$date
  nlres$date <- as.Date(nlres$date,"%d/%m/%Y")
  nlres$season[is.na(nlres$date)==FALSE] <- as.numeric(format(nlres$date[is.na(nlres$date)==FALSE],"%Y")) + as.numeric(as.numeric(format(nlres$date[is.na(nlres$date)==FALSE],"%m"))>7)

  nlres$team1 <- gsub("\t","",nlres$team1)
  nlres$team2 <- gsub("\t","",nlres$team2)
  #nlteams <- unique(c(nlres$team1,nlres$team2))
  nlres <- merge(nlres,corfs,by.x=c("team1"),by.y=c("thefootballarchive"))
  nlres <- merge(nlres,corfs,by.x=c("team2"),by.y=c("thefootballarchive"),suffixes=c("1","2"))
  
  if(teamnames=="soccerbase") {
    nlres$team1_id <- nlres$soccerbase.id1
    nlres$team2_id <- nlres$soccerbase.id2
    nlres$team1 <- nlres$soccerbase.team1
    nlres$team2 <- nlres$soccerbase.team2
    # nl.return <- nlres[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  } else if(teamnames=="11v11") {
    # nlres$soccerbase.team1 <- nlres$team1
    # nlres$soccerbase.team2 <- nlres$team2
    nlres$team1[nlres$X11v111!=""] <- nlres$X11v111[nlres$X11v111!=""]
    nlres$team2[nlres$X11v112!=""] <- nlres$X11v112[nlres$X11v112!=""]
    nlres$team1_id <- NA
    nlres$team2_id <- NA
    # nl.return <- nlres[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  }
  #  write.csv(nlteams[order(nlteams)],"/Users/jjreade/Dropbox/Research/Sport/Attendance/data/thefootballarchives-national-league.csv")
  
  nlres2 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/NLresults-1979-1988.csv",stringsAsFactors = FALSE)
  nlres2$date <- nlres2$team1
  nlres2$date <- na.locf(as.Date(gsub("1st","1",gsub("2nd","2",gsub("3rd","3",gsub("(\\d+)th","\\1",nlres2$date)))),"%a %d %B %Y"))
  nlres2 <- nlres2[nlres2$team2!="",]
  # nlres2$team1 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",nlres2$team1)))))))
  # nlres2$team2 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",nlres2$team2)))))))
  # nlres2$team1[regexpr("Gravesend",nlres2$team1)>-1] <- "Gravesend"
  # nlres2$team2[regexpr("Gravesend",nlres2$team2)>-1] <- "Gravesend"
  nlres2$team1[regexpr("Trowbridge",nlres2$team1)>-1] <- "Trowbridge T"
  nlres2$team2[regexpr("Trowbridge",nlres2$team2)>-1] <- "Trowbridge T"
  nlres2$team1[regexpr("Redditch",nlres2$team1)>-1] <- "Redditch U"
  nlres2$team2[regexpr("Redditch",nlres2$team2)>-1] <- "Redditch U"
  nlres2$team1[regexpr("Telford",nlres2$team1)>-1] <- "Telford U"
  nlres2$team2[regexpr("Telford",nlres2$team2)>-1] <- "Telford U"
  nlres2$team1[regexpr("Macclesfield",nlres2$team1)>-1] <- "Macclesfield"
  nlres2$team2[regexpr("Macclesfield",nlres2$team2)>-1] <- "Macclesfield"
  nlres2$season <- as.numeric(format(nlres2$date,"%Y")) - as.numeric(as.numeric(format(nlres2$date,"%m"))<7)
  
  nlres2 <- nlres2[nlres2$score!="A-A" & nlres2$score!="P-P",]

  # nlres2$team1 <- gsub("\t","",nlres2$team1)
  # nlres2$team2 <- gsub("\t","",nlres2$team2)
  #nlteams <- unique(c(nlres2$team1,nlres2$team2))
  nlres2 <- merge(nlres2,corfs,by.x=c("team1"),by.y=c("thefootballarchive"),all.x=TRUE)
  nlres2 <- merge(nlres2,corfs,by.x=c("team2"),by.y=c("thefootballarchive"),suffixes=c("1","2"),all.x=TRUE)
  if(teamnames=="soccerbase") {
    nlres2$team1_id <- nlres2$soccerbase.id1
    nlres2$team2_id <- nlres2$soccerbase.id2
    nlres2$team1 <- nlres2$soccerbase.team1
    nlres2$team2 <- nlres2$soccerbase.team2
    # nl2.return <- nlres2[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  } else if(teamnames=="11v11") {
    # nlres2$soccerbase.team1 <- nlres2$team1
    # nlres2$soccerbase.team2 <- nlres2$team2
    nlres2$team1 <- nlres2$X11v111
    nlres2$team2 <- nlres2$X11v112
    nlres2$team1_id <- NA
    nlres2$team2_id <- NA
    # nl2.return <- nlres2[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  }
  
  
  nlres <- merge(nlres,nlres2[,c("season","team1","team1_id",
                                 "team2","team2_id","date","attendance")],
                 by=c("season","team1","team2"),all.x=TRUE,
                 suffixes=c("",".2"))
  
  nlres3 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/NLresults-1989-1997.csv",stringsAsFactors = FALSE)
  nlres3$date <- nlres3$team1
  nlres3$date <- na.locf(as.Date(gsub("1st","1",gsub("2nd","2",gsub("3rd","3",gsub("(\\d+)th","\\1",nlres3$date)))),"%a %d %B %Y"))
  nlres3 <- nlres3[nlres3$team2!="",]
  # nlres3$team1 <- gsub("Dagenham","Dag &amp; Red",gsub(" and","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",nlres3$team1)))))))
  # nlres3$team2 <- gsub("Dagenham","Dag &amp; Red",gsub(" and","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",nlres3$team2)))))))
  # nlres3$team1[regexpr("Gravesend",nlres3$team1)>-1] <- "Gravesend"
  # nlres3$team2[regexpr("Gravesend",nlres3$team2)>-1] <- "Gravesend"
  nlres3$season <- as.numeric(format(nlres3$date,"%Y")) - as.numeric(as.numeric(format(nlres3$date,"%m"))<7)
  nlres3$team1 <- gsub("\t","",nlres3$team1)
  nlres3$team2 <- gsub("\t","",nlres3$team2)
  nlres3$team1[regexpr("Telford",nlres3$team1)>-1] <- "Telford U"
  nlres3$team2[regexpr("Telford",nlres3$team2)>-1] <- "Telford U"
  nlres3$team1[regexpr("Macclesfield",nlres3$team1)>-1] <- "Macclesfield"
  nlres3$team2[regexpr("Macclesfield",nlres3$team2)>-1] <- "Macclesfield"
  #nlteams <- unique(c(nlres3$team1,nlres3$team2))
  nlres3 <- nlres3[nlres3$score!="A-A" & nlres3$score!="P-P",]
  nlres3 <- merge(nlres3,corfs,by.x=c("team1"),by.y=c("thefootballarchive"))
  nlres3 <- merge(nlres3,corfs,by.x=c("team2"),by.y=c("thefootballarchive"),suffixes=c("1","2"))
  
  if(teamnames=="soccerbase") {
    nlres3$team1 <- nlres3$soccerbase.team1
    nlres3$team2 <- nlres3$soccerbase.team2
    nlres3$team1_id <- nlres3$soccerbase.id1
    nlres3$team2_id <- nlres3$soccerbase.id2
    # nl3.return <- nlres3[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  } else if(teamnames=="11v11") {
    # nlres3$soccerbase.team1 <- nlres3$team1
    # nlres3$soccerbase.team2 <- nlres3$team2
    nlres3$team1 <- nlres3$X11v111
    nlres3$team2 <- nlres3$X11v112
    nlres3$team1_id <- NA
    nlres3$team2_id <- NA
    # nl3.return <- nlres3[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")]
  }

  nlres <- merge(nlres,nlres3[,c("season","team1","team1_id",
                                 "team2","team2_id","date","attendance")],
                 by=c("season","team1","team2"),all.x=TRUE,
                 suffixes=c("",".3"))
  nlres19981999 <- nlres3[nlres3$season>1997,]
  
  nlres$date[is.na(nlres$date)==TRUE & is.na(nlres$date.2)==FALSE] <- nlres$date.2[is.na(nlres$date)==TRUE & is.na(nlres$date.2)==FALSE]
  nlres$date[is.na(nlres$date)==TRUE & is.na(nlres$date.3)==FALSE] <- nlres$date.3[is.na(nlres$date)==TRUE & is.na(nlres$date.3)==FALSE]
  nlres$attendance[is.na(nlres$attendance) & is.na(nlres$attendance.2)==FALSE] <- nlres$attendance.2[is.na(nlres$attendance) & is.na(nlres$attendance.2)==FALSE]
  nlres$attendance[is.na(nlres$attendance) & is.na(nlres$attendance.3)==FALSE] <- nlres$attendance.3[is.na(nlres$attendance) & is.na(nlres$attendance.3)==FALSE]
  nlres$team1_id[is.na(nlres$team1_id) & is.na(nlres$team1_id.2)==FALSE] <- nlres$team1_id.2[is.na(nlres$team1_id) & is.na(nlres$team1_id.2)==FALSE]
  nlres$team1_id[is.na(nlres$team1_id) & is.na(nlres$team1_id.3)==FALSE] <- nlres$team1_id.3[is.na(nlres$team1_id) & is.na(nlres$team1_id.3)==FALSE]
  nlres$team2_id[is.na(nlres$team2_id) & is.na(nlres$team2_id.2)==FALSE] <- nlres$team2_id.2[is.na(nlres$team2_id) & is.na(nlres$team2_id.2)==FALSE]
  nlres$team2_id[is.na(nlres$team2_id) & is.na(nlres$team2_id.3)==FALSE] <- nlres$team2_id.3[is.na(nlres$team2_id) & is.na(nlres$team2_id.3)==FALSE]
  
  nlres$division <- "National League"
  nlres$div_id <- 9
  # nlres$team1_id <- nlres$soccerbase.id1
  # nlres$team2_id <- nlres$soccerbase.id2
  # nlres$team1 <- nlres$soccerbase.team1
  # nlres$team2 <- nlres$soccerbase.team2
  nlres$match_id <- paste0("nl",1:NROW(nlres))

  nlres19981999$match_id <- paste0("nl",c(NROW(nlres)+1):c(NROW(nlres)+NROW(nlres19981999)))
  nlres19981999$division <- "National League"
  nlres19981999$div_id <- 9
  nlres19981999$goals1 <- gsub("^(\\d+)-(\\d+)$","\\1",nlres19981999$score)
  nlres19981999$goals2 <- gsub("^(\\d+)-(\\d+)$","\\2",nlres19981999$score)
  
  # nlres$season <- as.numeric(nlres$season)-1
  nlres$season <- as.numeric(format(nlres$date,"%Y")) - as.numeric(as.numeric(format(nlres$date,"%m"))<7)
  nlres19981999$season <- as.numeric(format(nlres19981999$date,"%Y")) - as.numeric(as.numeric(format(nlres19981999$date,"%m"))<7)
  
  nl.return <- rbind(nlres[duplicated(nlres[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
                           c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
                     nlres19981999[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
  return(nl.return)
}

load.npl.results <- function(teamnames="soccerbase") {
  #teamnames can be soccerbase and 11v11 for now. if neither specified, soccerbase defaults
  if(teamnames!="soccerbase" & teamnames!="11v11") { teamnames="soccerbase" }
  #need correspondence for team names to soccerbase (or 11v11) names
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  if(teamnames=="soccerbase") {
    corfs <- corr[,c("soccerbase.team","soccerbase.id","thefootballarchive")]
    corfs <- corfs[duplicated(corfs)==FALSE,]
    corfs <- corfs[corfs$thefootballarchive!="",]
    corfs <- corfs[duplicated(corfs$thefootballarchive)==FALSE,]
  } else if(teamnames=="11v11") {
    corfs <- corr[,c("X11v11","thefootballarchive")]
    corfs <- corfs[duplicated(corfs)==FALSE,]
    corfs <- corfs[corfs$thefootballarchive!="",]
    corfs <- corfs[duplicated(corfs$thefootballarchive)==FALSE,]
  }
  
  #load data
  nplres <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/Northern-Counties-League.csv",stringsAsFactors = FALSE)

  nplres$date <- nplres$team1
  nplres$date <- na.locf(as.Date(gsub("1st","1",gsub("2nd","2",gsub("3rd","3",gsub("(\\d+)th","\\1",nplres$date)))),"%a %d %B %Y"))
  nplres <- nplres[nplres$team2!="",]

  # nplteams <- unique(c(nplres$team1,nplres$team2))
  # 
  # write.csv(nplteams[order(nplteams)],"/Users/jjreade/Dropbox/Research/Sport/Attendance/data/thefootballarchives-northern.csv")
  
  # nplres$team1 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",nplres$team1)))))))
  # nplres$team2 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",nplres$team2)))))))
  # nplres$team1[regexpr("Gravesend",nplres$team1)>-1] <- "Gravesend"
  # nplres$team2[regexpr("Gravesend",nplres$team2)>-1] <- "Gravesend"
  nplres$team1 <- gsub("\t","",nplres$team1)
  nplres$team2 <- gsub("\t","",nplres$team2)
  nplres$team1[regexpr("Macclesfield",nplres$team1)>-1] <- "Macclesfield"
  nplres$team2[regexpr("Macclesfield",nplres$team2)>-1] <- "Macclesfield"
  nplres <- merge(nplres,corfs,by.x=c("team1"),by.y=c("thefootballarchive"))
  nplres <- merge(nplres,corfs,by.x=c("team2"),by.y=c("thefootballarchive"),suffixes=c("1","2"))
  
  nplres$goals1 <- gsub("^(\\d+)-(\\d+)$","\\1",nplres$score)
  nplres$goals2 <- gsub("^(\\d+)-(\\d+)$","\\2",nplres$score)
  
  nplres$season[is.na(nplres$date)==FALSE] <- as.numeric(format(nplres$date[is.na(nplres$date)==FALSE],"%Y")) - as.numeric(as.numeric(format(nplres$date[is.na(nplres$date)==FALSE],"%m"))<7)

  nplres$division <- "Northern Premier League"
  nplres$div_id <- 56
  if(teamnames=="soccerbase") {
    nplres$soccerbase.team1[nplres$team1=="Kirkby Town (1963)"] <- "Kirkby Town"
    nplres$soccerbase.team2[nplres$team2=="Kirkby Town (1963)"] <- "Kirkby Town"
    nplres$team1_id <- nplres$soccerbase.id1
    nplres$team2_id <- nplres$soccerbase.id2
    nplres$team1 <- nplres$soccerbase.team1
    nplres$team2 <- nplres$soccerbase.team2
  } else if (teamnames=="11v11") {
    nplres$X11v111[nplres$team1=="Kirkby Town (1963)"] <- "Kirkby Town"
    nplres$X11v112[nplres$team2=="Kirkby Town (1963)"] <- "Kirkby Town"
    nplres$X11v111[nplres$team1=="Ellesmere Port Town (1946)"] <- "Ellesmere Port Town"
    nplres$X11v112[nplres$team2=="Ellesmere Port Town (1946)"] <- "Ellesmere Port Town"
    #quick fix for now - assume that footballarchive team names are 11v11 ones
    nplres$X11v111[nplres$X11v111==""] <- nplres$team1[nplres$X11v111==""]
    nplres$X11v112[nplres$X11v112==""] <- nplres$team2[nplres$X11v112==""]
    nplres$team1_id <- NA
    nplres$team2_id <- NA
    nplres$team1 <- nplres$X11v111
    nplres$team2 <- nplres$X11v112
  }
  nplres$match_id <- paste0("npl",1:NROW(nplres))
  nplres <- nplres[nplres$score!="A-A" & nplres$score!="P-P",]
  
  return(nplres[duplicated(nplres[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
               c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
  
}

load.il.results <- function(teamnames="soccerbase") {
  #teamnames can be soccerbase and 11v11 for now. if neither specified, soccerbase defaults
  if(teamnames!="soccerbase" & teamnames!="11v11") { teamnames="soccerbase" }
  #need correspondence for team names to soccerbase (or 11v11) names
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  if(teamnames=="soccerbase") {
    corfs <- corr[,c("soccerbase.team","soccerbase.id","thefootballarchive")]
    corfs <- corfs[duplicated(corfs)==FALSE,]
    corfs <- corfs[corfs$thefootballarchive!="",]
    corfs <- corfs[duplicated(corfs$thefootballarchive)==FALSE,]
  } else if(teamnames=="11v11") {
    corfs <- corr[,c("X11v11","thefootballarchive")]
    corfs <- corfs[duplicated(corfs)==FALSE,]
    corfs <- corfs[corfs$thefootballarchive!="",]
    corfs <- corfs[duplicated(corfs$thefootballarchive)==FALSE,]
  }
  
  #load data
  ilres <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/Isthmian-Premier-League.csv",stringsAsFactors = FALSE)
  
  ilres$date <- ilres$team1
  ilres$date <- na.locf(as.Date(gsub("1st","1",gsub("2nd","2",gsub("3rd","3",gsub("(\\d+)th","\\1",ilres$date)))),"%a %d %B %Y"))
  ilres <- ilres[ilres$team2!="",]
  
  #ilteams <- unique(c(ilres$team1,ilres$team2))
  
  #write.csv(ilteams[order(ilteams)],"/Users/jjreade/Dropbox/Research/Sport/Attendance/data/thefootballarchives-isthmian.csv")
  
  ##need to load up correspondence with soccerbase team names
  
  #ilres$team1 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",ilres$team1)))))))
  #ilres$team2 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",ilres$team2)))))))
  #ilres$team1[regexpr("Gravesend",ilres$team1)>-1] <- "Gravesend"
  #ilres$team2[regexpr("Gravesend",ilres$team2)>-1] <- "Gravesend"
  ilres <- merge(ilres,corfs,by.x=c("team1"),by.y=c("thefootballarchive"),all.x=TRUE)
  ilres <- merge(ilres,corfs,by.x=c("team2"),by.y=c("thefootballarchive"),suffixes=c("1","2"),all.x=TRUE)
  
  ilres$season[is.na(ilres$date)==FALSE] <- as.numeric(format(ilres$date[is.na(ilres$date)==FALSE],"%Y")) - as.numeric(as.numeric(format(ilres$date[is.na(ilres$date)==FALSE],"%m"))<7)
  
  ilres$goals1 <- gsub("^(\\d+)-(\\d+)$","\\1",ilres$score)
  ilres$goals2 <- gsub("^(\\d+)-(\\d+)$","\\2",ilres$score)

  ilres$division <- "Isthmian League"
  ilres$div_id <- 80
  if(teamnames=="soccerbase") {
    ilres$soccerbase.team1[ilres$team1=="Purfleet"] <- "Purfleet"
    ilres$soccerbase.team2[ilres$team2=="Purfleet"] <- "Purfleet"
    ilres$team1_id <- ilres$soccerbase.id1
    ilres$team2_id <- ilres$soccerbase.id2
    ilres$team1 <- ilres$soccerbase.team1
    ilres$team2 <- ilres$soccerbase.team2
  } else if(teamnames=="11v11") {
    ilres$X11v111[ilres$team1=="Purfleet"] <- "Purfleet"
    ilres$X11v112[ilres$team2=="Purfleet"] <- "Purfleet"
    #quick fix for now - assume that footballarchive team names are 11v11 ones
    ilres$X11v111[ilres$X11v111==""] <- ilres$team1[ilres$X11v111==""]
    ilres$X11v112[ilres$X11v112==""] <- ilres$team2[ilres$X11v112==""]
    ilres$team1_id <- NA
    ilres$team2_id <- NA
    ilres$team1 <- ilres$X11v111
    ilres$team2 <- ilres$X11v112
  }
  ilres$match_id <- paste0("il",1:NROW(ilres))
  ilres <- ilres[ilres$match_id!="il8347" & ilres$match_id!="il1140",]
  ilres <- ilres[ilres$score!="A-A" & ilres$score!="P-P",]
  
  return(ilres[duplicated(ilres[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
                c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
  
}

load.sl.results <- function(teamnames="soccerbase") {
  #teamnames can be soccerbase and 11v11 for now. if neither specified, soccerbase defaults
  if(teamnames!="soccerbase" & teamnames!="11v11") { teamnames="soccerbase" }
  #need correspondence for team names to soccerbase (or 11v11) names
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  if(teamnames=="soccerbase") {
    corfs <- corr[,c("soccerbase.team","soccerbase.id","thefootballarchive")]
    corfs <- corfs[duplicated(corfs)==FALSE,]
    corfs <- corfs[corfs$thefootballarchive!="",]
    corfs <- corfs[duplicated(corfs$thefootballarchive)==FALSE,]
  } else if(teamnames=="11v11") {
    corfs <- corr[,c("X11v11","thefootballarchive")]
    corfs <- corfs[duplicated(corfs)==FALSE,]
    corfs <- corfs[corfs$thefootballarchive!="",]
    corfs <- corfs[duplicated(corfs$thefootballarchive)==FALSE,]
  }
  
  #load data
  slres <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/slres.csv",stringsAsFactors = FALSE)
  
  slres$date <- slres$team1
  slres$date <- na.locf(as.Date(gsub(";","",gsub("1st","1",gsub("2nd","2",gsub("3rd","3",gsub("(\\d+)th","\\1",slres$date))))),"%a %d %B %Y"))
  slres <- slres[slres$team2!="",]
  
  slres$team1 <- gsub(" [(].*?[)]","",slres$team1)
  slres$team2 <- gsub(" [(].*?[)]","",slres$team2)
  
  # slteams <- unique(c(slres$team1,slres$team2))
  # slteams <- slteams[regexpr(" Reserves",slteams)==-1]
  # 
  # write.csv(slteams[order(slteams)],"/Users/jjreade/Dropbox/Research/Sport/Attendance/data/thefootballarchives-sl.csv")
  
  ##need to load up correspondence with soccerbase team names
  
  #slres$team1 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",slres$team1)))))))
  #slres$team2 <- gsub("Dagenham","Dag &amp; Red",gsub(" &","",gsub(" \\w+$","",gsub("^AP ","",gsub(" [(]\\d{4}[)]$","",gsub("[.]","",gsub("\t","",slres$team2)))))))
  #slres$team1[regexpr("Gravesend",slres$team1)>-1] <- "Gravesend"
  #slres$team2[regexpr("Gravesend",slres$team2)>-1] <- "Gravesend"
  slres <- merge(slres,corfs,by.x=c("team1"),by.y=c("thefootballarchive"),all.x=TRUE)
  slres <- merge(slres,corfs,by.x=c("team2"),by.y=c("thefootballarchive"),suffixes=c("1","2"),all.x=TRUE)
  
  slres$season[is.na(slres$date)==FALSE] <- as.numeric(format(slres$date[is.na(slres$date)==FALSE],"%Y")) - as.numeric(as.numeric(format(slres$date[is.na(slres$date)==FALSE],"%m"))<7)
  
  slres$goals1 <- gsub("^(\\d+)-(\\d+)$","\\1",slres$score)
  slres$goals2 <- gsub("^(\\d+)-(\\d+)$","\\2",slres$score)
  
  slres$division <- "Southern League"
  slres$div_id <- 75
  if(teamnames=="soccerbase") {
    slres$soccerbase.team1[is.na(slres$soccerbase.team1)==TRUE] <- slres$team1[is.na(slres$soccerbase.team1)==TRUE]
    slres$soccerbase.team2[is.na(slres$soccerbase.team2)==TRUE] <- slres$team2[is.na(slres$soccerbase.team2)==TRUE]
    slres$soccerbase.team1[slres$soccerbase.team1==""] <- slres$team1[slres$soccerbase.team1==""]
    slres$soccerbase.team2[slres$soccerbase.team2==""] <- slres$team2[slres$soccerbase.team2==""]
    slres$team1_id <- slres$soccerbase.id1
    slres$team2_id <- slres$soccerbase.id2
    slres$team1 <- slres$soccerbase.team1
    slres$team2 <- slres$soccerbase.team2
  } else if(teamnames=="11v11") {
    slres$X11v111[is.na(slres$X11v111)==TRUE] <- slres$team1[is.na(slres$X11v111)==TRUE]
    slres$X11v112[is.na(slres$X11v112)==TRUE] <- slres$team2[is.na(slres$X11v112)==TRUE]
    slres$X11v111[slres$X11v111==""] <- slres$team1[slres$X11v111==""]
    slres$X11v112[slres$X11v112==""] <- slres$team2[slres$X11v112==""]
    slres$team1_id <- NA
    slres$team2_id <- NA
    slres$team1 <- slres$X11v111
    slres$team2 <- slres$X11v112
  }
  slres$match_id <- paste0("sl",1:NROW(slres))
  slres <- slres[slres$score!="A-A" & slres$score!="P-P",]
  
  return(slres[duplicated(slres[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
               c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
  
}

load.11v11.results <- function() {
  loc <- "/Users/jjreade/Dropbox/Research/Sport/"
  
  #att.all0 <- read.csv(paste0(loc,"Attendance/data/all-english-matches.csv"),stringsAsFactors = FALSE)
  att.all <- read.csv(paste0(loc,"Attendance/data/all-english-matches2.csv"),stringsAsFactors = FALSE)
  
  att.all$date <- as.Date(att.all$date,"%d %B %Y")  ##sort out date
  att.all <- att.all[is.na(att.all$date)==FALSE,]
  
  ##get rid of spaces at start/end of team names
  att.all$team1 <- gsub("^\\s+|\\s+$","",att.all$team1)
  att.all$team2 <- gsub("^\\s+|\\s+$","",att.all$team2)
  att.all$winner <- gsub("^\\s+|\\s+$","",att.all$winner)
  ##sort out name changes
  att.all$team1[att.all$team1=="Stoke"] <- "Stoke City"
  att.all$team2[att.all$team2=="Stoke"] <- "Stoke City"
  att.all$winner[att.all$winner=="Stoke"] <- "Stoke City"
  att.all$team1[att.all$team1=="Small Heath"] <- "Birmingham City"
  att.all$team2[att.all$team2=="Small Heath"] <- "Birmingham City"
  att.all$winner[att.all$winner=="Small Heath"] <- "Birmingham City"
  att.all$team1[att.all$team1=="Newton Heath"] <- "Manchester United"
  att.all$team2[att.all$team2=="Newton Heath"] <- "Manchester United"
  att.all$winner[att.all$winner=="Newton Heath"] <- "Manchester United"
  #att.all$team1[att.all$team1=="Swansea Town"] <- "Swansea City"
  #att.all$team2[att.all$team2=="Swansea Town"] <- "Swansea City"
  att.all$winner[att.all$winner=="Swansea Town"] <- "Swansea City"
  att.all$team1[att.all$team1=="Orient"] <- "Leyton Orient"
  att.all$team2[att.all$team2=="Orient"] <- "Leyton Orient"
  att.all$winner[att.all$winner=="Orient"] <- "Leyton Orient"
  att.all$team1[att.all$team1=="Ardwick"] <- "Manchester City"
  att.all$team2[att.all$team2=="Ardwick"] <- "Manchester City"
  att.all$winner[att.all$winner=="Ardwick"] <- "Manchester City"
  att.all$team1[regexpr("Rotherham",att.all$team1)>-1] <- "Rotherham United"
  att.all$team2[regexpr("Rotherham",att.all$team2)>-1] <- "Rotherham United"
  att.all$winner[regexpr("Rotherham",att.all$winner)>-1] <- "Rotherham United"
  att.all$team1[regexpr("South Shields",att.all$team1)>-1] <- "South Shields"
  att.all$team2[regexpr("South Shields",att.all$team2)>-1] <- "South Shields"
  att.all$winner[regexpr("South Shields",att.all$winner)>-1] <- "South Shields"
  att.all$team1[regexpr("Walsall",att.all$team1)>-1] <- "Walsall"
  att.all$team2[regexpr("Walsall",att.all$team2)>-1] <- "Walsall"
  att.all$winner[regexpr("Walsall",att.all$winner)>-1] <- "Walsall"
  att.all$team1[regexpr("Stevenage",att.all$team1)>-1] <- "Stevenage"
  att.all$team2[regexpr("Stevenage",att.all$team2)>-1] <- "Stevenage"
  att.all$winner[regexpr("Stevenage",att.all$winner)>-1] <- "Stevenage"
  att.all$team1[regexpr("Wolves",att.all$team1)>-1] <- "Wolverhampton Wanderers"
  att.all$team2[regexpr("Wolves",att.all$team2)>-1] <- "Wolverhampton Wanderers"
  att.all$winner[regexpr("Wolves",att.all$winner)>-1] <- "Wolverhampton Wanderers"
  att.all$team1[regexpr("Accrington",att.all$team1)>-1] <- "Accrington Stanley"
  att.all$team2[regexpr("Accrington",att.all$team2)>-1] <- "Accrington Stanley"
  att.all$winner[regexpr("Accrington",att.all$winner)>-1] <- "Accrington Stanley"
  att.all$team1[regexpr("Bournemouth",att.all$team1)>-1] <- "AFC Bournemouth"
  att.all$team2[regexpr("Bournemouth",att.all$team2)>-1] <- "AFC Bournemouth"
  att.all$winner[regexpr("Bournemouth",att.all$winner)>-1] <- "AFC Bournemouth"
  # att.all$team1[regexpr("Burton",att.all$team1)>-1] <- "Burton Albion"
  # att.all$team2[regexpr("Burton",att.all$team2)>-1] <- "Burton Albion"
  att.all$team1[regexpr("Leicester",att.all$team1)>-1] <- "Leicester City"
  att.all$team2[regexpr("Leicester",att.all$team2)>-1] <- "Leicester City"
  att.all$winner[regexpr("Leicester",att.all$winner)>-1] <- "Leicester City"
  att.all$team1[regexpr("Port Vale",att.all$team1)>-1] <- "Port Vale"
  att.all$team2[regexpr("Port Vale",att.all$team2)>-1] <- "Port Vale"
  att.all$winner[regexpr("Port Vale",att.all$winner)>-1] <- "Port Vale"
  att.all$team1[regexpr("Chesterfield",att.all$team1)>-1] <- "Chesterfield"
  att.all$team2[regexpr("Chesterfield",att.all$team2)>-1] <- "Chesterfield"
  att.all$winner[regexpr("Chesterfield",att.all$winner)>-1] <- "Chesterfield"
  att.all$team1[regexpr("Hartlepool",att.all$team1)>-1] <- "Hartlepool United"
  att.all$team2[regexpr("Hartlepool",att.all$team2)>-1] <- "Hartlepool United"
  att.all$winner[regexpr("Hartlepool",att.all$winner)>-1] <- "Hartlepool United"
  att.all$team1[regexpr("Arsenal",att.all$team1)>-1] <- "Arsenal"
  att.all$team2[regexpr("Arsenal",att.all$team2)>-1] <- "Arsenal"
  att.all$winner[regexpr("Arsenal",att.all$winner)>-1] <- "Arsenal"
  att.all$team1[regexpr("Aldershot",att.all$team1)>-1] <- "Aldershot"
  att.all$team2[regexpr("Aldershot",att.all$team2)>-1] <- "Aldershot"
  att.all$winner[regexpr("Aldershot",att.all$winner)>-1] <- "Aldershot"
  att.all$team1[regexpr("Boston",att.all$team1)>-1] <- "Boston United"
  att.all$team2[regexpr("Boston",att.all$team2)>-1] <- "Boston United"
  att.all$winner[regexpr("Boston",att.all$winner)>-1] <- "Boston United"
  att.all$team1[regexpr("Chester$",att.all$team1)>-1] <- "Chester City"
  att.all$team2[regexpr("Chester$",att.all$team2)>-1] <- "Chester City"
  att.all$winner[regexpr("Chester$",att.all$winner)>-1] <- "Chester City"
  
  att.all$season <- as.numeric(gsub("season-","",att.all$year))
  att.all$Year.Finish <- att.all$season
  
  att.all$Club <- tolower(att.all$team1)
  
  ##some duplicate entries remain - cup matches between different division teams
  att.all <- att.all[duplicated(att.all$filename)==FALSE,]
  
  att.all$competition <- gsub("^\\s+|\\s+$","",att.all$competition)
  att.all$tier <- as.numeric(att.all$competition=="Premier League" | att.all$competition=="League Division One") + 2*as.numeric(att.all$competition=="League Division 1" | att.all$competition=="League Division Two" | att.all$competition=="League Championship") + 3*as.numeric(att.all$competition=="League Division 2" | att.all$competition=="League Division Three" | att.all$competition=="League One") + 4*as.numeric(att.all$competition=="League Division 3" | att.all$competition=="League Division Four" | att.all$competition=="League Two") + 3.1*as.numeric(att.all$competition=="Division Three (North)") + 3.2*as.numeric(att.all$competition=="Division Three (South)")
  
  att.all$winner.goals <- att.all$goals1
  att.all$loser.goals <- att.all$goals2
  att.all$winner <- gsub("to ","",att.all$winner)
  att.all$goals1 <- as.numeric(att.all$winner==att.all$team1)*att.all$winner.goals + as.numeric(att.all$winner==att.all$team2)*att.all$loser.goals+as.numeric(att.all$winner=="")*att.all$winner.goals
  att.all$goals2 <- as.numeric(att.all$winner==att.all$team2)*att.all$winner.goals + as.numeric(att.all$winner==att.all$team1)*att.all$loser.goals+as.numeric(att.all$winner=="")*att.all$winner.goals
  att.all$outcome <- 0.5*as.numeric(att.all$goals1==att.all$goals2) + as.numeric(att.all$goals1>att.all$goals2)
  
  att.all <- att.all[is.na(att.all$date)==FALSE,]
  
  
  
  att.all$score <- paste0(att.all$goals1,"-",att.all$goals2)
  
  att.all <- att.all[order(att.all$date),]
  att.all$year0 <- format(att.all$date,"%Y")
  
  att.all <- att.all[duplicated(att.all$filename)==FALSE,]
  att.all <- att.all[order(att.all$date),]
  
  att.all$outcomeH <- as.numeric(att.all$outcome==1)
  att.all$outcomeD <- as.numeric(att.all$outcome==0.5)
  att.all$outcomeA <- as.numeric(att.all$outcome==0)
  return(att.all)
}

load.soccerbase.results <- function(upto = Sys.Date(),startingat = as.Date("1866-01-01"),region="all") {
  require(zoo)
  #floc <- "/Volumes/11330730-dp/Correct-score-data/soccerbase-data/"
  floc <- "/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/"
  files.sb <- list.files(floc,pattern="^historical_results_\\S+.csv$")
  files.sb <- files.sb[files.sb<=paste0("historical_results_",upto,"-2020.csv") & files.sb>paste0("historical_results_",startingat,"-2020.csv")]
  res0 <- data.frame()
  for(ff in files.sb) {
    print(ff)
    temp <- read.csv(paste0(floc,ff),stringsAsFactors = FALSE)
    temp$division[temp$division==""] <- NA
    temp$div_id[temp$division=="Championship Play-Off"] <- "2"
    temp$div_id[temp$division=="League One Play-Off"] <- "3"
    temp$div_id[temp$division=="League Two Play-Off"] <- "4"
    temp$div_id[temp$division=="English Div 2 (old) Play-Off"] <- "6"
    temp$div_id[temp$division=="English Div 3 (old) Play-Off"] <- "7"
    temp$div_id[temp$division=="English Div 4 Play-Off"] <- "8"
    temp$div_id[temp$division=="Football Conference Play-off"] <- "9"
    temp$div_id[temp$division=="Football Conf North Play-off"] <- "225"
    temp$div_id[temp$division=="Football Conf South Play-off"] <- "226"
    temp$div_id[temp$division=="Ryman League Play-off"] <- "80"
    temp$div_id[temp$division=="Southern League Play-off"] <- "75"
    temp$div_id[temp$division=="Unibond League Play-off"] <- "56"
    temp$div_id[temp$div_id=="n/a"] <- "-99"
    temp$div_id <- as.numeric(temp$div_id)
    temp$division <- na.locf(temp$division)
    temp$div_id <- na.locf(temp$div_id)
    temp$goals1 <- as.numeric(gsub("^.*?(\\d+)\\D+(\\d+).*?$","\\1",temp$goals1))
    temp$goals2 <- as.numeric(gsub("^.*?(\\d+)\\D+(\\d+).*?$","\\2",temp$goals2))
    res0 <- rbind(res0,temp)
    temp <- NULL
  }
  #write.csv(res0[,c("match_id","div_id")],"/Volumes/11330730-dp/Correct-score-data/soccerbase-data/match-div-ids.csv")
  
  ##get rid of duplicates
  res0 <- res0[duplicated(res0)==FALSE,]
  res0 <- res0[!(is.na(res0$goals1)==TRUE & res0$date<Sys.Date()),]
  
  res0$date <- as.Date(res0$date)
  res0$season <- as.numeric(format(res0$date,"%Y")) - as.numeric(as.numeric(format(res0$date,"%m"))<7)
  res0$season[format(res0$date,"%Y")=="2020" & format(res0$date,"%m") %in% c("07","08")] <- 2019
  res0 <- res0[order(res0$date),]
  
  #fix Birmingham v Southend in 1995 problem - listed as in Div 3 (div_id=3) when should be in div_id=2
  res0$div_id[res0$match_id=="tgc223235"] <- 2
  
  #fix Chester C --- only listed as having played two matches ever, in March 1996
  res0$team1_id[res0$team1=="Chester C (old)"] <- 663
  res0$team1[res0$team1=="Chester C (old)"] <- "Chester"
  res0$team2_id[res0$team2=="Chester C (old)"] <- 663
  res0$team2[res0$team2=="Chester C (old)"] <- "Chester"

  #fix middlesbrough/middlesbro
  res0$team1[res0$team1_id==1697] <- "Middlesbrough"
  res0$team2[res0$team2_id==1697] <- "Middlesbrough"
  
  res0$team1[res0$team1_id==1996] <- "Peterborough"
  res0$team2[res0$team2_id==1996] <- "Peterborough"
  
  #remove abandoned rotherham vs cardiff match from march 2023
  res0 <- res0[!(res0$match_id=="tgc860008" & res0$date=="2023-03-18"),]
  
  res0 <- res0[duplicated(res0)==FALSE,]

  #fix div_id problem --- harmonise where ID does exist elsewhere and create where non exists
  #first fill in NAs where a div_id does exist
  res0$division <- gsub("^\\s+|\\s+$","",res0$division)
  NAdivs <- unique(res0$division[res0$div_id==-99])
  nonNAdivs <- unique(res0$division[res0$div_id!=-99])
  partialdivs <- NAdivs[NAdivs %in% nonNAdivs]
  res0$div_id[res0$div_id==-99] <- NA
  res0 <- res0[order(res0$division,res0$div_id),]
  res0$div_id[res0$division %in% partialdivs] <- na.locf(res0$div_id[res0$division %in% partialdivs])
  #then create for each division that doesn't have a div_id
  allNAdivs <- NAdivs[!(NAdivs %in% nonNAdivs)]
  for(id0 in 500+c(1:NROW(allNAdivs))) {
    res0$div_id[res0$division==allNAdivs[id0-500]] <- id0
  }
  
  teamIDs0 <- res0[,c("team1","team1_id","div_id")]
  colnames(teamIDs0) <- c("team","team_id","div_id")
  teamIDs1 <- res0[,c("team2","team2_id","div_id")]
  colnames(teamIDs1) <- c("team","team_id","div_id")
  teamIDs <- rbind(teamIDs0,teamIDs1)
  teamIDs <- teamIDs[order(teamIDs$team,teamIDs$div_id),]
  teamIDs <- teamIDs[duplicated(teamIDs$team_id)==FALSE,]
  teamIDs <- teamIDs[order(teamIDs$div_id,teamIDs$team),]

  eng.league.ids <- c(1:11,56,75,80,225:226)
  eng.cup.ids <- c(58,60,63,70,78)
  england.ids <- c(eng.league.ids,eng.cup.ids)
  scot.league.ids <- c(12:18,82,353)
  scot.cup.ids <- c(59,61,62)
  scotland.ids <- c(scot.league.ids,scot.cup.ids)
  women.ids <- c(83,346,415,510,727,762,774)
  euro.cup.ids <- c(66,65,64,67,69,412)
  international.ids <- c(68,73,83,381,155,84,103,104,72,173,98)
  
  if(NROW(region)==1 & any(region=="all")) {
    return.res <- res0
  } else {
    return.res <- data.frame(stringsAsFactors = FALSE)
    if("england" %in% region) {
      return.res <- rbind(return.res,res0[is.na(res0$div_id)==FALSE & res0$div_id %in% england.ids,])
      write.csv(teamIDs[is.na(teamIDs$div_id)==FALSE,c("div_id","team","team_id")],"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/wanted-team-ids-england.csv")
      write.csv(res0,"/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/all-results-sb-england.csv")
      write.csv(res0[is.na(res0$div_id)==FALSE & res0$div_id %in% england.ids,],
                "/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/england-results-sb-england.csv")
      write.csv(res0[is.na(res0$div_id)==FALSE & res0$div_id %in% women.ids & regexpr("Women",res0$team1)>-1,],
                "/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/women-results-sb-england.csv")
    }
    if("scotland" %in% region) {
      return.res <- rbind(return.res,res0[is.na(res0$div_id)==FALSE & res0$div_id %in% scotland.ids,])
      write.csv(teamIDs[is.na(teamIDs$div_id)==FALSE,c("div_id","team","team_id")],paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/wanted-team-ids-scotland.csv"))
      write.csv(res0,paste0("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/all-results-sb-scotland.csv"))
      write.csv(res0[is.na(res0$div_id)==FALSE & res0$div_id %in% england.ids,],
                paste0("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/england-results-sb-scotland.csv"))
      write.csv(res0[is.na(res0$div_id)==FALSE & res0$div_id %in% women.ids & regexpr("Women",res0$team1)>-1,],
                paste0("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/women-results-sb-scotland.csv"))
    } 
    if("europe" %in% region) {
      return.res <- rbind(return.res,res0[is.na(res0$div_id)==FALSE & res0$div_id %in% euro.cup.ids,])
    } 
    if("women" %in% region) {
      return.res <- rbind(return.res,res0[is.na(res0$div_id)==FALSE & res0$div_id %in% women.ids & regexpr("Women",res0$team1)>-1,])
    }
    if("international" %in% region) {
      return.res <- rbind(return.res,res0[is.na(res0$div_id)==FALSE & res0$div_id %in% international.ids,])
    }
  }
  
  return(return.res)
}

load.worldfootball.results <- function() {
  wf.res0 <- data.frame(stringsAsFactors = FALSE)
  wfloc <- "/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/"
  wf.files <- list.files(wfloc,pattern="^.*?-worldfootball-results.csv$")
  for(ff in wf.files) {
    print(ff)
    temp <- read.csv(paste0(wfloc,ff),stringsAsFactors = FALSE)
    if(NROW(temp)>0) {
      colnames(temp) <- c("folname","season","h1","date","time","team1.0","team2.0","result","empty")
      temp$filename <- ff
      temp$country <- gsub("^(\\w+).*?$","\\1",temp$h1)
      temp$date <- as.Date(temp$date,"%d/%m/%Y")
      temp$date <- na.locf(temp$date)
      temp$team1 <- gsub("^<a href=[*]/teams/(\\S+)/[*] title=[*](.*?)[*]>(.*?)</a>","\\3",temp$team1.0)
      temp$team1.2 <- gsub("^<a href=[*]/teams/(\\S+)/[*] title=[*](.*?)[*]>(.*?)</a>","\\1",temp$team1.0)
      temp$team2 <- gsub("^<a href=[*]/teams/(\\S+)/[*] title=[*](.*?)[*]>(.*?)</a>","\\3",temp$team2.0)
      temp$team2.2 <- gsub("^<a href=[*]/teams/(\\S+)/[*] title=[*](.*?)[*]>(.*?)</a>","\\1",temp$team2.0)
      temp$goals1 <- as.numeric(gsub("^.*?(\\d+)[:](\\d+).*?$","\\1",temp$result))
      temp$goals2 <- as.numeric(gsub("^.*?(\\d+)[:](\\d+).*?$","\\2",temp$result))
      temp$competition <- gsub("^(\\D+)-\\d+.*?$","\\1",temp$folname)
      wf.res0 <- rbind(wf.res0,temp[,c("date","country","competition","team1.2","team1","goals1","goals2","team2","team2.2")])
    }
  }
  wf.res0 <- wf.res0[duplicated(wf.res0)==FALSE,]
  return(wf.res0)
}

load.wycombe.results <- function() {
  wyc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/wycombe-attendances4.csv",stringsAsFactors = FALSE)
  colnames(wyc) <- c("file","date0","opponents","venue","result","score","attendance","X8","X9","X10","X11","X12")
  
  wyc$Season0 <- gsub("^\\d{4}results-","",gsub("reports","",gsub("htmhtml","",gsub("news","",gsub("onthenet","",gsub("[.]","",gsub("season-","",wyc$file)))))))
  wyc$Season[regexpr("^\\d{2}_",wyc$Season0)>-1] <- 1900+as.numeric(gsub("^.*?(\\d{2})_(\\d{2}).*?$","\\2",wyc$Season0[regexpr("^\\d{2}_",wyc$Season0)>-1]))
  wyc$Season[regexpr("^\\d{4}_",wyc$Season0)>-1] <- 1+as.numeric(gsub("^.*?(\\d{4})_(\\d{2}).*?$","\\1",wyc$Season0[regexpr("^\\d{4}_",wyc$Season0)>-1]))
  wyc$Season[regexpr("\\d{8}",wyc$Season0)>-1] <- as.numeric(gsub("^.*?(\\d{4})(\\d{4}).*?$","\\2",wyc$Season0[regexpr("\\d{8}",wyc$Season0)>-1]))
  wyc$Season <- as.numeric(wyc$Season)

  wyc$date[nchar(wyc$date0)<12] <- as.Date(wyc$date0[nchar(wyc$date0)<12],"%d-%b-%Y")
  wyc$date.day <- as.numeric(gsub("^(\\w{3}) (\\d+) (\\w{3})$","\\2",wyc$date0))
  wyc$date.month[is.na(wyc$date.day)==FALSE] <- gsub("^(\\w{3}) (\\d+) (\\w{3})$","\\3",wyc$date0[is.na(wyc$date.day)==FALSE])
  wyc$date.year[is.na(wyc$date.day)==FALSE] <- wyc$Season[is.na(wyc$date.day)==FALSE]
  wyc$date.year[is.na(wyc$date.day)==FALSE & wyc$date.month %in% c("Jul","Aug","Sep","Oct","Nov","Dec")] <- wyc$Season[is.na(wyc$date.day)==FALSE & wyc$date.month %in% c("Jul","Aug","Sep","Oct","Nov","Dec")]-1
  wyc$date[is.na(wyc$date.day)==FALSE] <- as.Date(paste0(wyc$date.day[is.na(wyc$date.day)==FALSE],"-",
                                                         wyc$date.month[is.na(wyc$date.day)==FALSE],"-",
                                                         wyc$date.year[is.na(wyc$date.day)==FALSE]),"%d-%b-%Y")
  wyc$date[is.na(wyc$date.day) & regexpr("\\d{4}",wyc$date0)>-1 & nchar(wyc$date0)<12] <- as.Date(wyc$date0[is.na(wyc$date.day) & regexpr("\\d{4}",wyc$date0)>-1 & nchar(wyc$date0)<12],"%d-%b-%Y")
  wyc$date <- as.Date(as.numeric(wyc$date),origin="1970-01-01")
  wyc <- wyc[is.na(wyc$date)==FALSE,]
  
  #clean up the competition/attendance variable
  wyc$goals[regexpr("\\d+-\\d+",wyc$score)>-1] <- gsub("^.*?(\\d+)-(\\d+).*?$","\\1",wyc$score[regexpr("\\d+-\\d+",wyc$score)>-1])
  wyc$opp.goals[regexpr("\\d+-\\d+",wyc$score)>-1] <- gsub("^.*?(\\d+)-(\\d+).*?$","\\2",wyc$score[regexpr("\\d+-\\d+",wyc$score)>-1])

  wyc$competition[regexpr("[(]",wyc$opponents)>-1] <- gsub("^.*?[(](.*?)[)].*?$","\\1",wyc$opponents[regexpr("[(]",wyc$opponents)>-1])
  wyc$competition[regexpr("\\s{5,}\\S+$",wyc$opponents)>-1] <- gsub("^.*?\\s{5,}(\\S+)$","\\1",wyc$opponents[regexpr("\\s{5,}\\S+$",wyc$opponents)>-1])
  wyc$competition[regexpr("EL2",wyc$opponents)>-1] <- "EL2"
  wyc$competition[regexpr("WyHC",wyc$opponents)>-1] <- "WyHC"
  wyc$competition[regexpr("B&B",wyc$opponents)>-1] <- "B&B"
  
  wyc$opponents <- gsub("^(.*?)\\s{5,}(.*?)$","\\1",wyc$opponents)
  wyc$opponents <- gsub("^\\s+|\\s+$","",gsub("[(].*?[)]","",wyc$opponents))
  wyc$opponents <- gsub("A[.]P[.] ","",wyc$opponents)
  
  wyc$opponents <- gsub("^<A HREF=.*?>","",wyc$opponents)
  wyc$opponents <- gsub("^<a href=.*?>","",wyc$opponents)
  wyc$opponents <- gsub("\\s\\S$","",wyc$opponents)
  wyc$opponents <- gsub("St[.]","Saint ",wyc$opponents)
  wyc$opponents <- gsub("[.]","",wyc$opponents)
  wyc$opponents <- toTitleCase(tolower(wyc$opponents))
  wyc$opponents <- gsub("[(].*?[)]","",wyc$opponents)
  wyc$opponents <- gsub("^Manchester (\\w+)$","Man \\1",wyc$opponents)
  wyc$opponents <- gsub("^Sheffield (\\w+)$","Sheff \\1",wyc$opponents)
  wyc$opponents <- gsub("^Nottingham (\\w+)$","Nottm \\1",wyc$opponents)
  wyc$opponents <- gsub("^\\s+|\\s+$","",gsub("[(].*?[)]","",wyc$opponents))
  wyc$opponents[regexpr("Bournemouth$",wyc$opponents)>-1] <- "AFC Bournemouth"
  wyc$opponents[regexpr("Clapton Orient",wyc$opponents)>-1] <- "Leyton Orient"
  wyc$opponents[regexpr("West Brom",wyc$opponents)>-1] <- "West Brom"
  wyc$opponents[regexpr("Brighton",wyc$opponents)>-1] <- "Brighton"
  wyc$opponents[regexpr("Crewe",wyc$opponents)>-1] <- "Crewe"
  wyc$opponents[regexpr("Derby",wyc$opponents)>-1] <- "Derby"
  wyc$opponents[regexpr("Doncaster",wyc$opponents)>-1] <- "Doncaster"
  wyc$opponents[regexpr("Forest Green",wyc$opponents)>-1] <- "Forest Green"
  wyc$opponents[regexpr("Dagenham &",wyc$opponents)>-1] <- "Dag &amp; Red"
  wyc$opponents[regexpr("Gravesend",wyc$opponents)>-1] <- "Gravesend"
  wyc$opponents[regexpr("Middlesb",wyc$opponents)>-1] <- "Middlesbro"
  wyc$opponents[regexpr("Newport",wyc$opponents)>-1] <- "Newport"
  wyc$opponents[regexpr("Plymouth",wyc$opponents)>-1] <- "Plymouth"
  wyc$opponents[regexpr("Preston",wyc$opponents)>-1] <- "Preston"
  wyc$opponents[regexpr("Qpr",wyc$opponents)>-1] <- "QPR"
  wyc$opponents[regexpr("Rotherham",wyc$opponents)>-1] <- "Rotherham"
  wyc$opponents[regexpr("Reading",wyc$opponents)>-1] <- "Reading"
  wyc$opponents[regexpr("Scunthorpe",wyc$opponents)>-1] <- "Scunthorpe"
  wyc$opponents[regexpr("Stockport",wyc$opponents)>-1] <- "Stockport"
  wyc$opponents[regexpr("Tottenham",wyc$opponents)>-1] <- "Tottenham"
  wyc$opponents[regexpr("Tranmere",wyc$opponents)>-1] <- "Tranmere"
  wyc$opponents[regexpr("Wigan",wyc$opponents)>-1] <- "Wigan"
  wyc$opponents <- gsub(" Wednesday"," Wed",wyc$opponents)
  wyc$opponents <- gsub(" and "," &amp; ",wyc$opponents)
  wyc$opponents <- gsub("&#39;","'",wyc$opponents)
  wyc$opponents[regexpr("Milton Keynes",wyc$opponents)>-1] <- "MK Dons"
  wyc$opponents[regexpr("franchise",tolower(wyc$opponents))>-1] <- "MK Dons"
  wyc$opponents[regexpr("^Man ",wyc$opponents)==-1 & regexpr("^Sheff ",wyc$opponents)==-1 & regexpr("^Bristol ",wyc$opponents)==-1] <- gsub(" Albion","",gsub(" Rovers","",gsub(" Harriers","",gsub(" Tydfil","",gsub(" Wanderers","",gsub(" Athletic","",gsub(" Rangers","",gsub(" Borough","",gsub(" Town","",gsub(" Victoria","",gsub(" Utd","",gsub(" United","",gsub(" City","",wyc$opponents[regexpr("^Man ",wyc$opponents)==-1 & regexpr("^Sheff ",wyc$opponents)==-1 & regexpr("^Bristol ",wyc$opponents)==-1])))))))))))))
  wyc$opponents[regexpr("Cambridge",wyc$opponents)>-1] <- "Cambridge U"
  
  
  library(tools)
  wyc$team1 <- "Wycombe Wanderers"
  wyc$team1[wyc$venue=="A"] <- wyc$opponents[wyc$venue=="A"]
  wyc$team2 <- "Wycombe Wanderers"
  wyc$team2[wyc$venue=="H"] <- wyc$opponents[wyc$venue=="H"]

  wyc$goals1 <- wyc$goals
  wyc$goals1[wyc$venue=="A"] <- wyc$opp.goals[wyc$venue=="A"]
  wyc$goals2 <- wyc$goals
  wyc$goals2[wyc$venue=="H"] <- wyc$opp.goals[wyc$venue=="H"]

  wyc$match_id <- paste0("wwfc",1:NROW(wyc))
  wyc$division <- wyc$competition
  wyc$div_id[wyc$division %in% c("GL","GMVC")] <- 9
  wyc$div_id[wyc$division %in% c("RIL","SIL","BIL")] <- 80
  wyc$team1_id <- NA
  wyc$team2_id <- NA

  return(wyc[duplicated(wyc[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
             c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
}

load.barnet.results <- function() {
  barnet <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/barnet-attendances.csv",stringsAsFactors = FALSE)
  colnames(barnet) <- c(colnames(barnet)[-1],"empty")
  barnet$date <- as.Date(barnet$date,"%d/%m/%Y")
  barnet$season <- as.numeric(format(barnet$date,"%Y")) - as.numeric(as.numeric(format(barnet$date,"%m"))<7)
  
  
  barnet$opponent <- gsub("^\\s+|\\s+$","",gsub("[(].*?[)]","",barnet$opponent))
  barnet$opponent[regexpr("Bournemouth",barnet$opponent)>-1] <- "AFC Bournemouth"
  barnet$opponent <- gsub(" and "," &amp; ",barnet$opponent)
  barnet$opponent <- gsub("&#39;","'",barnet$opponent)
  barnet$opponent[barnet$opponent=="Milton Keynes"] <- "Milton Keynes Dons"
  barnet$opponent[barnet$opponent=="Ebbsfleet United"] <- "Gravesend"
  barnet$opponent <- gsub("A[.]P[.] ","",barnet$opponent)
  barnet$opponent <- gsub(" Harriers","",gsub(" Tydfil","",gsub(" Wanderers","",gsub(" Athletic","",gsub(" Rangers","",gsub(" Borough","",gsub(" Town","",gsub(" Victoria","",gsub(" United","",gsub(" City","",barnet$opponent))))))))))

  barnet$team1 <- "Barnet"
  barnet$team1[barnet$venue=="A"] <- barnet$opponent[barnet$venue=="A"]
  barnet$team2 <- "Barnet"
  barnet$team2[barnet$venue=="H"] <- barnet$opponent[barnet$venue=="H"]
  
  barnet$goals <- as.numeric(gsub("^(\\d+)\\s+-\\s+(\\d+).*?$","\\1",barnet$score))
  barnet$opp.goals <- as.numeric(gsub("^(\\d+)\\s+-\\s+(\\d+).*?$","\\2",barnet$score))

  barnet$goals1 <- barnet$goals
  barnet$goals1[barnet$venue=="A"] <- barnet$opp.goals[barnet$venue=="A"]
  
  barnet$goals2 <- barnet$opp.goals
  barnet$goals2[barnet$venue=="A"] <- barnet$goals[barnet$venue=="A"]
  
  barnet$attendance <- gsub("^(\\d+)\\s+[(](\\d+)[)]","\\1",barnet$attendance)
  
  barnet$match_id <- paste0("bfc",1:NROW(barnet))
  barnet$division <- barnet$competition
  barnet$div_id <- NA
  barnet$team1_id <- NA
  barnet$team2_id <- NA
  
  return(barnet[duplicated(barnet[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
             c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
}

load.boston.results <- function() {
  require(stringr)
  bufc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/boston-attendances.csv",stringsAsFactors = FALSE)
  colnames(bufc) <- c("filename","date0","venue","competition","opponents","attendance","result","goals","opp.goals","team","extra1","extra2")
  bufc$opponents[regexpr("<a href=",bufc$opponents)>-1] <- gsub("<a href=\\S+[.]html>(.*?)</a>","\\1",bufc$opponents[regexpr("<a href=",bufc$opponents)>-1])
  bufc$opponents <- gsub("^\\s+|\\s+$","",gsub("AP ","",gsub("A F C","",gsub("[.]","",bufc$opponents))))
  bufc$opponents <- toTitleCase(tolower(bufc$opponents))
  bufc$opponents <- gsub(" Harriers","",gsub(" Tydfil","",gsub(" Wanderers","",gsub(" Athletic","",gsub(" Rangers","",gsub(" Borough","",gsub(" Town","",gsub(" Victoria","",gsub(" United","",gsub(" City","",bufc$opponents))))))))))
  bufc$opponents[regexpr("Gravesend",bufc$opponents)>-1] <- "Gravesend"
  
  # bufc$upperratio <- str_count(bufc$opponents, "[A-Z]")/str_count(bufc$opponents, "\\w")
  # bufc$venue <- "A"
  # bufc$venue[bufc$upperratio==1] <- "H"
  
  bufc$team1 <- "Boston"
  bufc$team1[bufc$venue=="A"] <- bufc$opponents[bufc$venue=="A"]
  
  bufc$team2 <- "Boston"
  bufc$team2[bufc$venue=="H"] <- bufc$opponents[bufc$venue=="H"]
  
  bufc$season <- as.numeric(gsub("season-(\\d+)-(\\d+)[.]html","\\2",bufc$filename))
  
  bufc$date.month[regexpr("/",bufc$date0)>-1] <- gsub("^(\\w+)\\s+(\\d+)/(\\d+)$","\\3",bufc$date0[regexpr("/",bufc$date0)>-1])
  bufc$date.month[regexpr("/",bufc$date0)==-1] <- gsub("^(\\w{3})\\s+(\\d+)\\w{2}\\s+(\\w{3})$","\\3",bufc$date0[regexpr("/",bufc$date0)==-1])
  bufc$date.day[regexpr("/",bufc$date0)>-1] <- as.numeric(gsub("^(\\w+)\\s+(\\d+)/(\\d+)$","\\2",bufc$date0[regexpr("/",bufc$date0)>-1]))
  bufc$date.day[regexpr("/",bufc$date0)==-1] <- as.numeric(gsub("^(\\w{3})\\s+(\\d+)\\w{2}\\s+(\\w{3})$","\\2",bufc$date0[regexpr("/",bufc$date0)==-1]))
  bufc$date.year <- bufc$season - as.numeric(bufc$date.month %in% c("Aug","Sep","Oct","Nov","Dec",8:12))
  
  bufc$date <- as.Date(paste0(bufc$date.year,"-",bufc$date.month,"-",bufc$date.day),"%Y-%b-%d")
  bufc$date[is.na(bufc$date)] <- as.Date(paste0(bufc$date.year[is.na(bufc$date)],"-",
                                                bufc$date.month[is.na(bufc$date)],"-",
                                                bufc$date.day[is.na(bufc$date)]),"%Y-%m-%d")
  bufc$goals1 <- bufc$goals
  bufc$goals1[bufc$venue=="A"] <- bufc$opp.goals[bufc$venue=="A"]
  
  bufc$goals2 <- bufc$opp.goals
  bufc$goals2[bufc$venue=="A"] <- bufc$goals[bufc$venue=="A"]
  
  bufc$match_id <- paste0("bufc",1:NROW(bufc))
  bufc$division <- bufc$competition
  bufc$division <- gsub("\\s{4,}.*?$","",bufc$division)
  bufc$div_id <- NA
  bufc$div_id[bufc$division=="League Cup"] <- 60
  bufc$div_id[bufc$division=="FA Cup"] <- 58
  bufc$div_id[bufc$division=="FL Trophy"] <- 70
  bufc$div_id[bufc$division=="FA Trophy"] <- 78
  bufc$team1_id <- NA
  bufc$team2_id <- NA

  return(bufc[duplicated(bufc[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
                c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
}

load.southport.results <- function() {
  require(stringr)
  require(tools)
  sfc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/southport-attendances.csv",stringsAsFactors = FALSE)
  sfc$date <- as.Date(sfc$date,"%d/%m/%Y")
  sfc$opponents <- gsub("^<a href=/opponents/[?]opp=\\d+>(.*?)</a>$","\\1",sfc$opponents)

  sfc$opponents <- toTitleCase(tolower(sfc$opponents))
  sfc$opponents <- gsub(" Harriers","",gsub(" Tydfil","",gsub(" Wanderers","",gsub(" Athletic","",gsub(" Rangers","",gsub(" Borough","",gsub(" Town","",gsub(" Victoria","",gsub(" United","",gsub(" City","",sfc$opponents))))))))))
  sfc$opponents[regexpr("Gravesend",sfc$opponents)>-1] <- "Gravesend"
  
  # sfc$upperratio <- str_count(sfc$opponents, "[A-Z]")/str_count(sfc$opponents, "\\w")
  # sfc$venue <- "A"
  # sfc$venue[sfc$upperratio==1] <- "H"
  
  sfc$team1 <- "Southport"
  sfc$team1[is.na(sfc$venue)==FALSE & sfc$venue=="A"] <- sfc$opponents[is.na(sfc$venue)==FALSE & sfc$venue=="A"]
  
  sfc$team2 <- "Southport"
  sfc$team2[is.na(sfc$venue)==FALSE & sfc$venue=="H"] <- sfc$opponents[is.na(sfc$venue)==FALSE & sfc$venue=="H"]
  
  sfc$season <- as.numeric(format(sfc$date,"%Y")) - as.numeric(as.numeric(format(sfc$date,"%m"))<7)
  
  sfc$goals <- as.numeric(gsub("^<a href=[.][.]/match-details/[?]id=\\d+>(\\d+)\\s*-\\s*(\\d+)</a>$","\\1",sfc$score))
  sfc$opp.goals <- as.numeric(gsub("^<a href=[.][.]/match-details/[?]id=\\d+>(\\d+)\\s*-\\s*(\\d+)</a>$","\\2",sfc$score))

  sfc$goals1 <- sfc$goals
  sfc$goals1[is.na(sfc$venue)==FALSE & sfc$venue=="A"] <- sfc$opp.goals[is.na(sfc$venue)==FALSE & sfc$venue=="A"]
  
  sfc$goals2 <- sfc$opp.goals
  sfc$goals2[is.na(sfc$venue)==FALSE & sfc$venue=="A"] <- sfc$goals[is.na(sfc$venue)==FALSE & sfc$venue=="A"]
  
  sfc$match_id <- paste0("sfc",1:NROW(sfc))
  sfc$division <- sfc$competition
  sfc$division <- gsub("^League / ","",sfc$division)
  sfc$division[regexpr("Cup",sfc$division)>-1] <- gsub("^(.*?Cup) / \\S+","\\1",sfc$division[regexpr("Cup",sfc$division)>-1])
  sfc$division <- gsub("Blue Square","National League",gsub("Conference","National League",sfc$division))
  sfc$div_id <- NA
  sfc$div_id[sfc$division=="League Cup"] <- 60
  sfc$div_id[sfc$division=="FA Cup"] <- 58
  sfc$div_id[sfc$division=="FL Trophy"] <- 70
  sfc$div_id[sfc$division=="FA Trophy"] <- 78
  sfc$div_id[sfc$division=="National League"] <- 9
  sfc$div_id[sfc$division=="National League North"] <- 225
  sfc$team1_id <- NA
  sfc$team2_id <- NA
  
  return(sfc[duplicated(sfc[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
              c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
}

load.wealdstone.results <- function() {
  weald <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/wealdstone-attendances.csv",stringsAsFactors = FALSE)
  weald <- weald[duplicated(weald)==FALSE,]
  colnames(weald)[2] <- "date0"
  
  weald$opponents[regexpr("<a href=",weald$opponents)>-1] <- gsub("^(.*?) <a href.*?</a>$","\\1",weald$opponents[regexpr("<a href=",weald$opponents)>-1])
  weald$opponents <- gsub("^\\s+|\\s+$","",gsub("AP ","",gsub("A F C","",gsub("[.]","",weald$opponents))))
  weald$opponents <- gsub(" Harriers","",gsub(" Tydfil","",gsub(" Wanderers","",gsub(" Athletic","",gsub(" Rangers","",gsub(" Borough","",gsub(" Town","",gsub(" Victoria","",gsub(" United","",gsub(" City","",weald$opponents))))))))))
  weald$opponents <- gsub(" AFC$","",gsub(" FC [(]1897[)]","",weald$opponents))
  weald$opponents[regexpr("Gravesend",weald$opponents)>-1] <- "Gravesend"
  
  # weald$upperratio <- str_count(weald$opponents, "[A-Z]")/str_count(weald$opponents, "\\w")
  # weald$venue <- "A"
  # weald$venue[weald$upperratio==1] <- "H"
  
  weald$team1 <- "Wealdstone"
  weald$team1[weald$venue=="Away"] <- weald$opponents[weald$venue=="Away"]
  
  weald$team2 <- "Wealdstone"
  weald$team2[weald$venue=="Home"] <- weald$opponents[weald$venue=="Home"]
  
  weald$season <- as.numeric(gsub("season-(\\d+)-(\\d+).*?[.]html","\\1",weald$file))+1
  weald <- weald[is.na(weald$season)==FALSE,]
  
  weald$date.month <- gsub("^(\\w{3})\\s+(\\d+)\\s+(\\w{3})$","\\3",weald$date0)
  weald$date.day <- as.numeric(gsub("^(\\w{3})\\s+(\\d+)\\s+(\\w{3})$","\\2",weald$date0))
  weald$date.year <- weald$season - as.numeric(weald$date.month %in% c("Aug","Sep","Oct","Nov","Dec"))
  
  weald$date <- as.Date(paste0(weald$date.year,"-",weald$date.month,"-",weald$date.day),"%Y-%b-%d")
  
  weald$competition <- gsub("  "," ",weald$competition)
  weald$attendance <- as.numeric(gsub("^Att: (\\d+)$","\\1",weald$attendance))
  
  weald$goals <- as.numeric(gsub("^.*?(\\d+) - (\\d+).*?$","\\1",weald$score))
  weald$opp.goals <- as.numeric(gsub("^.*?(\\d+) - (\\d+).*?$","\\2",weald$score))
  
  weald$goals1 <- weald$goals
  weald$goals1[weald$venue=="A"] <- weald$opp.goals[weald$venue=="A"]
  
  weald$goals2 <- weald$opp.goals
  weald$goals2[weald$venue=="A"] <- weald$goals[weald$venue=="A"]
  
  weald$match_id <- paste0("wdfc",1:NROW(weald))
  weald$division <- weald$competition
  weald$division <- gsub("Alliance Premier","National",gsub("Gola","National",gsub("Conference","National League",gsub("GM Vauxhall ","",gsub("Vanarama ","",weald$division)))))
  
  weald$div_id <- NA
  weald$div_id[weald$division=="League Cup"] <- 60
  weald$div_id[regexpr("FA Cup",weald$division)>-1] <- 58
  weald$div_id[weald$division %in% c("Isthmian League","Ryman League Premier Division","Beazer Homes League Premier Division","Southern League Premier Division")] <- 80
  weald$div_id[weald$division=="FA Trophy"] <- 78
  weald$div_id[weald$division=="National League"] <- 9
  weald$div_id[weald$division=="National League South"] <- 225
  weald$team1_id <- NA
  weald$team2_id <- NA
  
  return(weald[duplicated(weald[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
               c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
}

load.weymouth.results <- function() {
  weym <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/weymouth-attendances.csv",stringsAsFactors = FALSE)
  colnames(weym) <- c("date","opponents","competition","venue","score","attendance","scorers","empty")
  
  weym$date <- gsub("1900","1956",gsub("9173","1973",weym$date))
  weym$date <- as.Date(weym$date,"%d/%m/%Y")
  
  weym$season <- as.numeric(format(weym$date,"%Y")) - as.numeric(as.numeric(format(weym$date,"%m"))<7)
  
  weym$opponents <- gsub("^\\s+|\\s+$","",gsub("AP ","",gsub("A F C","",gsub("[.]","",weym$opponents))))
  weym$opponents <- gsub(" Harriers","",gsub(" Tydfil","",gsub(" Wanderers","",gsub(" Athletic","",gsub(" Rangers","",gsub(" Borough","",gsub(" Town","",gsub(" Victoria","",gsub(" United","",gsub(" City","",weym$opponents))))))))))
  weym$opponents <- gsub(" AFC$","",gsub(" FC [(]1897[)]","",weym$opponents))
  weym$opponents[regexpr("Gravesend",weym$opponents)>-1] <- "Gravesend"
  
  weym$team1 <- "Weymouth"
  weym$team1[weym$venue=="Away"] <- weym$opponents[weym$venue=="Away"]
  
  weym$team2 <- "Weymouth"
  weym$team2[weym$venue=="Home"] <- weym$opponents[weym$venue=="Home"]
  
  weym$goals <- as.numeric(gsub("^.*?(\\d+)\\s*-\\s*(\\d+).*?$","\\1",weym$score))
  weym$opp.goals <- as.numeric(gsub("^.*?(\\d+)\\s*-\\s*(\\d+).*?$","\\2",weym$score))
  
  weym$goals1 <- weym$goals
  weym$goals1[weym$venue=="Away"] <- weym$opp.goals[weym$venue=="Away"]
  
  weym$goals2 <- weym$opp.goals
  weym$goals2[weym$venue=="Away"] <- weym$goals[weym$venue=="Away"]

  weym$match_id <- paste0("wdfc",1:NROW(weym))
  weym$division <- weym$competition
  weym$division <- gsub("Blue Square","National",gsub("Blue Square Premier","National League",gsub("Alliance Premier","National",gsub("Gola","National",gsub("Conference","National League",gsub("General Motors Vauxhall ","",gsub("Nationwide ","",gsub("Vanarama ","",weym$division))))))))
  weym$division <- gsub("League League","League",weym$division)
  
  weym$div_id <- NA
  weym$div_id[weym$division=="League Cup"] <- 60
  weym$div_id[regexpr("FA Cup",weym$division)>-1] <- 58
  weym$div_id[weym$division %in% c("Southern League","Southern League (Premier Division)","Isthmian League","Ryman League Premier Division","Beazer Homes League Premier Division","Southern League Premier Division","The Evo-Stik League Southern Premier","Evo Stick Southern Premier League","Zameretto Premier","Calor Gas Southern Premier")] <- 80
  weym$div_id[regexpr("FAT|FA Trophy",weym$division)>-1] <- 78
  weym$div_id[weym$division=="National League"] <- 9
  weym$div_id[weym$division=="National League South"] <- 225
  weym$team1_id <- NA
  weym$team2_id <- NA
  
  return(weym[duplicated(weym[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
              c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
}

load.catalan.results <- function() {
  cat <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/catalonia-results-1903-1939.csv",
                  stringsAsFactors = FALSE)
  cat$date <- as.Date(cat$date,"%d-%m-%Y")
  cat$match_id <- paste0("cat",1:NROW(cat))
  cat$division <- "Catalan League"
  cat$div_id <- NA
  cat$team1_id <- NA
  cat$team2_id <- NA
  cat$attendance <- NA
  cat$team1 <- gsub("Espantol","Espanyol",gsub("Espamyol","Espanyol",gsub("^RCD ","",gsub("^Fc ","",gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",gsub("^02-01-1938    ","",cat$team1)))))))
  cat$team2 <- gsub("Espantol","Espanyol",gsub("Espamyol","Espanyol",gsub("^RCD ","",gsub("^Fc ","",gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",gsub("^02-01-1938    ","",cat$team2)))))))
  cat$team1[regexpr("Sabadell",cat$team1)>-1] <- "Sabadell"
  cat$team1[regexpr("frugel",cat$team1)>-1] <- "Palafrugel"
  cat$team2[regexpr("Sabadell",cat$team2)>-1] <- "Sabadell"
  cat$team2[regexpr("frugel",cat$team2)>-1] <- "Palafrugel"
  return(cat[duplicated(cat[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
              c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
}

load.central.spain.results <- function() {
  cesp <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/central-spain-results-1902-1925.csv",
                  stringsAsFactors = FALSE)
  cesp$date <- as.Date(cesp$date,"%d-%m-%Y")
  cesp$match_id <- paste0("cesp",1:NROW(cesp))
  cesp$division <- "Central Spanish League"
  cesp$div_id <- NA
  cesp$team1_id <- NA
  cesp$team2_id <- NA
  cesp$attendance <- NA
  cesp$team1[cesp$team1=="Athletic Madrid"] <- "Atletico Madrid"
  cesp$team2[cesp$team2=="Athletic Madrid"] <- "Atletico Madrid"
  cesp$team1[cesp$team1=="Madrid FC"] <- "Real Madrid"
  cesp$team2[cesp$team2=="Madrid FC"] <- "Real Madrid"
  cesp$team1 <- gsub("^Fc ","",gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",gsub("^02-01-1938    ","",cesp$team1))))
  cesp$team2 <- gsub("^Fc ","",gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",gsub("^02-01-1938    ","",cesp$team2))))
  return(cesp[duplicated(cesp[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
             c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
}

load.copa.del.rey.results <- function() {
  cdr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/copa-del-ray-1900-2022.csv",stringsAsFactors = FALSE)
  cdr$date <- as.Date(cdr$date,"%d/%m/%Y")

  cdr$match_id <- paste0("cdr",1:NROW(cdr))
  cdr$division <- "copa del rey"
  cdr$div_id <- NA
  cdr$team1_id <- NA
  cdr$team2_id <- NA
  cdr$attendance <- NA
  cdr$team1[cdr$team1=="Racing de Santander"] <- "Racing"
  cdr$team2[cdr$team2=="Racing de Santander"] <- "Racing"
  cdr$team1 <- gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",cdr$team1))
  cdr$team2 <- gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",cdr$team2))
  return(cdr[duplicated(cdr[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
              c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
}

load.italian.results <- function() {
  require(tools)
  ita <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/italy-results-1898-1923.csv",
                  stringsAsFactors = FALSE)
  ita$date.month <- as.numeric(gsub("^(\\d+)-(\\d+)$","\\2",ita$date))
  ita$date.day <- as.numeric(gsub("^(\\d+)-(\\d+)$","\\1",ita$date))
  ita$year[regexpr("/",ita$season)==-1] <- as.numeric(ita$season[regexpr("/",ita$season)==-1])
  ita$year[regexpr("/",ita$season)>-1] <- as.numeric(gsub("^(\\d{4})/\\d{2}$","\\1",ita$season[regexpr("/",ita$season)>-1]))+1
  ita$date <- as.Date(paste0(ita$date.day,"/",ita$date.month,"/",ita$year),"%d/%m/%Y")
  
  ita$match_id <- paste0("ita",1:NROW(ita))
  ita$division <- "italian championship"
  ita$div_id <- NA
  ita$team1_id <- NA
  ita$team2_id <- NA
  ita$attendance <- NA
  ita$team1 <- gsub("^\\s+|\\s+$","",gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",gsub("[.]","",toTitleCase(tolower(ita$team1))))))
  ita$team2 <- gsub("^\\s+|\\s+$","",gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",gsub("[.]","",toTitleCase(tolower(ita$team2))))))
  ita$team1 <- gsub("^Internazionale$","Inter",ita$team1)
  ita$team2 <- gsub("^Internazionale$","Inter",ita$team2)
  
  return(ita[duplicated(ita[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
             c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
}

load.mitropa.results <- function() {
  mit <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/mitropa-cup.csv",stringsAsFactors = FALSE)
  mit$date <- as.Date(mit$date,"%d/%m/%Y")
  
  mit$match_id <- paste0("mit",1:NROW(mit))
  mit$division <- "copa del rey"
  mit$div_id <- NA
  mit$team1_id <- NA
  mit$team2_id <- NA
  mit$attendance <- NA
  mit$team1[mit$team1=="Racing de Santander"] <- "Racing"
  mit$team2[mit$team2=="Racing de Santander"] <- "Racing"
  mit$team1 <- gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",mit$team1))
  mit$team2 <- gsub("^[A-Z]+ ","",gsub(" [A-Z]+$","",mit$team2))
  return(mit[duplicated(mit[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])==FALSE,
             c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])  
  
}

wgs84 = "+init=epsg:4326"
bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
+ellps=airy +datum=OSGB36 +units=m +no_defs'

ConvertCoordinates <- function(easting,northing) {
  out = cbind(easting,northing)
  mask = !is.na(easting)
  sp <-  sp::spTransform(sp::SpatialPoints(list(easting[mask],northing[mask]),proj4string=sp::CRS(bng)),sp::CRS(wgs84))
  out[mask,]=sp@coords
  out
}

load.eflt <- function() {
  eflt <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/eflt-data.csv",stringsAsFactors = FALSE)
  eflt$date <- as.Date(gsub("[.]html","",eflt$date))
  eflt <- eflt[is.na(eflt$date)==FALSE,]
  eflt <- eflt[eflt$comp!="Footnotes",]
  
  #team names
  eflt$team1 <- gsub("&\\S* ","and ",eflt$team1)
  eflt$team2 <- gsub("&\\S* ","and ",eflt$team2)
  eflt$team2 <- gsub("<a href=[.][.]/[.][.]/club/.*?[.]html>(.*?)</a>","\\1",eflt$team2)
  eflt$team2 <- gsub("<strong>(.*?)</strong>","\\1",eflt$team2)
  eflt$team1 <- gsub("<a href=[.][.]/[.][.]/club/.*?[.]html>(.*?)</a>","\\1",eflt$team1)
  eflt$team1 <- gsub("<strong>(.*?)</strong>","\\1",eflt$team1)
  
  eflt$team1 <- gsub("    "," ",eflt$team1)
  eflt$team2 <- gsub("    "," ",eflt$team2)
  eflt$team1 <- gsub("\t","",eflt$team1)
  eflt$team2 <- gsub("\t","",eflt$team2)
  eflt$team1 <- gsub("\\t","",eflt$team1)
  eflt$team2 <- gsub("\\t","",eflt$team2)
  eflt$team1 <- gsub("^\\s+|\\s+$","",eflt$team1)
  eflt$team2 <- gsub("^\\s+|\\s+$","",eflt$team2)
  eflt$team1[regexpr("Newport C",eflt$team1)>-1 & eflt$season<1990] <- "Newport County (old)"
  eflt$team2[regexpr("Newport C",eflt$team1)>-1 & eflt$season<1990] <- "Newport County (old)"
  
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  corse <- corr[,c("soccerbase.team","soccerbase.id","eflt")]
  corse <- corse[corse$soccerbase.team!="" & corse$eflt!="",]
  corse <- corse[is.na(corse$soccerbase.team)==FALSE & is.na(corse$eflt)==FALSE,]
  corse <- corse[duplicated(corse$eflt)==FALSE,]
  
  eflt <- merge(eflt,corse,by.x="team1",by.y="eflt",all.x=TRUE)
  eflt <- merge(eflt,corse,by.x="team2",by.y="eflt",all.x=TRUE,suffixes = c("1","2"))
  
  eflt$season <- as.numeric(gsub("^season-(\\d{4})-\\d+$","\\1",eflt$season))
  
  eflt$div_id <- NA
  eflt$div_id[is.na(eflt$div)==FALSE & eflt$div=="div1"] <- 1 + 4*as.numeric(eflt$season[is.na(eflt$div)==FALSE & eflt$div=="div1"]<=1991)
  eflt$div_id[is.na(eflt$div)==FALSE & eflt$div=="div2"] <- 2 + 4*as.numeric(eflt$season[is.na(eflt$div)==FALSE & eflt$div=="div2"]<=1991)
  eflt$div_id[is.na(eflt$div)==FALSE & eflt$div=="div3"] <- 3 + 4*as.numeric((eflt$season[is.na(eflt$div)==FALSE & eflt$div=="div3"]<=1991 & eflt$season[is.na(eflt$div)==FALSE & eflt$div=="div3"]>=1958) | eflt$season[is.na(eflt$div)==FALSE & eflt$div=="div3"]==1920)
  eflt$div_id[is.na(eflt$div)==FALSE & eflt$div=="div4"] <- 4 + 4*as.numeric(eflt$season[is.na(eflt$div)==FALSE & eflt$div=="div4"]<=1991 & eflt$season[is.na(eflt$div)==FALSE & eflt$div=="div4"]>=1958)
  eflt$div_id[is.na(eflt$div)==FALSE & eflt$div=="div3" & eflt$season<1958 & eflt$season>1920] <- 10#north
  eflt$div_id[is.na(eflt$div)==FALSE & eflt$div=="div4" & eflt$season<1958] <- 11#south
  eflt$comp <- gsub("^\\s+|\\s+$","",eflt$comp)
  eflt$div_id[is.na(eflt$div)==FALSE & eflt$div=="cup" & regexpr("^FA Cup",eflt$comp)>-1] <- 58
  eflt$div_id[is.na(eflt$div)==FALSE & eflt$div=="cup" & regexpr("^League Cup",eflt$comp)>-1] <- 60
  
  return(eflt)
}

load.english.attendances <- function() {
  require(zoo)
  eng.league.ids <- c(1:11,56,75,80,225:226)
  
  ##### loading results data from soccerbase #####
  res.sb <- load.soccerbase.results(region="england")  

#  facres <- load.facup.results()
#  old.slres <- loading.old.sl.results()
  ilres <- load.il.results()
  nplres <- load.npl.results()
  nlres <- load.nl.results()
  slres <- load.sl.results()
  
  sb.data <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/results-all-basic.csv",stringsAsFactors = FALSE)
  res0 <- merge(res.sb,sb.data[,c("match.id","attendance")],by.x=c("match_id"),by.y=c("match.id"),all.x=TRUE)
  
  eflt <- load.eflt()
  eflt <- eflt[is.na(as.numeric(eflt$goals1))==FALSE,]
  res0 <- res0[!(res0$div_id==80 & res0$season<2022),]
  res0 <- res0[!(res0$div_id==56 & res0$season<2022),]
  res0 <- res0[!(res0$div_id==75 & res0$season<2022),]
  res1 <- rbind(res0[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
                nlres[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
                ilres[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
                nplres[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
                slres[,c("match_id","season","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
  res1$season <- as.numeric(format(res1$date,"%Y")) - as.numeric(as.numeric(format(res1$date,"%m"))<7)
  res1$season[format(res1$date,"%Y")=="2020" & format(res1$date,"%m") %in% c("07","08")] <- 2019
  res0.att <- merge(res1,
                    eflt[is.na(eflt$div_id)==FALSE,c("soccerbase.team1","soccerbase.team2","soccerbase.id1","soccerbase.id2",
                            "season","div_id","attendance",
                            "venue","comp")],
                    by.x=c("team1_id","team2_id","season","div_id"),
                    by.y=c("soccerbase.id1","soccerbase.id2","season","div_id"),all.x=TRUE)

  #league tables
  ##league tables####  
  res.tab <- data.frame()
  res.final.tabs <- data.frame()
  outcomes.known.all <- data.frame()
  #  all.lg.pld <- data.frame()
  
  seasons <- unique(res0.att$season)
  seasons <- seasons[order(seasons)]

  res0.att <- res0.att[order(res0.att$date),]
  res0.att <- res0.att[duplicated(res0.att)==FALSE,]
  for( ss in seasons) { #[seasons>1949]
    print(ss)
    
    for( ll in eng.league.ids) {
      #if(ss==2019 & ll<5) { next }
      print(ll)
      matches <- res0.att[res0.att$season==ss & res0.att$div_id==ll,c("match_id","date","team1","goals1","goals2","team2")]
      matches <- matches[is.na(matches$match_id)==F,]
      if(NROW(matches)>10) {
        if(var(matches$date)==0) {
          matches$date <- matches$date + 1:NROW(matches)
        }
        lge.all <- league.tab(matches$match_id,as.Date(matches$date),matches$team1,
                              matches$goals1,matches$goals2,matches$team2,
                              2+as.numeric( (ss>1980 & ll %in% 1:11 ) | (ss>1994 & !(ll %in% 1:11) ) ))
        lge <- lge.all$matches
        
        final.lge.tab <- lge.all$final.table
        final.lge.tab$season <- ss
        final.lge.tab$div_id <- ll
        lg.pts <- data.frame(t(lge.all$points.grid[,-1]))
        colnames(lg.pts) <- paste0("match",1:NCOL(lg.pts))
        lg.pts$team <- rownames(lg.pts)
        lg.pts <- merge(lg.pts,final.lge.tab[,c("team","points")],all.x=TRUE)
        lg.pts <- lg.pts[order(lg.pts$points,decreasing = TRUE),]
        lg.pts.block <- matrix(NA,NROW(lg.pts),46+2-NCOL(lg.pts)) #need this to be 46 matches wide to bind
        if(NCOL(lg.pts)-1<=46) {
          colnames(lg.pts.block) <- paste0("match",c(NCOL(lg.pts)-1):46)
        }
        final.lge.tab <- cbind(final.lge.tab,lg.pts[,c(-1,-NCOL(lg.pts))],lg.pts.block)
        
        colnames(lge)[grep("game_id",colnames(lge))] <- "match_id"
        colnames(lge)[grep("g1",colnames(lge))] <- "goals1"
        colnames(lge)[grep("g2",colnames(lge))] <- "goals2"
        lge$season <- ss
        lge$div_id <- ll
        
        outcomes.known <- lge.all$outcomes.known.match
        outcomes.known$season <- ss
        outcomes.known$div_id <- ll
        # all.lg.pld <- rbind(all.lg.pld,lg.pld)
        res.tab <- rbind(res.tab,lge)
        outcomes.known.all <- rbind(outcomes.known,outcomes.known.all)
        res.final.tabs <- rbind(res.final.tabs,final.lge.tab)
      }
    }
  }
  
  res.tab <- res.tab[duplicated(res.tab$match_id)==F,]
  write.csv(res.tab,"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/league-tabs-1888-2023-09-19.csv")
  write.csv(res.final.tabs,"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/final-league-tabs-1888-2023-09-19.csv")
  write.csv(outcomes.known.all,"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/outcomes-known-1888-2023-09-19.csv")
  res0.att <- res0.att[duplicated(res0.att$match_id)==FALSE,]
  res0.att <- merge(res0.att,res.tab[,c("match_id","achieve1.1","achieve1.2",
                                        "achieveCL.1","achieveCL.2","achieveEL.1","achieveEL.2",
                                        "avoidREL.1","avoidREL.2","pts1","pts2",
                                        "pld1","pld2","wins1","wins2","draws1","draws2",
                                        "gs1","gs2","gd1","gd2","pos1","pos2","form1","form2",
                                        "points.from3.1","points.from3.2",
                                        "points.from6.1","points.from6.2")],by=c("match_id"),all.x=T)
  res0.att <- merge(res0.att,outcomes.known.all[,c("date","div_id","titlewinsneeded","CL","EL","REL",
                                                   "ISD","HHI")],
                    by=c("date","div_id"),all.x=TRUE)
  
  #elo
  elorank <- list()
  
  res0.att$elostrength.1 <- NA
  res0.att$elostrength.2 <- NA
  res0.att$elopredict <- NA
  
  res0.att <- res0.att[order(res0.att$date),]
  
  res0.att <- res0.att[is.na(res0.att$team1)==FALSE & is.na(res0.att$team2)==FALSE & res0.att$team1!="" & res0.att$team2!="",]
  
  res0.att$outcome <- 0.5*(res0.att$goals1==res0.att$goals2) + (res0.att$goals1>res0.att$goals2)
  
  res0.att <- res0.att[is.na(res0.att$team1)==FALSE,]
  
  for(mm in c(1:NROW(res0.att))) { 
    print(100*mm/NROW(res0.att))
    if(!(res0.att$team1[mm] %in% names(elorank))) {
      elorank[[res0.att$team1[mm]]] <- 1000
    }
    if(!(res0.att$team2[mm] %in% names(elorank))) {
      elorank[[res0.att$team2[mm]]] <- 1000
    }
    res0.att$elostrength.1[mm] <- elorank[[res0.att$team1[mm]]]
    res0.att$elostrength.2[mm] <- elorank[[res0.att$team2[mm]]]
    res0.att$elopredict[mm] <- 1/(1+(10^((res0.att$elostrength.2[mm]-res0.att$elostrength.1[mm])/400)))
    adjustment <- res0.att$outcome[mm] - res0.att$elopredict[mm]
    if(is.na(adjustment)==FALSE) {
      elorank[res0.att$team1[mm]] <- elorank[[res0.att$team1[mm]]] + 10*adjustment
      elorank[res0.att$team2[mm]] <- elorank[[res0.att$team2[mm]]] - 10*adjustment
    }
  }
  res0.att <- res0.att[is.na(res0.att$date)==FALSE,]

  res0.att$attendance <- res0.att$attendance.x
  res0.att$attendance[is.na(res0.att$attendance.x)==TRUE & is.na(res0.att$attendance.y)==FALSE] <- res0.att$attendance.y[is.na(res0.att$attendance.x)==TRUE & is.na(res0.att$attendance.y)==FALSE]
  
  write.csv(res0.att,"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/all-res-1888-2023-09-22.csv")
  
  res0.att$gd1pg[is.na(res0.att$pld1)==FALSE & res0.att$pld1>0] <- res0.att$gd1[is.na(res0.att$pld1)==FALSE & res0.att$pld1>0]/res0.att$pld1[is.na(res0.att$pld1)==FALSE & res0.att$pld1>0]
  res0.att$gd1pg[is.na(res0.att$pld1)==FALSE & res0.att$pld1==0] <- 0
  res0.att$gd2pg[is.na(res0.att$pld2)==FALSE & res0.att$pld2>0] <- res0.att$gd2[is.na(res0.att$pld2)==FALSE & res0.att$pld2>0]/res0.att$pld2[is.na(res0.att$pld2)==FALSE & res0.att$pld2>0]
  res0.att$gd2pg[is.na(res0.att$pld2)==FALSE & res0.att$pld2==0] <- 0
  res0.att$gs1pg[is.na(res0.att$pld1)==FALSE & res0.att$pld1>0] <- res0.att$gs1[is.na(res0.att$pld1)==FALSE & res0.att$pld1>0]/res0.att$pld1[is.na(res0.att$pld1)==FALSE & res0.att$pld1>0]
  res0.att$gs1pg[is.na(res0.att$pld1)==FALSE & res0.att$pld1==0] <- 0
  res0.att$gs2pg[is.na(res0.att$pld2)==FALSE & res0.att$pld2>0] <- res0.att$gs2[is.na(res0.att$pld2)==FALSE & res0.att$pld2>0]/res0.att$pld2[is.na(res0.att$pld2)==FALSE & res0.att$pld2>0]
  res0.att$gs2pg[is.na(res0.att$pld2)==FALSE & res0.att$pld2==0] <- 0
  res0.att$level <- as.numeric(res0.att$div_id %in% eng.league.ids) + as.numeric(res0.att$div_id==2 | res0.att$div_id==6) + 2*as.numeric(res0.att$div_id %in% c(3,7,10,11)) + 3*as.numeric(res0.att$div_id %in% c(4,8)) + 4*as.numeric(res0.att$div_id==9 | (res0.att$div_id %in% c(56,75,80) & res0.att$season<1979)) + 5*as.numeric(res0.att$div_id %in% c(225:226) | (res0.att$div_id %in% c(56,75,80) & res0.att$season>=1979 & res0.att$season<2004)) + 6*as.numeric(res0.att$div_id %in% c(56,75,80) & res0.att$season>=2004)
  res0.att$lattendance <- log(as.numeric(res0.att$attendance))
  
  #first split out quality from uncertainty by regressing the elo prediction on a range of variables
  summary(eloreg <- lm(elopredict ~ as.character(div_id) + pld1 + pld2 + pos1 + pos2 + gd1pg + gd2pg + gs1pg + gs2pg, data=res0.att, na.action=na.exclude))
  res0.att$elohat <- residuals(eloreg)
  res0.att$elohat2 <- res0.att$elohat^2
  
  ##run regressions looking at sensitivity of attendance to competitive balance measures
  ###1) match level via elo and elo^2
  summary(attreg <- lfe::felm(lattendance ~ elohat*as.character(level) + elohat2*as.character(level) + pos1 + pos2 + gd1pg + gd2pg | level + season + team1,data=res0.att[res0.att$attendance>0,]))
  ###2) in-season level via HHI  - merge in the weekly info
  ###3) previous seasonal level via last season HHI
  final.tabs.summary <- aggregate(res.final.tabs[,c("HHI","h1","h2","h3","h4","ISD")],
                                  by=list(res.final.tabs$div_id,res.final.tabs$season),
                                  FUN=mean)#need final season values (maybe mean too?)
  final.tabs.N <- aggregate(res.final.tabs$ISD/res.final.tabs$ISD,
                                  by=list(res.final.tabs$div_id,res.final.tabs$season),
                                  FUN=sum)#need final season values (maybe mean too?)
  ###4) inter-seasonal via number of champions of top flight
  top.flight.champs <- res.final.tabs[res.final.tabs$position==1 & (res.final.tabs$div_id==1 | res.final.tabs$div_id==5),c("season","team")]
  winners.last10 <- data.frame()
  for(ii in c(1:NROW(top.flight.champs))) {
    last10 <- top.flight.champs[max(0,c(ii-9)):ii,"team"]
    winners.last10 <- rbind(winners.last10,c(top.flight.champs$season[ii],NROW(table(last10))))
  }
  colnames(winners.last10) <- c("season","no.winners10")
  res0.att <- merge(res0.att,winners.last10,by="season",all.x=TRUE)
  
  ###want to also control for macroeconomic effects - unemployment, nominal GDP, inflation
  macro <- data.frame(readxl::read_excel("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/a-millennium-of-macroeconomic-data-for-the-uk (1).xlsx",
                                         sheet = "A1. Headline series"))
  colnames(macro) <- macro[3,]
  macro <- macro[-1:-6,]
  # pop <- data.frame(readxl::read_excel("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/a-millennium-of-macroeconomic-data-for-the-uk (1).xlsx",
  #                                      sheet = "A18. Population 1680+"))
  # inf <- 
  ##estimate over whole sample
  ##estimate over whole sample with team, division and season fixed effects
  ##estimate over decades
  ##estimate over decades with team and division FEs
  ##estimate over clubs
  
  
  ##season predicting
  season.predicting <- res.final.tabs
  season.predicting <- season.predicting[order(season.predicting$team,season.predicting$season),]
  season.predicting$team.1 <- c(NA,season.predicting$team[-NROW(season.predicting)])
  season.predicting$team.2 <- c(NA,season.predicting$team.1[-NROW(season.predicting)])
  season.predicting$season.1 <- c(NA,season.predicting$season[-NROW(season.predicting)])
  season.predicting$season.1[season.predicting$team.1!=season.predicting$team] <- NA
  season.predicting$season.2 <- c(NA,season.predicting$season.1[-NROW(season.predicting)])
  season.predicting$season.2[season.predicting$team.2!=season.predicting$team] <- NA
  season.predicting$team.p1 <- c(season.predicting$team[-1],NA)
  season.predicting$position.1 <- c(NA,season.predicting$position[-NROW(season.predicting)])
  season.predicting$position.1[season.predicting$team.1!=season.predicting$team] <- NA
  season.predicting$position.1[season.predicting$season>c(season.predicting$season.1+1)] <- NA
  season.predicting$level <- as.numeric(season.predicting$div_id %in% eng.league.ids) + as.numeric(season.predicting$div_id==2 | season.predicting$div_id==6) + 2*as.numeric(season.predicting$div_id %in% c(3,7,10,11)) + 3*as.numeric(season.predicting$div_id %in% c(4,8)) + 4*as.numeric(season.predicting$div_id==9 | (season.predicting$div_id %in% c(56,75,80) & season.predicting$season<1979)) + 5*as.numeric(season.predicting$div_id %in% c(225:226) | (season.predicting$div_id %in% c(56,75,80) & season.predicting$season>=1979 & season.predicting$season<2004)) + 6*as.numeric(season.predicting$div_id %in% c(56,75,80) & season.predicting$season>=2004)
  season.predicting$level.1 <- c(NA,season.predicting$level[-NROW(season.predicting)])
  season.predicting$level.1[season.predicting$team.1!=season.predicting$team] <- NA
  season.predicting$level.1[season.predicting$season>c(season.predicting$season.1+1)] <- NA
  season.predicting$level.2 <- c(NA,season.predicting$level.1[-NROW(season.predicting)])
  season.predicting$level.2[season.predicting$team.2!=season.predicting$team] <- NA
  season.predicting$level.2[season.predicting$season>c(season.predicting$season.2+2)] <- NA
  season.predicting$level.p1 <- c(season.predicting$level[-1],NA)
  season.predicting$level.p1[season.predicting$team.p1!=season.predicting$team] <- NA
  
  season.predicting$promotion <- as.numeric(season.predicting$level>season.predicting$level.p1)
  season.predicting$promotion.last <- as.numeric(season.predicting$level.1>season.predicting$level)
  season.predicting$promotion.2last <- as.numeric(season.predicting$level.2>season.predicting$level.1)
  season.predicting$relegation <- as.numeric(season.predicting$level<season.predicting$level.p1)
  season.predicting$relegation.last <- as.numeric(season.predicting$level.1<season.predicting$level)
  season.predicting$relegation.2last <- as.numeric(season.predicting$level.2<season.predicting$level.1)
  
  ##no. of matches
  season.predicting$no.matches <- rowSums(!is.na(season.predicting[,grep("match\\d+",colnames(season.predicting))]))
  no.matches <- sort(unique(season.predicting$no.matches[season.predicting$no.matches>=22]))
  season.predicting$ppg.h1 <- NA
  season.predicting$ppg.h2 <- NA
  for(nn in no.matches) {
    ##1st half of season
    season.predicting$ppg.h1[season.predicting$no.matches==nn] <- rowSums(season.predicting[season.predicting$no.matches==nn,paste0("match",1:round(nn/2))],na.rm=TRUE)/(nn/2)
    ##second half of season
    season.predicting$ppg.h2[season.predicting$no.matches==nn] <- rowSums(season.predicting[season.predicting$no.matches==nn,paste0("match",c(1+round(nn/2)):nn)],na.rm=TRUE)/(nn/2)
  }
  ##now lag for last season to see if it predicts...
  season.predicting <- season.predicting[order(season.predicting$team,season.predicting$season),]
  season.predicting$ppg.h1.1 <- c(NA,season.predicting$ppg.h1[-NROW(season.predicting)])
  season.predicting$ppg.h1.1[season.predicting$team.1!=season.predicting$team] <- NA
  season.predicting$ppg.h1.1[season.predicting$season>c(season.predicting$season.1+1)] <- NA
  season.predicting$ppg.h2.1 <- c(NA,season.predicting$ppg.h2[-NROW(season.predicting)])
  season.predicting$ppg.h2.1[season.predicting$team.1!=season.predicting$team] <- NA
  season.predicting$ppg.h2.1[season.predicting$season>c(season.predicting$season.1+1)] <- NA
  
  allTeams <- tolower(unlist(getTeams(season=2023)))
  
  #manager information
  managers <- load.managers()
  res.manager <- results.managers(results.data = res0.att,manager.data = managers)
  manager.season.stats <- aggregate(res.manager[,c("played","won","drawn","lost","goals","oppgoals")],by=list(res.manager$team,res.manager$season),FUN=sum)
  #want days and matches manager in position at start of each season...
  manager.seasons <- res.manager[is.na(res.manager$pld)==FALSE & res.manager$pld==0,c("coach","team","season","Date.Started","date","spell.length","total.experience","tenure.days")]
  manager.seasons <- manager.seasons[duplicated(manager.seasons[,c("team","season")])==FALSE,]
  
  season.predicting$team <- tolower(season.predicting$team)
  season.predicting <- merge(season.predicting,manager.seasons,by=c("team","season"),all.x=TRUE)
  
  #squad info
  squads <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/squads-sb.csv",stringsAsFactors = FALSE)
  #squads <- squads[regexpr("\\d{4}-\\d{2}-\\d{2}",squads$filename)==-1,]

  collection.dates <- sort(unique(gsub("^.*?(\\d{4}-\\d{2}-\\d{2}).*?$","\\1",squads$filename[regexpr("\\d{4}-\\d{2}-\\d{2}",squads$filename)>-1])))
  for(dd in collection.dates[-NROW(collection.dates)]) {
    squads <- squads[regexpr(dd,squads$filename)==-1,]
  }
  
  #squads <- squads[regexpr("155",squads$filename)==-1,]
  # squads$filename <- gsub("2021-08-23","154",squads$filename)
  squads$filename <- gsub(collection.dates[NROW(collection.dates)],"156",squads$filename)
  
  squads$team <- tolower(gsub("^(.*?)_(\\d+)[.]html$","\\1",squads$filename))
  squads <- squads[squads$team!="afc w'don" & squads$team!="afc wimbledon" & squads$team!="c palace" & squads$team!="middlesbro" & squads$team!="erzegbirge a",]
  squads$season <- paste0(1870+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))>145),"-",
                          1871+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))>145))
  squads$season0 <- as.numeric(1870+as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))-3*as.numeric(as.numeric(gsub("^(.*?)_(\\d+)[.]html$","\\2",squads$filename))>145))
  squads <- squads[order(squads$season),]
  
  squads$league.apps[is.na(squads$league.apps)==TRUE] <- "0 (0)"
  squads$facup.apps[is.na(squads$facup.apps)==TRUE] <- "0 (0)"
  squads$lgecup.apps[is.na(squads$lgecup.apps)==TRUE] <- "0 (0)"
  squads$other.apps[is.na(squads$other.apps)==TRUE] <- "0 (0)"
  squads$league.goals[is.na(squads$league.goals)==TRUE] <- 0
  squads$facup.goals[is.na(squads$facup.goals)==TRUE] <- 0
  squads$lgecup.goals[is.na(squads$lgecup.goals)==TRUE] <- 0
  squads$other.goals[is.na(squads$other.goals)==TRUE] <- 0
  squads$total.goals <- squads$league.goals + squads$facup.goals + squads$lgecup.goals + squads$other.goals
  
  squads$player.position <- gsub("^(.*?) [(](.*?)[)]","\\2",squads$player.name)
  squads$surname <- gsub("^(.*?) (.*?) [(](.*?)[)]","\\2",squads$player.name)
  
  squads$league.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$league.apps))
  squads$league.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$league.apps))
  squads$facup.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$facup.apps))
  squads$facup.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$facup.apps))
  squads$lgecup.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$lgecup.apps))
  squads$lgecup.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$lgecup.apps))
  squads$other.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",squads$other.apps))
  squads$other.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",squads$other.apps))
  
  squads$total.apps <- squads$league.apps + squads$facup.apps + squads$lgecup.apps + squads$other.apps
  squads$total.sub.apps <- squads$league.sub.apps + squads$facup.sub.apps + squads$lgecup.sub.apps + squads$other.sub.apps
  squads$total.total.apps <- squads$total.apps + squads$total.sub.apps
  
  all.players <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/all-players-sb.csv",stringsAsFactors = FALSE)
  
  all.player.careers <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/soccerbase/all-player-careers-sb.csv",stringsAsFactors = FALSE)
  #want to cumulate appearances by player so that it's appearances/goals at start of each season
  all.player.careers$league.apps[is.na(all.player.careers$league.apps)==TRUE] <- "0 (0)"
  all.player.careers$facup.apps[is.na(all.player.careers$facup.apps)==TRUE] <- "0 (0)"
  all.player.careers$lgecup.apps[is.na(all.player.careers$lgecup.apps)==TRUE] <- "0 (0)"
  all.player.careers$other.apps[is.na(all.player.careers$other.apps)==TRUE] <- "0 (0)"
  all.player.careers$league.goals[is.na(all.player.careers$league.goals)==TRUE] <- 0
  all.player.careers$facup.goals[is.na(all.player.careers$facup.goals)==TRUE] <- 0
  all.player.careers$lgecup.goals[is.na(all.player.careers$lgecup.goals)==TRUE] <- 0
  all.player.careers$other.goals[is.na(all.player.careers$other.goals)==TRUE] <- 0
  all.player.careers$total.goals <- all.player.careers$league.goals + all.player.careers$facup.goals + all.player.careers$lgecup.goals + all.player.careers$other.goals
  all.player.careers$league.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",all.player.careers$league.apps))
  all.player.careers$league.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",all.player.careers$league.apps))
  all.player.careers$facup.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",all.player.careers$facup.apps))
  all.player.careers$facup.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",all.player.careers$facup.apps))
  all.player.careers$lgecup.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",all.player.careers$lgecup.apps))
  all.player.careers$lgecup.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",all.player.careers$lgecup.apps))
  all.player.careers$other.sub.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\2",all.player.careers$other.apps))
  all.player.careers$other.apps <- as.numeric(gsub("^(\\d+) [(](\\d+)[)]","\\1",all.player.careers$other.apps))
  
  all.player.careers$total.apps <- all.player.careers$league.apps + all.player.careers$facup.apps + all.player.careers$lgecup.apps + all.player.careers$other.apps
  all.player.careers$total.sub.apps <- all.player.careers$league.sub.apps + all.player.careers$facup.sub.apps + all.player.careers$lgecup.sub.apps + all.player.careers$other.sub.apps
  all.player.careers$total.total.apps <- all.player.careers$total.apps + all.player.careers$total.sub.apps
  
  squads <- merge(squads,all.players[,c("player.id","dob")],by="player.id",all.x=TRUE)
  squads$age <- as.numeric(as.Date(paste0(squads$season0,"-08-01")) - squads$dob)/365.25
  squads <- squads[order(squads$player.id,squads$season0),]
  squads$cumulative.total.apps <- ave(squads$total.total.apps,squads$player.id,FUN=cumsum) - squads$total.total.apps
  squads$cumulative.total.goals <- ave(squads$total.goals,squads$player.id,FUN=cumsum) - squads$total.goals
  ##want for teams:
  ##1) squad size
  ##2) mean age of squad (from player DOB)
  ##3) mean experience of squad (need player career stage for this)
  ##4) mean goals per game of squad
  season.squads <- aggregate(squads[,c("age","cumulative.total.apps","cumulative.total.goals")],
                             by=list(squads$team,squads$season0),FUN=mean,na.rm=TRUE)
  squads$const <- 1
  season.squads.N <- aggregate(squads$const,by=list(squads$team,squads$season0),FUN=sum)
  
  all.players$position <- gsub("^\\s+|\\s+$","",gsub("^\\s*(\\S+)\\s*<.*?$","\\1",all.players$pos))
  
  all.players$surname <- gsub("^.*? (\\w+)$","\\1",all.players$player.name)
  all.players$player.id <- as.numeric(gsub("[.]html","",all.players$player.id))
  
  all.players$dob <- as.Date(all.players$dob,"%d %b, %Y")
  all.players$dob.m.d <- format(all.players$dob,"%m-%d")
  
  all.players$age <- paste0(floor((Sys.Date() - all.players$dob)/365.25)," years and ",round(((Sys.Date() - all.players$dob)/365.25-floor((Sys.Date() - all.players$dob)/365.25))*365)," days")
  all.players$squad.no <- as.numeric(gsub("^(\\d+).*?$","\\1",all.players$player.name))
  all.players <- all.players[order(all.players$squad.no),]
  
  all.player.careers$surname <- gsub("^.*? (\\w+)$","\\1",all.player.careers$player.name)
  all.player.careers$player.id <- as.numeric(gsub("[.]html","",all.player.careers$player.id))
  
  all.player.careers$dob <- as.Date(all.player.careers$dob,"%d %b, %Y")
  all.player.careers$dob.m.d <- format(all.player.careers$dob,"%m-%d")
  
  all.player.careers$age <- paste0(floor((Sys.Date() - all.player.careers$dob)/365.25)," years and ",round(((Sys.Date() - all.player.careers$dob)/365.25-floor((Sys.Date() - all.player.careers$dob)/365.25))*365)," days")
  all.player.careers$squad.no <- as.numeric(gsub("^(\\d+).*?$","\\1",all.player.careers$player.name))
  all.player.careers <- all.player.careers[order(all.player.careers$squad.no),]
  
  library(fixest)
  
  summary(pos.reg1 <- feols(position ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last | season + level,data=season.predicting))
  summary(pro.reg1 <- feols(promotion ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last | season + level,data=season.predicting))
  
  season.predicting$tenure.days2 <- season.predicting$tenure.days^2
  season.predicting$spell.length2 <- season.predicting$spell.length^2
  
  summary(pos.reg2 <- feols(position ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 | season + level,data=season.predicting))
  summary(pro.reg2 <- feols(promotion ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 | season + level,data=season.predicting))

  summary(pos.reg3 <- feols(position ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 + ppg.h1.1 + ppg.h2.1 | season + level,data=season.predicting))
  summary(pro.reg3 <- feols(promotion ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 + ppg.h1.1 + ppg.h2.1 | season + level,data=season.predicting))
  
  transfers <- load.transfers()

  transfers$year <- as.numeric(format(as.Date(transfers$date),"%Y"))
  transfers <- merge(transfers,all.players[,c("player.id","dob","position","nationality")],
                     by="player.id",all.x=TRUE)
  #want transfer activity in off season (May to August)
  summer.transfers <- aggregate(transfers[c("players.in","players.out","loan.in","loan.out",
                                            "free.in","free.out","undisc.in","undisc.out",
                                            "releases","fees.paid","fees.received")],
                                by=list(transfers$team,transfers$year),FUN=sum)
  #summer.transfers <- summer.transfers[duplicated(summer.transfers[,c("Group.1","Group.2")])==FALSE,]
  season.predicting <- merge(season.predicting,summer.transfers,by.x=c("team","season"),
                             by.y=c("Group.1","Group.2"),all.x=TRUE)
  season.predicting$fees.paid.m <- season.predicting$fees.paid/1000000
  season.predicting$fees.received.m <- season.predicting$fees.received/1000000
  
  summary(pos.reg4 <- feols(position ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 + ppg.h1.1 + ppg.h2.1 + players.in + players.out + free.in + free.out + fees.paid.m + fees.received.m | season + level,data=season.predicting))
  summary(pro.reg4 <- feols(promotion ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 + ppg.h1.1 + ppg.h2.1 + players.in + players.out + free.in + free.out + fees.paid.m + fees.received.m | season + level,data=season.predicting))

  summary(pro.reg4lm <- lm(promotion ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 + ppg.h1.1 + ppg.h2.1 + players.in + players.out + free.in + free.out + fees.paid.m + fees.received.m + as.character(level),data=season.predicting))
  
  season.predicting <- merge(season.predicting,season.squads,by.x=c("team","season"),
                             by.y=c("Group.1","Group.2"),all.x=TRUE)
  colnames(season.squads.N) <- c("Group.1","Group.2","squad.size")
  season.predicting <- merge(season.predicting,season.squads.N,by.x=c("team","season"),
                             by.y=c("Group.1","Group.2"),all.x=TRUE)
  season.predicting$squad.size.2 <- season.predicting$squad.size^2
  summary(pos.reg5 <- feols(position ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 + ppg.h1.1 + ppg.h2.1 + players.in + players.out + free.in + free.out + fees.paid.m + fees.received.m + age + cumulative.total.apps + cumulative.total.goals + squad.size + squad.size.2 | season + level,data=season.predicting))
  summary(pro.reg5 <- feols(promotion ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 + ppg.h1.1 + ppg.h2.1 + players.in + players.out + free.in + free.out + fees.paid.m + fees.received.m + age + cumulative.total.apps + cumulative.total.goals + squad.size + squad.size.2 | season + level,data=season.predicting))
  summary(pro.reg5lm <- lm(promotion ~ position.1 + promotion.last + promotion.2last + relegation.last + relegation.2last + tenure.days + tenure.days2 + spell.length + spell.length2 + ppg.h1.1 + ppg.h2.1 + players.in + players.out + free.in + free.out + fees.paid.m + fees.received.m + age + cumulative.total.apps + cumulative.total.goals + squad.size + squad.size.2 + as.character(level),data=season.predicting))
  
  stargazer::stargazer(pos.reg1,pos.reg2,pos.reg3,pos.reg4,pos.reg5)
  
  promotion.2023 <- cbind(season.predicting[season.predicting$season==2023,c("team","div_id")],predict(pro.reg4lm,newdata = season.predicting[season.predicting$season==2023,]))
  promotion.2023[promotion.2023$div_id==9,]
  
  ##modelling match outcomes####
  res0.att$team1 <- tolower(res0.att$team1)
  res0.att$team2 <- tolower(res0.att$team2)
  res.att2 <- merge(res0.att,season.squads,by.x=c("team1","season"),by.y=c("Group.1","Group.2"),all.x=TRUE)
  res.att2 <- merge(res.att2,season.squads,by.x=c("team2","season"),by.y=c("Group.1","Group.2"),all.x=TRUE,suffixes = c(".1",".2"))
  res.att2 <- merge(res.att2,summer.transfers,by.x=c("team1","season"),by.y=c("Group.1","Group.2"),all.x=TRUE)
  res.att2 <- merge(res.att2,summer.transfers,by.x=c("team2","season"),by.y=c("Group.1","Group.2"),all.x=TRUE,suffixes = c(".1",".2"))
  res.att2 <- merge(res.att2,res.manager[,c("team","match_id","coach","tenure.days","cplayed")],
                    by.x=c("team1","match_id"),by.y=c("team","match_id"),all.x=TRUE)
  res.att2 <- merge(res.att2,res.manager[,c("team","match_id","coach","tenure.days","cplayed")],
                    by.x=c("team2","match_id"),by.y=c("team","match_id"),all.x=TRUE,suffixes = c(".1",".2"))
  
  res.att2 <- merge(res.att2,season.predicting[,c("team","season","promotion.last","relegation.last"),],
                    by.x=c("team1","season"),by.y=c("team","season"),all.x=TRUE)
  res.att2 <- merge(res.att2,season.predicting[,c("team","season","promotion.last","relegation.last"),],
                    by.x=c("team2","season"),by.y=c("team","season"),all.x=TRUE,suffixes = c(".1",".2"))
  
  res.att2$goals1 <- as.numeric(res.att2$goals1)
  res.att2$goals2 <- as.numeric(res.att2$goals2)
  summary(m1d.0 <- glm(goals1 ~ elostrength.1 + elostrength.2 + form1 + form2 + gd1pg + gd2pg + gs1pg + gs2pg + age.1 + age.2 + cumulative.total.apps.1 + cumulative.total.apps.2 + cumulative.total.goals.1 + cumulative.total.goals.2 + cplayed.1 + cplayed.2 + players.in.1 + players.out.1 + promotion.last.1 + relegation.last.1 + promotion.last.2 + relegation.last.2, family="poisson", data=res.att2, na.action=na.exclude))
  summary(m2d.0 <- glm(goals2 ~ elostrength.1 + elostrength.2 + form1 + form2 + gd1pg + gd2pg + gs1pg + gs2pg + age.1 + age.2 + cumulative.total.apps.1 + cumulative.total.apps.2 + cumulative.total.goals.1 + cumulative.total.goals.2 + cplayed.1 + cplayed.2 + players.in.1 + players.out.1 + promotion.last.1 + relegation.last.1 + promotion.last.2 + relegation.last.2, family="poisson", data=res.att2, na.action=na.exclude))
}

# create.top6divs.results <- function() {
#   require(zoo)
#   eng.league.ids <- c(1:11,56,75,80,225:226)
#   
#   ##### loading results data from soccerbase #####
#   res.sb <- load.soccerbase.results(region="england")  
#   
#   ##loading various other sources of results (non-soccerbase) ####
#   ilres <- load.il.results()
#   nplres <- load.npl.results()
#   nlres <- load.nl.results()
#   slres <- load.sl.results()
#   
#   facres <- load.facup.results()
#   old.slres <- loading.old.sl.results()
#   
#   sb.data <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/results-all-basic.csv",stringsAsFactors = FALSE)
#   res0 <- merge(res.sb,sb.data[,c("match.id","attendance")],by.x=c("match_id"),by.y=c("match.id"),all.x=TRUE)
#   
#   ##merge in footballwebpages teamnames, save to update english football home/away attendances ####
#   corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
#   corsf <- corr[,c("soccerbase.team","footballwebpages.teams")]
#   corsf <- corsf[duplicated(corsf)==FALSE,]
#   corsf <- corsf[corsf$soccerbase.team!="",]
#   corsf <- corsf[duplicated(corsf$soccerbase.team)==FALSE,]
#   
#   ##fix middlesbrough, crystal palace, afc wimbledon, dorking wanders and dagenham and redbridge
#   res0$team1[res0$team1=="Peterboro"] <- "Peterborough"
#   res0$team1[res0$team1=="Middlesbrough"] <- "Middlesbro"
#   res0$team1[res0$team1=="Crystal Palace"] <- "C Palace"
#   res0$team1[res0$team1=="AFC W'bledon"] <- "AFC Wimbledon"
#   res0$team1[res0$team1=="Dorking W"] <- "Dorking Wanderers"
#   res0$team1[res0$team1=="Dag &amp; Red"] <- "Dag and Red"
#   res0$team2[res0$team2=="Peterboro"] <- "Peterborough"
#   res0$team2[res0$team2=="Middlesbrough"] <- "Middlesbro"
#   res0$team2[res0$team2=="Crystal Palace"] <- "C Palace"
#   res0$team2[res0$team2=="AFC W'bledon"] <- "AFC Wimbledon"
#   res0$team2[res0$team2=="Dorking W"] <- "Dorking Wanderers"
#   res0$team2[res0$team2=="Dag &amp; Red"] <- "Dag and Red"
# 
#   #geographic centre of top flight
#   corr$location1.0 <- corr$location1.8
#   corr$location1.0[is.na(corr$location1.0)==TRUE] <- corr$location1.7[is.na(corr$location1.0)==TRUE]
#   corr$location1.0[is.na(corr$location1.0)==TRUE] <- corr$location1.6[is.na(corr$location1.0)==TRUE]
#   corr$location1.0[is.na(corr$location1.0)==TRUE] <- corr$location1.5[is.na(corr$location1.0)==TRUE]
#   corr$location1.0[is.na(corr$location1.0)==TRUE] <- corr$location1.4[is.na(corr$location1.0)==TRUE]
#   corr$location1.0[is.na(corr$location1.0)==TRUE] <- corr$location1.3[is.na(corr$location1.0)==TRUE]
#   corr$location1.0[is.na(corr$location1.0)==TRUE] <- corr$location1.2[is.na(corr$location1.0)==TRUE]
#   corr$location1.0[is.na(corr$location1.0)==TRUE] <- corr$location1[is.na(corr$location1.0)==TRUE]
#   corr$location2.0 <- corr$location2.8
#   corr$location2.0[is.na(corr$location2.0)==TRUE] <- corr$location2.7[is.na(corr$location2.0)==TRUE]
#   corr$location2.0[is.na(corr$location2.0)==TRUE] <- corr$location2.6[is.na(corr$location2.0)==TRUE]
#   corr$location2.0[is.na(corr$location2.0)==TRUE] <- corr$location2.5[is.na(corr$location2.0)==TRUE]
#   corr$location2.0[is.na(corr$location2.0)==TRUE] <- corr$location2.4[is.na(corr$location2.0)==TRUE]
#   corr$location2.0[is.na(corr$location2.0)==TRUE] <- corr$location2.3[is.na(corr$location2.0)==TRUE]
#   corr$location2.0[is.na(corr$location2.0)==TRUE] <- corr$location2.2[is.na(corr$location2.0)==TRUE]
#   corr$location2.0[is.na(corr$location2.0)==TRUE] <- corr$location2[is.na(corr$location2.0)==TRUE]
#   
#   locations <- corr[,c("soccerbase.team","footballwebpages.teams","location1.0","location2.0")]
#   locations <- locations[duplicated(locations)==FALSE,]
#   locations <- locations[locations$soccerbase.team!="",]
#   locations <- locations[locations$footballwebpages.teams!="",]
#   locations <- locations[duplicated(locations$soccerbase.team)==FALSE,]
#   locations <- locations[locations$location1.0!="",]
#   locations <- locations[is.na(locations$location1.0)==FALSE,]
#   locations$soccerbase.team <- tolower(locations$soccerbase.team)
#   colnames(locations) <- c("team","nlm.team","location1","location2")
#   
#   res0.il <- res0[res0$div_id==80 & res0$season<2022,]
#   res0 <- res0[!(res0$div_id==80 & res0$season<2022),]
#   res0.npl <- res0[res0$div_id==56 & res0$season<2022,]
#   res0 <- res0[!(res0$div_id==56 & res0$season<2022),]
#   res0.sl <- res0[res0$div_id==75 & res0$season<2022,]
#   res0 <- res0[!(res0$div_id==75 & res0$season<2022),]
#   all.res0 <- rbind(res0[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
#                     nlres,ilres,nplres,slres,facres,old.slres)
#                     #,wyc,weald,weym,sfc,bfc,bufc,
#                     #hist.res[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
#   all.res0$season <- as.numeric(format(all.res0$date,"%Y")) - as.numeric(as.numeric(format(all.res0$date,"%m"))<7)
# 
#   all.res0 <- all.res0[is.na(all.res0$goals1)==FALSE,]
#   #want to remove duplicates though only those that have an attendance==NA if other duplicate has attendance
#   all.res0 <- all.res0[order(all.res0$team1,all.res0$team2,all.res0$date,all.res0$attendance),]
#   all.res0 <- all.res0[duplicated(all.res0[,c("team1","team2","date")])==FALSE,]
#   #just get rid of the rest of the duplicates now
#   all.res0 <- all.res0[duplicated(all.res0$match_id)==FALSE,]
#   
#   #removing playoff matches - not cup matches where team1, team2 and season is duplicated
#   all.res0 <- all.res0[!(all.res0$div_id %in% eng.league.ids & duplicated(all.res0[,c("team1","team2","season","div_id")])),]
#   
#   #fill in where NAs for team IDs
#   all.res0 <- all.res0[order(all.res0$team1,all.res0$date,decreasing = TRUE),]
#   #put an ID in where team has no ID
#   all_team_ids1 <- all.res0[,c("team1","team1_id")]
#   all_team_ids2 <- all.res0[,c("team2","team2_id")]
#   colnames(all_team_ids1) <- c("team","team_id")
#   colnames(all_team_ids2) <- c("team","team_id")
#   all_team_ids <- rbind(all_team_ids1,all_team_ids2)
#   all_team_ids <- all_team_ids[is.na(all_team_ids$team_id)==FALSE,]
#   all_team_ids <- all_team_ids[duplicated(all_team_ids)==FALSE,]
#   all_team_ids <- all_team_ids[duplicated(all_team_ids$team)==FALSE,] #dissatisfyingly there are multiple team IDs for a team in soccerbase (e.g. Palace, Hereford). This is a temporary fix for now.
#   
#   all_team <- data.frame("team"=c(all.res0[,c("team1")],all.res0[,c("team2")]),stringsAsFactors = FALSE)
#   all_team <- all_team[is.na(all_team)==FALSE,]
#   all_team <- data.frame("team"=all_team[duplicated(all_team)==FALSE])
#   
#   all_team <- merge(all_team,all_team_ids,by="team",all.x=TRUE)
#   
#   #assign an ID to all teams without an ID
#   all_team$team_id[is.na(all_team$team_id)==TRUE] <- seq(7416,length=2605)
#   
#   colnames(all_team) <- c("team","team1_id")
#   all.res0 <- merge(all.res0,all_team,by.x="team1",by.y="team",all.x=TRUE,suffixes = c(".old",""))
#   colnames(all_team) <- c("team","team2_id")
#   all.res0 <- merge(all.res0,all_team,by.x="team2",by.y="team",all.x=TRUE,suffixes = c(".old",""))
#   
#   ##league tables####  
#   res.tab <- data.frame()
#   res.final.tabs <- data.frame()
# #  all.lg.pld <- data.frame()
#   
#   all.res0$season <- as.numeric(format(all.res0$date,"%Y")) - as.numeric(as.numeric(format(all.res0$date,"%m"))<7)
#   all.res0$season[format(all.res0$date,"%Y")=="2020" & format(all.res0$date,"%m") %in% c("07","08")] <- 2019
#   seasons <- unique(all.res0$season)
#   seasons <- seasons[order(seasons)]
#   eng.league.ids <- c(1:11,56,75,80,225,226)
# #  seasons = 2004:2022
# #  eng.league.ids = 225:226
#   all.res0 <- all.res0[order(all.res0$date),]
#   for( ss in seasons) { #[seasons>1949]
#     print(ss)
#     
#     for( ll in eng.league.ids) {
#       #if(ss==2019 & ll<5) { next }
#       print(ll)
#       matches <- all.res0[all.res0$season==ss & all.res0$div_id==ll,c("match_id","date","team1","goals1","goals2","team2")]
#       matches <- matches[is.na(matches$match_id)==F,]
#       if(NROW(matches)>10) {
#         if(var(matches$date)==0) {
#           matches$date <- matches$date + 1:NROW(matches)
#         }
#         lge.all <- league.tab(matches$match_id,as.Date(matches$date),matches$team1,
#                               matches$goals1,matches$goals2,matches$team2,
#                               2+as.numeric( (ss>1980 & ll %in% 1:11 ) | (ss>1994 & !(ll %in% 1:11) ) ))
#         lge <- lge.all$matches
# 
#         final.lge.tab <- lge.all$final.table
#         final.lge.tab$season <- ss
#         final.lge.tab$div_id <- ll
#         lg.pts <- data.frame(t(lge.all$points.grid[,-1]))
#         colnames(lg.pts) <- paste0("match",1:NCOL(lg.pts))
#         lg.pts$team <- rownames(lg.pts)
#         lg.pts <- merge(lg.pts,final.lge.tab[,c("team","points")],all.x=TRUE)
#         lg.pts <- lg.pts[order(lg.pts$points,decreasing = TRUE),]
#         lg.pts.block <- matrix(NA,NROW(lg.pts),46+2-NCOL(lg.pts)) #need this to be 46 matches wide to bind
#         if(NCOL(lg.pts)-1<=46) {
#           colnames(lg.pts.block) <- paste0("match",c(NCOL(lg.pts)-1):46)
#         }
#         final.lge.tab <- cbind(final.lge.tab,lg.pts[,c(-1,-NCOL(lg.pts))],lg.pts.block)
# 
#         colnames(lge)[grep("game_id",colnames(lge))] <- "match_id"
#         colnames(lge)[grep("g1",colnames(lge))] <- "goals1"
#         colnames(lge)[grep("g2",colnames(lge))] <- "goals2"
#         lge$season <- ss
#         lge$div_id <- ll
#         # all.lg.pld <- rbind(all.lg.pld,lg.pld)
#         res.tab <- rbind(res.tab,lge)
#         res.final.tabs <- rbind(res.final.tabs,final.lge.tab)
#       }
#     }
#   }
#   
#   res.tab <- res.tab[duplicated(res.tab$match_id)==F,]
#   all.res0 <- all.res0[duplicated(all.res0$match_id)==FALSE,]
#   all.res0 <- merge(all.res0,res.tab[,c(1,7:22)],by=c("match_id"),all.x=T)
#   
#   season.leagues0 <- res.final.tabs[duplicated(res.final.tabs[,c("season","div_id")])==FALSE,c("season","div_id","ISD","HHI","h1","h2","h3","h4")]
#   res.final.tabs$const <- 1
#   season.leagues.N <- aggregate(res.final.tabs$const,by=list(res.final.tabs$season,res.final.tabs$div_id),FUN=sum)
#   season.leagues0 <- merge(season.leagues0,season.leagues.N,by.x=c("season","div_id"),
#                            by.y=c("Group.1","Group.2"),all.x=TRUE)
#   season.leagues0$HHIn <- season.leagues0$HHI/(100^2) - 1/season.leagues0$x
#   
#   plot(res.final.tabs$season[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5)],res.final.tabs$ISD[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5)],pch=16,cex=0.5,type="o",main="Idealised Standard Deviation - Div1/EPL",ylab="Idealised Standard Deviation",xlab="Season (year ending)")
# 
#   plot(season.leagues0$season[(season.leagues0$div_id==1 | season.leagues0$div_id==5)],
#        season.leagues0$HHIn[(season.leagues0$div_id==1 | season.leagues0$div_id==5)],pch=16,cex=0.5,
#        type="o",main="HHIn - Div1/EPL",ylab="Idealised Standard Deviation",xlab="Season (year ending)")
#   
#   res.final.tabs$points.prop <- res.final.tabs$points/(3*res.final.tabs$played)
#   
#   jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Attendance/tex/point-props-",Sys.Date(),".jpg"),
#        width = 8, height = 5, units = "in", res=300)
#   plot(res.final.tabs$season[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5)],res.final.tabs$points.prop[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5)],pch=16,cex=0.5,main="Proportion of points won",ylab="Proportion of points",xlab="Season (year ending)")
#   lines(res.final.tabs$season[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Man City"],res.final.tabs$points.prop[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Man City"],pch=16,cex=0.75,col="lightblue",type="p")
#   lines(res.final.tabs$season[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Liverpool"],res.final.tabs$points.prop[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Liverpool"],pch=17,cex=0.75,col="red1",type="p")
#   lines(res.final.tabs$season[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Man Utd"],res.final.tabs$points.prop[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Man Utd"],pch=18,cex=0.75,col="red3",type="p")
#   lines(res.final.tabs$season[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Arsenal"],res.final.tabs$points.prop[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Arsenal"],pch=19,cex=0.75,col="red4",type="p")
#   lines(res.final.tabs$season[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Chelsea"],res.final.tabs$points.prop[(res.final.tabs$div_id==1 | res.final.tabs$div_id==5) & res.final.tabs$team=="Chelsea"],pch=16,cex=0.75,col="blue",type="p")
#   legend("bottomleft",col=c("lightblue","blue","red1","red3","red4","black"),
#          pch=c(16,16,17,18,19,16),legend=c("MCI","CHE","LIV","MU","ARS","rest"),bty="n",
#          ncol=6)
#   dev.off()
#   
#   jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Attendance/tex/top1-prop-divs-",Sys.Date(),".jpg"),
#        width = 8, height = 5, units = "in", res=300)
#   plot(season.leagues0$season[(season.leagues0$div_id==1 | season.leagues0$div_id==5)],
#        season.leagues0$h1[(season.leagues0$div_id==1 | season.leagues0$div_id==5)],pch=16,cex=0.5,
#        type="o",main="Proportion of points won by top team",ylab="Proportion of points",
#        xlab="Season (year ending)",ylim=range(season.leagues0$h1[season.leagues0$div_id<10]))
#   lines(season.leagues0$season[(season.leagues0$div_id==2 | season.leagues0$div_id==6)],
#         season.leagues0$h1[(season.leagues0$div_id==2 | season.leagues0$div_id==6)],pch=16,cex=0.5,type="o",col=2)
#   lines(season.leagues0$season[(season.leagues0$div_id==3 | season.leagues0$div_id==7)],
#         season.leagues0$h1[(season.leagues0$div_id==3 | season.leagues0$div_id==7)],pch=16,cex=0.5,type="o",col=3)
#   lines(season.leagues0$season[(season.leagues0$div_id==4 | season.leagues0$div_id==8)],
#         season.leagues0$h1[(season.leagues0$div_id==4 | season.leagues0$div_id==8)],pch=16,cex=0.5,type="o",col=4)
#   legend("topright",legend=paste0("Div",1:4),col=1:4,pch=16,lty=1,bty="n")
#   dev.off()
#   
#   plot(season.leagues0$season[(season.leagues0$div_id==1 | season.leagues0$div_id==5)],
#        season.leagues0$HHIn[(season.leagues0$div_id==1 | season.leagues0$div_id==5)],pch=16,cex=0.5,
#        type="o",main="HHIn - Divs 1-4",ylab="Idealised Standard Deviation",xlab="Season (year ending)")
#   lines(season.leagues0$season[(season.leagues0$div_id==2 | season.leagues0$div_id==6)],
#         season.leagues0$HHIn[(season.leagues0$div_id==2 | season.leagues0$div_id==6)],pch=16,cex=0.5,type="o",col=2)
#   lines(season.leagues0$season[(season.leagues0$div_id==3 | season.leagues0$div_id==7)],
#         season.leagues0$HHIn[(season.leagues0$div_id==3 | season.leagues0$div_id==7)],pch=16,cex=0.5,type="o",col=3)
#   lines(season.leagues0$season[(season.leagues0$div_id==4 | season.leagues0$div_id==8)],
#         season.leagues0$HHIn[(season.leagues0$div_id==4 | season.leagues0$div_id==8)],pch=16,cex=0.5,type="o",col=4)
#   
#   #want all the EFL teams that got relegated to look at their "average" position
#   #want promoted and relegated sides
#   res.final.tabs$tier <- res.final.tabs$div_id
#   res.final.tabs$tier[res.final.tabs$div_id>=5 & res.final.tabs$div_id<9] <- res.final.tabs$tier[res.final.tabs$div_id>=5 & res.final.tabs$div_id<9]-4
#   res.final.tabs$tier[res.final.tabs$div_id>=10 & res.final.tabs$div_id<11] <- 3
# 
#   res.final.tabs <- res.final.tabs[order(res.final.tabs$team,res.final.tabs$season),]
#   res.final.tabs$team.1 <- c(NA,res.final.tabs$team[-NROW(res.final.tabs)])
#   res.final.tabs$tier.1 <- c(NA,res.final.tabs$tier[-NROW(res.final.tabs)])
#   res.final.tabs$tier.1[res.final.tabs$team!=res.final.tabs$team.1] <- NA
#   res.final.tabs$season.1 <- c(NA,res.final.tabs$season[-NROW(res.final.tabs)])
#   res.final.tabs$season.1[res.final.tabs$team!=res.final.tabs$team.1] <- NA
#   res.final.tabs$promoted <- as.numeric(res.final.tabs$tier<res.final.tabs$tier.1)
#   res.final.tabs$relegated <- as.numeric(res.final.tabs$tier>res.final.tabs$tier.1)
#   res.final.tabs$relegated.1 <- c(NA,res.final.tabs$relegated[-NROW(res.final.tabs)])
#   res.final.tabs$relegated.1[res.final.tabs$team!=res.final.tabs$team.1] <- NA
#   
#   
#   
#   ##Elo ranking routine ####
#   elorank <- list()
# 
#   all.res0$elostrength.1 <- NA
#   all.res0$elostrength.2 <- NA
#   all.res0$elopredict <- NA
# 
#   all.res0 <- all.res0[order(all.res0$date),]
#   
#   all.res0 <- all.res0[is.na(all.res0$team1)==FALSE & is.na(all.res0$team2)==FALSE & all.res0$team1!="" & all.res0$team2!="",]
#   
#   all.res0$outcome <- 0.5*(all.res0$goals1==all.res0$goals2) + (all.res0$goals1>all.res0$goals2)
# 
#   all.res0 <- all.res0[is.na(all.res0$team1)==FALSE,]
#   
#   for(mm in c(1:NROW(all.res0))) { 
#     print(100*mm/NROW(all.res0))
#     if(!(all.res0$team1[mm] %in% names(elorank))) {
#       elorank[[all.res0$team1[mm]]] <- 1000
#     }
#     if(!(all.res0$team2[mm] %in% names(elorank))) {
#       elorank[[all.res0$team2[mm]]] <- 1000
#     }
#     all.res0$elostrength.1[mm] <- elorank[[all.res0$team1[mm]]]
#     all.res0$elostrength.2[mm] <- elorank[[all.res0$team2[mm]]]
#     all.res0$elopredict[mm] <- 1/(1+(10^((all.res0$elostrength.2[mm]-all.res0$elostrength.1[mm])/400)))
#     adjustment <- all.res0$outcome[mm] - all.res0$elopredict[mm]
#     if(is.na(adjustment)==FALSE) {
#       elorank[all.res0$team1[mm]] <- elorank[[all.res0$team1[mm]]] + 10*adjustment
#       elorank[all.res0$team2[mm]] <- elorank[[all.res0$team2[mm]]] - 10*adjustment
#     }
#   }
#   all.res0 <- all.res0[is.na(all.res0$date)==FALSE,]
# 
#   league.elos <- aggregate(all.res0$elostrength.1,by=list(all.res0$div_id,all.res0$season),
#                            FUN=mean)
#   
#   ##2023-24 competitions####
#   allteams <- getTeams(2023)
#   
#   ##order the 23/24 NL teams by Elo
#   nl2324 <- data.frame(sort(unlist(elorank[allteams$NLteams]),decreasing = TRUE))
#   colnames(nl2324) <- "Elo"
#   sort(unlist(elorank[allteams$NLNteams]))
#   sort(unlist(elorank[allteams$NLSteams]))
#   efl12324 <- data.frame(sort(unlist(elorank[allteams$EFL1teams]),decreasing = TRUE))
#   colnames(efl12324) <- "Elo"
#   
#   ##want model looking at final league position based on:
#   ##1) previous season position
#   res.final.tabs <- res.final.tabs[order(res.final.tabs$team,res.final.tabs$season),]
#   res.final.tabs$team.1 <- c(NA,res.final.tabs$team[-NROW(res.final.tabs)])
#   res.final.tabs$position.1 <- c(NA,res.final.tabs$position[-NROW(res.final.tabs)])
#   res.final.tabs$position.1[is.na(res.final.tabs$team.1)==FALSE & res.final.tabs$team.1!=res.final.tabs$team] <- NA
#   
#   ##2) elo rating start of season (first august/sept home elo rating each year)
#   aggregate(all.res0$elostrength.1,by=list(all.res0$team1,all.res0$season),FUN=tail,1)
#   
#   ##3) promotion/relegation
#   res.final.tabs$tier <- NA
#   res.final.tabs$tier[res.final.tabs$div_id==1 | res.final.tabs$div_id==5] <- 1
#   res.final.tabs$tier[res.final.tabs$div_id==2 | res.final.tabs$div_id==6] <- 2
#   res.final.tabs$tier[res.final.tabs$div_i %in% c(3,7,10,11)] <- 3
#   res.final.tabs$tier[res.final.tabs$div_id==4 | res.final.tabs$div_id==8] <- 4
#   res.final.tabs$tier[res.final.tabs$div_id==9 | (res.final.tabs$div_id %in% c(56,75,80) & res.final.tabs$season<1979)] <- 5
#   res.final.tabs$tier[res.final.tabs$div_id %in% 225:226 | (res.final.tabs$div_id %in% c(56,75,80) & (res.final.tabs$season>=1979 & res.final.tabs$season<2004))] <- 6
#   res.final.tabs$tier[res.final.tabs$div_id %in% c(56,75,80) & res.final.tabs$season>=2004] <- 7
#   
#   res.final.tabs <- res.final.tabs[order(res.final.tabs$team,res.final.tabs$season),]
#   res.final.tabs$team.m1 <- c(res.final.tabs$team[-1],NA)
#   res.final.tabs$tier.m1 <- c(res.final.tabs$tier[-1],NA)
#   res.final.tabs$tier.m1[res.final.tabs$team!=res.final.tabs$team.m1] <- NA
#   res.final.tabs$season.m1 <- c(res.final.tabs$season[-1],NA)
#   res.final.tabs$season.m1[res.final.tabs$team!=res.final.tabs$team.m1] <- NA
#   res.final.tabs$promoted <- as.numeric(res.final.tabs$tier>res.final.tabs$tier.m1)
#   res.final.tabs$relegated <- as.numeric(res.final.tabs$tier<res.final.tabs$tier.m1)
#   
#   
#   jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Elo-ratings/tex/eng-divs-elo10-",Sys.Date(),".jpg"),
#        width = 8, height = 5, units = "in", res=300)
#   plot(league.elos$Group.2[league.elos$Group.1==5 | league.elos$Group.1==1],
#        league.elos$x[league.elos$Group.1==5 | league.elos$Group.1==1],type="o",pch=16,cex=0.5,
#        ylim=range(league.elos$x[league.elos$Group.1 %in% eng.league.ids]),
#        main="Seasonal Mean Elo Rating By Division",ylab="Mean Elo rating",xlab="Season (year beginning)")
#   lines(league.elos$Group.2[league.elos$Group.1==6 | league.elos$Group.1==2],
#         league.elos$x[league.elos$Group.1==6 | league.elos$Group.1==2],type="o",col=1,pch=17,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==7 | league.elos$Group.1==3],
#         league.elos$x[league.elos$Group.1==7 | league.elos$Group.1==3],type="o",col=1,pch=18,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==8 | league.elos$Group.1==4],
#         league.elos$x[league.elos$Group.1==8 | league.elos$Group.1==4],type="o",col=1,pch=15,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==10],
#         league.elos$x[league.elos$Group.1==10],type="o",col=1,lty=2,pch=15,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==11],
#         league.elos$x[league.elos$Group.1==11],type="o",col=1,lty=2,pch=18,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==9],
#         league.elos$x[league.elos$Group.1==9],type="o",col=2,pch=16,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==56],#NPL
#         league.elos$x[league.elos$Group.1==56],type="o",col=3,pch=17,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==75],#SL
#         league.elos$x[league.elos$Group.1==75],type="o",col=4,pch=18,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==80],#IL
#         league.elos$x[league.elos$Group.1==80],type="o",col=5,pch=15,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==225],#NLN
#         league.elos$x[league.elos$Group.1==225],type="o",col=3,lty=3,pch=16,cex=0.5)  
#   lines(league.elos$Group.2[league.elos$Group.1==226],#NLS
#         league.elos$x[league.elos$Group.1==226],type="o",col=4,lty=3,pch=17,cex=0.5)  
#   legend("topleft",col=c(1,1,1,1,1,1,2,3,3,4,4,5),
#                    lty=c(1,1,1,1,2,2,1,1,2,1,2,1),
#                    pch=c(16,17,18,15,15,18,16,17,16,18,17,15),
#          legend=c("EPL","EFLC","EFL1","EFL2","EFL3N","EFL3S","NL","NPL","NLN","SL","NLS","IL"),
#          bty="n",ncol=4)
#   dev.off()
#   
#   ##are D3 promoted Elo's higher than D1 relegated Elo's?
#   #want end of season Elo ratings
#   all.res.elo0 <- all.res0[all.res0$div_id %in% eng.league.ids,c("team2","elostrength.2","div_id","date","season")]
#   colnames(all.res.elo0) <- c("team1","elostrength.1","div_id","date","season")
#   all.res.elo <- rbind(all.res.elo0,all.res0[all.res0$div_id %in% eng.league.ids,c("team1","elostrength.1","div_id","date","season")])
#   rm(all.res.elo0)
#   all.res.elo <- all.res.elo[order(all.res.elo$team1,all.res.elo$date),]
#   all.res.elo.y <- aggregate(all.res.elo[,c("elostrength.1","div_id")],by=list(all.res.elo$team1,all.res.elo$season),
#                              FUN=tail,1)
#   colnames(all.res.elo.y)[1:2] <- c("team","season")
#   
#   all.res.elo.y$tier <- NA
#   all.res.elo.y$tier[all.res.elo.y$div_id==1 | all.res.elo.y$div_id==5] <- 1
#   all.res.elo.y$tier[all.res.elo.y$div_id==2 | all.res.elo.y$div_id==6] <- 2
#   all.res.elo.y$tier[all.res.elo.y$div_i %in% c(3,7,10,11)] <- 3
#   all.res.elo.y$tier[all.res.elo.y$div_id==4 | all.res.elo.y$div_id==8] <- 4
#   all.res.elo.y$tier[all.res.elo.y$div_id==9 | (all.res.elo.y$div_id %in% c(56,75,80) & all.res.elo.y$season<1979)] <- 5
#   all.res.elo.y$tier[all.res.elo.y$div_id %in% 225:226 | (all.res.elo.y$div_id %in% c(56,75,80) & (all.res.elo.y$season>=1979 & all.res.elo.y$season<2004))] <- 6
#   all.res.elo.y$tier[all.res.elo.y$div_id %in% c(56,75,80) & all.res.elo.y$season>=2004] <- 7
#   
#   all.res.elo.y <- all.res.elo.y[order(all.res.elo.y$team,all.res.elo.y$season),]
#   all.res.elo.y$team.m1 <- c(all.res.elo.y$team[-1],NA)
#   all.res.elo.y$tier.m1 <- c(all.res.elo.y$tier[-1],NA)
#   all.res.elo.y$tier.m1[all.res.elo.y$team!=all.res.elo.y$team.m1] <- NA
#   all.res.elo.y$season.m1 <- c(all.res.elo.y$season[-1],NA)
#   all.res.elo.y$season.m1[all.res.elo.y$team!=all.res.elo.y$team.m1] <- NA
#   all.res.elo.y$promoted <- as.numeric(all.res.elo.y$tier>all.res.elo.y$tier.m1)
#   all.res.elo.y$relegated <- as.numeric(all.res.elo.y$tier<all.res.elo.y$tier.m1)
# #  all.res.elo.y$relegated.1 <- c(NA,all.res.elo.y$relegated[-NROW(all.res.elo.y)])
# #  all.res.elo.y$relegated.1[all.res.elo.y$team!=all.res.elo.y$team.1] <- NA
#   
#   jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Elo-ratings/tex/elo-d1r-d2p-d3p-",Sys.Date(),".jpg"),
#        width = 8, height = 5, units = "in", res=300)
#   plot(all.res.elo.y$season[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==1],
#        all.res.elo.y$elostrength.1[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==1],
#        type="p",pch=16,cex=0.5,
#        ylim=range(900,1350),
#        main="End of season Elo ratings for promoted/relegated clubs",
#        ylab="Elo rating",xlab="Season (year beginning)")
#   lines(all.res.elo.y$season[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==2],
#        all.res.elo.y$elostrength.1[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==2],
#        type="p",pch=16,cex=0.5,col=2)
#   lines(all.res.elo.y$season[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==3],
#         all.res.elo.y$elostrength.1[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==3],
#         type="p",pch=16,cex=0.5,col=3)
#   legend("topleft",col=1:3,pch=16,bty="n",ncol=2,legend=c("Relegated D1","Promoted D2","Promoted D3"))
#   dev.off()
# 
#   jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Elo-ratings/tex/elo-d12-45-",Sys.Date(),".jpg"),
#        width = 8, height = 5, units = "in", res=300)
#   par(mfrow=c(2,2))
#   plot(all.res.elo.y$season[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==1],
#        all.res.elo.y$elostrength.1[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==1],
#        type="p",pch=16,cex=0.5,
#        ylim=range(900,1350),
#        main="D1 relegated, D2 promoted",
#        ylab="Elo rating",xlab="Season (year beginning)")
#   lines(all.res.elo.y$season[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==2],
#         all.res.elo.y$elostrength.1[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==2],
#         type="p",pch=16,cex=0.5,col=2)
#   legend("topleft",col=1:2,pch=16,bty="n",ncol=2,legend=c("Relegated","Promoted"),pt.cex=0.5)
#   plot(all.res.elo.y$season[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==2],
#        all.res.elo.y$elostrength.1[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==2],
#        type="p",pch=16,cex=0.5,
#        ylim=range(800,1260),xlim=range(1920,2022),
#        main="D2 relegated, D3 promoted",
#        ylab="Elo rating",xlab="Season (year beginning)")
#   lines(all.res.elo.y$season[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==3],
#         all.res.elo.y$elostrength.1[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==3],
#         type="p",pch=16,cex=0.5,col=2)
#   plot(all.res.elo.y$season[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==3],
#        all.res.elo.y$elostrength.1[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==3],
#        type="p",pch=16,cex=0.5,
#        ylim=range(800,1260),xlim=range(1959,2022),
#        main="D3 relegated, D4 promoted",
#        ylab="Elo rating",xlab="Season (year beginning)")
#   lines(all.res.elo.y$season[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==4],
#         all.res.elo.y$elostrength.1[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==4],
#         type="p",pch=16,cex=0.5,col=2)
#   plot(all.res.elo.y$season[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==4],
#        all.res.elo.y$elostrength.1[is.na(all.res.elo.y$relegated)==FALSE & all.res.elo.y$relegated==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==4],
#        type="p",pch=16,cex=0.5,
#        ylim=range(800,1260),xlim=range(1986,2022),
#        main="D4 relegated, D5 promoted",
#        ylab="Elo rating",xlab="Season (year beginning)")
#   lines(all.res.elo.y$season[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==5],
#         all.res.elo.y$elostrength.1[is.na(all.res.elo.y$promoted)==FALSE & all.res.elo.y$promoted==1 & is.na(all.res.elo.y$tier)==FALSE & all.res.elo.y$tier==5],
#         type="p",pch=16,cex=0.5,col=2)
#   dev.off()
#   #all.res0 <- merge(all.res0,res.final.tabs)
# 
#   ##need to check FA Cup matches in the 1960s between SL and EFL3 and EFL4 clubs
#   #we first need the tier each team was from for a match
#   all.res0$tier <- NA
#   all.res0$tier[all.res0$div_id==1 | all.res0$div_id==5] <- 1
#   all.res0$tier[all.res0$div_id==2 | all.res0$div_id==6] <- 2
#   all.res0$tier[all.res0$div_i %in% c(3,7,10,11)] <- 3
#   all.res0$tier[all.res0$div_id==4 | all.res0$div_id==8] <- 4
#   all.res0$tier[all.res0$div_id==9 | (all.res0$div_id %in% c(56,75,80) & all.res0$season<1979)] <- 5
#   all.res0$tier[all.res0$div_id %in% 225:226 | (all.res0$div_id %in% c(56,75,80) & (all.res0$season>=1979 & all.res0$season<2004))] <- 6
#   all.res0$tier[all.res0$div_id %in% c(56,75,80) & all.res0$season>=2004] <- 7
# 
#   all.res0$tier1 <- all.res0$tier
#   all.res0$tier2 <- all.res0$tier
#   
#   all.res0 <- all.res0[order(all.res0$team1,all.res0$date),]
#   all.res0$tier1 <- na.locf(all.res0$tier1,na.rm=FALSE)
#   all.res0 <- all.res0[order(all.res0$team2,all.res0$date),]
#   all.res0$tier2 <- na.locf(all.res0$tier2,na.rm=FALSE)
#   
#   elod3d5 <- rbind(all.res0[is.na(all.res0$tier1)==FALSE & is.na(all.res0$tier2)==FALSE & all.res0$tier1==5 & all.res0$tier2 %in% 3:4 & all.res0$season<1979,c("outcome","elopredict")],
#                    all.res0[is.na(all.res0$tier1)==FALSE & is.na(all.res0$tier2)==FALSE & all.res0$tier1 %in% 3:4 & all.res0$tier2==5 & all.res0$season<1979,c("outcome","elopredict")])
#   plot(aggregate(elod3d5$outcome,by=list(round(elod3d5$elopredict,2)),FUN=mean),
#        ylim=range(0,1),xlim=range(0,1))
#   abline(0,1)
#   
#   teams.above <- full.position(all.res0)
#   
#   
# }

##load up all domestic results and save in file - including OAFC file
create.domestic.results <- function() {
  require(zoo)
  
  ##### loading results data from soccerbase #####
  res.sb <- load.soccerbase.results(region="england")  

  ##loading various other sources of results (non-soccerbase) ####
  ilres <- load.il.results()
  nplres <- load.npl.results()
  nlres <- load.nl.results()
  wyc <- load.wycombe.results()
  weald <- load.wealdstone.results()
  weym <- load.weymouth.results()
  sfc <- load.southport.results()
  bfc <- load.barnet.results()
  bufc <- load.boston.results()

  sb.data <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/results-all-basic.csv",stringsAsFactors = FALSE)
  res0 <- merge(res.sb,sb.data[,c("match.id","attendance")],by.x=c("match_id"),by.y=c("match.id"),all.x=TRUE)
  
  ##merge in footballwebpages teamnames, save to update english football home/away attendances ####
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  corsf <- corr[,c("soccerbase.team","footballwebpages.teams")]
  corsf <- corsf[duplicated(corsf)==FALSE,]
  corsf <- corsf[corsf$soccerbase.team!="",]
  corsf <- corsf[duplicated(corsf$soccerbase.team)==FALSE,]

  ##fix middlesbrough, crystal palace, afc wimbledon, dorking wanders and dagenham and redbridge
  res0$team1[res0$team1=="Middlesbrough"] <- "Middlesbro"
  res0$team1[res0$team1=="Crystal Palace"] <- "C Palace"
  res0$team1[res0$team1=="AFC W'bledon"] <- "AFC Wimbledon"
  res0$team1[res0$team1=="Dorking W"] <- "Dorking Wanderers"
  res0$team1[res0$team1=="Dag &amp; Red"] <- "Dag and Red"
  res0$team2[res0$team2=="Middlesbrough"] <- "Middlesbro"
  res0$team2[res0$team2=="Crystal Palace"] <- "C Palace"
  res0$team2[res0$team2=="AFC W'bledon"] <- "AFC Wimbledon"
  res0$team2[res0$team2=="Dorking W"] <- "Dorking Wanderers"
  res0$team2[res0$team2=="Dag &amp; Red"] <- "Dag and Red"

  hist.res <- loading.historical.results()
  hist.res$team1_id <- NA
  hist.res$team2_id <- NA
  hist.res$div_id <- NA
  
  ## save file to add soccerbase historic matches to away attendances dataset ####
  # att.update <- merge(res0,corsf,by.x="team2",by.y="soccerbase.team",all.x=TRUE)
  # att.update <- merge(att.update,corsf,by.x="team1",by.y="soccerbase.team",all.x=TRUE,suffixes=c(".2",".1"))
  # att.update$awayatt <- NA
  # att.update <- att.update[att.update$div_id %in% england.ids,]
  # att.update <- att.update[order(att.update$date,att.update$div_id,att.update$team1),]
  
  ##save instalments to enable continual updating as season progresses
  # write.csv(att.update[att.update$date>="2022-07-01" & att.update$date<="2022-08-31",
  #                     c("footballwebpages.teams.1","footballwebpages.teams.2",
  #                       "date","goals1","goals2","division","attendance","awayatt","team1","team2")],
  #          "/Users/jjreade/Dropbox/Research/Sport/Attendance/data/home-away-2022-07-01--2022-08-31.csv")
  # write.csv(att.update[att.update$date>="2022-08-29" & att.update$date<="2022-08-31",
  #                      c("footballwebpages.teams.1","footballwebpages.teams.2",
  #                        "date","goals1","goals2","division","attendance","awayatt","team1","team2")],
  #           "/Users/jjreade/Dropbox/Research/Sport/Attendance/data/home-away-2022-08-29--2022-08-31.csv")
  
  ### put all results together in one file #####
  all.res0 <- rbind(res0[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
                    nlres,ilres,nplres,wyc,weald,weym,sfc,bfc,bufc,
                    hist.res[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
  
  #remove based on duplicates: two teams playing on same day...
  all.res0 <- all.res0[order(all.res0$div_id),]
  all.res0 <- all.res0[order(all.res0$team1,all.res0$team2,all.res0$goals1),]
  all.res0 <- all.res0[duplicated(all.res0[,c("team1","team2","date")])==FALSE,]

  all.res0$goals1 <- as.numeric(all.res0$goals1)
  all.res0$goals2 <- as.numeric(all.res0$goals2)

  #### harmonise division names #####
  all.res0$division <- tolower(all.res0$division)
  all.res0$division <- gsub("^\\s+|\\s+$","",all.res0$division)
  all.res0$division <- gsub("\\s{3,}","",gsub("fat","fa trophy",gsub("fac","fa cup",gsub("faac","fa amateur cup",all.res0$division))))
  all.res0$division <- gsub("fa challenge","fa",gsub("football association","fa",all.res0$division))
  all.res0$division <- gsub("\\d*qr$","",gsub("qf$","",gsub("sf$","",gsub("\\dq$","",gsub("\\dr$","",gsub("\\sf$","",all.res0$division))))))
  all.res0$division <- gsub("\\s+replay","",gsub("\\s+final","",gsub("\\s+semi final","",all.res0$division)))
  all.res0$division <- gsub("\\d\\w{2} leg","",all.res0$division)
  all.res0$division <- gsub("^(.*?) / .*?$","\\1",gsub("[.]","",all.res0$division))
  all.res0$division <- gsub("^(.*?) / .*?$","\\1",gsub("\\d\\w{2} round","",all.res0$division))
  all.res0$division[regexpr("friend",all.res0$division)>-1] <- "friendly"
  all.res0$division[all.res0$division=="fr"] <- "friendly"
  #fa cup
  all.res0$division[regexpr("fa cup",all.res0$division)>-1 & regexpr("uefa",all.res0$division)==-1 & regexpr("afa",all.res0$division)==-1 & regexpr("scottish",all.res0$division)==-1] <- "fa cup"
  #fa trophy
  all.res0$division[regexpr("fa trophy",all.res0$division)>-1] <- "fa trophy"
  #efl trophy
  all.res0$division[regexpr("fl trophy",all.res0$division)>-1] <- "fl trophy"
  all.res0$division <- gsub("associate members trophy","fl trophy",gsub("football league trop$","fl trophy",all.res0$division))
  #national league
  all.res0$division[regexpr("conference south",all.res0$division)>-1] <- "national league south"
  all.res0$division[regexpr("conference north",all.res0$division)>-1] <- "national league north"
  all.res0$division[regexpr("conference",all.res0$division)>-1 & regexpr("europa",all.res0$division)==-1 & regexpr("cup|shield|trophy",all.res0$division)==-1] <- "national league north"
  all.res0$division <- gsub("^gmvc$","national league",all.res0$division)
  all.res0$division <- gsub("blue sq premier","national league",all.res0$division)
  
  all.res0$division <- gsub("^conference","national league",gsub("football conference","national league",all.res0$division))
  all.res0$division[is.na(all.res0$division)==FALSE & regexpr("national league",all.res0$division)>-1] <- gsub("^eng ","",all.res0$division[is.na(all.res0$division)==FALSE & regexpr("national league",all.res0$division)>-1])
  
  ## harmonise team names ####
  all.res0$team1[all.res0$team1_id==5656] <- "AFC Mansfield"
  all.res0$team2[all.res0$team2_id==5656] <- "AFC Mansfield"
  all.res0$team1[all.res0$team1_id==6395] <- "Stockport Town"
  all.res0$team2[all.res0$team2_id==6395] <- "Stockport Town"
  all.res0$team1[all.res0$team1_id==5311] <- "Reading Town"
  all.res0$team2[all.res0$team2_id==5311] <- "Reading Town"
  all.res0$team1[all.res0$team1_id==7170] <- "Reading City"
  all.res0$team2[all.res0$team2_id==7170] <- "Reading City"
  
  
  all.res0$team1 <- toTitleCase(tolower(all.res0$team1))
  all.res0$team1 <- gsub("^\\s+|\\s+$","",all.res0$team1)
  
  all.res0$team1 <- gsub("Uni","Univ\\w*",gsub("Blackburn Rovers","Blackburn",gsub("Stortford","St",gsub("Bishop's","Bishops",gsub("Bishop Auckland","Bishop Auck",gsub("Berry's","Berrys",gsub("B'ham","Birmingham",gsub("Collieries","Colls",gsub("Distiller\\w+","Dist",gsub(" [(].*?[)]","",all.res0$team1))))))))))
  
  all.res0$team1[regexpr("Atherstone",all.res0$team1)>-1] <- "Atherstone"
  all.res0$team1[regexpr("Bamber",all.res0$team1)>-1] <- "Bamber Br."
  all.res0$team1[regexpr("Banbury",all.res0$team1)>-1] <- "Banbury"
  all.res0$team1[regexpr("Bangor",all.res0$team1)>-1] <- "Bangor"
  all.res0$team1[regexpr("Banstead",all.res0$team1)>-1] <- "Banstead"
  all.res0$team1[regexpr("Barking &",all.res0$team1)>-1] <- "Barking & E Ham"
  all.res0$team1[regexpr("Barnstaple",all.res0$team1)>-1] <- "Barnstaple"
  all.res0$team1[regexpr("Bath",all.res0$team1)>-1] <- "Bath"
  all.res0$team1[regexpr("Birdwell",all.res0$team1)>-1] <- "Birdwell"
  all.res0$team1[regexpr("Blackburn Park",all.res0$team1)>-1] <- "Blackburn Park R"
  all.res0$team1[regexpr("Blyth",all.res0$team1)>-1] <- "Blyth S"
  all.res0$team1[regexpr("Bolton Wanderers",all.res0$team1)>-1] <- "Bolton"
  all.res0$team1[regexpr("Boreham",all.res0$team1)>-1] <- "Boreham W"
  all.res0$team1[regexpr("Boston Utd",all.res0$team1)>-1] <- "Boston"
  all.res0$team1[regexpr("Bowers ",all.res0$team1)>-1] <- "Bowers and Pitsea"
  all.res0$team1[regexpr("Bradford",all.res0$team1)>-1 & (regexpr("Park",all.res0$team1)>-1 | regexpr("PA",all.res0$team1)>-1) & regexpr("Reserves",all.res0$team1)==-1] <- "Bradford PA"
  all.res0$team1[regexpr("Bradford City",all.res0$team1)>-1] <- "Bradford"
  all.res0$team1[regexpr("Brentwood",all.res0$team1)>-1] <- "Brentwood"
  all.res0$team1[regexpr("Bridlington",all.res0$team1)>-1] <- "Bridlington"
  all.res0$team1[regexpr("Brightlingsea",all.res0$team1)>-1] <- "Brightlingsea"
  all.res0$team1[regexpr("Brighton",all.res0$team1)>-1 & regexpr("Hove",all.res0$team1)>-1] <- "Brighton"
  all.res0$team1[regexpr("Bristol City$",all.res0$team1)>-1] <- "Bristol C"
  all.res0$team1[regexpr("Bristol Manor F",all.res0$team1)>-1] <- "Bristol Manor F"
  all.res0$team1[regexpr("Bristol Rovers$",all.res0$team1)>-1] <- "Bristol R"
  all.res0$team1[regexpr("Bromsgrove",all.res0$team1)>-1] <- "Bromsgrove"
  all.res0$team1[regexpr("Buckingham",all.res0$team1)>-1] <- "Buckingham"
  all.res0$team1[regexpr("Burnham",all.res0$team1)>-1] <- "Burnham"
  all.res0$team1[regexpr("Burslem Port Vale",all.res0$team1)>-1] <- "Port Vale"
  all.res0$team1[regexpr("Burton Albion",all.res0$team1)>-1] <- "Burton"
  all.res0$team1[regexpr("Burton W",all.res0$team1)>-1] <- "Burton W"
  all.res0$team1[regexpr("Cambridge Utd",all.res0$team1)>-1] <- "Cambridge U"
  all.res0$team1 <- gsub("Cammell","Cammell Laird",gsub("Cammel L","Cammell L",all.res0$team1))
  all.res0$team1[regexpr("Canvey",all.res0$team1)>-1] <- "Canvey"
  ###CARRY ON HERE!!!!####
  
  
  all.res0 <- all.res0[all.res0$team1!="" & all.res0$team2!="",]
  all.res0 <- all.res0[is.na(all.res0$team1)==FALSE & is.na(all.res0$team2)==FALSE,]
  
  ##### Give team ID to non-soccerbase teams #####
  all.res0$team1_id <- as.numeric(all.res0$team1_id)
  all.res0$team2_id <- as.numeric(all.res0$team2_id)
  all.teams0 <- all.res0[,c("team1","team1_id")]
  colnames(all.teams0) <- c("team","team_id")
  all.teams1 <- all.res0[,c("team2","team2_id")]
  colnames(all.teams1) <- c("team","team_id")
  all.teams <- rbind(all.teams0,all.teams1)
  all.teams <- all.teams[is.na(all.teams$team)==FALSE,]
  #first remove all duplicates
  all.teams <- all.teams[duplicated(all.teams)==FALSE,]
  #now remove if team has both NA and actual ID
  all.teams <- all.teams[order(all.teams$team,all.teams$team_id,decreasing = TRUE),]
  all.teams <- all.teams[duplicated(all.teams$team)==FALSE,]
  #now need all teams with NA to have an ID - possibility all teams won't have Soccerbase ID
  all.teams$team_id[is.na(all.teams$team_id)==TRUE] <- seq(max(all.teams$team_id,na.rm=TRUE)+1,max(all.teams$team_id,na.rm=TRUE)+NROW(all.teams$team_id[is.na(all.teams$team_id)==TRUE]))
  #match up some old team names to later team names
  all.teams$team_id[all.teams$team=="Burslem Port Vale"] <- all.teams$team_id[all.teams$team=="Port Vale"]
  
  all.res0 <- merge(all.res0,all.teams,by.x=c("team1"),by.y=c("team"),all.x=TRUE)
  all.res0 <- merge(all.res0,all.teams,by.x=c("team2"),by.y=c("team"),all.x=TRUE,suffixes = c("1","2"))
  all.res0$team1_id <- NULL
  all.res0$team2_id <- NULL
  colnames(all.res0)[colnames(all.res0) %in% c("team_id1","team_id2")] <- c("team1_id","team2_id")
  
  #all divs without an ID need an ID...
  all.res0$division[regexpr("Friend",all.res0$division)>-1] <- "Friendly"
  all.divs <- all.res0[,c("division","div_id")]
  all.divs <- all.divs[is.na(all.divs$division)==FALSE,]
  #first remove all duplicates
  all.divs <- all.divs[duplicated(all.divs)==FALSE,]
  #now remove if div has both NA and actual ID
  all.divs <- all.divs[order(all.divs$division,all.divs$div_id,decreasing = TRUE),]
  all.divs <- all.divs[duplicated(all.divs$division)==FALSE,]
  #now need all divs with NA to have an ID
  all.divs$div_id[is.na(all.divs$div_id)==TRUE] <- seq(max(all.divs$div_id,na.rm=TRUE)+1,max(all.divs$div_id,na.rm=TRUE)+NROW(all.divs$div_id[is.na(all.divs$div_id)==TRUE]))
  
  census.date <- "2022-06-01"
  
  #remove matches taking place after August 5 (i.e. the 2022/23 season)
  #all.res0 <- all.res0[all.res0$date<=census.date,]
  #######
  
  ##remove duplicate entries ####
  all.res0 <- all.res0[duplicated(all.res0)==FALSE,]
  all.res0 <- all.res0[order(all.res0$div_id),]
  all.res0 <- all.res0[duplicated(all.res0[,c("date","team1","team2")])==FALSE,]
  
  ##get rid of any matches that haven't occurred before today (i.e. have NAs in the score) ####
  all.res0 <- all.res0[!(is.na(all.res0$goals1)==TRUE & all.res0$date<census.date),]
  
  #get rid of matches that are "winner matches"
  #res <- res[regexpr("winner",tolower(res$team1))>-1,]
  all.res0 <- all.res0[!(all.res0$match_id=="tgc811051" & all.res0$goals2==4),] #remove odd duplicate with wrong score
  all.res0 <- all.res0[!(all.res0$match_id=="tgc813129" & all.res0$goals2==4),] #remove odd duplicate with wrong score
  all.res0$outcome <- 0.5*(all.res0$goals1==all.res0$goals2) + (all.res0$goals1>all.res0$goals2)
  
  all.res0 <- all.res0[order(all.res0$date),]
  all.res0$team1 <- gsub(" [(]old[)]","",all.res0$team1)
  all.res0$team2 <- gsub(" [(]old[)]","",all.res0$team2)
  all.res0$team1 <- gsub("^\\s+|\\s+$","",all.res0$team1)
  all.res0$team2 <- gsub("^\\s+|\\s+$","",all.res0$team2)
  
  #weights for Elo ranking --- rank league highest for club, non-friendly for national
  # all.res0$weight <- elo.weight
  
  ##Elo ranking routine ####
  elorank01 <- list()
  elorank05 <- list()
  elorank10 <- list()
  elorank20 <- list()
  elorank30 <- list()
  
  all.res0$elostrength01.1 <- NA
  all.res0$elostrength01.2 <- NA
  all.res0$elo01predict <- NA
  all.res0$elostrength05.1 <- NA
  all.res0$elostrength05.2 <- NA
  all.res0$elo05predict <- NA
  all.res0$elostrength10.1 <- NA
  all.res0$elostrength10.2 <- NA
  all.res0$elo10predict <- NA
  all.res0$elostrength20.1 <- NA
  all.res0$elostrength20.2 <- NA
  all.res0$elo20predict <- NA
  all.res0$elostrength30.1 <- NA
  all.res0$elostrength30.2 <- NA
  all.res0$elo30predict <- NA
  
  all.res0 <- all.res0[order(all.res0$date),]
  
  for(mm in c(582495:NROW(all.res0))) { 
    print(100*mm/NROW(all.res0))
    if(all.res0$date[mm]==census.date) {
      break
    }
    if(!(paste0("id",all.res0$team1_id[mm]) %in% names(elorank01))) {
      elorank01[[paste0("id",all.res0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team1_id[mm]) %in% names(elorank05))) {
      elorank05[[paste0("id",all.res0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team1_id[mm]) %in% names(elorank10))) {
      elorank10[[paste0("id",all.res0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team1_id[mm]) %in% names(elorank20))) {
      elorank20[[paste0("id",all.res0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team1_id[mm]) %in% names(elorank30))) {
      elorank30[[paste0("id",all.res0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team2_id[mm]) %in% names(elorank01))) {
      elorank01[[paste0("id",all.res0$team2_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team2_id[mm]) %in% names(elorank05))) {
      elorank05[[paste0("id",all.res0$team2_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team2_id[mm]) %in% names(elorank10))) {
      elorank10[[paste0("id",all.res0$team2_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team2_id[mm]) %in% names(elorank20))) {
      elorank20[[paste0("id",all.res0$team2_id[mm])]] <- 1000
    }
    if(!(paste0("id",all.res0$team2_id[mm]) %in% names(elorank30))) {
      elorank30[[paste0("id",all.res0$team2_id[mm])]] <- 1000
    }
    all.res0$elostrength01.1[mm] <- elorank01[[paste0("id",all.res0$team1_id[mm])]]
    all.res0$elostrength01.2[mm] <- elorank01[[paste0("id",all.res0$team2_id[mm])]]
    all.res0$elo01predict[mm] <- 1/(1+(10^((all.res0$elostrength01.2[mm]-all.res0$elostrength01.1[mm])/400)))
    all.res0$elostrength05.1[mm] <- elorank05[[paste0("id",all.res0$team1_id[mm])]]
    all.res0$elostrength05.2[mm] <- elorank05[[paste0("id",all.res0$team2_id[mm])]]
    all.res0$elo05predict[mm] <- 1/(1+(10^((all.res0$elostrength05.2[mm]-all.res0$elostrength05.1[mm])/400)))
    all.res0$elostrength10.1[mm] <- elorank10[[paste0("id",all.res0$team1_id[mm])]]
    all.res0$elostrength10.2[mm] <- elorank10[[paste0("id",all.res0$team2_id[mm])]]
    all.res0$elo10predict[mm] <- 1/(1+(10^((all.res0$elostrength10.2[mm]-all.res0$elostrength10.1[mm])/400)))
    all.res0$elostrength20.1[mm] <- elorank20[[paste0("id",all.res0$team1_id[mm])]]
    all.res0$elostrength20.2[mm] <- elorank20[[paste0("id",all.res0$team2_id[mm])]]
    all.res0$elo20predict[mm] <- 1/(1+(10^((all.res0$elostrength20.2[mm]-all.res0$elostrength20.1[mm])/400)))
    all.res0$elostrength30.1[mm] <- elorank30[[paste0("id",all.res0$team1_id[mm])]]
    all.res0$elostrength30.2[mm] <- elorank30[[paste0("id",all.res0$team2_id[mm])]]
    all.res0$elo30predict[mm] <- 1/(1+(10^((all.res0$elostrength30.2[mm]-all.res0$elostrength30.1[mm])/400)))
    adjustment01 <- all.res0$outcome[mm] - all.res0$elo10predict[mm]
    adjustment05 <- all.res0$outcome[mm] - all.res0$elo10predict[mm]
    adjustment10 <- all.res0$outcome[mm] - all.res0$elo10predict[mm]
    adjustment20 <- all.res0$outcome[mm] - all.res0$elo20predict[mm]
    adjustment30 <- all.res0$outcome[mm] - all.res0$elo30predict[mm]
    if(is.na(adjustment01)==FALSE) {
      elorank01[paste0("id",all.res0$team1_id[mm])] <- elorank01[[paste0("id",all.res0$team1_id[mm])]] + 01*adjustment01
      elorank01[paste0("id",all.res0$team2_id[mm])] <- elorank01[[paste0("id",all.res0$team2_id[mm])]] - 01*adjustment01
    }
    if(is.na(adjustment05)==FALSE) {
      elorank05[paste0("id",all.res0$team1_id[mm])] <- elorank05[[paste0("id",all.res0$team1_id[mm])]] + 05*adjustment05
      elorank05[paste0("id",all.res0$team2_id[mm])] <- elorank05[[paste0("id",all.res0$team2_id[mm])]] - 05*adjustment05
    }
    if(is.na(adjustment10)==FALSE) {
      elorank10[paste0("id",all.res0$team1_id[mm])] <- elorank10[[paste0("id",all.res0$team1_id[mm])]] + 10*adjustment10
      elorank10[paste0("id",all.res0$team2_id[mm])] <- elorank10[[paste0("id",all.res0$team2_id[mm])]] - 10*adjustment10
    }
    if(is.na(adjustment20)==FALSE) {
      elorank20[paste0("id",all.res0$team1_id[mm])] <- elorank20[[paste0("id",all.res0$team1_id[mm])]] + 20*adjustment20
      elorank20[paste0("id",all.res0$team2_id[mm])] <- elorank20[[paste0("id",all.res0$team2_id[mm])]] - 20*adjustment20
    }
    if(is.na(adjustment30)==FALSE) {
      elorank30[paste0("id",all.res0$team1_id[mm])] <- elorank30[[paste0("id",all.res0$team1_id[mm])]] + 30*adjustment30
      elorank30[paste0("id",all.res0$team2_id[mm])] <- elorank30[[paste0("id",all.res0$team2_id[mm])]] - 30*adjustment30
    }
  }
  all.res0 <- all.res0[is.na(all.res0$date)==FALSE,]
  ##end of elo routine ####
  
  ##Tier information ####
  
  ##### TIERS #####
  all.res0$country <- NA
  all.res0$tier <- NA
  all.res0$tier[all.res0$div_id==1 | all.res0$div_id==5] <- 1
  all.res0$tier[all.res0$div_id==2 | all.res0$div_id==6] <- 2
  all.res0$tier[all.res0$div_id==3 | all.res0$div_id==7] <- 3
  all.res0$tier[all.res0$div_id==4 | all.res0$div_id==8] <- 4
  all.res0$tier[all.res0$division=="Southern League Division One"] <- 2.5
  all.res0$country[all.res0$div_id %in% england.ids | (is.na(all.res0$div_id)==TRUE & all.res0$date<"1920-01-01")] <- "England"
  all.res0$tier[all.res0$div_id==12 | all.res0$div_id==16] <- 1.01
  all.res0$tier[all.res0$div_id==13 | all.res0$div_id==17] <- 2.01
  all.res0$tier[all.res0$div_id==14 | all.res0$div_id==18] <- 3.01
  all.res0$tier[all.res0$div_id==82 | all.res0$div_id==353] <- 4.01
  all.res0$tier[all.res0$div_id==9] <- 5
  all.res0$tier[all.res0$div_id==225] <- 6.1
  all.res0$tier[all.res0$div_id==226] <- 6.2
  all.res0$tier[all.res0$div_id==80] <- 7
  all.res0$tier[all.res0$div_id==75] <- 7
  all.res0$tier[all.res0$div_id==56] <- 7
  all.res0$tier[all.res0$div_id==10] <- 3.1
  all.res0$tier[all.res0$div_id==11] <- 3.2
  
  all.res0$outcomeH <- as.numeric(all.res0$outcome==1)
  all.res0$outcomeD <- as.numeric(all.res0$outcome==0.5)
  all.res0$outcomeA <- as.numeric(all.res0$outcome==0)
  all.res0$goals1 <- as.numeric(all.res0$goals1)
  all.res0$goals2 <- as.numeric(all.res0$goals2)
  
  all.res0 <- all.res0[order(all.res0$team1,all.res0$date),]
  library(dplyr)
  tier1 <- all.res0[,c("team1","tier")] %>% group_by(team1) %>% transmute(tier=na.locf(tier, na.rm=FALSE))
  all.res0$tier1 <- tier1$tier
  # all.res0$tier1 <- na.locf(all.res0$tier,na.rm = FALSE)
  all.res0 <- all.res0[order(all.res0$team2,all.res0$date),]
  tier2 <- all.res0[,c("team2","tier")] %>% group_by(team2) %>% transmute(tier=na.locf(tier, na.rm=FALSE))
  all.res0$tier2 <- tier2$tier
  # all.res0$tier2 <- na.locf(all.res0$tier,na.rm = FALSE)
  all.res0$league <- as.numeric(all.res0$tier1==all.res0$tier2)
  all.res0$league[is.na(all.res0$league)==TRUE] <- 0
  
  all.res0 <- all.res0[order(all.res0$date),]
  
  ##calculate league tables ####
  #sb.divs <- res0[duplicated(res0$div_id)==FALSE,c("div_id","division")] ##for reference
  
  res.tab <- data.frame()
  res.final.tabs <- data.frame()
  all.res0$season <- as.numeric(format(all.res0$date,"%Y")) - as.numeric(as.numeric(format(all.res0$date,"%m"))<7)
  seasons <- unique(all.res0$season)
  seasons <- seasons[order(seasons)]
  
  for( ss in seasons) {
    print(ss)
    
    for( ll in eng.league.ids) {
      #if(ss==2019 & ll<5) { next }
      print(ll)
      matches <- all.res0[all.res0$season==ss & all.res0$div_id==ll,c("match_id","date","team1","goals1","goals2","team2")]
      matches <- matches[is.na(matches$match_id)==F,]
      if(NROW(matches)>10) {
        if(var(matches$date)==0) {
          matches$date <- matches$date + 1:NROW(matches)
        }
        lge.all <- league.tab(matches$match_id,as.Date(matches$date),matches$team1,
                              matches$goals1,matches$goals2,matches$team2,
                              2+as.numeric( (ss>1980 & ll %in% 1:11 ) | (ss>1994 & !(ll %in% 1:11) ) ))
        lge <- lge.all$matches
        
        final.lge.tab <- lge.all$final.table
        final.lge.tab$season <- ss
        final.lge.tab$div_id <- ll
        
        colnames(lge)[grep("game_id",colnames(lge))] <- "match_id"
        colnames(lge)[grep("g1",colnames(lge))] <- "goals1"
        colnames(lge)[grep("g2",colnames(lge))] <- "goals2"
        lge$season <- ss
        lge$div_id <- ll
        res.tab <- rbind(res.tab,lge)
        res.final.tabs <- rbind(res.final.tabs,final.lge.tab)
      }
    }
  }
  
  res.tab <- res.tab[duplicated(res.tab$match_id)==F,]
  all.res0 <- all.res0[duplicated(all.res0$match_id)==FALSE,]
  all.res0 <- merge(all.res0,res.tab[,c(1,7:22)],by=c("match_id"),all.x=T)
  
  ##save league tables up to end of previous season ####
  write.csv(res.tab,paste0(dbloc,"/data/league-tabs-1888-2022.csv"))
  write.csv(res.final.tabs,"/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/final-league-tabs-1888-2022.csv")

  
  teams.above <- full.position(all.res0)
  
  all.res0 <- merge(all.res0,teams.above,by=c("season","tier"),all.x=TRUE)
  
  all.res0$full.pos1 <- all.res0$pos1 + all.res0$teams.above
  all.res0$full.pos2 <- all.res0$pos2 + all.res0$teams.above
  
  
  ## write all data to files to load when updating each week ####
  dbloc <- "/Users/jjreade/Dropbox/Research/Sport/Correct-score/"

  write.csv(elorank10,paste0(dbloc,"/data/elorank-",census.date,".csv"))
  
  
  write.csv(all.res0[all.res0$date<=census.date,],paste0(dbloc,"/data/res0-",census.date,".csv"))
  
}

full.position <- function(data = data) {
  ##full pyramid league position ####
  if(any(colnames(data)=="team1")) {
    colnames(data)[colnames(data)=="team1"] <- "team"
  }
  div.tot.teams0 <- data[duplicated(data[,c("team","div_id","season")])==FALSE & data$div_id<12 & data$div_id>0,c("team","div_id","season")]
  div.tot.teams0$tier <- div.tot.teams0$div_id
  div.tot.teams0$tier[div.tot.teams0$tier==5] <- 1
  div.tot.teams0$tier[div.tot.teams0$tier==6] <- 2
  div.tot.teams0$tier[div.tot.teams0$tier==7] <- 3
  div.tot.teams0$tier[div.tot.teams0$tier==8] <- 4
  div.tot.teams0$tier[div.tot.teams0$tier==9] <- 5
  div.tot.teams0$tier[div.tot.teams0$tier==10] <- 3.1
  div.tot.teams0$tier[div.tot.teams0$tier==11] <- 3.2
  
  div.tot.teams0$const <- 1
  
  div.tot.teams <- aggregate(div.tot.teams0$const,by=list(div.tot.teams0$tier,div.tot.teams0$season),FUN=sum)
  
  div.tot.teams1 <- div.tot.teams[div.tot.teams$Group.1==1,]
  div.tot.teams2 <- div.tot.teams[div.tot.teams$Group.1==2,]
  div.tot.teams3 <- div.tot.teams[div.tot.teams$Group.1==3,]
  div.tot.teams3.1 <- div.tot.teams[div.tot.teams$Group.1==3.1,]
  div.tot.teams3.2 <- div.tot.teams[div.tot.teams$Group.1==3.2,]
  div.tot.teams4 <- div.tot.teams[div.tot.teams$Group.1==4,]
  
  div.tots <- merge(div.tot.teams1[c("Group.2","x")],
                    div.tot.teams2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".1",".2"))
  div.tots <- merge(div.tots,
                    div.tot.teams3[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams4[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3",".4"))
  div.tots <- merge(div.tots,
                    div.tot.teams3.1[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams3.2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3n",".3s"))
  
  div.tots$cum.1 <- 0
  div.tots$cum.2 <- div.tots$x.1
  div.tots$cum.3 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.1 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.2 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.4 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3
  div.tots$cum.5 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4
  
  div.tots$lg.total.n <- rowSums(div.tots[,c("x.1","x.2","x.3n","x.3","x.4")],na.rm=TRUE)
  div.tots$lg.total.s <- rowSums(div.tots[,c("x.1","x.2","x.3s","x.3","x.4")],na.rm=TRUE)
  
  divs <- c(1,2,3,3.1,3.2,4,5)
  teams.above <- data.frame(stringsAsFactors = FALSE)
  
  for(dd in divs) {
    temp <- div.tots[,c("Group.2",paste0("cum.",dd))]
    colnames(temp) <- c("season","teams.above")
    temp$tier <- dd
    teams.above <- rbind(teams.above,temp)
  }
  return <- teams.above
}

##load up the soccerbase results - and other results - up to August 5
initialise2022season <- function(write.date=Sys.Date(),elo.weight=20) {
  require(zoo)
  
  ##### loading results data from soccerbase #####
  res0 <- load.soccerbase.results()  
  ##### loading results data from worldfootball.net #####
  wf.res0 <- load.worldfootball.results()  
  colnames(wf.res0) <- gsub("[.]2$","_id",colnames(wf.res0))
  colnames(wf.res0)[colnames(wf.res0)=="competition"] <- "division"
  wf.res0$div_id <- NA
  wf.res0$match_id <- paste0("wf",1:NROW(wf.res0))
  wf.res0$attendance <- NA

  ilres <- load.il.results()
  nplres <- load.npl.results()
  nlres <- load.nl.results()
  wyc <- load.wycombe.results()
  weald <- load.wealdstone.results()
  weym <- load.weymouth.results()
  sfc <- load.southport.results()
  bfc <- load.barnet.results()
  bufc <- load.boston.results()
  
  cat <- load.catalan.results()
  cesp <- load.central.spain.results()
  cdr <- load.copa.del.rey.results()
  ita <- load.italian.results()
  
  sb.data <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Global-Football-Database/results-all-basic.csv",stringsAsFactors = FALSE)
  res0 <- merge(res0,sb.data[,c("match.id","attendance")],by.x=c("match_id"),by.y=c("match.id"),all.x=TRUE)

  
  hist.res <- loading.historical.results()
  hist.res$team1_id <- NA
  hist.res$team2_id <- NA
  hist.res$div_id <- NA
  # res0 <- rbind(all.res,res0[,colnames(all.res)])
  
  #just merging national league and northern/southern non-league
#  all.res0 <- rbind(res0[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
#                    nlres,ilres,nplres)
  
    
  all.res0 <- rbind(res0[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
                    nlres,ilres,nplres,wyc,weald,weym,sfc,bfc,bufc,cat,cesp,
                    hist.res[,c("match_id","division","date","div_id","team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
  
  #remove based on duplicates: two teams playing on same day...
  all.res0 <- all.res0[order(all.res0$div_id),]
  all.res0 <- all.res0[duplicated(all.res0[,c("team1","team2","date")])==FALSE,]
  ##question: will we lose some attendances in doing this?
  
  all.res0$country <- NA
  ##merge in European results from World Football
  wf.not.wanted.countries <- c("England","Scotland")
  ##what years do various european countries start in soccerbase?
  
  
  bigres0 <- rbind(all.res0[duplicated(all.res0)==FALSE,c("match_id","date","country","div_id","division",
                           "team1","team1_id","goals1","goals2","team2","team2_id","attendance")],
                   wf.res0[!(wf.res0$country %in% wf.not.wanted.countries),c("match_id","date","country","div_id","division",
                              "team1","team1_id","goals1","goals2","team2","team2_id","attendance")])
  bigres0$goals1 <- as.numeric(bigres0$goals1)
  bigres0$goals2 <- as.numeric(bigres0$goals2)
  
  ##### need to match up competition names #####
  bigres0$division <- gsub("^\\s+|\\s+$"," ",gsub("-"," ",gsub("--","-",gsub("\\d{4}","",tolower(bigres0$division)))))
  bigres0$division <- gsub("^swe ","swedish ",gsub("^pol ","polish ",gsub("^fra ","french ",gsub("^por ","portuguese ",gsub("^esp ","spanish ",gsub("^ita ","italian ",gsub("^sco ","scottish ",gsub("^aut ","austrian ",bigres0$division))))))))
  bigres0$division <- gsub("^cze ","czech ",gsub("^cyp ","cypriot ",gsub("^col ","colombian ",gsub("^chn ","chinese ",gsub("^chi ","chilian ",gsub("^can ","canadian ",gsub("^bul ","bulgarian ",gsub("^bra ","brazilian ",bigres0$division))))))))
  bigres0$division <- gsub("^geo ","georgian ",gsub("^gdr ","east german ",gsub("^fro ","faroe islands ",gsub("^fin ","finnish ",gsub("^est ","estonian ",gsub("^egy ","egyptian ",gsub("^ecu ","ecudor ",gsub("^den ","danish ",bigres0$division))))))))
  bigres0$division <- gsub("^kaz ","kazakhstan ",gsub("^jpn ","japanese ",gsub("^idn ","indonesia ",gsub("^hun ","hungarian ",gsub("^hon ","honduras ",gsub("^hkg ","hong kong ",gsub("^gua ","guatemala ",gsub("^gre ","greek ",bigres0$division))))))))
  bigres0$division <- gsub("^irn ","iran ",gsub("^irq ","iraq ",gsub("^isr ","israel ",gsub("^irl ","ireland ",gsub("^kuw ","kuwait ",gsub("^ksa ","saudi arabia ",gsub("^kos ","kosovo ",gsub("^kor ","korean ",bigres0$division))))))))
  bigres0$division <- gsub("^ned ","dutch ",gsub("^mon ","montenegro ",gsub("^mlt ","malta ",gsub("^mda ","moldova  ",gsub("^mex ","mexico ",gsub("^mar ","morocco ",gsub("^lat ","latvian ",gsub("^ind ","indian ",bigres0$division))))))))
  bigres0$division <- gsub("^rom ","romanian ",gsub("^rsa ","south african ",gsub("^rus ","russian ",gsub("^par ","paraguay  ",gsub("^per ","peru ",gsub("^nor ","norwegian ",gsub("^nir ","ni ",gsub("^nga ","nigerian ",bigres0$division))))))))
  bigres0$division <- gsub("^svn ","slovenian ",gsub("^svk ","slovakian ",gsub("^sui ","swiss ",gsub("^sud ","sudan  ",gsub("^smr ","san marino ",gsub("^slv ","salvadoran ",gsub("^sin ","singapore ",gsub("^srb ","serbian ",bigres0$division))))))))
  bigres0$division <- gsub("^yug prva liga ","yugoslavian league ",gsub("^wal ","welsh ",gsub("^uzb ","uzbek ",gsub("^uru ","uruguat  ",gsub("^ukr ","ukrainian ",gsub("^tur ","turkish ",gsub("^tun ","tunisian ",gsub("^tha ","thai ",bigres0$division))))))))
  bigres0$division[regexpr("^ven",bigres0$division)>-1] <- "venezuela primera apertura"
  bigres0$division[regexpr("ltu a",bigres0$division)>-1] <- "lithuanian league"
  bigres0$division[regexpr("^ro\\w+\\s+liga",bigres0$division)>-1] <- "romanian liga"
  bigres0 <- bigres0[regexpr("^em ",bigres0$division)==-1 & regexpr("^wm ",bigres0$division)==-1 & regexpr("junioren",bigres0$division)==-1 & regexpr("u\\d+",bigres0$division)==-1 & regexpr("nations-league",bigres0$division)==-1,]
  bigres0$division <- gsub("^\\s+|\\s+$"," ",bigres0$division)
  bigres0$division <- gsub("^scottish div (\\d) [(]old[)]","scottish division \\1",bigres0$division)
  bigres0$division <- gsub("\\s{3,}","",gsub("fat","fa trophy",gsub("fac","fa cup",gsub("faac","fa amateur cup",bigres0$division))))
  bigres0$division <- gsub("fa challenge","fa",gsub("football association","fa",bigres0$division))
  bigres0$division <- gsub("\\d*qr$","",gsub("qf$","",gsub("sf$","",gsub("\\dq$","",gsub("\\dr$","",gsub("\\sf$","",bigres0$division))))))
  bigres0$division <- gsub("\\s+replay","",gsub("\\s+final","",gsub("\\s+semi final","",bigres0$division)))
  bigres0$division <- gsub("\\d\\w{2} leg","",bigres0$division)
  bigres0$division <- gsub("^(.*?) / .*?$","\\1",gsub("[.]","",bigres0$division))
  bigres0$division <- gsub("^(.*?) / .*?$","\\1",gsub("\\d\\w{2} round","",bigres0$division))
  bigres0$division <- gsub("^la liga$","spanish la liga",bigres0$division)
  bigres0$division <- gsub("^french ligue$","french ligue 1",bigres0$division)
  bigres0$division[regexpr("friend",bigres0$division)>-1] <- "friendly"
  bigres0$division[bigres0$division=="fr"] <- "friendly"
  #fa cup
  bigres0$division[regexpr("fa cup",bigres0$division)>-1 & regexpr("uefa",bigres0$division)==-1 & regexpr("afa",bigres0$division)==-1 & regexpr("scottish",bigres0$division)==-1] <- "fa cup"
  #fa trophy
  bigres0$division[regexpr("fa trophy",bigres0$division)>-1] <- "fa trophy"
  #efl trophy
  bigres0$division[regexpr("fl trophy",bigres0$division)>-1] <- "fl trophy"
  bigres0$division <- gsub("associate members trophy","fl trophy",gsub("football league trop","fl trophy",bigres0$division))
  #national league
  bigres0$division[regexpr("conference south",bigres0$division)>-1] <- "national league south"
  bigres0$division[regexpr("conference north",bigres0$division)>-1] <- "national league north"
  bigres0$division[regexpr("conference",bigres0$division)>-1 & regexpr("europa",bigres0$division)==-1 & regexpr("cup|shield|trophy",bigres0$division)==-1] <- "national league north"
  bigres0$division <- gsub("^gmvc$","national league",bigres0$division)
  bigres0$division <- gsub("blue sq premier","national league",bigres0$division)
  
  bigres0$division <- gsub("^conference","national league",gsub("football conference","national league",bigres0$division))
  bigres0$division[is.na(bigres0$division)==FALSE & regexpr("national league",bigres0$division)>-1] <- gsub("^eng ","",bigres0$division[is.na(bigres0$division)==FALSE & regexpr("national league",bigres0$division)>-1])
  bigres0$division[bigres0$division=="ned eredivisie"] <- "dutch eredivisie"
  bigres0$division[bigres0$division=="french ligue"] <- "french ligue 1"
  bigres0$division[regexpr("^serie a",bigres0$division)>-1] <- "italian serie a"
  bigres0$division[bigres0$division=="spanish segunda"] <- "spanish segunda division"
  bigres0$division[bigres0$division=="spanish primera division"] <- "spanish la liga"
  bigres0$division[bigres0$division=="spanish primera liga"] <- "spanish la liga"
  bigres0$division[bigres0$division=="la liga"] <- "spanish la liga"
  bigres0$division <- gsub("^2 bundesliga","german bundesliga 2",bigres0$division)
  bigres0$division <- gsub("^bundesliga$","german bundesliga",bigres0$division)
  bigres0$division[regexpr("german bundesliga 2",bigres0$division)>-1] <- "german bundesliga 2"
  bigres0$division <- gsub("^\\s+|\\s+$","",bigres0$division)

  #premier league/first division
#  bigres0$division[bigres0$division=="premiership"] <- "premier league"
  bigres0$division[bigres0$division %in% c("english div 1 (old)","premiership","premier league","eng premier league")] <- "english premier league"
  #championship/second division
  bigres0$division[bigres0$division %in% c("english div 2 (old)","football league div 1","championship","eng championship")] <- "english championship"
  #league one/division two/third division
  bigres0$division[bigres0$division %in% c("english div 3 (old)","football league div 2","league one","eng league one")] <- "english league one"
  #league two/division three/fourth division
  bigres0$division[bigres0$division %in% c("english div 4 (old)","football league div 3","league two","eng league two")] <- "english league one"
  
  # bigres0$division[c("scottish division 1","scottish premier","scottish premier league","scottish premiership")] <- "scottish premier league"
  # bigres0$division[c("scottish division 2","scottish premier","scottish premiership","scottish championship")] <- "scottish championship"
  # bigres0$division[c("scottish division 1","scottish premier","scottish premiership","scottish premiership")] <- "scottish league one"
  # bigres0$division[c("scottish division 1","scottish premier","scottish premiership","scottish premiership")] <- "scottish league two"
  
  
#  table(bigres0$division)
  
  ##### once leagues sorted, then harmonise team names across competitions #####
  ##### Spain #####
  bigres0$team1 <- gsub("ó","o",gsub("í","i",gsub("à","a",gsub("á","a",gsub("ñ","n",gsub("é","e",gsub("á","a",bigres0$team1)))))))
  bigres0$team1 <- gsub("î","i",gsub("è","e",gsub("ç","c",gsub("É","E",bigres0$team1))))
  bigres0$team1 <- gsub("ß","ss",gsub("ö","o",gsub("ü","u",bigres0$team1)))
  bigres0$team1 <- gsub("Å","A",gsub("ä","a",gsub("å","a",gsub("Ö","O",bigres0$team1))))

  bigres0$team2 <- gsub("ó","o",gsub("í","i",gsub("à","a",gsub("á","a",gsub("ñ","n",gsub("é","e",gsub("á","a",bigres0$team2)))))))
  bigres0$team2 <- gsub("î","i",gsub("è","e",gsub("ç","c",gsub("É","E",bigres0$team2))))
  bigres0$team2 <- gsub("ß","ss",gsub("ö","o",gsub("ü","u",bigres0$team2)))
  bigres0$team2 <- gsub("Å","A",gsub("ä","a",gsub("å","a",gsub("Ö","O",bigres0$team2))))
  
  #bigres0$team1 <- gsub("[.]","",bigres0$team1)
  bigres0$team1 <- gsub("^[0-9]+\\s+|\\s+[0-9]+$","",bigres0$team1)
  bigres0$team1 <- gsub("^[A-Z]+\\s+|\\s+[A-Z]+$","",bigres0$team1)
  bigres0$team1 <- gsub("^[0-9]+\\s+|\\s+[0-9]+$","",bigres0$team1)
  bigres0$team1 <- gsub("^[A-Z]+\\s+|\\s+[A-Z]+$","",bigres0$team1)
  bigres0$team1 <- gsub("Saint","St",bigres0$team1)
  bigres0$team1 <- gsub(" La Coruna","",gsub(" Santander","",gsub("Rayo ","R. ",gsub("Sporting ","Sp ",bigres0$team1))))
  bigres0$team1[bigres0$team1=="Espanyol Barcelona"] <- "Espanyol"
  bigres0$team1[bigres0$team1=="Athletic Bilbao"] <- "Ath Bilbao"
  bigres0$team1[bigres0$team1=="Atletico Madrid"] <- "Atl Madrid"
  bigres0$team1[bigres0$team1=="alcoy"] <- "Alcoyano"
  bigres0$team1[regexpr("Zaragoza",bigres0$team1)>-1] <- "Real Zaragoza"
  bigres0$team1[regexpr("Sociedad",bigres0$team1)>-1] <- "Real Sociedad"
  bigres0$team1[regexpr("Burgos",bigres0$team1)>-1] <- "Real Burgos"
  bigres0$team1[regexpr("Valladolid",bigres0$team1)>-1] <- "Real Valladolid"
  # table(bigres0$team1[bigres0$division=="spanish la liga"])

  bigres0$team2 <- gsub("^[0-9]+\\s+|\\s+[0-9]+$","",bigres0$team2)
  bigres0$team2 <- gsub("^[A-Z]+\\s+|\\s+[A-Z]+$","",bigres0$team2)
  bigres0$team2 <- gsub("^[0-9]+\\s+|\\s+[0-9]+$","",bigres0$team2)
  bigres0$team2 <- gsub("^[A-Z]+\\s+|\\s+[A-Z]+$","",bigres0$team2)
  bigres0$team2 <- gsub("Saint","St",bigres0$team2)
  bigres0$team2 <- gsub(" La Coruna","",gsub(" Santander","",gsub("Rayo ","R. ",gsub("Sporting ","Sp ",bigres0$team2))))
  bigres0$team2[bigres0$team2=="Espanyol Barcelona"] <- "Espanyol"
  bigres0$team2[bigres0$team2=="Athletic Bilbao"] <- "Ath Bilbao"
  bigres0$team2[bigres0$team2=="Atletico Madrid"] <- "Atl Madrid"
  bigres0$team2[bigres0$team2=="alcoy"] <- "Alcoyano"
  bigres0$team2[regexpr("Zaragoza",bigres0$team2)>-1] <- "Real Zaragoza"
  bigres0$team2[regexpr("Sociedad",bigres0$team2)>-1] <- "Real Sociedad"
  bigres0$team2[regexpr("Burgos",bigres0$team2)>-1] <- "Real Burgos"
  bigres0$team2[regexpr("Valladolid",bigres0$team2)>-1] <- "Real Valladolid"
  
  ##### France #####
  bigres0$team1[regexpr("Arles-",bigres0$team1)>-1] <- "Arles"
  bigres0$team1 <- gsub("Gazelec Football Club","GFC",bigres0$team1)
  bigres0$team1 <- gsub("Girondins ","",bigres0$team1)
  bigres0$team1[bigres0$team1_id=="gfc-ajaccio"] <- "GFC Ajaccio"
  bigres0$team1[bigres0$team1_id=="racing-club-de-france"] <- "Racing Club de France"
  bigres0$team1[regexpr("Brest",bigres0$team1)>-1 & bigres0$country=="France"] <- "Brest"
  bigres0$team1[regexpr("Stade Brest",bigres0$team1)>-1] <- "Brest"
  bigres0$team1[regexpr("Evian",bigres0$team1)>-1] <- "Evian"
  bigres0$team1[regexpr("Cannes",bigres0$team1)>-1] <- "Cannes"
  bigres0$team1[regexpr("Clermont",bigres0$team1)>-1] <- "Clermont"
  bigres0$team1[regexpr("Libourne",bigres0$team1)>-1] <- "Libourne"
  bigres0$team1[regexpr("Grenoble",bigres0$team1)>-1] <- "Grenoble"
  bigres0$team1[regexpr("^Olympique Lyon$",bigres0$team1)>-1] <- "Lyon"
  bigres0$team1[regexpr("Lyon",bigres0$team1)>-1 & regexpr("Duch",bigres0$team1)>-1] <- "Lyon Duchere"
  bigres0$team1[regexpr("Marseille",bigres0$team1)>-1 & regexpr("Athl",bigres0$team1)>-1] <- "Marseille Consolat"
  bigres0$team1[regexpr("Marseille",bigres0$team1)>-1 & regexpr("Consolat",bigres0$team1)==-1 & regexpr("CFA",bigres0$team1)==-1] <- "Marseille"
  bigres0$team1[regexpr("Montpellier",bigres0$team1)>-1 & regexpr("CFA",bigres0$team1)==-1] <- "Montpellier"
  bigres0$team1[regexpr("Troyes",bigres0$team1)>-1] <- "Troyes"
  bigres0$team1[regexpr("Toulon",bigres0$team1)>-1] <- "Toulon"
  bigres0$team1[regexpr("Stade Reims",bigres0$team1)>-1 & regexpr("CFA",bigres0$team1)==-1] <- "Reims"
  bigres0$team1[regexpr("Rennes",bigres0$team1)>-1] <- "Rennes"
  bigres0$team1[regexpr("Valenciennes",bigres0$team1)>-1] <- "Valenciennes"
  bigres0$team1[regexpr("Nimes",bigres0$team1)>-1] <- "Nimes"
  bigres0$team1[regexpr("Paris S",bigres0$team1)>-1] <- "PSG"
  bigres0$team1[regexpr("Sedan",bigres0$team1)>-1] <- "Sedan"
  bigres0$team1[regexpr("Stade de Paris",bigres0$team1)>-1] <- "Stade Francais"
  bigres0$team1[regexpr("Stade Red Star",bigres0$team1)>-1] <- "Stade Francais"
  bigres0$team1[bigres0$team1_id=="red-star-fc_2"] <- "Red Star"
  bigres0$team1[bigres0$team1_id==2193] <- "Red Star Belgrade"
  # table(bigres0$team1[regexpr("ligue 1",bigres0$division)>-1])

  bigres0$team2[regexpr("Arles-",bigres0$team2)>-1] <- "Arles"
  bigres0$team2 <- gsub("Gazelec Football Club","GFC",bigres0$team2)
  bigres0$team2 <- gsub("Girondins ","",bigres0$team2)
  bigres0$team2[bigres0$team2_id=="gfc-ajaccio"] <- "GFC Ajaccio"
  bigres0$team2[bigres0$team2_id=="racing-club-de-france"] <- "Racing Club de France"
  bigres0$team2[regexpr("Brest",bigres0$team2)>-1 & bigres0$country=="France"] <- "Brest"
  bigres0$team2[regexpr("Stade Brest",bigres0$team2)>-1] <- "Brest"
  bigres0$team2[regexpr("Evian",bigres0$team2)>-1] <- "Evian"
  bigres0$team2[regexpr("Cannes",bigres0$team2)>-1] <- "Cannes"
  bigres0$team2[regexpr("Clermont",bigres0$team2)>-1] <- "Clermont"
  bigres0$team2[regexpr("Libourne",bigres0$team2)>-1] <- "Libourne"
  bigres0$team2[regexpr("Grenoble",bigres0$team2)>-1] <- "Grenoble"
  bigres0$team2[regexpr("^Olympique Lyon$",bigres0$team2)>-1] <- "Lyon"
  bigres0$team2[regexpr("Lyon",bigres0$team2)>-1 & regexpr("Duch",bigres0$team2)>-1] <- "Lyon Duchere"
  bigres0$team2[regexpr("Marseille",bigres0$team2)>-1 & regexpr("Athl",bigres0$team2)>-1] <- "Marseille Consolat"
  bigres0$team2[regexpr("Marseille",bigres0$team2)>-1 & regexpr("Consolat",bigres0$team2)==-1 & regexpr("CFA",bigres0$team2)==-1] <- "Marseille"
  bigres0$team2[regexpr("Montpellier",bigres0$team2)>-1 & regexpr("CFA",bigres0$team2)==-1] <- "Montpellier"
  bigres0$team2[regexpr("Troyes",bigres0$team2)>-1] <- "Troyes"
  bigres0$team2[regexpr("Toulon",bigres0$team2)>-1] <- "Toulon"
  bigres0$team2[regexpr("Stade Reims",bigres0$team2)>-1 & regexpr("CFA",bigres0$team2)==-1] <- "Reims"
  bigres0$team2[regexpr("Rennes",bigres0$team2)>-1] <- "Rennes"
  bigres0$team2[regexpr("Valenciennes",bigres0$team2)>-1] <- "Valenciennes"
  bigres0$team2[regexpr("Nimes",bigres0$team2)>-1] <- "Nimes"
  bigres0$team2[regexpr("Paris S",bigres0$team2)>-1] <- "PSG"
  bigres0$team2[regexpr("Sedan",bigres0$team2)>-1] <- "Sedan"
  bigres0$team2[regexpr("Stade de Paris",bigres0$team2)>-1] <- "Stade Francais"
  bigres0$team2[regexpr("Stade Red Star",bigres0$team2)>-1] <- "Stade Francais"
  bigres0$team2[bigres0$team2_id=="red-star-fc_2"] <- "Red Star"
  bigres0$team2[bigres0$team2_id==2193] <- "Red Star Belgrade"
  
  ##### Italy #####
  bigres0$team1 <- gsub("^Calcio\\s+|\\s+Calcio$","",bigres0$team1)
  bigres0$team1[regexpr("Ancon",bigres0$team1)>-1] <- "Ancona"
  bigres0$team1[regexpr("Messina",bigres0$team1)>-1] <- "Messina"
  bigres0$team1[bigres0$team1_id=="vicenza-virtus"] <- "Vicenza"
  bigres0$team1[regexpr("Chievo",bigres0$team1)>-1] <- "Chievo"
  bigres0$team1[regexpr("Genova",bigres0$team1)>-1] <- "Genoa"
  bigres0$team1[regexpr("Hellas Verona",bigres0$team1)>-1] <- "Verona"
  bigres0$team1[regexpr("Lazio",bigres0$team1)>-1] <- "Lazio"
  bigres0$team1[regexpr("Salernitana",bigres0$team1)>-1] <- "Salernitana"
  bigres0$team1[regexpr("Venezia",bigres0$team1)>-1] <- "Venezia"
  # table(bigres0$team1[bigres0$division=="italian serie a"])
  
  bigres0$team2 <- gsub("^Calcio\\s+|\\s+Calcio$","",bigres0$team2)
  bigres0$team2[regexpr("Ancon",bigres0$team2)>-1] <- "Ancona"
  bigres0$team2[regexpr("Messina",bigres0$team2)>-1] <- "Messina"
  bigres0$team2[bigres0$team2_id=="vicenza-virtus"] <- "Vicenza"
  bigres0$team2[regexpr("Chievo",bigres0$team2)>-1] <- "Chievo"
  bigres0$team2[regexpr("Genova",bigres0$team2)>-1] <- "Genoa"
  bigres0$team2[regexpr("Hellas Verona",bigres0$team2)>-1] <- "Verona"
  bigres0$team2[regexpr("Lazio",bigres0$team2)>-1] <- "Lazio"
  bigres0$team2[regexpr("Salernitana",bigres0$team2)>-1] <- "Salernitana"
  bigres0$team2[regexpr("Venezia",bigres0$team2)>-1] <- "Venezia"
  
  ##### Germany #####
  bigres0$team1 <- gsub("^(.*?) \\d+ (.*?)$","\\1 \\2",bigres0$team1)
  bigres0$team1 <- gsub("Nuremberg","Nurnberg",gsub("Cologne","Koln",bigres0$team1))
  bigres0$team1[bigres0$team1=="Berlin"] <- "TeBe Berlin"
  bigres0$team1[bigres0$team1_id=="bfc-dynamo"] <- "Dynamo Berlin"
  bigres0$team1[bigres0$team1_id=="1-fc-frankfurt-oder"] <- "Vorwaerts Berlin"
  bigres0$team1[bigres0$team1=="Arminia"] <- "Arminia Bielefeld"
  bigres0$team1[bigres0$team1=="1. FC Saarbrucken"] <- "Saarbrucken"
  bigres0$team1[bigres0$team1_id==398] <- "Borussia Dortmund"
  bigres0$team1[bigres0$team1_id %in% c(5002,"fsv-frankfurt")] <- "FSV Frankfurt"
  bigres0$team1[bigres0$team1_id %in% c(884,"eintracht-frankfurt")] <- "Eintracht Frankfurt"
  bigres0$team1[bigres0$team1_id %in% c(1777,"tsv-1860-muenchen")] <- "1860 Munich"
  bigres0$team1[bigres0$team1_id %in% c(469,"bayern-muenchen")] <- "Bayern Munich"
  bigres0$team1[bigres0$team1_id %in% c(2694,"viktoria-koeln")] <- "Viktoria Koln"
  bigres0$team1[bigres0$team1_id %in% c(970,"1-fc-koeln")] <- "Koln"
  bigres0$team1[bigres0$team1_id %in% c(7697,"tsv-schott-mainz")] <- "Schott Mainz"
  bigres0$team1[bigres0$team1_id=="wuerzburger-kickers"] <- "Wurz Kickers"
  bigres0$team1[bigres0$team1_id=="wuerzburger-fv"] <- "Wurzburger"
  bigres0$team1[regexpr("Braunschweig",bigres0$team1)>-1] <- "Braunschweig"
  bigres0$team1[regexpr("Cottbus",bigres0$team1)>-1 & regexpr("Lokom",bigres0$team1)==-1] <- "Cottbus"
  bigres0$team1[regexpr("Dusseldorf",bigres0$team1)>-1] <- "Dusseldorf"
  bigres0$team1[regexpr("Erzgebirge",bigres0$team1)>-1] <- "Erzgebirge"
  bigres0$team1[regexpr("Dyn Dresden",bigres0$team1)>-1] <- "Dynamo Dresden"
  bigres0$team1[regexpr("Heidenheim",bigres0$team1)>-1] <- "Heidenheim"
  bigres0$team1[regexpr("Kaiserslautern",bigres0$team1)>-1 & regexpr("VfR",bigres0$team1)==-1] <- "Heidenheim"
  bigres0$team1[regexpr("FSV Mainz",bigres0$team1)>-1] <- "Mainz"
  bigres0$team1[regexpr("Magdeburg",bigres0$team1)>-1] <- "Magdeburg"
  bigres0$team1[regexpr("Nurnberg",bigres0$team1)>-1] <- "Nurnberg"
  bigres0$team1[regexpr("Neunkirchen",bigres0$team1)>-1] <- "Neunkirchen"
  bigres0$team1[regexpr("M'gladbach",bigres0$team1)>-1] <- "Bor. Monchengladbach"
  bigres0$team1[regexpr("Hamburger",bigres0$team1)>-1] <- "Hamburg"
  bigres0$team1[regexpr("Novo Hamburgo",bigres0$team1)>-1] <- "Novo Hamburgo"
  bigres0$team1[regexpr("Karlsruhe",bigres0$team1)>-1] <- "Karlsruher"
  bigres0$team1[regexpr("Regensburg",bigres0$team1)>-1] <- "Regensburg"
  bigres0$team1[regexpr("Kickers Off",bigres0$team1)>-1] <- "Kickers Offenbach"
  bigres0$team1[regexpr("Lev'kusen",bigres0$team1)>-1] <- "Leverkusen"
  bigres0$team1[regexpr("Leverkusen",bigres0$team1)>-1] <- "Leverkusen"
  bigres0$team1[bigres0$team1_id==2854] <- "Werder Bremen"
  bigres0$team1[regexpr("R-Weiss Essen",bigres0$team1)>-1] <- "Rot-Weiss Essen"
  bigres0$team1[regexpr("Oberhausen",bigres0$team1)>-1] <- "Oberhausen"
  bigres0$team1[regexpr("Furth",bigres0$team1)>-1] <- "Furth"
  bigres0$team1[bigres0$team1_id %in% c(2301,"stuttgarter-kickers")] <- "Stuttgart Kickers"
  bigres0$team1[bigres0$team1_id %in% c(2682,"vfb-stuttgart")] <- "VfB Stuttgart"
  
  bigres0$team1[regexpr("Uerdingen",bigres0$team1)>-1] <- "Uerdingen"
  bigres0$team1[regexpr("Unterhaching",bigres0$team1)>-1] <- "Unterhaching"
  bigres0$team1 <- gsub("Einheit Ost","1. FC Lok",gsub("1. FC Lokomotive","1. FC Lok",bigres0$team1))
  bigres0$team1[bigres0$team1_id %in% c(5709,"rb-leipzig")] <- "Stuttgart Kickers"
  bigres0$team1[bigres0$team1_id %in% c(1566,"vfb-stuttgart")] <- "VfB Stuttgart"
  bigres0$team1[regexpr("Wolfsburg",bigres0$team1)>-1] <- "Wolfsburg"
  bigres0$team1[regexpr("Waldhof Mannheim",bigres0$team1)>-1] <- "Mannheim"
  
  bigres0$team1[regexpr("Union Berlin",bigres0$team1)>-1] <- "Union Berlin"
  bigres0$team1[regexpr("Zeiss Jena",bigres0$team1)>-1] <- "Carl Zeiss Jena"
  bigres0$team1[regexpr("Wehen-Wiesb",bigres0$team1)>-1] <- "Wehen Wiesbaden"
  # table(bigres0$team1[regexpr("german",bigres0$division)>-1 & regexpr("bundesliga",bigres0$division)>-1])

  bigres0$team2 <- gsub("^(.*?) \\d+ (.*?)$","\\1 \\2",bigres0$team2)
  bigres0$team2 <- gsub("Nuremberg","Nurnberg",gsub("Cologne","Koln",bigres0$team2))
  bigres0$team2[bigres0$team2=="Berlin"] <- "TeBe Berlin"
  bigres0$team2[bigres0$team2_id=="bfc-dynamo"] <- "Dynamo Berlin"
  bigres0$team2[bigres0$team2_id=="1-fc-frankfurt-oder"] <- "Vorwaerts Berlin"
  bigres0$team2[bigres0$team2=="Arminia"] <- "Arminia Bielefeld"
  bigres0$team2[bigres0$team2=="1. FC Saarbrucken"] <- "Saarbrucken"
  bigres0$team2[bigres0$team2_id==398] <- "Borussia Dortmund"
  bigres0$team2[bigres0$team2_id %in% c(5002,"fsv-frankfurt")] <- "FSV Frankfurt"
  bigres0$team2[bigres0$team2_id %in% c(884,"eintracht-frankfurt")] <- "Eintracht Frankfurt"
  bigres0$team2[bigres0$team2_id %in% c(1777,"tsv-1860-muenchen")] <- "1860 Munich"
  bigres0$team2[bigres0$team2_id %in% c(469,"bayern-muenchen")] <- "Bayern Munich"
  bigres0$team2[bigres0$team2_id %in% c(2694,"viktoria-koeln")] <- "Viktoria Koln"
  bigres0$team2[bigres0$team2_id %in% c(970,"1-fc-koeln")] <- "Koln"
  bigres0$team2[bigres0$team2_id %in% c(7697,"tsv-schott-mainz")] <- "Schott Mainz"
  bigres0$team2[bigres0$team2_id=="wuerzburger-kickers"] <- "Wurz Kickers"
  bigres0$team2[bigres0$team2_id=="wuerzburger-fv"] <- "Wurzburger"
  bigres0$team2[regexpr("Braunschweig",bigres0$team2)>-1] <- "Braunschweig"
  bigres0$team2[regexpr("Cottbus",bigres0$team2)>-1 & regexpr("Lokom",bigres0$team2)==-1] <- "Cottbus"
  bigres0$team2[regexpr("Dusseldorf",bigres0$team2)>-1] <- "Dusseldorf"
  bigres0$team2[regexpr("Erzgebirge",bigres0$team2)>-1] <- "Erzgebirge"
  bigres0$team2[regexpr("Dyn Dresden",bigres0$team2)>-1] <- "Dynamo Dresden"
  bigres0$team2[regexpr("Heidenheim",bigres0$team2)>-1] <- "Heidenheim"
  bigres0$team2[regexpr("Kaiserslautern",bigres0$team2)>-1 & regexpr("VfR",bigres0$team2)==-1] <- "Heidenheim"
  bigres0$team2[regexpr("FSV Mainz",bigres0$team2)>-1] <- "Mainz"
  bigres0$team2[regexpr("Magdeburg",bigres0$team2)>-1] <- "Magdeburg"
  bigres0$team2[regexpr("Nurnberg",bigres0$team2)>-1] <- "Nurnberg"
  bigres0$team2[regexpr("Neunkirchen",bigres0$team2)>-1] <- "Neunkirchen"
  bigres0$team2[regexpr("M'gladbach",bigres0$team2)>-1] <- "Bor. Monchengladbach"
  bigres0$team2[regexpr("Hamburger",bigres0$team2)>-1] <- "Hamburg"
  bigres0$team2[regexpr("Novo Hamburgo",bigres0$team2)>-1] <- "Novo Hamburgo"
  bigres0$team2[regexpr("Karlsruhe",bigres0$team2)>-1] <- "Karlsruher"
  bigres0$team2[regexpr("Regensburg",bigres0$team2)>-1] <- "Regensburg"
  bigres0$team2[regexpr("Kickers Off",bigres0$team2)>-1] <- "Kickers Offenbach"
  bigres0$team2[regexpr("Lev'kusen",bigres0$team2)>-1] <- "Leverkusen"
  bigres0$team2[regexpr("Leverkusen",bigres0$team2)>-1] <- "Leverkusen"
  bigres0$team2[bigres0$team2_id==2854] <- "Werder Bremen"
  bigres0$team2[regexpr("R-Weiss Essen",bigres0$team2)>-1] <- "Rot-Weiss Essen"
  bigres0$team2[regexpr("Oberhausen",bigres0$team2)>-1] <- "Oberhausen"
  bigres0$team2[regexpr("Furth",bigres0$team2)>-1] <- "Furth"
  bigres0$team2[bigres0$team2_id %in% c(2301,"stuttgarter-kickers")] <- "Stuttgart Kickers"
  bigres0$team2[bigres0$team2_id %in% c(2682,"vfb-stuttgart")] <- "VfB Stuttgart"
  
  bigres0$team2[regexpr("Uerdingen",bigres0$team2)>-1] <- "Uerdingen"
  bigres0$team2[regexpr("Unterhaching",bigres0$team2)>-1] <- "Unterhaching"
  bigres0$team2 <- gsub("Einheit Ost","1. FC Lok",gsub("1. FC Lokomotive","1. FC Lok",bigres0$team2))
  bigres0$team2[bigres0$team2_id %in% c(5709,"rb-leipzig")] <- "Stuttgart Kickers"
  bigres0$team2[bigres0$team2_id %in% c(1566,"vfb-stuttgart")] <- "VfB Stuttgart"
  bigres0$team2[regexpr("Wolfsburg",bigres0$team2)>-1] <- "Wolfsburg"
  bigres0$team2[regexpr("Waldhof Mannheim",bigres0$team2)>-1] <- "Mannheim"
  
  bigres0$team2[regexpr("Union Berlin",bigres0$team2)>-1] <- "Union Berlin"
  bigres0$team2[regexpr("Zeiss Jena",bigres0$team2)>-1] <- "Carl Zeiss Jena"
  bigres0$team2[regexpr("Wehen-Wiesb",bigres0$team2)>-1] <- "Wehen Wiesbaden"
  
  ##### Sweden #####
  # table(bigres0$team1[regexpr("swedish allsvenskan",bigres0$division)>-1])
  bigres0$team1 <- gsub("^BP$","Brommapojkarna",gsub("Br'pojkarna","Brommapojkarna",gsub("Enkopings","Enkoping",gsub("^Eskilstuna City$","Eskilstuna",gsub("Goth'burg","Goteborg",gsub(" BoIS","",bigres0$team1))))))
  bigres0$team2 <- gsub("^BP$","Brommapojkarna",gsub("Br'pojkarna","Brommapojkarna",gsub("Enkopings","Enkoping",gsub("^Eskilstuna City$","Eskilstuna",gsub("Goth'burg","Goteborg",gsub(" BoIS","",bigres0$team2))))))
  
  ##### Austria (another time --- need to collect archived leagues) #####
  # table(bigres0$team1[regexpr("austrian",bigres0$division)>-1])
  
  ##### Scottish teams #####
  bigres0$team1[regexpr("heart of m",tolower(bigres0$team1))>-1] <- "Hearts"
  bigres0$team2[regexpr("heart of m",tolower(bigres0$team2))>-1] <- "Hearts"
  
  ##### English teams #####
  bigres0$team1 <- gsub("^Manchester ","Man ",bigres0$team1)
  bigres0$team1 <- gsub("^Sheffield ","Sheff ",bigres0$team1)
  bigres0$team1[regexpr("Yorkshire Am",tolower(bigres0$team1))>-1] <- "Yorkshire Am"
  bigres0$team2[regexpr("Yorkshire Am",tolower(bigres0$team2))>-1] <- "Yorkshire Am"
  bigres0$team1[regexpr("York City",tolower(bigres0$team1))>-1] <- "York"
  bigres0$team2[regexpr("York City",tolower(bigres0$team2))>-1] <- "York"
  bigres0$team1[regexpr("Anorthosis",tolower(bigres0$team1))>-1] <- "Fam Anorthosis Fam"
  bigres0$team1[regexpr("Aris Salonik",tolower(bigres0$team1))>-1] <- "Aris Saloniki"
  bigres0$team1 <- gsub("B'ham","Birmingham",bigres0$team1)
  bigres0$team1 <- gsub("Wellingborough","Wellingboro",bigres0$team1)
  bigres0$team1 <- gsub("West Bromwich Albion","West Brom",gsub("Wolverhampton","Wolves",gsub(" Wanderers","",bigres0$team1)))
  bigres0$team1[is.na(bigres0$team1)==FALSE & regexpr("^Man|Sheff|Bris|Ash|Read|Stock",bigres0$team1)==-1] <- gsub(" County"," Co",gsub(" Town","",gsub(" Rovers","",gsub(" Athletic","",gsub(" Albion","",gsub(" United","",gsub(" Utd","",gsub(" City","",bigres0$team1[is.na(bigres0$team1)==FALSE & regexpr("^Man|Sheff|Bris|Ash|Read|Stock",bigres0$team1)==-1]))))))))
  bigres0$team1[regexpr("Wingate and",bigres0$team1)>-1 | regexpr("Wingate &",bigres0$team1)>-1] <- "Wingate and Finchley"
  bigres0$team1[regexpr("Windsor and",bigres0$team1)>-1 | regexpr("Windsor &",bigres0$team1)>-1 | regexpr("Windsor Eton",bigres0$team1)>-1] <- "Windsor and Eton"
  bigres0$team1[regexpr("Walton and",bigres0$team1)>-1 | regexpr("Walton &",bigres0$team1)>-1] <- "Walton and Hersham"
  bigres0$team1[regexpr("Weston",bigres0$team1)>-1 & regexpr("Workers",bigres0$team1)==-1] <- "Weston-Super-Mare"
  bigres0$team1 <- gsub("AlbinoLeffe","Albinoleffe",bigres0$team1)
  bigres0$team1 <- gsub("[.]","",bigres0$team1)
  bigres0$team1 <- gsub("^\\s+|\\s+$","",bigres0$team1)
  
  bigres0$team2 <- gsub("^Manchester ","Man ",bigres0$team2)
  bigres0$team2 <- gsub("^Sheffield ","Sheff ",bigres0$team2)
  bigres0$team2[regexpr("Anorthosis",tolower(bigres0$team2))>-1] <- "Fam Anorthosis Fam"
  bigres0$team2[regexpr("Aris Salonik",tolower(bigres0$team2))>-1] <- "Aris Saloniki"
  bigres0$team2 <- gsub("B'ham","Birmingham",bigres0$team2)
  bigres0$team2 <- gsub("Wellingborough","Wellingboro",bigres0$team2)
  bigres0$team2 <- gsub("West Bromwich Albion","West Brom",gsub("Wolverhampton","Wolves",gsub(" Wanderers","",bigres0$team2)))
  bigres0$team2[is.na(bigres0$team2)==FALSE & regexpr("^Man|Sheff|Bris|Ash|Read|Stock",bigres0$team2)==-1] <- gsub(" County"," Co",gsub(" Town","",gsub(" Rovers","",gsub(" Athletic","",gsub(" Albion","",gsub(" United","",gsub(" Utd","",gsub(" City","",bigres0$team2[is.na(bigres0$team2)==FALSE & regexpr("^Man|Sheff|Bris|Ash|Read|Stock",bigres0$team2)==-1]))))))))
  bigres0$team2[regexpr("Wingate and",bigres0$team2)>-1 | regexpr("Wingate &",bigres0$team2)>-1] <- "Wingate and Finchley"
  bigres0$team2[regexpr("Windsor and",bigres0$team2)>-1 | regexpr("Windsor &",bigres0$team2)>-1 | regexpr("Windsor Eton",bigres0$team2)>-1] <- "Windsor and Eton"
  bigres0$team2[regexpr("Walton and",bigres0$team2)>-1 | regexpr("Walton &",bigres0$team2)>-1] <- "Walton and Hersham"
  bigres0$team2[regexpr("Weston",bigres0$team2)>-1 & regexpr("Workers",bigres0$team2)==-1] <- "Weston-Super-Mare"
  bigres0$team2 <- gsub("AlbinoLeffe","Albinoleffe",bigres0$team2)
  bigres0$team2 <- gsub("[.]","",bigres0$team2)
  bigres0$team2 <- gsub("^\\s+|\\s+$","",bigres0$team2)
  
  bigres0$team1[bigres0$team1_id==5656] <- "AFC Mansfield"
  bigres0$team2[bigres0$team2_id==5656] <- "AFC Mansfield"
  bigres0$team1[bigres0$team1_id==6395] <- "Stockport Town"
  bigres0$team2[bigres0$team2_id==6395] <- "Stockport Town"
  bigres0$team1[bigres0$team1_id==5311] <- "Reading Town"
  bigres0$team2[bigres0$team2_id==5311] <- "Reading Town"
  bigres0$team1[bigres0$team1_id==7170] <- "Reading City"
  bigres0$team2[bigres0$team2_id==7170] <- "Reading City"
  
  ##### Separate women's teams by team1 and team2 which will be used for Elo #####
  bigres0$team1[is.na(bigres0$team1_id)==FALSE & regexpr("frauen",bigres0$team1_id)>-1] <- paste0(bigres0$team1[is.na(bigres0$team1_id)==FALSE & regexpr("frauen",bigres0$team1_id)>-1]," womens")
  bigres0$team2[is.na(bigres0$team2_id)==FALSE & regexpr("frauen",bigres0$team2_id)>-1] <- paste0(bigres0$team2[is.na(bigres0$team2_id)==FALSE & regexpr("frauen",bigres0$team2_id)>-1]," womens")
  


  bigres0 <- bigres0[bigres0$team1!="" & bigres0$team2!="",]
  bigres0 <- bigres0[is.na(bigres0$team1)==FALSE & is.na(bigres0$team2)==FALSE,]
  
  ##### Give team ID to non-soccerbase teams #####
  bigres0$team1_id <- as.numeric(bigres0$team1_id)
  bigres0$team2_id <- as.numeric(bigres0$team2_id)
  all.teams0 <- bigres0[,c("team1","team1_id")]
  colnames(all.teams0) <- c("team","team_id")
  all.teams1 <- bigres0[,c("team2","team2_id")]
  colnames(all.teams1) <- c("team","team_id")
  all.teams <- rbind(all.teams0,all.teams1)
  all.teams <- all.teams[is.na(all.teams$team)==FALSE,]
  #first remove all duplicates
  all.teams <- all.teams[duplicated(all.teams)==FALSE,]
  #now remove if team has both NA and actual ID
  all.teams <- all.teams[order(all.teams$team,all.teams$team_id,decreasing = TRUE),]
  all.teams <- all.teams[duplicated(all.teams$team)==FALSE,]
  #now need all teams with NA to have an ID - possibility all teams won't have Soccerbase ID
  all.teams$team_id[is.na(all.teams$team_id)==TRUE] <- seq(max(all.teams$team_id,na.rm=TRUE)+1,max(all.teams$team_id,na.rm=TRUE)+NROW(all.teams$team_id[is.na(all.teams$team_id)==TRUE]))
  #match up some old team names to later team names
  all.teams$team_id[all.teams$team=="Burslem Port Vale"] <- all.teams$team_id[all.teams$team=="Port Vale"]
  
  bigres0 <- merge(bigres0,all.teams,by.x=c("team1"),by.y=c("team"),all.x=TRUE)
  bigres0 <- merge(bigres0,all.teams,by.x=c("team2"),by.y=c("team"),all.x=TRUE,suffixes = c("1","2"))
  bigres0$team1_id <- NULL
  bigres0$team2_id <- NULL
  colnames(bigres0)[colnames(bigres0) %in% c("team_id1","team_id2")] <- c("team1_id","team2_id")
  
  #all divs without an ID need an ID...
  bigres0$division[regexpr("Friend",bigres0$division)>-1] <- "Friendly"
  all.divs <- bigres0[,c("division","div_id")]
  all.divs <- all.divs[is.na(all.divs$division)==FALSE,]
  #first remove all duplicates
  all.divs <- all.divs[duplicated(all.divs)==FALSE,]
  #now remove if div has both NA and actual ID
  all.divs <- all.divs[order(all.divs$division,all.divs$div_id,decreasing = TRUE),]
  all.divs <- all.divs[duplicated(all.divs$division)==FALSE,]
  #now need all divs with NA to have an ID
  all.divs$div_id[is.na(all.divs$div_id)==TRUE] <- seq(max(all.divs$div_id,na.rm=TRUE)+1,max(all.divs$div_id,na.rm=TRUE)+NROW(all.divs$div_id[is.na(all.divs$div_id)==TRUE]))
  
  census.date <- "2022-06-01"
  
  #remove matches taking place after August 5 (i.e. the 2022/23 season)
  #bigres0 <- bigres0[bigres0$date<=census.date,]
  #######
  
  ##remove duplicate entries
  bigres0 <- bigres0[duplicated(bigres0)==FALSE,]
  bigres0 <- bigres0[order(bigres0$div_id),]
  bigres0 <- bigres0[duplicated(bigres0[,c("date","team1","team2")])==FALSE,]
  
  #get rid of any matches that haven't occurred before today (i.e. have NAs in the score)
  bigres0 <- bigres0[!(is.na(bigres0$goals1)==TRUE & bigres0$date<census.date),]
  
  #get rid of matches that are "winner matches"
  #res <- res[regexpr("winner",tolower(res$team1))>-1,]
  bigres0 <- bigres0[!(bigres0$match_id=="tgc811051" & bigres0$goals2==4),] #remove odd duplicate with wrong score
  bigres0 <- bigres0[!(bigres0$match_id=="tgc813129" & bigres0$goals2==4),] #remove odd duplicate with wrong score
  bigres0$outcome <- 0.5*(bigres0$goals1==bigres0$goals2) + (bigres0$goals1>bigres0$goals2)

  bigres0 <- bigres0[order(bigres0$date),]
  bigres0$team1 <- gsub(" [(]old[)]","",bigres0$team1)
  bigres0$team2 <- gsub(" [(]old[)]","",bigres0$team2)
  bigres0$team1 <- gsub("^\\s+|\\s+$","",bigres0$team1)
  bigres0$team2 <- gsub("^\\s+|\\s+$","",bigres0$team2)

  ## Elo ranking routine ####
  elorank01 <- list()
  elorank05 <- list()
  elorank10 <- list()
  elorank20 <- list()
  elorank30 <- list()
  
  bigres0$elostrength01.1 <- NA
  bigres0$elostrength01.2 <- NA
  bigres0$elo01predict <- NA
  bigres0$elostrength05.1 <- NA
  bigres0$elostrength05.2 <- NA
  bigres0$elo05predict <- NA
  bigres0$elostrength10.1 <- NA
  bigres0$elostrength10.2 <- NA
  bigres0$elo10predict <- NA
  bigres0$elostrength20.1 <- NA
  bigres0$elostrength20.2 <- NA
  bigres0$elo20predict <- NA
  bigres0$elostrength30.1 <- NA
  bigres0$elostrength30.2 <- NA
  bigres0$elo30predict <- NA

  bigres0 <- bigres0[order(bigres0$date),]
  
  for(mm in c(527511:NROW(bigres0))) {
    print(100*mm/NROW(bigres0))
    if(!(paste0("id",bigres0$team1_id[mm]) %in% names(elorank01))) {
      elorank01[[paste0("id",bigres0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team1_id[mm]) %in% names(elorank05))) {
      elorank05[[paste0("id",bigres0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team1_id[mm]) %in% names(elorank10))) {
      elorank10[[paste0("id",bigres0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team1_id[mm]) %in% names(elorank20))) {
      elorank20[[paste0("id",bigres0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team1_id[mm]) %in% names(elorank30))) {
      elorank30[[paste0("id",bigres0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team2_id[mm]) %in% names(elorank01))) {
      elorank01[[paste0("id",bigres0$team2_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team2_id[mm]) %in% names(elorank05))) {
      elorank05[[paste0("id",bigres0$team2_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team2_id[mm]) %in% names(elorank10))) {
      elorank10[[paste0("id",bigres0$team2_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team2_id[mm]) %in% names(elorank20))) {
      elorank20[[paste0("id",bigres0$team2_id[mm])]] <- 1000
    }
    if(!(paste0("id",bigres0$team2_id[mm]) %in% names(elorank30))) {
      elorank30[[paste0("id",bigres0$team2_id[mm])]] <- 1000
    }
    bigres0$elostrength01.1[mm] <- elorank01[[paste0("id",bigres0$team1_id[mm])]]
    bigres0$elostrength01.2[mm] <- elorank01[[paste0("id",bigres0$team2_id[mm])]]
    bigres0$elo01predict[mm] <- 1/(1+(10^((bigres0$elostrength01.2[mm]-bigres0$elostrength01.1[mm])/400)))
    bigres0$elostrength05.1[mm] <- elorank05[[paste0("id",bigres0$team1_id[mm])]]
    bigres0$elostrength05.2[mm] <- elorank05[[paste0("id",bigres0$team2_id[mm])]]
    bigres0$elo05predict[mm] <- 1/(1+(10^((bigres0$elostrength05.2[mm]-bigres0$elostrength05.1[mm])/400)))
    bigres0$elostrength10.1[mm] <- elorank10[[paste0("id",bigres0$team1_id[mm])]]
    bigres0$elostrength10.2[mm] <- elorank10[[paste0("id",bigres0$team2_id[mm])]]
    bigres0$elo10predict[mm] <- 1/(1+(10^((bigres0$elostrength10.2[mm]-bigres0$elostrength10.1[mm])/400)))
    bigres0$elostrength20.1[mm] <- elorank20[[paste0("id",bigres0$team1_id[mm])]]
    bigres0$elostrength20.2[mm] <- elorank20[[paste0("id",bigres0$team2_id[mm])]]
    bigres0$elo20predict[mm] <- 1/(1+(10^((bigres0$elostrength20.2[mm]-bigres0$elostrength20.1[mm])/400)))
    bigres0$elostrength30.1[mm] <- elorank30[[paste0("id",bigres0$team1_id[mm])]]
    bigres0$elostrength30.2[mm] <- elorank30[[paste0("id",bigres0$team2_id[mm])]]
    bigres0$elo30predict[mm] <- 1/(1+(10^((bigres0$elostrength30.2[mm]-bigres0$elostrength30.1[mm])/400)))
    adjustment01 <- bigres0$outcome[mm] - bigres0$elo10predict[mm]
    adjustment05 <- bigres0$outcome[mm] - bigres0$elo10predict[mm]
    adjustment10 <- bigres0$outcome[mm] - bigres0$elo10predict[mm]
    adjustment20 <- bigres0$outcome[mm] - bigres0$elo20predict[mm]
    adjustment30 <- bigres0$outcome[mm] - bigres0$elo30predict[mm]
    if(is.na(adjustment01)==FALSE) {
      elorank01[paste0("id",bigres0$team1_id[mm])] <- elorank01[[paste0("id",bigres0$team1_id[mm])]] + 01*adjustment01
      elorank01[paste0("id",bigres0$team2_id[mm])] <- elorank01[[paste0("id",bigres0$team2_id[mm])]] - 01*adjustment01
    }
    if(is.na(adjustment05)==FALSE) {
      elorank05[paste0("id",bigres0$team1_id[mm])] <- elorank05[[paste0("id",bigres0$team1_id[mm])]] + 05*adjustment05
      elorank05[paste0("id",bigres0$team2_id[mm])] <- elorank05[[paste0("id",bigres0$team2_id[mm])]] - 05*adjustment05
    }
    if(is.na(adjustment10)==FALSE) {
      elorank10[paste0("id",bigres0$team1_id[mm])] <- elorank10[[paste0("id",bigres0$team1_id[mm])]] + 10*adjustment10
      elorank10[paste0("id",bigres0$team2_id[mm])] <- elorank10[[paste0("id",bigres0$team2_id[mm])]] - 10*adjustment10
    }
    if(is.na(adjustment20)==FALSE) {
      elorank20[paste0("id",bigres0$team1_id[mm])] <- elorank20[[paste0("id",bigres0$team1_id[mm])]] + 20*adjustment20
      elorank20[paste0("id",bigres0$team2_id[mm])] <- elorank20[[paste0("id",bigres0$team2_id[mm])]] - 20*adjustment20
    }
    if(is.na(adjustment30)==FALSE) {
      elorank30[paste0("id",bigres0$team1_id[mm])] <- elorank30[[paste0("id",bigres0$team1_id[mm])]] + 30*adjustment30
      elorank30[paste0("id",bigres0$team2_id[mm])] <- elorank30[[paste0("id",bigres0$team2_id[mm])]] - 30*adjustment30
    }
  }
  bigres0 <- bigres0[is.na(bigres0$date)==FALSE,]
  
  ## end of elo rating routine ####
  
  plot(bigres0$date[bigres0$team1=="Nottm Forest"],bigres0$elostrength10.1[bigres0$team1=="Nottm Forest"],type = "p",col="red",xlim=range(bigres0$date[is.na(bigres0$elo10predict)==FALSE],na.rm=TRUE),pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team2=="Nottm Forest"],bigres0$elostrength10.2[bigres0$team2=="Nottm Forest"],type = "p",col="red",pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team1=="Man Utd"],bigres0$elostrength10.1[bigres0$team1=="Man Utd"],
        type = "p",col="darkred",xlim=range(bigres0$date[is.na(bigres0$elo10predict)==FALSE],na.rm=TRUE),pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team2=="Man Utd"],bigres0$elostrength10.2[bigres0$team2=="Man Utd"],
        type = "p",col="darkred",pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team1=="Barcelona"],bigres0$elostrength10.1[bigres0$team1=="Barcelona"],
        type = "p",col="purple",xlim=range(bigres0$date[is.na(bigres0$elo10predict)==FALSE],na.rm=TRUE),pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team2=="Barcelona"],bigres0$elostrength10.2[bigres0$team2=="Barcelona"],type = "p",col="purple",pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team1=="Real Madrid"],bigres0$elostrength10.1[bigres0$team1=="Real Madrid"],
        type = "p",col="darkblue",pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team2=="Real Madrid"],bigres0$elostrength10.2[bigres0$team2=="Real Madrid"],
        type = "p",col="darkblue",pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team1=="Juventus"],bigres0$elostrength10.1[bigres0$team1=="Juventus"],
        type = "p",col="black",pch=16,cex=0.5)
  lines(bigres0$date[bigres0$team2=="Juventus"],bigres0$elostrength10.2[bigres0$team2=="Juventus"],
        type = "p",col="black",pch=16,cex=0.5)
  

  ##### TIERS #####
  england.ids <- c(1:11,56,58,60,63,70,75,78,80,225,226)
  scotland.ids <- c(12:18,59,61,62,82,353)
  bigres0$country <- NA
  bigres0$tier <- NA
  bigres0$tier[bigres0$div_id==1 | bigres0$div_id==5] <- 1
  bigres0$tier[bigres0$div_id==2 | bigres0$div_id==6] <- 2
  bigres0$tier[bigres0$div_id==3 | bigres0$div_id==7] <- 3
  bigres0$tier[bigres0$div_id==4 | bigres0$div_id==8] <- 4
  bigres0$tier[bigres0$division=="Southern League Division One"] <- 2.5
  bigres0$country[bigres0$div_id %in% england.ids | (is.na(bigres0$div_id)==TRUE & bigres0$date<"1920-01-01")] <- "England"
  bigres0$tier[bigres0$div_id==12 | bigres0$div_id==16] <- 1.01
  bigres0$tier[bigres0$div_id==13 | bigres0$div_id==17] <- 2.01
  bigres0$tier[bigres0$div_id==14 | bigres0$div_id==18] <- 3.01
  bigres0$tier[bigres0$div_id==82 | bigres0$div_id==353] <- 4.01
  bigres0$country[bigres0$div_id %in% scotland.ids] <- "Scotland"
  bigres0$tier[bigres0$div_id==19] <- 1.02 #serie a
  bigres0$country[bigres0$div_id==19 & bigres0$div_id==92] <- "Italy" #serie a
  bigres0$tier[bigres0$div_id==20] <- 1.03 #bundesliga
  bigres0$tier[bigres0$div_id==170] <- 2.03
  bigres0$country[bigres0$div_id==20 | bigres0$div_id==170 | bigres0$div_id==91] <- "Germany" #serie a
  bigres0$tier[bigres0$div_id==21] <- 1.04 #la liga
  bigres0$tier[bigres0$div_id==194] <- 2.04 #spain 2
  bigres0$country[bigres0$div_id==21 | bigres0$div_id==194 | bigres0$div_id==96 | bigres0$div_id==137] <- "Spain" #serie a
  bigres0$tier[bigres0$div_id==22] <- 1.05 #ligue 1
  bigres0$country[bigres0$div_id==22 | bigres0$div_id==101 | bigres0$div_id==166] <- "France" #ligue 1
  bigres0$tier[bigres0$div_id==23] <- 1.06 #eredivisie
  bigres0$country[bigres0$div_id==23] <- "Netherlands" #eredivisie
  bigres0$tier[bigres0$div_id==24] <- 1.07
  bigres0$country[bigres0$div_id==24] <- "Portugal"
  bigres0$tier[bigres0$div_id==25] <- 1.08
  bigres0$country[bigres0$div_id==25] <- "Belgium"
  bigres0$tier[bigres0$div_id==47] <- 1.09
  bigres0$country[bigres0$div_id==47] <- "Wales"
  bigres0$tier[bigres0$div_id==48] <- 1.10
  bigres0$country[bigres0$div_id==48] <- "Northern Ireland"
  bigres0$tier[bigres0$div_id==76] <- 1.11
  bigres0$country[bigres0$div_id==76] <- "Ireland"
  bigres0$tier[bigres0$div_id==111] <- 1.12
  bigres0$country[bigres0$div_id==111] <- "Russia"
  bigres0$tier[bigres0$div_id==112] <- 1.13
  bigres0$country[bigres0$div_id==112] <- "Denmark"
  bigres0$tier[bigres0$div_id==113] <- 1.14
  bigres0$country[bigres0$div_id==113] <- "Turkey"
  bigres0$tier[bigres0$div_id==114] <- 1.15
  bigres0$country[bigres0$div_id==114] <- "Greece"
  bigres0$tier[bigres0$div_id==116] <- 1.16
  bigres0$country[bigres0$div_id==116] <- "Austria"
  bigres0$tier[bigres0$div_id==117] <- 1.17
  bigres0$country[bigres0$div_id==117] <- "Czech Rep"
  bigres0$tier[bigres0$div_id==118] <- 1.18
  bigres0$country[bigres0$div_id==118] <- "Ukraine"
  bigres0$tier[bigres0$div_id==122] <- 1.19
  bigres0$country[bigres0$div_id==122] <- "Switzerland"
  bigres0$tier[bigres0$div_id==123] <- 1.20
  bigres0$country[bigres0$div_id==123] <- "Romania"
  bigres0$tier[bigres0$div_id==124] <- 1.21
  bigres0$country[bigres0$div_id==124] <- "Poland"
  bigres0$tier[bigres0$div_id==126] <- 1.22
  bigres0$country[bigres0$div_id==126] <- "Slovenia"
  bigres0$tier[bigres0$div_id==127] <- 1.23
  bigres0$country[bigres0$div_id==127] <- "Croatia"
  bigres0$tier[bigres0$div_id==132] <- 1.24
  bigres0$country[bigres0$div_id==132] <- "Israel"
  bigres0$tier[bigres0$div_id==133] <- 1.25
  bigres0$country[bigres0$div_id==133] <- "Bulgaria"
  bigres0$tier[bigres0$div_id==134] <- 1.26
  bigres0$country[bigres0$div_id==134] <- "Hungary"
  bigres0$tier[bigres0$div_id==171] <- 1.27
  bigres0$country[bigres0$div_id==171] <- "Belarus"
  bigres0$tier[bigres0$div_id==203] <- 1.28
  bigres0$country[bigres0$div_id %in% c(203,428:430,435)] <- "Brazil"
  bigres0$tier[bigres0$div_id==205] <- 1.29
  bigres0$country[bigres0$div_id %in% c(205,268)] <- "Sweden"
  bigres0$tier[bigres0$div_id==206] <- 1.30
  bigres0$country[bigres0$div_id==206] <- "Norway"
  bigres0$tier[bigres0$div_id==207] <- 1.31
  bigres0$country[bigres0$div_id==207] <- "USA"
  bigres0$tier[bigres0$div_id==208] <- 1.32
  bigres0$country[bigres0$div_id==208] <- "Finland"
  bigres0$tier[bigres0$div_id==264] <- 1.33
  bigres0$country[bigres0$div_id==264] <- "Serbia"
  bigres0$tier[bigres0$div_id==311] <- 1.34
  bigres0$country[bigres0$div_id==311] <- "Australia"
  bigres0$tier[bigres0$div_id==417] <- 1.35
  bigres0$country[bigres0$div_id==417 | bigres0$div_id==447] <- "Japan"
  bigres0$tier[bigres0$div_id==447] <- 2.35
  bigres0$tier[bigres0$div_id==418] <- 1.36
  bigres0$country[bigres0$div_id==418] <- "Chile"
  bigres0$tier[bigres0$div_id==419 | bigres0$div_id==432] <- 1.37
  bigres0$country[bigres0$div_id==419 | bigres0$div_id==432] <- "Mexico"
  bigres0$tier[bigres0$div_id==420] <- 1.38
  bigres0$tier[bigres0$div_id==424] <- 2.38
  bigres0$country[bigres0$div_id==420 | bigres0$div_id==424] <- "South Korea"
  bigres0$tier[bigres0$div_id==421] <- 1.39
  bigres0$country[bigres0$div_id==421] <- "Colombia"
  bigres0$tier[bigres0$div_id==422] <- 1.40
  bigres0$country[bigres0$div_id==422] <- "Ecuador"
  bigres0$tier[bigres0$div_id==425] <- 1.41
  bigres0$country[bigres0$div_id==425] <- "Paraguay"
  bigres0$tier[bigres0$div_id==426] <- 1.42
  bigres0$country[bigres0$div_id==426] <- "Peru"
  bigres0$tier[bigres0$div_id==433] <- 1.43
  bigres0$country[bigres0$div_id==433] <- "Venezuela"
  bigres0$tier[bigres0$div_id==436] <- 1.44
  bigres0$country[bigres0$div_id==436 | bigres0$div_id==444] <- "Argentina"
  bigres0$tier[bigres0$div_id==444] <- 2.44
  bigres0$tier[bigres0$div_id==439] <- 1.45
  bigres0$country[bigres0$div_id==439] <- "Uruguay"
  bigres0$tier[bigres0$div_id==9] <- 5
  bigres0$tier[bigres0$div_id==225] <- 6.1
  bigres0$tier[bigres0$div_id==226] <- 6.2
  bigres0$tier[bigres0$div_id==80] <- 7
  bigres0$tier[bigres0$div_id==75] <- 7
  bigres0$tier[bigres0$div_id==56] <- 7
  bigres0$tier[bigres0$div_id==10] <- 3.1
  bigres0$tier[bigres0$div_id==11] <- 3.2

  bigres0$outcomeH <- as.numeric(bigres0$outcome==1)
  bigres0$outcomeD <- as.numeric(bigres0$outcome==0.5)
  bigres0$outcomeA <- as.numeric(bigres0$outcome==0)
  bigres0$goals1 <- as.numeric(bigres0$goals1)
  bigres0$goals2 <- as.numeric(bigres0$goals2)

  bigres0 <- bigres0[order(bigres0$team1,bigres0$date),]
  library(dplyr)
  tier1 <- bigres0[,c("team1","tier")] %>% group_by(team1) %>% transmute(tier=na.locf(tier, na.rm=FALSE))
  bigres0$tier1 <- tier1$tier
  # bigres0$tier1 <- na.locf(bigres0$tier,na.rm = FALSE)
  bigres0 <- bigres0[order(bigres0$team2,bigres0$date),]
  tier2 <- bigres0[,c("team2","tier")] %>% group_by(team2) %>% transmute(tier=na.locf(tier, na.rm=FALSE))
  bigres0$tier2 <- tier2$tier
  # bigres0$tier2 <- na.locf(bigres0$tier,na.rm = FALSE)
  bigres0$league <- as.numeric(bigres0$tier1==bigres0$tier2)
  bigres0$league[is.na(bigres0$league)==TRUE] <- 0

  bigres0 <- bigres0[order(bigres0$date),]
  
  stargazer::stargazer(res0[,c("date","goals1","goals2","outcomeH","outcomeD","outcomeA","league")])
  
  mean.elo.div <- aggregate(res0[,c("elostrength01.1","elostrength05.1","elostrength10.1","elostrength20.1","elostrength30.1")],
                            by=list(res0$season,res0$tier),FUN=mean,na.rm=TRUE)
  
  plot(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
       mean.elo.div$elostrength01.1[mean.elo.div$Group.2==1],type="l",
       ylim=range(c(mean.elo.div$elostrength01.1,
                    mean.elo.div$elostrength05.1,
                    mean.elo.div$elostrength10.1,
                    mean.elo.div$elostrength20.1,
                    mean.elo.div$elostrength30.1),
                  na.rm=TRUE))
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
        mean.elo.div$elostrength05.1[mean.elo.div$Group.2==1],type="l",lty=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1],type="l",lty=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
        mean.elo.div$elostrength20.1[mean.elo.div$Group.2==1],type="l",lty=4)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==1],type="l",lty=5)
  legend("topleft",ncol=5,bty="n",lty=1:5,legend=c(1,5,10,20,30))
  
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==2],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==2],type="l",col=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==2],
        mean.elo.div$elostrength20.1[mean.elo.div$Group.2==2],type="l",lty=2,col=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==2],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==2],type="l",lty=3,col=2)

  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==3],type="l",col=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3],
        mean.elo.div$elostrength20.1[mean.elo.div$Group.2==3],type="l",lty=2,col=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==3],type="l",lty=3,col=3)

  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==4],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==4],type="l",col=4)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==4],
        mean.elo.div$elostrength20.1[mean.elo.div$Group.2==4],type="l",lty=2,col=4)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==4],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==4],type="l",lty=3,col=4)
  
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==5],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==5],type="l",col=5)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==5],
        mean.elo.div$elostrength20.1[mean.elo.div$Group.2==5],type="l",lty=2,col=5)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==5],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==5],type="l",lty=3,col=5)

  jpeg("/Users/jjreade/Dropbox/Research/Sport/Elo-ratings/tex/all-divs-elo10.jpg",
       width = 8, height = 5, units = "in", res=300)
  plot(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
       mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1],type="l",
       ylim=range(c(mean.elo.div$elostrength10.1),
                  na.rm=TRUE),ylab="Elo rating",xlab="Year",
       main="Mean Elo ratings (adj=10) by tier of English Football")
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==2],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==2],type="l",col=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==2.5],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==2.5],type="l",col=2,lty=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==3],type="l",col=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3.1],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==3.1],type="l",col=3,lty=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3.2],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==3.2],type="l",col=3,lty=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==4],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==4],type="l",col=4)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==5],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==5],type="l",col=5)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==6.1],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==6.1],type="l",col=6,lty=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==6.2],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==6.2],type="l",col=6,lty=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==7],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==7],type="l",col=7)
  legend("topleft",col=c(1:2,2,3,3,4:6,6,7),lty=c(1,1,3,1,2,3,1,1,2,3,1),
         bty="n",legend=paste0("Div",c(1:2,"SL",3,"3N","3S",4:5,"6N","6S",7)),ncol = 3)
  dev.off()

  jpeg("/Users/jjreade/Dropbox/Research/Sport/Elo-ratings/tex/euro-divs-elo10.jpg",
       width = 8, height = 5, units = "in", res=300)
  plot(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
       mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1],type="l",
       ylim=range(c(mean.elo.div$elostrength10.1),
                  na.rm=TRUE),ylab="Elo rating",xlab="Year",
       main="Mean Elo ratings (adj=10) by top tier of European Football")
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1.01],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1.01],type="l",col=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1.02],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1.02],type="l",col=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1.03],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1.03],type="l",col=4)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1.04],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1.04],type="l",col=5)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1.05],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1.05],type="l",col=6)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1.06],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1.06],type="l",col=7)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==1.07],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1.07],type="l",col=8)
  legend("bottomleft",col=c(1:8),lty=1,
         bty="n",legend=c("ENG","SCO","ITA","GER","SPA","FRA","NED","POR"),ncol = 3)
  dev.off()
  
  plot(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
       mean.elo.div$elostrength05.1[mean.elo.div$Group.2==1],type="l",
       ylim=range(c(mean.elo.div$elostrength05.1),
                  na.rm=TRUE),ylab="Elo rating",xlab="Year",
       main="Mean Elo ratings (adjustment=5) by tier of English Football")
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==2],
        mean.elo.div$elostrength05.1[mean.elo.div$Group.2==2],type="l",col=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3],
        mean.elo.div$elostrength05.1[mean.elo.div$Group.2==3],type="l",col=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==4],
        mean.elo.div$elostrength05.1[mean.elo.div$Group.2==4],type="l",col=4)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==5],
        mean.elo.div$elostrength05.1[mean.elo.div$Group.2==5],type="l",col=5)
  legend("topleft",col=1:5,lty=1,bty="n",legend=paste0("Div",1:5))
  
  plot(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
       mean.elo.div$elostrength10.1[mean.elo.div$Group.2==1],type="l",
       ylim=range(c(mean.elo.div$elostrength10.1),
                  na.rm=TRUE),ylab="Elo rating",xlab="Year",
       main="Mean Elo ratings (adj=10) by tier of English Football")
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==2],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==2],type="l",col=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==3],type="l",col=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==4],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==4],type="l",col=4)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==5],
        mean.elo.div$elostrength10.1[mean.elo.div$Group.2==5],type="l",col=5)
  legend("topleft",col=1:5,lty=1,bty="n",legend=paste0("Div",1:5))
  
  plot(mean.elo.div$Group.1[mean.elo.div$Group.2==1],
       mean.elo.div$elostrength30.1[mean.elo.div$Group.2==1],type="l",
       ylim=range(c(mean.elo.div$elostrength30.1),
                  na.rm=TRUE),ylab="Elo rating",xlab="Year",
       main="Mean Elo ratings by tier of English Football")
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==2],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==2],type="l",col=2)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==3],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==3],type="l",col=3)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==4],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==4],type="l",col=4)
  lines(mean.elo.div$Group.1[mean.elo.div$Group.2==5],
        mean.elo.div$elostrength30.1[mean.elo.div$Group.2==5],type="l",col=5)
  legend("topleft",col=1:5,lty=1,bty="n",legend=paste0("Div",1:5))
  
  plot(res0$date[res0$team1=="Oldham"],res0$elostrength01.1[res0$team1=="Oldham"],type="l",
       ylim=range(c(res0$elostrength01.1[res0$team1=="Oldham"],
                    res0$elostrength05.1[res0$team1=="Oldham"],
                    res0$elostrength10.1[res0$team1=="Oldham"],
                    res0$elostrength20.1[res0$team1=="Oldham"],
                    res0$elostrength30.1[res0$team1=="Oldham"]),na.rm=TRUE))
  lines(res0$date[res0$team1=="Oldham"],res0$elostrength05.1[res0$team1=="Oldham"],type="l",col=2)
  lines(res0$date[res0$team1=="Oldham"],res0$elostrength10.1[res0$team1=="Oldham"],type="l",col=3)
  lines(res0$date[res0$team1=="Oldham"],res0$elostrength20.1[res0$team1=="Oldham"],type="l",col=4)
  lines(res0$date[res0$team1=="Oldham"],res0$elostrength30.1[res0$team1=="Oldham"],type="l",col=5)
  
  ##forecast FA Cup matches using Elo alone
  mz.elo <- list()
  summary(mz.elo[["01.0"]] <- lm(outcome ~ elo01predict,data=res0[res0$season>=1900 & res0$tier1==res0$tier2,]))
  summary(mz.elo[["05.0"]] <- lm(outcome ~ elo05predict,data=res0[res0$season>=1900 & res0$tier1==res0$tier2,]))
  summary(mz.elo[["10.0"]] <- lm(outcome ~ elo10predict,data=res0[res0$season>=1900 & res0$tier1==res0$tier2,]))
  summary(mz.elo[["20.0"]] <- lm(outcome ~ elo20predict,data=res0[res0$season>=1900 & res0$tier1==res0$tier2,]))
  summary(mz.elo[["30.0"]] <- lm(outcome ~ elo30predict,data=res0[res0$season>=1900 & res0$tier1==res0$tier2,]))
  summary(mz.elo[["01.0d"]] <- lm(outcome ~ elo01predict,data=res0[res0$season>=1900 & res0$tier1!=res0$tier2,]))
  summary(mz.elo[["05.0d"]] <- lm(outcome ~ elo05predict,data=res0[res0$season>=1900 & res0$tier1!=res0$tier2,]))
  summary(mz.elo[["10.0d"]] <- lm(outcome ~ elo10predict,data=res0[res0$season>=1900 & res0$tier1!=res0$tier2,]))
  summary(mz.elo[["20.0d"]] <- lm(outcome ~ elo20predict,data=res0[res0$season>=1900 & res0$tier1!=res0$tier2,]))
  summary(mz.elo[["30.0d"]] <- lm(outcome ~ elo30predict,data=res0[res0$season>=1900 & res0$tier1!=res0$tier2,]))
  # summary(mz.elo[["01.50"]] <- lm(outcome ~ elo01predict,data=res0[res0$div_id==58 & res0$season>=1950,]))
  # summary(mz.elo[["05.50"]] <- lm(outcome ~ elo05predict,data=res0[res0$div_id==58 & res0$season>=1950,]))
  # summary(mz.elo[["10.50"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1950,]))
  # summary(mz.elo[["20.50"]] <- lm(outcome ~ elo20predict,data=res0[res0$div_id==58 & res0$season>=1950,]))
  # summary(mz.elo[["30.50"]] <- lm(outcome ~ elo30predict,data=res0[res0$div_id==58 & res0$season>=1950,]))
  # summary(mz.elo[["01.80"]] <- lm(outcome ~ elo01predict,data=res0[res0$div_id==58 & res0$season>=1980,]))
  # summary(mz.elo[["05.80"]] <- lm(outcome ~ elo05predict,data=res0[res0$div_id==58 & res0$season>=1980,]))
  # summary(mz.elo[["10.80"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1980,]))
  # summary(mz.elo[["20.80"]] <- lm(outcome ~ elo20predict,data=res0[res0$div_id==58 & res0$season>=1980,]))
  # summary(mz.elo[["30.80"]] <- lm(outcome ~ elo30predict,data=res0[res0$div_id==58 & res0$season>=1980,]))
  stargazer::stargazer(mz.elo)
  stargazer::stargazer(mz.elo[6:10])
  
  mz.elo2 <- list()
  summary(mz.elo2[["10.00s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season<1910 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.10s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1910 & res0$season<1920 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.20s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1920 & res0$season<1930 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.30s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1930 & res0$season<1940 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.40s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1940 & res0$season<1950 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.50s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1950 & res0$season<1960 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.60s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1960 & res0$season<1970 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.70s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1970 & res0$season<1980 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.80s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1980 & res0$season<1990 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.90s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=1990 & res0$season<2000 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.2000s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=2000 & res0$season<2010 & res0$tier1!=res0$tier2,]))
  summary(mz.elo2[["10.2010s"]] <- lm(outcome ~ elo10predict,data=res0[res0$div_id==58 & res0$season>=2010 & res0$season<2023,]))
  stargazer::stargazer(mz.elo2)
  
  #next want to try to predict matches between teams from different divisions (and countries)
  
  res0$elobalance <- -(res0$elopredict-0.5)^2
  
  res0$team1[res0$team1=="aw'bledon"] <- "afc w'bledon"
  res0$team2[res0$team2=="aw'bledon"] <- "afc w'bledon"
  #  elorank[["afc w'bledon"]] <- elorank$`aw'bledon`
  
  ##get rid of matches before now that haven't got a result inserted ####
  res0 <- res0[!(is.na(res0$goals1)==TRUE & res0$date<"2023-03-07"),]
  
  #setup data for simulations
  #res00 <- datasetup2022(res0)
  #res00 <- data
  
  ##calculate league tables ####
  league.ids <- c(1:25,47:48,56,75,76,80,82,111:118,122:134,170:171,194)#,205:311,353)

  res.tab <- data.frame()
  res.final.tabs <- data.frame()
  outcomes.tabs <- data.frame()
  seasons <- unique(res0$season)
  seasons <- seasons[order(seasons)]
  
  for( ss in seasons[seasons>=1998]) {
    print(ss)
    
    for( ll in league.ids) {
      #if(ss==1991 & ll<8) { next }
      print(ll)
      matches <- res0[res0$season==ss & res0$div_id==ll,c("match_id","date","team1","goals1","goals2","team2")]
      #remove playoff matches (which must be duplicates and come later)
      matches <- matches[order(matches$date),]
      matches <- matches[duplicated(matches[,c("team1","team2")])==FALSE,]
      matches <- matches[is.na(matches$match_id)==FALSE,]
      if(NROW(matches)>10) {
        if(var(matches$date)==0) {
          matches$date <- matches$date + 1:NROW(matches)
        }
        lge.all <- league.tab(game_id=matches$match_id,
                              date0=as.Date(matches$date),
                              team1=matches$team1,
                              g1=matches$goals1,
                              g2=matches$goals2,
                              team2=matches$team2,
                              div_id=ll,season=ss,
                              pfw=2+as.numeric( (ss>1980 & ll %in% 1:11 ) | (ss>1994 & !(ll %in% 1:11) ) ))
        # ,date0,team1,g1,g2,team2,
        # div_id = NA,season=NA,pfw=3,points.deduct = NULL, wc=FALSE
        lge <- lge.all$matches
        
        final.lge.tab <- lge.all$final.table
        final.lge.tab$season <- ss
        final.lge.tab$div_id <- ll

        outcomes.known.tab <- lge.all$final.table
        outcomes.known.tab$season <- ss
        outcomes.known.tab$div_id <- ll
        
        colnames(lge)[grep("game_id",colnames(lge))] <- "match_id"
        colnames(lge)[grep("g1",colnames(lge))] <- "goals1"
        colnames(lge)[grep("g2",colnames(lge))] <- "goals2"
        lge$season <- ss
        lge$div_id <- ll
        res.tab <- rbind(res.tab,lge)
        res.final.tabs <- rbind(res.final.tabs,final.lge.tab)
        outcomes.tabs <- rbind(outcomes.tabs,outcomes.known.tab)
      }
    }
  }
  
  res.tab <- res.tab[duplicated(res.tab$match_id)==F,]
  res0 <- res0[duplicated(res0$match_id)==FALSE,]
  res0 <- merge(res0,res.tab,by=c("match_id"),all.x=T)
  
  #out of the FA Cup?
  english.teams <- unique(res0$team1[res0$div_id<12])
  res0 <- res0[order(res0$date),]
  res0$inFAcup.1 <- NA
  res0$inFAcup.2 <- NA
  for(tt in english.teams) {
    club.results <- res0[res0$team1==tt | res0$team2==tt,c("match_id","div_id","season","date","team1","goals1","goals2","team2")]
    club.results <- club.results[order(club.results$date),]
    club.results$season.1 <- c(NA,club.results$season[-NROW(club.results)])
    club.results$inFAcup <- as.numeric(club.results$season!=club.results$season.1)
    club.results$inFAcup[club.results$div_id==58 & ((club.results$team1==tt & club.results$goals1<club.results$goals2) | (club.results$team2==tt & club.results$goals1>club.results$goals2))] <- -1
    club.results$inFAcup <- ave(club.results$inFAcup,by=club.results$season,FUN=cumsum)
    club.results$inFAcup[club.results$inFAcup<0] <- 0
    #merging back in...
    res0$inFAcup.1[res0$team1==tt] <- club.results$inFAcup[club.results$team1==tt]
    res0$inFAcup.2[res0$team2==tt] <- club.results$inFAcup[club.results$team2==tt]
  }
  # res0$inFAcup <- blah
  write.csv(res0[,c("match_id","inFAcup.1","inFAcup.2")],paste0(dbloc,"/data/still-in-fa-cup-1888-2023.csv"))
  
  #repeat fixtures --- if in both league and fa cup in a season
  res0$repeat.fix <- as.numeric(duplicated(res0[,c("team1_id","team2_id","season")]))
  res0$repeat.fix.lg <- as.numeric(duplicated(res0[,c("team1_id","team2_id","season","div_id")]))
  
  ##save league tables up to end of previous season
  write.csv(res.tab,paste0(dbloc,"/data/league-tabs-1888-2023.csv"))
  write.csv(res.final.tabs,paste0(dbloc,"/data/final-league-tabs-1888-2023.csv"))
#  res.final.tabs <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/final-league-tabs-1888-2223.csv",stringsAsFactors = FALSE)
  
  res0.f <- merge(res0,res.final.tabs[,c("team","season","position")],by.x=c("team1","season"),by.y=c("team","season"))
  res0.f <- merge(res0.f,res.final.tabs,by.x=c("team2","season"),by.y=c("team","season"),all.x=TRUE,suffixes=c(".H",".A"))

  fin0.0 <- res0.f[res0.f$pld1==4 & res0.f$pts1==0,c("team1","season","position.H")]
  colnames(fin0.0) <- c("team","season","final.position")
  fin0.1 <- res0.f[res0.f$pld2==4 & res0.f$pts2==0,c("team2","season","position.A")]
  colnames(fin0.1) <- c("team","season","final.position")
  fin0 <- rbind(fin0.0,fin0.1)
  
  jpeg(paste0(dbloc,"/data/teams-on-zero-points-pld4-final-pos.jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(table(fin0$final.position[fin0$final.position<25]),
       main="Final Position of Teams Pointless After Four Matches",
       sub=paste0("Data from Soccerbase.com, ",NROW(fin0[is.na(fin0$final.position[fin0$final.position<25])==FALSE,])," observations"),
       ylab="Number of occasions",
       xlab="Final Position")
  dev.off()

  fin0.0 <- res0.f[res0.f$pld1==5 & res0.f$pts1==0,c("team1","season","position.H")]
  colnames(fin0.0) <- c("team","season","final.position")
  fin0.1 <- res0.f[res0.f$pld2==5 & res0.f$pts2==0,c("team2","season","position.A")]
  colnames(fin0.1) <- c("team","season","final.position")
  fin0 <- rbind(fin0.0,fin0.1)

  jpeg(paste0(dbloc,"/data/teams-on-zero-points-pld5-final-pos.jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(table(fin0$final.position[fin0$final.position<25]),
       main="Final Position of Teams Pointless After Five Matches",
       sub=paste0("Data from Soccerbase.com, ",NROW(fin0[is.na(fin0$final.position[fin0$final.position<25])==FALSE,])," observations"),
       ylab="Number of occasions",
       xlab="Final Position")
  dev.off()
  
  ##FULL PYRAMID LEAGUE POSITION
  div.tot.teams0 <- res0[duplicated(res0[,c("team1","div_id","season")])==FALSE & res0$div_id<12 & res0$div_id>0,c("team1","div_id","season")]
  div.tot.teams0$tier <- div.tot.teams0$div_id
  div.tot.teams0$tier[div.tot.teams0$tier==5] <- 1
  div.tot.teams0$tier[div.tot.teams0$tier==6] <- 2
  div.tot.teams0$tier[div.tot.teams0$tier==7] <- 3
  div.tot.teams0$tier[div.tot.teams0$tier==8] <- 4
  div.tot.teams0$tier[div.tot.teams0$tier==9] <- 5
  div.tot.teams0$tier[div.tot.teams0$tier==10] <- 3.1
  div.tot.teams0$tier[div.tot.teams0$tier==11] <- 3.2
  
  div.tot.teams0$const <- 1
  
  div.tot.teams <- aggregate(div.tot.teams0$const,by=list(div.tot.teams0$tier,div.tot.teams0$season),FUN=sum)
  
  div.tot.teams1 <- div.tot.teams[div.tot.teams$Group.1==1,]
  div.tot.teams2 <- div.tot.teams[div.tot.teams$Group.1==2,]
  div.tot.teams3 <- div.tot.teams[div.tot.teams$Group.1==3,]
  div.tot.teams3.1 <- div.tot.teams[div.tot.teams$Group.1==3.1,]
  div.tot.teams3.2 <- div.tot.teams[div.tot.teams$Group.1==3.2,]
  div.tot.teams4 <- div.tot.teams[div.tot.teams$Group.1==4,]
  
  div.tots <- merge(div.tot.teams1[c("Group.2","x")],
                    div.tot.teams2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".1",".2"))
  div.tots <- merge(div.tots,
                    div.tot.teams3[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams4[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3",".4"))
  div.tots <- merge(div.tots,
                    div.tot.teams3.1[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams3.2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3n",".3s"))
  
  div.tots$cum.1 <- 0
  div.tots$cum.2 <- div.tots$x.1
  div.tots$cum.3 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.1 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.2 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.4 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3
  div.tots$cum.5 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4
  
  div.tots$lg.total.n <- rowSums(div.tots[,c("x.1","x.2","x.3n","x.3","x.4")],na.rm=TRUE)
  div.tots$lg.total.s <- rowSums(div.tots[,c("x.1","x.2","x.3s","x.3","x.4")],na.rm=TRUE)
  
  divs <- c(1,2,3,3.1,3.2,4,5)
  teams.above <- data.frame(stringsAsFactors = FALSE)
  
  for(dd in divs) {
    temp <- div.tots[,c("Group.2",paste0("cum.",dd))]
    colnames(temp) <- c("season","teams.above")
    temp$tier <- dd
    teams.above <- rbind(teams.above,temp)
  }
  
  
  
  res0 <- merge(res0,teams.above,by=c("season","tier"),all.x=TRUE)
  
  res0$full.pos1 <- res0$pos1 + res0$teams.above
  res0$full.pos2 <- res0$pos2 + res0$teams.above
  

  dbloc <- "/Users/jjreade/Dropbox/Research/Sport/Correct-score/"
  write.date = "2022-07-17" # Sys.Date()
  write.csv(elorank10,paste0(dbloc,"/data/elorank-",write.date,".csv"))
  
  
  write.csv(res0,paste0(dbloc,"/data/res0-",write.date,".csv"))
  
  #list2022 <- list("res0"=res00,"elorank"=elorank)
  returnlist <- list("res0"=res00,"elorank"=elorank)
  return <- returnlist
}

##load up the soccerbase results - up to August 5
initialise2223season <- function(write.date=Sys.Date(),elo.weight=20) {
  require(zoo)
  floc <- "/Volumes/11330730-dp/Correct-score-data/soccerbase-data/"
  files2223 <- list.files(floc,pattern="^historical_results_\\S+.csv$")
  files2223 <- files2223[files2223<="historical_results_2223-08-02-2020.csv"]
  res0 <- data.frame()
  for(ff in files2223) {
    print(ff)
    temp <- read.csv(paste0(floc,ff),stringsAsFactors = FALSE)
    temp$division[temp$division==""] <- NA
    temp$div_id[temp$div_id=="n/a"] <- "-99"
    temp$div_id <- as.numeric(temp$div_id)
    temp$division <- na.locf(temp$division)
    temp$div_id <- na.locf(temp$div_id)
    temp$goals1 <- as.numeric(gsub("^<a href=# class=vs title=View Match info><em>(\\d+)</em>&nbsp;-&nbsp;<em>(\\d+)</em></a>$","\\1",temp$goals1))
    temp$goals2 <- as.numeric(gsub("^<a href=# class=vs title=View Match info><em>(\\d+)</em>&nbsp;-&nbsp;<em>(\\d+)</em></a>$","\\2",temp$goals2))
    res0 <- rbind(res0,temp)
    temp <- NULL
  }
  #write.csv(res0[,c("match_id","div_id")],"/Volumes/11330730-dp/Correct-score-data/soccerbase-data/match-div-ids.csv")
  
  res0$date <- as.Date(res0$date)
  
  census.date <- "2022-08-06"
  
  #remove matches taking place aafter Aaugust 5 (i.e. the 2223/22 season)
  res0 <- res0[res0$date<=census.date,]
  #######
  
  ##remove duplicate entries
  res0 <- res0[duplicated(res0)==FALSE,]
  
  #get rid of any matches that haven't occurred before today (i.e. have NAs in the score)
  res0 <- res0[!(is.na(res0$goals1)==TRUE & res0$date<census.date),]
  
  #get rid of matches that are "winner matches"
  #res <- res[regexpr("winner",tolower(res$team1))>-1,]
  res0 <- res0[!(res0$match_id=="tgc811051" & res0$goals2==4),] #remove odd duplicate with wrong score
  res0 <- res0[!(res0$match_id=="tgc813129" & res0$goals2==4),] #remove odd duplicate with wrong score
  res0$outcome <- 0.5*(res0$goals1==res0$goals2) + (res0$goals1>res0$goals2)
  
  res0 <- res0[order(res0$date),]
  res0$team1 <- gsub(" [(]old[)]","",res0$team1)
  res0$team2 <- gsub(" [(]old[)]","",res0$team2)
  res0$team1 <- gsub("^\\s+|\\s+$","",res0$team1)
  res0$team2 <- gsub("^\\s+|\\s+$","",res0$team2)
  
  #weights for Elo ranking --- rank league highest for club, non-friendly for national
  res0$weight <- elo.weight
  
  ##Elo ranking routine
  elorank <- list()
  
  res0$elostrength1 <- NA
  res0$elostrength2 <- NA
  res0$elopredict <- NA
  ##res0.eng <- res0.eng[duplicated(res0.eng$match.link)==FALSE,]
  
  res0 <- res0[order(res0$date),]
  
  for(mm in c(1:NROW(res0))) {
    print(100*mm/NROW(res0))
    if(!(paste0("id",res0$team1_id[mm]) %in% names(elorank))) {
      elorank[[paste0("id",res0$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",res0$team2_id[mm]) %in% names(elorank))) {
      elorank[[paste0("id",res0$team2_id[mm])]] <- 1000
    }
    res0$elostrength1[mm] <- elorank[[paste0("id",res0$team1_id[mm])]]
    res0$elostrength2[mm] <- elorank[[paste0("id",res0$team2_id[mm])]]
    res0$elopredict[mm] <- 1/(1+(10^((res0$elostrength2[mm]-res0$elostrength1[mm])/400)))
    adjustment <- res0$outcome[mm] - res0$elopredict[mm]
    if(is.na(adjustment)==FALSE) {
      elorank[paste0("id",res0$team1_id[mm])] <- elorank[[paste0("id",res0$team1_id[mm])]] + elo.weight*adjustment
      elorank[paste0("id",res0$team2_id[mm])] <- elorank[[paste0("id",res0$team2_id[mm])]] - elo.weight*adjustment
    }
  }
  res0 <- res0[is.na(res0$date)==FALSE,]
  
  res0$elobalance <- -(res0$elopredict-0.5)^2
  
  res0$team1[res0$team1=="aw'bledon"] <- "afc w'bledon"
  res0$team2[res0$team2=="aw'bledon"] <- "afc w'bledon"
  #  elorank[["afc w'bledon"]] <- elorank$`aw'bledon`
  
  ##get rid of matches before now that haven't got a result inserted
  res0 <- res0[!(is.na(res0$goals1)==TRUE & res0$date<"2223-08-02"),]
  
  #setup data for simulations
  #res00 <- datasetup2223(res0)
  #res00 <- data
  
  ##LEAGUE TABLES
  league.ids <- c(1:25,47:48,56,75,76,80,82,111:118,122:134,170:171,194,205:311,353)
  
  res.tab <- data.frame()
  res.final.tabs <- data.frame()
  res0$season <- as.numeric(format(res0$date,"%Y")) - as.numeric(as.numeric(format(res0$date,"%m"))<7)
  res0$season[res0$date>"2022-07-01"] <- 2022
  seasons <- unique(res0$season)
  seasons <- seasons[order(seasons)]
  
  for( ss in seasons) {
    print(ss)
    
    for( ll in league.ids) {
      #if(ss==2019 & ll<5) { next }
      print(ll)
      matches <- res0[res0$season==ss & res0$div_id==ll,c("match_id","date","team1","goals1","goals2","team2")]
      matches <- matches[is.na(matches$match_id)==F,]
      if(NROW(matches)>10) {
        if(var(matches$date)==0) {
          matches$date <- matches$date + 1:NROW(matches)
        }
        lge.all <- league.tab(matches$match_id,as.Date(matches$date),matches$team1,
                              matches$goals1,matches$goals2,matches$team2,
                              2+as.numeric( (ss>1980 & ll %in% 1:11 ) | (ss>1994 & !(ll %in% 1:11) ) ))
        lge <- lge.all$matches
        
        final.lge.tab <- lge.all$final.table
        final.lge.tab$season <- ss
        final.lge.tab$div_id <- ll
        
        colnames(lge)[grep("game_id",colnames(lge))] <- "match_id"
        colnames(lge)[grep("g1",colnames(lge))] <- "goals1"
        colnames(lge)[grep("g2",colnames(lge))] <- "goals2"
        lge$season <- ss
        lge$div_id <- ll
        res.tab <- rbind(res.tab,lge)
        res.final.tabs <- rbind(res.final.tabs,final.lge.tab)
      }
    }
  }
  
  res.tab <- res.tab[duplicated(res.tab$match_id)==F,]
  res0 <- res0[duplicated(res0$match_id)==FALSE,]
  res0 <- merge(res0,res.tab[,c(1,7:22)],by=c("match_id"),all.x=T)
  
  ##save league tables up to end of previous season
  write.csv(res.tab,paste0(dbloc,"/data/league-tabs-1888-2022.csv"))
  write.csv(res.final.tabs,paste0(dbloc,"/data/final-league-tabs-1888-2022.csv"))
  res.final.tabs <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/final-league-tabs-1888-2021.csv",stringsAsFactors = FALSE)
  
  res0.f <- merge(res0,res.final.tabs[,c("team","season","position")],by.x=c("team1","season"),by.y=c("team","season"))
  res0.f <- merge(res0.f,res.final.tabs,by.x=c("team2","season"),by.y=c("team","season"),all.x=TRUE,suffixes=c(".H",".A"))
  
  fin0.0 <- res0.f[res0.f$pld1==4 & res0.f$pts1==0,c("team1","season","position.H")]
  colnames(fin0.0) <- c("team","season","final.position")
  fin0.1 <- res0.f[res0.f$pld2==4 & res0.f$pts2==0,c("team2","season","position.A")]
  colnames(fin0.1) <- c("team","season","final.position")
  fin0 <- rbind(fin0.0,fin0.1)
  
  jpeg(paste0(dbloc,"/data/teams-on-zero-points-pld4-final-pos.jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(table(fin0$final.position[fin0$final.position<25]),
       main="Final Position of Teams Pointless After Four Matches",
       sub=paste0("Data from Soccerbase.com, ",NROW(fin0[is.na(fin0$final.position[fin0$final.position<25])==FALSE,])," observations"),
       ylab="Number of occasions",
       xlab="Final Position")
  dev.off()
  
  fin0.0 <- res0.f[res0.f$pld1==5 & res0.f$pts1==0,c("team1","season","position.H")]
  colnames(fin0.0) <- c("team","season","final.position")
  fin0.1 <- res0.f[res0.f$pld2==5 & res0.f$pts2==0,c("team2","season","position.A")]
  colnames(fin0.1) <- c("team","season","final.position")
  fin0 <- rbind(fin0.0,fin0.1)
  
  jpeg(paste0(dbloc,"/data/teams-on-zero-points-pld5-final-pos.jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(table(fin0$final.position[fin0$final.position<25]),
       main="Final Position of Teams Pointless After Five Matches",
       sub=paste0("Data from Soccerbase.com, ",NROW(fin0[is.na(fin0$final.position[fin0$final.position<25])==FALSE,])," observations"),
       ylab="Number of occasions",
       xlab="Final Position")
  dev.off()
  
  ##FULL PYRAMID LEAGUE POSITION
  div.tot.teams0 <- res0[duplicated(res0[,c("team1","div_id","season")])==FALSE & res0$div_id<12 & res0$div_id>0,c("team1","div_id","season")]
  div.tot.teams0$tier <- div.tot.teams0$div_id
  div.tot.teams0$tier[div.tot.teams0$tier==5] <- 1
  div.tot.teams0$tier[div.tot.teams0$tier==6] <- 2
  div.tot.teams0$tier[div.tot.teams0$tier==7] <- 3
  div.tot.teams0$tier[div.tot.teams0$tier==8] <- 4
  div.tot.teams0$tier[div.tot.teams0$tier==9] <- 5
  div.tot.teams0$tier[div.tot.teams0$tier==10] <- 3.1
  div.tot.teams0$tier[div.tot.teams0$tier==11] <- 3.2
  
  div.tot.teams0$const <- 1
  
  div.tot.teams <- aggregate(div.tot.teams0$const,by=list(div.tot.teams0$tier,div.tot.teams0$season),FUN=sum)
  
  div.tot.teams1 <- div.tot.teams[div.tot.teams$Group.1==1,]
  div.tot.teams2 <- div.tot.teams[div.tot.teams$Group.1==2,]
  div.tot.teams3 <- div.tot.teams[div.tot.teams$Group.1==3,]
  div.tot.teams3.1 <- div.tot.teams[div.tot.teams$Group.1==3.1,]
  div.tot.teams3.2 <- div.tot.teams[div.tot.teams$Group.1==3.2,]
  div.tot.teams4 <- div.tot.teams[div.tot.teams$Group.1==4,]
  
  div.tots <- merge(div.tot.teams1[c("Group.2","x")],
                    div.tot.teams2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".1",".2"))
  div.tots <- merge(div.tots,
                    div.tot.teams3[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams4[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3",".4"))
  div.tots <- merge(div.tots,
                    div.tot.teams3.1[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE)
  div.tots <- merge(div.tots,
                    div.tot.teams3.2[c("Group.2","x")],
                    by=c("Group.2"),all.x=TRUE,suffixes=c(".3n",".3s"))
  
  div.tots$cum.1 <- 0
  div.tots$cum.2 <- div.tots$x.1
  div.tots$cum.3 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.1 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.3.2 <- div.tots$x.1 + div.tots$x.2
  div.tots$cum.4 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3
  div.tots$cum.5 <- div.tots$x.1 + div.tots$x.2 + div.tots$x.3 + div.tots$x.4
  
  div.tots$lg.total.n <- rowSums(div.tots[,c("x.1","x.2","x.3n","x.3","x.4")],na.rm=TRUE)
  div.tots$lg.total.s <- rowSums(div.tots[,c("x.1","x.2","x.3s","x.3","x.4")],na.rm=TRUE)
  
  divs <- c(1,2,3,3.1,3.2,4,5)
  teams.above <- data.frame(stringsAsFactors = FALSE)
  
  for(dd in divs) {
    temp <- div.tots[,c("Group.2",paste0("cum.",dd))]
    colnames(temp) <- c("season","teams.above")
    temp$tier <- dd
    teams.above <- rbind(teams.above,temp)
  }
  
  res0$tier <- NA
  res0$tier[res0$div_id==1] <- 1
  res0$tier[res0$div_id==2] <- 2
  res0$tier[res0$div_id==3] <- 3
  res0$tier[res0$div_id==4] <- 4
  res0$tier[res0$div_id==5] <- 1
  res0$tier[res0$div_id==6] <- 2
  res0$tier[res0$div_id==7] <- 3
  res0$tier[res0$div_id==8] <- 4
  res0$tier[res0$div_id==9] <- 5
  res0$tier[res0$div_id==10] <- 3.1
  res0$tier[res0$div_id==11] <- 3.2
  
  
  res0 <- merge(res0,teams.above,by=c("season","tier"),all.x=TRUE)
  
  res0$full.pos1 <- res0$pos1 + res0$teams.above
  res0$full.pos2 <- res0$pos2 + res0$teams.above
  
  
  
  write.date = "2223-08-06" # Sys.Date()
  write.csv(elorank,paste0(dbloc,"/data/elorank-",write.date,".csv"))
  
  
  write.csv(res0,paste0(dbloc,"/data/res0-",write.date,".csv"))
  
  #list2223 <- list("res0"=res00,"elorank"=elorank)
  returnlist <- list("res0"=res00,"elorank"=elorank)
  return <- returnlist
}

do_elorank <- function(data,elo.weight=20) {
  #weights for Elo ranking --- rank league highest for club, non-friendly for national
  data$weight <- elo.weight
  
  ##Elo ranking routine
  elorank <- list()
  
  data$elostrength1 <- NA
  data$elostrength2 <- NA
  data$elopredict <- NA
  ##data.eng <- data.eng[duplicated(data.eng$match.link)==FALSE,]
  
  data <- data[order(data$date),]
  
  for(mm in c(1:NROW(data))) {
    print(100*mm/NROW(data))
    if(!(data$team1.2[mm] %in% names(elorank))) {
      elorank[[data$team1.2[mm]]] <- 1000
    }
    if(!(data$team2.2[mm] %in% names(elorank))) {
      elorank[[data$team2.2[mm]]] <- 1000
    }
    data$elostrength1[mm] <- elorank[[data$team1.2[mm]]]
    data$elostrength2[mm] <- elorank[[data$team2.2[mm]]]
    data$elopredict[mm] <- 1/(1+(10^((data$elostrength2[mm]-data$elostrength1[mm])/400)))
    adjustment <- data$outcome[mm] - data$elopredict[mm]
    if(is.na(adjustment)==FALSE) {
      elorank[data$team1.2[mm]] <- elorank[[data$team1.2[mm]]] + elo.weight*adjustment
      elorank[data$team2.2[mm]] <- elorank[[data$team2.2[mm]]] - elo.weight*adjustment
    }
  }
  data <- data[is.na(data$date)==FALSE,]
  return(data,elorank)
}

HeadToHead <- function(Opp.team0="Torquay United",dbloc="/Users/jjreade/Dropbox/Research/Sport/Correct-score") {
#  Opp.team <- tolower(Opp.team0)
#  allres <- read.csv(paste0(dbloc,"/data/res0-2022-07-17.csv"),stringsAsFactors = FALSE)
#  allres$team1 <- tolower(allres$team1)
#  allres$team2 <- tolower(allres$team2)
  Opp.team <- Opp.team0
  allres <- read.csv("/Users/jjreade/Dropbox/Research/Sport/OAFC/oafc-all-history-1907-08-on.csv",stringsAsFactors = FALSE)
  allres$Date[regexpr("/",allres$Date)>-1] <- gsub("^(\\d+)/(\\d+)/(\\d+)$","\\3-\\2-\\1",allres$Date[regexpr("/",allres$Date)>-1])
  allres$Date <- as.Date(allres$Date)
  VsOpp <- allres[allres$opposition==Opp.team,c("Date","opposition","goals1","goals2","division","venue","attendance","awayatt")]
  rm(allres)
  VsOpp$goals <- as.numeric(VsOpp$goals1)
  VsOpp$opp.goals <- as.numeric(VsOpp$goals2)
  VsOpp$tot.goals <- VsOpp$opp.goals + VsOpp$goals
  VsOpp$outcome <- 0.5*as.numeric(VsOpp$goals==VsOpp$opp.goals) + as.numeric(VsOpp$goals>VsOpp$opp.goals)
  VsOpp$home <- as.numeric(VsOpp$venue=="H")
  VsOpp$gdiff <- VsOpp$goals - VsOpp$opp.goals
  VsOpp$tgoals <- VsOpp$goals + VsOpp$opp.goals
  summary(VsOpp)
  
  print(paste0("P",NROW(VsOpp[is.na(VsOpp$goals1)==FALSE,]),
               " W",sum(VsOpp$outcome[is.na(VsOpp$goals1)==FALSE]==1),
               " D",sum(VsOpp$outcome[is.na(VsOpp$goals1)==FALSE]==0.5),
               " L",sum(VsOpp$outcome[is.na(VsOpp$goals1)==FALSE]==0),
               " Scored ",sum(VsOpp$goals[is.na(VsOpp$goals1)==FALSE]),
               " Conceded ",sum(VsOpp$opp.goals[is.na(VsOpp$goals1)==FALSE])))
  print(paste0("Win percentage = ",round(100*sum(VsOpp$outcome[is.na(VsOpp$goals1)==FALSE]==1)/NROW(VsOpp[is.na(VsOpp$goals1)==FALSE,]),2),"%"))
  print(paste0("(Home) P",NROW(VsOpp[VsOpp$home==1 & is.na(VsOpp$goals)==FALSE,]),
               " W",sum(VsOpp$outcome[VsOpp$home==1 & is.na(VsOpp$goals)==FALSE]==1),
               " D",sum(VsOpp$outcome[VsOpp$home==1 & is.na(VsOpp$goals)==FALSE]==0.5),
               " L",sum(VsOpp$outcome[VsOpp$home==1 & is.na(VsOpp$goals)==FALSE]==0),
               " Scored ",sum(VsOpp$goals[VsOpp$home==1 & is.na(VsOpp$goals)==FALSE]),
               " Conceded ",sum(VsOpp$opp.goals[VsOpp$home==1 & is.na(VsOpp$goals)==FALSE])))
  print(paste0("Home Win percentage = ",round(100*sum(VsOpp$outcome[VsOpp$home==1 & is.na(VsOpp$goals1)==FALSE]==1)/NROW(VsOpp[VsOpp$home==1 & is.na(VsOpp$goals1)==FALSE,]),2),"%"))
  print(paste0("(Away) P",NROW(VsOpp[VsOpp$home==0 & is.na(VsOpp$goals)==FALSE,]),
               " W",sum(VsOpp$outcome[VsOpp$home==0 & is.na(VsOpp$goals)==FALSE]==1),
               " D",sum(VsOpp$outcome[VsOpp$home==0 & is.na(VsOpp$goals)==FALSE]==0.5),
               " L",sum(VsOpp$outcome[VsOpp$home==0 & is.na(VsOpp$goals)==FALSE]==0),
               " Scored ",sum(VsOpp$goals[VsOpp$home==0 & is.na(VsOpp$goals1)==FALSE]),
               " Conceded ",sum(VsOpp$opp.goals[VsOpp$home==0 & is.na(VsOpp$goals)==FALSE])))
  print(paste0("Away Win percentage = ",round(100*sum(VsOpp$outcome[VsOpp$home==0 & is.na(VsOpp$goals1)==FALSE]==1)/NROW(VsOpp[VsOpp$home==0 & is.na(VsOpp$goals1)==FALSE,]),2),"%"))
  
  print(table(VsOpp$gdiff))
  print(table(VsOpp$tgoals))
  
  View(VsOpp)
  print(VsOpp)
  write.csv(VsOpp[,c("Date","opposition","goals","opp.goals","venue","division","attendance","awayatt")],
            paste0("/Users/jjreade/Dropbox/Research/Sport/OAFC/head-to-head-oafc-",Opp.team,"-",Sys.Date(),".csv"))
  return <- VsOpp
}

OTD <- function(date0=Sys.Date(),dbloc="/Users/jjreade/Dropbox/Research/Sport/Correct-score") {

#  allres <- read.csv(paste0(dbloc,"/data/res0-2022-07-17.csv"),stringsAsFactors = FALSE)
#  allres$date <- as.Date(allres$date)
  allres <- read.csv("/Users/jjreade/Dropbox/Research/Sport/OAFC/oafc-all-history-1907-08-on.csv",stringsAsFactors = FALSE)
  allres$Date[regexpr("/",allres$Date)>-1] <- gsub("^(\\d+)/(\\d+)/(\\d+)$","\\3-\\2-\\1",allres$Date[regexpr("/",allres$Date)>-1])
  allres$Date <- as.Date(allres$Date)
  allres <- allres[order(allres$Date),]
  date0 <- as.Date(date0)
  month=format(date0,"%m")
  day=format(date0,"%d")
  OTD <- allres[format(allres$Date,"%m")==month & format(allres$Date,"%d")==day,c("Date","opposition","goals1","goals2","division","venue","attendance","awayatt")]
  OTD <- OTD[is.na(OTD$goals1)==FALSE,]
  OTD$year <- format(OTD$Date,"%Y")
  OTD$goals <- as.numeric(OTD$goals1)
  OTD$opp.goals <- as.numeric(OTD$goals2)
  OTD$outcome <- 0.5*as.numeric(OTD$goals==OTD$opp.goals) + as.numeric(OTD$goals>OTD$opp.goals)
  OTD$home <- as.numeric(OTD$venue=="H")
  OTD$gdiff <- OTD$goals-OTD$opp.goals
  OTD <- OTD[is.na(OTD$outcome)==FALSE,]
  #summary(OTD)
  
  write.csv(OTD[,c("year","opposition","goals","opp.goals","division","venue","attendance","awayatt")],
            paste0("/Users/jjreade/Dropbox/Research/Sport/OAFC/OTD-oafc-",month,"-",day,".csv"))
  
  print(paste0("P",NROW(OTD),
               " W",sum(OTD$outcome==1),
               " D",sum(OTD$outcome==0.5),
               " L",sum(OTD$outcome==0),
               " Scored ",sum(OTD$goals),
               " Conceded ",sum(OTD$opp.goals)))
  print(paste0("(Home) P",NROW(OTD[OTD$home==1,])," W",sum(OTD$outcome[OTD$home==1]==1)," D",sum(OTD$outcome[OTD$home==1]==0.5)," L",sum(OTD$outcome[OTD$home==1]==0)," Scored ",sum(OTD$goals[OTD$home==1])," Conceded ",sum(OTD$opp.goals[OTD$home==1])))
  print(paste0("(Away) P",NROW(OTD[OTD$home==0,])," W",sum(OTD$outcome[OTD$home==0]==1)," D",sum(OTD$outcome[OTD$home==0]==0.5)," L",sum(OTD$outcome[OTD$home==0]==0)," Scored ",sum(OTD$goals[OTD$home==0])," Conceded ",sum(OTD$opp.goals[OTD$home==0])))

  print(OTD)
  View(OTD)
  
}

compile.oafc.historical.results <- function() {
  oafc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/old-OAFC-results.csv",stringsAsFactors = FALSE)
  oafc$date[regexpr("/",oafc$date)>-1] <- gsub("^(\\d+)/(\\d+)/(\\d+)$","\\3-\\2-\\1",oafc$date[regexpr("/",oafc$date)>-1])
  oafc$date <- as.Date(oafc$date)
  oafc$season <- as.numeric(format(oafc$date,"%Y")) - as.numeric(as.numeric(format(oafc$date,"%m"))<7)
  oafc$venue[oafc$venue!="N" & oafc$team1=="Oldham"] <- "H"
  oafc$venue[oafc$venue!="N" & oafc$team2=="Oldham"] <- "A"
  colnames(oafc)[colnames(oafc)=="Attendance"] <- "attendance"
  colnames(oafc)[colnames(oafc)=="date"] <- "Date"
  oafc$team1[oafc$team1=="Oldham"] <- "Oldham Athletic"
  oafc$team2[oafc$team2=="Oldham"] <- "Oldham Athletic"
  oafc$opposition[oafc$team1=="Oldham Athletic"] <- oafc$team2[oafc$team1=="Oldham Athletic"]
  oafc$opposition[oafc$team2=="Oldham Athletic"] <- oafc$team1[oafc$team2=="Oldham Athletic"]
  oafc$awayatt <- NA
  
  eflt <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/eflt-data.csv",stringsAsFactors = FALSE)
  eflt.oafc <- eflt[regexpr("Oldham",eflt$team1)>-1 | regexpr("Oldham",eflt$team2)>-1,]
  eflt.oafc$date <- as.Date(gsub("[.]html","",eflt.oafc$date))
  eflt.oafc$attendance2 <- as.numeric(eflt.oafc$attendance)
  table(is.na(eflt.oafc$attendance2))
  eflt.oafc$attendance2[is.na(eflt.oafc$attendance2)==TRUE] <- gsub("^(\\d+).*?$","\\1",eflt.oafc$attendance[is.na(eflt.oafc$attendance2)==TRUE])
  eflt.oafc$attendance2 <- as.numeric(eflt.oafc$attendance2)
  table(is.na(eflt.oafc$attendance2))
  eflt.oafc$attendance2[is.na(eflt.oafc$attendance2)==TRUE] <- eflt.oafc$venue[is.na(eflt.oafc$attendance2)==TRUE]
  eflt.oafc$attendance2 <- as.numeric(eflt.oafc$attendance2)
  table(is.na(eflt.oafc$attendance2))
  eflt.oafc$attendance2[is.na(eflt.oafc$attendance2)==TRUE] <- eflt.oafc$div2[is.na(eflt.oafc$attendance2)==TRUE]
  eflt.oafc$attendance2 <- as.numeric(eflt.oafc$attendance2)
  table(is.na(eflt.oafc$attendance2))
  
}

save.oafc.historical.results <- function(input.data = big.data) {
  oafc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/RFC-150/old-OAFC-results.csv",stringsAsFactors = FALSE)
  oafc$date[regexpr("/",oafc$date)>-1] <- gsub("^(\\d+)/(\\d+)/(\\d+)$","\\3-\\2-\\1",oafc$date[regexpr("/",oafc$date)>-1])
  oafc$date <- as.Date(oafc$date)
  oafc$season <- as.numeric(format(oafc$date,"%Y")) - as.numeric(as.numeric(format(oafc$date,"%m"))<7)
  oafc$venue[oafc$venue!="N" & oafc$team1=="Oldham"] <- "H"
  oafc$venue[oafc$venue!="N" & oafc$team2=="Oldham"] <- "A"
  colnames(oafc)[colnames(oafc)=="Attendance"] <- "attendance"
  colnames(oafc)[colnames(oafc)=="date"] <- "Date"
  oafc$team1[oafc$team1=="Oldham"] <- "Oldham Athletic"
  oafc$team2[oafc$team2=="Oldham"] <- "Oldham Athletic"
  oafc$opposition[oafc$team1=="Oldham Athletic"] <- oafc$team2[oafc$team1=="Oldham Athletic"]
  oafc$opposition[oafc$team2=="Oldham Athletic"] <- oafc$team1[oafc$team2=="Oldham Athletic"]
  oafc$awayatt <- NA
  
  oafc.h.atts <- big.data[is.na(big.data$footballwebpages.teams.1)==FALSE & big.data$footballwebpages.teams.1=="Oldham Athletic",
                          c("footballwebpages.teams.2","team2.11v11","goals1","goals2","Date",
                            "attendance","awayatt","division")]
  oafc.h.atts$opposition <- oafc.h.atts$footballwebpages.teams.2
  oafc.h.atts$opposition[oafc.h.atts$footballwebpages.teams.2==""] <- oafc.h.atts$team2.11v11[oafc.h.atts$footballwebpages.teams.2==""]
  
  oafc.h.atts <- oafc.h.atts[order(oafc.h.atts$Date),]
  write.csv(oafc.h.atts[,c("opposition","goals1","goals2","Date","attendance","awayatt","division")],
            "/Users/jjreade/Dropbox/Research/Sport/OAFC/oafc-attendance-history.csv")
  oafc.h.atts$cum.att[is.na(oafc.h.atts$attendance)==FALSE] <- cumsum(oafc.h.atts$attendance[is.na(oafc.h.atts$attendance)==FALSE])
  oafc.h.atts$cum.att.4 <- c(NA,NA,NA,NA,oafc.h.atts$cum.att[1:c(NROW(oafc.h.atts)-4)])
  oafc.h.atts$sum4matches <- oafc.h.atts$cum.att - oafc.h.atts$cum.att.4
  # oafc.h.atts <- oafc.h.atts[order(oafc.h.atts$attendance),]
  oafc.h.atts$venue <- "H"
  
  oafc.a.atts <- big.data[is.na(big.data$footballwebpages.teams.1)==FALSE & big.data$footballwebpages.teams.2=="Oldham Athletic",
                          c("footballwebpages.teams.1","team1.11v11","goals1","goals2","Date",
                            "attendance","awayatt","division")]
  oafc.a.atts$opposition <- oafc.a.atts$footballwebpages.teams.1
  oafc.a.atts$opposition[oafc.a.atts$footballwebpages.teams.1==""] <- oafc.a.atts$team1.11v11[oafc.a.atts$footballwebpages.teams.1==""]
  tempgoals2 <- oafc.a.atts$goals2
  oafc.a.atts$goals2 <- oafc.a.atts$goals1
  oafc.a.atts$goals1 <- tempgoals2
  oafc.a.atts$venue <- "A"
  
  oafc.all <- rbind(oafc.h.atts[,c("opposition","goals1","goals2","division","venue","Date",
                                   "attendance","awayatt")],
                    oafc.a.atts[,c("opposition","goals1","goals2","division","venue","Date",
                                   "attendance","awayatt")])
  oafc.all <- rbind(oafc.all,oafc[,c("opposition","goals1","goals2","Date","division","venue",
                                     "attendance","awayatt")])
  oafc.all <- oafc.all[order(oafc.all$Date),]
  
  write.csv(oafc.all,"/Users/jjreade/Dropbox/Research/Sport/OAFC/oafc-all-history-1907-08-on.csv")
  return(oafc.all)
}

load.latics.gatebooks <- function() {
  oafc.fols <- c("/Users/jjreade/Dropbox/Latics/turnstiles/Return of Match Receipts GATEBOOK/2016-17 games/",
                 "/Users/jjreade/Dropbox/Latics/turnstiles/Return of Match Receipts GATEBOOK/2017-18 games/",
                 "/Users/jjreade/Dropbox/Latics/turnstiles/Return of Match Receipts GATEBOOK/2018-19 games/",
                 "/Users/jjreade/Dropbox/Latics/turnstiles/Return of Match Receipts GATEBOOK/2019-20 games/",
                 "/Users/jjreade/Dropbox/Latics/turnstiles/Return of Match Receipts GATEBOOK/2021-22 games/")
  oafc.data <- data.frame(stringsAsFactors = FALSE)
  for(fol in oafc.fols) {
    print(fol)
    files <- list.files(fol)
    for(fl in files) {
      print(fl)
      temp <- data.frame(read_excel(paste0(fol,fl)),stringsAsFactors = FALSE)
      if(is.na(temp[3,6])==FALSE) {
        add=1
      } else {
        add=0
      }
      temp0 <- temp[1:c(6+add),]
      temp1 <- temp[c(9+add):NROW(temp),]
      colnames(temp1) <- temp[8+add,]
      temp1 <- temp1[is.na(temp1$`Turnstile No`)==FALSE,]
      rowdata <- data.frame("Date"=as.Date(gsub("^(\\d+)\\w+ (\\w+) (\\d+)","\\1 \\2 \\3",temp0[4+add,10]),"%d %B %Y"),
                            "hometeam"="Oldham Athletic","awayteam"=temp0[4+add,6],
                            "attendance"=sum(as.numeric(temp1$Total[regexpr("Ticket Sales|Total",temp1$`Turnstile No`)==-1]),na.rm=TRUE),
                            "awayatt"=sum(as.numeric(temp1$Total[regexpr("CRE AWAY",temp1$`Turnstile No`)>-1]),na.rm=TRUE),
                            stringsAsFactors = FALSE)
      oafc.data <- rbind(oafc.data,rowdata)
    }
  }
  
  #oafc.data <- merge(oafc.data)
  
  oafc.data <- oafc.data[is.na(oafc.data$awayteam)==FALSE,]
  oafc.data$awayteam <- stringr::str_to_title(gsub("Under ","U",gsub("MK ","Milton Keynes ",oafc.data$awayteam)))
  oafc.data$awayteam <- gsub("Afc","AFC",oafc.data$awayteam)
  oafc.data$awayteam[oafc.data$awayteam=="Halifax Town"] <- "FC Halifax Town"
  oafc.data$awayteam <- gsub("Birtmingham","Birmingham",oafc.data$awayteam)
  oafc.data$Date[oafc.data$Date=="2018-10-27"] <- as.Date("2018-10-23")
  oafc.data$Date <- as.Date(gsub("210","201",oafc.data$Date))
  oafc.data$Date[oafc.data$awayteam=="Bury" & oafc.data$attendance==5183] <- "2017-10-24"
  
  return(oafc.data)
}

load.eng.oddsportal.results <- function() {
  ##### correspondence files #####
  corr <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/correspondence-file.csv",stringsAsFactors = FALSE)
  corof <- corr[,c("oddsportal.team","footballwebpages.teams")]
  corof <- corof[duplicated(corof)==FALSE,]
  corof <- corof[corof$oddsportal.team!="",]
  corof <- corof[duplicated(corof$oddsportal.team)==FALSE,]
  corofx <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/correspondence-file-op-fwb-extra.csv",stringsAsFactors = FALSE)
  corofx <- corofx[duplicated(corofx)==FALSE,]
  corofx <- corofx[corofx$oddsportal!="",]
  corofx <- corofx[duplicated(corofx$oddsportal)==FALSE,]
  
  oploc <- "/Users/jjreade/Dropbox/Research/Sport/Betting Markets/Data/"
  opfiles <- c("england-results-championship-exch.csv","england-results-efl-cup-exch.csv",
               "england-results-efl-trophy-exch.csv","england-results-fa-community-shield-exch.csv",
               "england-results-fa-cup-exch.csv","england-results-fa-trophy-exch.csv",
               "england-results-isthmian-league-premier-division-exch.csv",
               "england-results-league-one-exch.csv","england-results-league-two-exch.csv",
               "england-results-national-league-exch.csv","england-results-national-league-north-exch.csv",
               "england-results-national-league-south-exch.csv",
               "england-results-non-league-premier-play-offs-exch.csv",
               "england-results-npl-premier-division-exch.csv","england-results-premier-league-2-exch.csv",
               "england-results-premier-league-cup-exch.csv","england-results-premier-league-exch.csv",
               "england-results-premier-league-international-cup-exch.csv",
               "england-results-premier-league-u18-exch.csv","england-results-premier-league-u21-exch.csv",
               "england-results-professional-development-league-exch.csv",
               "england-results-southern-league-central-division-exch.csv",
               "england-results-southern-league-south-division-exch.csv",
               "england-results-southern-premier-league-exch.csv",
               "england-results-women-s-championship-exch.csv",
               "england-results-women-s-fa-community-shield-exch.csv",
               "england-results-women-s-fa-cup-exch.csv","england-results-women-s-league-cup-exch.csv",
               "england-results-women-s-super-league-exch.csv")
  #world-results-club-friendly-exch.csv
  ##collapse on match level using mean probabilities
  want.opfiles <- c(1:3,5:14,22)
  op.seasons <- data.frame(stringsAsFactors = FALSE)
  op.matches <- data.frame(stringsAsFactors = FALSE)
  for(ww in 1:22) {
    temp <- read.csv(paste0(oploc,opfiles[ww]),stringsAsFactors = FALSE)
    temp$team1 <- gsub("^(.*?) - (.*?)$","\\1",temp$teams)
    temp$team2 <- gsub("^(.*?) - (.*?)$","\\2",temp$teams)
    
    temp$team1 <- gsub("Rushden$","Rushden &amp; Diamonds",temp$team1)
    temp$team2 <- gsub("Rushden$","Rushden &amp; Diamonds",temp$team2)
    temp$team1 <- gsub("Blyth$","Blyth Spartan",temp$team1)
    temp$team1 <- gsub("^Fylde","AFC Fylde",temp$team1)
    
    temp <- temp[is.na(temp$odds1)==FALSE,]
    temp <- temp[is.na(temp$odds2)==FALSE,]
    temp <- temp[is.na(temp$odds3)==FALSE,]
    
    if(any(regexpr("/",temp$odds1)>-1)) {
      temp$odds1[regexpr("/",temp$odds1)>-1] <- as.numeric(gsub("^(\\d+)/(\\d+)$","\\1",temp$odds1[regexpr("/",temp$odds1)>-1]))/as.numeric(gsub("^(\\d+)/(\\d+)$","\\2",temp$odds1[regexpr("/",temp$odds1)>-1]))+1
    }
    if(any(regexpr("/",temp$odds2)>-1)) {
      temp$odds2[regexpr("/",temp$odds2)>-1] <- as.numeric(gsub("^(\\d+)/(\\d+)$","\\1",temp$odds2[regexpr("/",temp$odds2)>-1]))/as.numeric(gsub("^(\\d+)/(\\d+)$","\\2",temp$odds2[regexpr("/",temp$odds2)>-1]))+1
    }
    if(any(regexpr("/",temp$odds3)>-1)) {
      temp$odds3[regexpr("/",temp$odds3)>-1] <- as.numeric(gsub("^(\\d+)/(\\d+)$","\\1",temp$odds3[regexpr("/",temp$odds3)>-1]))/as.numeric(gsub("^(\\d+)/(\\d+)$","\\2",temp$odds3[regexpr("/",temp$odds3)>-1]))+1
    }
    
    temp$pH0 <- 1/as.numeric(temp$odds1)
    temp$pD0 <- 1/as.numeric(temp$odds2)
    temp$pA0 <- 1/as.numeric(temp$odds3)
    temp$overround <- temp$pH0 + temp$pD0 + temp$pA0
    temp$pH <- temp$pH0/temp$overround
    temp$pD <- temp$pD0/temp$overround
    temp$pA <- temp$pA0/temp$overround
    
    temp$Date <- as.Date(gsub("^\\w+, (\\d+)\\s+(\\w+)\\s+(\\d{4}), .*?$","\\1 \\2 \\3",temp$date2),"%d %b %Y")
    temp$const <- 1
    temp0 <- temp[duplicated(temp$match.id)==FALSE,c("match.id","team1","goals1","goals2","team2","Date")]
    temp1 <- aggregate(temp[,c("pH","pD","pA","overround")],by=list(temp$match.id),FUN=mean)
    temp1b <- aggregate(temp$const,by=list(temp$match.id),FUN=sum)
    colnames(temp1b)[2] <- "no.bookies"
    
    temp2 <- merge(temp0,temp1,by.x=c("match.id"),by.y=c("Group.1"),all=TRUE)
    temp2 <- merge(temp2,temp1b,by.x=c("match.id"),by.y=c("Group.1"),all=TRUE)
    
    temp2$team1[regexpr("Halifax",temp2$team1)>-1] <- "FC Halifax"
    temp2$team2[regexpr("Halifax",temp2$team2)>-1] <- "FC Halifax"
    temp2 <- merge(temp2,corof,by.x=c("team1"),by.y=c("oddsportal.team"),all.x=TRUE)
    temp2 <- merge(temp2,corof,by.x=c("team2"),by.y=c("oddsportal.team"),all.x=TRUE,suffixes=c(".1",".2"))
    temp2 <- merge(temp2,corofx,by.x=c("team1"),by.y=c("oddsportal"),all.x=TRUE)
    temp2 <- merge(temp2,corofx,by.x=c("team2"),by.y=c("oddsportal"),all.x=TRUE,suffixes=c("1","2"))
    temp2$footballwebpages.teams.1[is.na(temp2$footballwebpages.teams.1)==TRUE & is.na(temp2$fwb1)==FALSE] <- temp2$fwb1[is.na(temp2$footballwebpages.teams.1)==TRUE & is.na(temp2$fwb1)==FALSE]
    temp2$footballwebpages.teams.2[is.na(temp2$footballwebpages.teams.2)==TRUE & is.na(temp2$fwb2)==FALSE] <- temp2$fwb2[is.na(temp2$footballwebpages.teams.2)==TRUE & is.na(temp2$fwb2)==FALSE]
    
    temp2$div <- gsub("england-results-(\\S+)-exch[.]csv","\\1",opfiles[ww])
    op.matches <- rbind(op.matches,temp2[,c("team1","team2","Date","match.id","div","goals1","goals2",
                                            "pH","pD","pA","overround","no.bookies",
                                            "footballwebpages.teams.1","footballwebpages.teams.2")])
    
    temp$season <- as.numeric(format(temp$Date,"%Y")) - as.numeric(as.numeric(format(temp$Date,"%m"))<7)
    temp$outcomeH <- as.numeric(temp$goals1>temp$goals2)
    temp$outcomeD <- as.numeric(temp$goals1==temp$goals2)
    temp$outcomeA <- as.numeric(temp$goals1<temp$goals2)
    temp.season <- aggregate(temp[,c("pH","pD","pA","goals1","goals2","outcomeH","outcomeD","outcomeA","overround","season")],by=list(temp$season),FUN=mean,na.rm=TRUE)
    temp.season$div <- gsub("england-results-(\\S+)-exch[.]csv","\\1",opfiles[ww])
    op.seasons <- rbind(op.seasons,temp.season)
  }

  op <- list()
  op$matches <- op.matches
  op$seasons <- op.seasons
  return(op)
}

load.wycombe.att.data <- function() {
  require(tools)
  wyc <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/wycombe-attendances2.csv",stringsAsFactors = FALSE)
  #clean up the opponents variable
  wyc$Opposition <- gsub("^\\s+|\\s+$","",gsub("[(].*?[)]","",tolower(wyc$Opposition)))
  wyc$Opposition[regexpr("yeovil",wyc$Opposition)>-1] <- "yeovil town"
  wyc$Opposition[regexpr("tranmere",wyc$Opposition)>-1] <- "tranmere rovers"
  wyc$Opposition[regexpr("torquay",wyc$Opposition)>-1] <- "torquay united"
  wyc$Opposition[regexpr("tottenham",wyc$Opposition)>-1] <- "tottenham hotspur"
  wyc$Opposition[regexpr("stockport",wyc$Opposition)>-1] <- "stockport county"
  wyc$Opposition[regexpr("stevenage",wyc$Opposition)>-1] <- "stevenage"
  wyc$Opposition[regexpr("southend",wyc$Opposition)>-1] <- "southend united"
  wyc$Opposition[regexpr("shrewsbury",wyc$Opposition)>-1] <- "shrewsbury town"
  wyc$Opposition[regexpr("sheffield w",wyc$Opposition)>-1] <- "sheffield wednesday"
  wyc$Opposition[regexpr("sheffield u",wyc$Opposition)>-1] <- "sheffield united"
  wyc$Opposition[regexpr("rotherha",wyc$Opposition)>-1] <- "rotherham united"
  wyc$Opposition[regexpr("qpr",wyc$Opposition)>-1] <- "queens park rangers"
  wyc$Opposition[regexpr("preston",wyc$Opposition)>-1] <- "preston north end"
  wyc$Opposition[regexpr("plymouth",wyc$Opposition)>-1] <- "plymouth argyle"
  wyc$Opposition[regexpr("peterborough",wyc$Opposition)>-1] <- "peterborough united"
  wyc$Opposition[regexpr("oxford",wyc$Opposition)>-1] <- "oxford united"
  wyc$Opposition[regexpr("notts",wyc$Opposition)>-1] <- "notts county"
  wyc$Opposition[regexpr("morecamb",wyc$Opposition)>-1] <- "morecambe"
  wyc$Opposition[regexpr("mk ",wyc$Opposition)>-1] <- "milton keynes"
  wyc$Opposition[regexpr("franchise",wyc$Opposition)>-1] <- "milton keynes dons"
  wyc$Opposition[regexpr("milton keynes",wyc$Opposition)>-1] <- "milton keynes dons"
  wyc$Opposition[regexpr("mansfield",wyc$Opposition)>-1] <- "mansfield town"
  wyc$Opposition[regexpr("man city",wyc$Opposition)>-1] <- "manchester city"
  wyc$Opposition[regexpr("macclesfield",wyc$Opposition)>-1] <- "macclesfield town"
  wyc$Opposition[regexpr("lincoln",wyc$Opposition)>-1] <- "lincoln city"
  wyc$Opposition[regexpr("leeds",wyc$Opposition)>-1] <- "leeds united"
  wyc$Opposition[regexpr("kidderminster",wyc$Opposition)>-1] <- "kidderminster harriers"
  wyc$Opposition[regexpr("hartlepool",wyc$Opposition)>-1] <- "hartlepool united"
  wyc$Opposition[regexpr("gilling",wyc$Opposition)>-1] <- "gillingham"
  wyc$Opposition[regexpr("forest green",wyc$Opposition)>-1] <- "forest green rovers"
  wyc$Opposition[regexpr("exeter",wyc$Opposition)>-1] <- "exeter city"
  wyc$Opposition[regexpr("doncaster",wyc$Opposition)>-1] <- "doncaster rovers"
  wyc$Opposition[regexpr("dagenham",wyc$Opposition)>-1] <- "dagenham &amp; redbridge"
  wyc$Opposition[regexpr("crewe",wyc$Opposition)>-1] <- "crewe alexandra"
  wyc$Opposition[regexpr("crawley",wyc$Opposition)>-1] <- "crawley town"
  wyc$Opposition[regexpr("colchester",wyc$Opposition)>-1] <- "colchester united"
  wyc$Opposition[regexpr("torquay",wyc$Opposition)>-1] <- "torquay united"
  wyc$Opposition[regexpr("chesterf",wyc$Opposition)>-1] <- "chesterfield"
  wyc$Opposition[regexpr("chester ",wyc$Opposition)>-1] <- "chester"
  wyc$Opposition[regexpr("cheltenham",wyc$Opposition)>-1] <- "cheltenham town"
  wyc$Opposition[regexpr("charlton",wyc$Opposition)>-1] <- "charlton athletic"
  wyc$Opposition[regexpr("carlisle",wyc$Opposition)>-1] <- "carlisle united"
  wyc$Opposition[regexpr("cambridge",wyc$Opposition)>-1] <- "cambridge united"
  wyc$Opposition[regexpr("burton",wyc$Opposition)>-1] <- "burton albion"
  wyc$Opposition[regexpr("bristol r",wyc$Opposition)>-1] <- "bristol rovers"
  wyc$Opposition[regexpr("brighton",wyc$Opposition)>-1] <- "brighton &amp; hove albion"
  wyc$Opposition[regexpr("boston",wyc$Opposition)>-1] <- "boston united"
  wyc$Opposition[regexpr("aldershot",wyc$Opposition)>-1] <- "aldershot town"
  wyc$Opposition[regexpr("accrington",wyc$Opposition)>-1] <- "accrington stanley"
  
  wyc$Date[wyc$Opposition=="doncaster rovers" & wyc$Date=="2019 Sat 29 Feb"] <- "2020 Sat 29 Feb"
  
  wyc$Date <- as.Date(wyc$Date,"%Y %a %d %b")
  wyc <- wyc[is.na(wyc$Date)==FALSE,]
  
  #need to add year for matches in jan, feb, mar, apr, may, jun, jul
  wyc$Year <- as.numeric(format(wyc$Date,"%Y"))
  wyc$Date[format(wyc$Date,"%m")<"08"] <- as.Date(paste0(wyc$Year[format(wyc$Date,"%m")<"08"]+1,"-",
                                                         format(wyc$Date[format(wyc$Date,"%m")<"08"],"%m"),"-",
                                                         format(wyc$Date[format(wyc$Date,"%m")<"08"],"%d")))
  wyc$Date[wyc$Opposition=="doncaster rovers" & is.na(wyc$Date)==TRUE] <- as.Date("2020-02-29")
  
  wyc$Venue[wyc$Date=="2007-12-04" & wyc$Opposition=="hereford united"] <- "H"
  wyc$Date[wyc$Date=="2008-12-26"] <-as.Date("2008-12-28")
  wyc$Date[wyc$Date=="2008-12-24" & wyc$Opposition=="exeter city"] <- as.Date("2008-12-26")
  
  
  # library(tools)
  wyc$HomeTeam <- "wycombe wanderers"
  wyc$HomeTeam[wyc$Venue=="A"] <- wyc$Opposition[wyc$Venue=="A"]
  wyc$HomeTeam <- toTitleCase(wyc$HomeTeam)
  wyc$HomeTeam[regexpr("Bournemouth",wyc$HomeTeam)>-1] <- "AFC Bournemouth"
  wyc$HomeTeam[regexpr("Wimbledon",wyc$HomeTeam)>-1] <- "AFC Wimbledon"
  wyc$AwayTeam <- "wycombe wanderers"
  wyc$AwayTeam[wyc$Venue=="H"] <- wyc$Opposition[wyc$Venue=="H"]
  wyc$AwayTeam <- toTitleCase(wyc$AwayTeam)
  wyc$AwayTeam[regexpr("Bournemouth",wyc$AwayTeam)>-1] <- "AFC Bournemouth"
  wyc$AwayTeam[regexpr("Wimbledon",wyc$AwayTeam)>-1] <- "AFC Wimbledon"
  
  return(wyc)
}

load.barnet.att.data <- function() {
  barnet <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Attendance/data/barnet-attendances.csv",stringsAsFactors = FALSE)
  colnames(barnet) <- c(colnames(barnet)[-1],"empty")
  
  barnet$opponent <- gsub("^\\s+|\\s+$","",gsub("[(].*?[)]","",barnet$opponent))
  barnet$opponent[regexpr("Bournemouth",barnet$opponent)>-1] <- "AFC Bournemouth"
  barnet$opponent <- gsub(" and "," &amp; ",barnet$opponent)
  barnet$opponent <- gsub("&#39;","'",barnet$opponent)
  barnet$opponent[barnet$opponent=="Milton Keynes"] <- "Milton Keynes Dons"
  
  barnet$HomeTeam <- "Barnet"
  barnet$HomeTeam[barnet$venue=="A"] <- barnet$opponent[barnet$venue=="A"]
  # barnet$HomeTeam <- toTitleCase(barnet$HomeTeam)
  # barnet$HomeTeam[regexpr("Bournemouth",barnet$HomeTeam)>-1] <- "AFC Bournemouth"
  # barnet$HomeTeam[regexpr("Wimbledon",barnet$HomeTeam)>-1] <- "AFC Wimbledon"
  barnet$AwayTeam <- "Barnet"
  barnet$AwayTeam[barnet$venue=="H"] <- barnet$opponent[barnet$venue=="H"]
  # barnet$AwayTeam <- toTitleCase(barnet$AwayTeam)
  # barnet$AwayTeam[regexpr("Bournemouth",barnet$AwayTeam)>-1] <- "AFC Bournemouth"
  # barnet$AwayTeam[regexpr("Wimbledon",barnet$AwayTeam)>-1] <- "AFC Wimbledon"
  
  barnet$Date <- as.Date(barnet$date,"%d/%m/%Y")
  
  barnet$Attendance <- gsub("^(\\d+)\\s+[(](\\d+)[)]","\\1",barnet$attendance)
  barnet$away.att <- gsub("^(\\d+)\\s+[(](\\d+)[)]","\\2",barnet$attendance)
  barnet$away.att[regexpr("[)]",barnet$attendance)==-1 & barnet$Attendance==barnet$away.att] <- NA
  return(barnet)
}

simulate.world.cup <- function() {
  require(zoo)
  require(xtable)
  ##### load data up to current season #####
  international.competitions <- data.frame("division"=c("European Championships","World Cup",
                                                        "International Friendly","Nations League",
                                                        "Asian Cup","Copa America","Concacaf Gold Cup",
                                                        "Confederations Cup","Inter Continental Championship",
                                                        "Olympic Games","African Nations Cup"),
                                           "div_id"=c(68,73,83,381,155,84,103,104,72,173,98))
  
  all.sb <- load.soccerbase.results(region="international")
  all.sb <- all.sb[!(is.na(all.sb$goals1)==TRUE & all.sb$date=="2024-06-22"),]

  groups <- list("A"=c("germany","scotland","hungary","switzerland"),
                 "B"=c("spain","croatia","italy","albania"),
                 "C"=c("slovenia","denmark","serbia","england"),
                 "D"=c("poland","netherlands","austria","france"),
                 "E"=c("belgium","slovakia","romania","ukraine"),
                 "F"=c("turkey","georgia","portugal","czech rep"))
                 # ,
                 # "G"=c("brazil","serbia","switzerland","cameroon"),
                 # "H"=c("portugal","ghana","uruguay","south korea")
  #dbloc="/Volumes/11330730-dp/Correct-score/"
  dbloc="/Users/jjreade/Dropbox/Research/Sport/Correct-score"
  
  # elorank10 <- read.csv(paste0(dbloc,"/data/elorank-2022-06-01.csv"),stringsAsFactors = FALSE)
  # full.data0 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/res0-2022-06-01.csv",stringsAsFactors = FALSE, encoding = "UTF-8")
  # full.data0$date <- as.Date(full.data0$date)
  # full.data.int <- full.data0[full.data0$div_id %in% international.competitions$div_id,]
  # full.data.int$team1 <- tolower(full.data.int$team1)
  # full.data.int$team2 <- tolower(full.data.int$team2)
  # full.data0 <- res0

  #want team and division IDs to merge into kaggle dataset
  all_sb_ids1 <- all.sb[,c("team1","team1_id")]
  colnames(all_sb_ids1) <- c("team","team_id")
  all_sb_ids2 <- all.sb[,c("team2","team2_id")]
  colnames(all_sb_ids2) <- c("team","team_id")
  all_sb_ids <- rbind(all_sb_ids1,all_sb_ids2)
  all_sb_ids <- all_sb_ids[duplicated(all_sb_ids)==FALSE,]
  all_sb_ids$team <- tolower(all_sb_ids$team)
  # all_sb_div_ids <- full.data.int[duplicated(full.data.int$div_id)==FALSE,c("division","div_id")]
  
  int.res0 <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Development/data/results-2024-06-03.csv",stringsAsFactors = FALSE)
  int.res0$date <- as.Date(int.res0$date)
  int.res0$match_id <- NA
  
  ##first round of group matches in every euro tournament
  int.res0 <- int.res0[order(int.res0$date),]
  int.res0$euro.matchno[int.res0$tournament=="UEFA Euro"] <- sequence(rle(paste0(int.res0$tournament[int.res0$tournament=="UEFA Euro"],"-",format(int.res0$date[int.res0$tournament=="UEFA Euro"],"%Y")))$lengths)
  int.res0$first.group.round <- as.numeric(int.res0$tournament=="UEFA Euro" & format(int.res0$date,"%Y") %in% c("1980","1984","1988","1992") & int.res0$euro.matchno<=4) + as.numeric(int.res0$tournament=="UEFA Euro" & format(int.res0$date,"%Y") %in% c("1996","2000","2004","2008","2012") & int.res0$euro.matchno<=8) + as.numeric(int.res0$tournament=="UEFA Euro" & format(int.res0$date,"%Y") %in% c("2016","2021","2024") & int.res0$euro.matchno<=12)
  int.res0$group.stage <- as.numeric(int.res0$tournament=="UEFA Euro" & format(int.res0$date,"%Y") %in% c("1980","1984","1988","1992") & int.res0$euro.matchno<=12) + as.numeric(int.res0$tournament=="UEFA Euro" & format(int.res0$date,"%Y") %in% c("1996","2000","2004","2008","2012") & int.res0$euro.matchno<=24) + as.numeric(int.res0$tournament=="UEFA Euro" & format(int.res0$date,"%Y") %in% c("2016","2021","2024") & int.res0$euro.matchno<=36)
  int.res0$total_score <- int.res0$home_score+int.res0$away_score
  int.res0$goal_diff <- abs(int.res0$home_score-int.res0$away_score)
  int.res0$outcome <- 0.5*as.numeric(int.res0$home_score==int.res0$away_score) + as.numeric(int.res0$home_score>int.res0$away_score)
  
  summary(int.res0$total_score[int.res0$first.group.round==1 & int.res0$date<"1994-01-01"])
  summary(int.res0$total_score[int.res0$first.group.round==1 & int.res0$date>"1994-01-01" & int.res0$date<"2016-01-01"])
  summary(int.res0$total_score[int.res0$first.group.round==1 & int.res0$date>"2016-01-01"])
  
  colnames(int.res0)[colnames(int.res0) %in% c("home_team","away_team","home_score","away_score")] <- c("team1","team2","goals1","goals2")
  int.res0$division <- int.res0$tournament
  int.res0$division <- gsub("Friendly","International Friendly",gsub("Gold Cup","Concacaf Gold Cup",gsub("African Cup of Nations","African Nations Cup",gsub("Intercontinental Cup","Inter Continental Championship",gsub(" qualification","",gsub("Copa América","Copa America",gsub("FIFA ","",gsub("CONIFA ","",gsub("CFU ","",gsub("UEFA ","",gsub("CONCACAF ","",gsub("AFC ","",int.res0$division))))))))))))
  int.res0$division[int.res0$division=="Euro"] <- "European Championships"
  int.res0$division[!(int.res0$division %in% international.competitions$division)] <- "International Friendly"
  # int.res0$division <- tolower(int.res0$division)
  int.res0 <- merge(int.res0,international.competitions,by=c("division"),all.x=TRUE)
  
  int.res0$team1 <- tolower(int.res0$team1)
  int.res0$team2 <- tolower(int.res0$team2)
  int.res0$team1[int.res0$team1=="northern ireland"] <- "n ireland"
  int.res0$team1[int.res0$team1=="republic of ireland"] <- "ireland"
  int.res0$team1[int.res0$team1=="united states"] <- "usa"
  int.res0$team1[int.res0$team1=="united arab emirates"] <- "uae"
  int.res0$team1[int.res0$team1=="trinidad and tobago"] <- "trinidad"
  int.res0$team1[int.res0$team1=="suriname"] <- "surinam"
  int.res0$team1[int.res0$team1=="são tomé and príncipe"] <- "sao tome &amp; p"
  int.res0$team1[int.res0$team1=="saint vincent and the grenadines"] <- "st vin &amp; gren"
  int.res0$team1[int.res0$team1=="solomon islands"] <- "soloman islands"
  int.res0$team1[int.res0$team1=="north macedonia"] <- "n macedonia"
  int.res0$team1[int.res0$team1=="czech republic"] <- "czech rep"
  int.res0$team1[int.res0$team1=="curaçao"] <- "curacao"
  int.res0$team1[int.res0$team1=="dominican republic"] <- "dominican rep"
  int.res0$team1[int.res0$team1=="china pr"] <- "china"
  int.res0$team1[int.res0$team1=="central african republic"] <- "cen af rep"
  int.res0$team1[int.res0$team1=="cape verde"] <- "cape verde is."
  int.res0$team1[int.res0$team1=="bosnia and herzegovina"] <- "bosnia-hz."
  int.res0$team1[int.res0$team1=="antigua and barbuda"] <- "antigua &amp; barb"
  int.res0$team1[int.res0$team1=="vietnam republic"] <- "vietnam"
  int.res0$team1[int.res0$team1=="saint lucia"] <- "st lucia"
  int.res0$team1[int.res0$team1=="saint kitts and nevis"] <- "st kitts and nev"
  int.res0$team1[int.res0$team1=="saint lucia"] <- "st lucia"
  int.res0$team1[int.res0$team1=="papua new guinea"] <- "pap new guinea"
  int.res0$team1[int.res0$team1=="equatorial guinea"] <- "eq guinea"
  int.res0$team1[int.res0$team1=="united states virgin islands"] <- "virgin isl. (us)"
  int.res0$team1[int.res0$team1=="british virgin islands"] <- "virgin isl. (uk)"
  int.res0$team1[int.res0$team1=="northern ireland"] <- "n ireland"
  int.res0$team1[int.res0$team1=="german dr"] <- "east germany"
  int.res0$team1[int.res0$team1=="american samoa"] <- "am samoa"
  int.res0$team1[int.res0$team1=="turks and caicos islands"] <- "turks &amp; caicos"
  int.res0$team2[int.res0$team2=="republic of ireland"] <- "ireland"
  int.res0$team2[int.res0$team2=="united states"] <- "usa"
  int.res0$team2[int.res0$team2=="united arab emirates"] <- "uae"
  int.res0$team2[int.res0$team2=="trinidad and tobago"] <- "trinidad"
  int.res0$team2[int.res0$team2=="suriname"] <- "surinam"
  int.res0$team2[int.res0$team2=="são tomé and príncipe"] <- "sao tome &amp; p"
  int.res0$team2[int.res0$team2=="saint vincent and the grenadines"] <- "st vin &amp; gren"
  int.res0$team2[int.res0$team2=="solomon islands"] <- "soloman islands"
  int.res0$team2[int.res0$team2=="north macedonia"] <- "n macedonia"
  int.res0$team2[int.res0$team2=="czech republic"] <- "czech rep"
  int.res0$team2[int.res0$team2=="curaçao"] <- "curacao"
  int.res0$team2[int.res0$team2=="dominican republic"] <- "dominican rep"
  int.res0$team2[int.res0$team2=="china pr"] <- "china"
  int.res0$team2[int.res0$team2=="central african republic"] <- "cen af rep"
  int.res0$team2[int.res0$team2=="cape verde"] <- "cape verde is."
  int.res0$team2[int.res0$team2=="bosnia and herzegovina"] <- "bosnia-hz."
  int.res0$team2[int.res0$team2=="antigua and barbuda"] <- "antigua &amp; barb"
  int.res0$team2[int.res0$team2=="vietnam republic"] <- "vietnam"
  int.res0$team2[int.res0$team2=="saint lucia"] <- "st lucia"
  int.res0$team2[int.res0$team2=="saint kitts and nevis"] <- "st kitts and nev"
  int.res0$team2[int.res0$team2=="saint lucia"] <- "st lucia"
  int.res0$team2[int.res0$team2=="papua new guinea"] <- "pap new guinea"
  int.res0$team2[int.res0$team2=="equatorial guinea"] <- "eq guinea"
  int.res0$team2[int.res0$team2=="united states virgin islands"] <- "virgin isl. (us)"
  int.res0$team2[int.res0$team2=="british virgin islands"] <- "virgin isl. (uk)"
  int.res0$team2[int.res0$team2=="german dr"] <- "east germany"
  int.res0$team2[int.res0$team2=="american samoa"] <- "am samoa"
  int.res0$team2[int.res0$team2=="turks and caicos islands"] <- "turks &amp; caicos"
  
  int.res0 <- merge(int.res0,all_sb_ids,by.x="team2",by.y = "team",all.x=TRUE)
  int.res0 <- merge(int.res0,all_sb_ids,by.x="team1",by.y = "team",all.x=TRUE,suffixes = c("2","1"))

  all.sb$city <- NA
  all.sb$country <- NA
  all.sb$neutral <- NA
  
  colnames(int.res0)[regexpr("team_id",colnames(int.res0))>-1] <- c("team2_id","team1_id")
  
  ##add in all matches *not* in kaggle to kaggle...
  all.sb$team1 <- tolower(all.sb$team1)
  all.sb$team2 <- tolower(all.sb$team2)
  int.res0$date.teams <- paste0(int.res0$date,"-",int.res0$team1,"-",int.res0$team2)
  int.res0 <- int.res0[duplicated(int.res0$date.teams)==FALSE,]
  all.sb$date.teams <- paste0(all.sb$date,"-",all.sb$team1,"-",all.sb$team2)
  all.sb <- all.sb[duplicated(all.sb$date.teams)==FALSE,]
  int.res0 <- int.res0[!(int.res0$div_id==68 & int.res0$date>="2024-06-14" & int.res0$date<="2024-07-07"),]
  all.sb <- all.sb[regexpr("group",all.sb$team1)==-1,]
  
  all.sb$tournament <- NA
  
  int.res <- rbind(int.res0[,c("match_id","date.teams","date","team1_id","team1","goals1","goals2",
                               "team2","team2_id","division","div_id","city","country","neutral","tournament")],
                   all.sb[!(all.sb$date.teams %in% int.res0$date.teams),c("match_id","date.teams","date",
                                                                          "team1_id","team1","goals1","goals2",
                                                                          "team2","team2_id","division","div_id",
                                                                          "city","country","neutral","tournament")])
  
  ##want match_id from soccerbase to go into the int.res dataset
  int.res <- merge(int.res,all.sb[c("date.teams","match_id")],by="date.teams",all.x=TRUE,suffixes=c("","2"))
  int.res$match_id[is.na(int.res$match_id)==TRUE & is.na(int.res$match_id2)==FALSE] <- int.res$match_id2[is.na(int.res$match_id)==TRUE & is.na(int.res$match_id2)==FALSE]
  
  ##### load data updates --- once tournament starts? #####
  # update.files <- list.files("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/",pattern="^historical_results_202.*?.csv$")
  # update.files <- sort(update.files[update.files>="historical_results_2022-06-01-2020.csv"])
  # 
  # int.res.update <- data.frame()
  # for(ff in update.files) {
  #   print(ff)
  #   temp <- read.csv(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/soccerbase-data/",ff),stringsAsFactors = FALSE)
  #   temp$division[temp$division==""] <- NA
  #   temp$div_id[temp$div_id=="n/a"] <- "-99"
  #   temp$div_id <- as.numeric(temp$div_id)
  #   temp$division <- na.locf(temp$division)
  #   temp$div_id <- na.locf(temp$div_id)
  #   temp <- temp[temp$div_id %in% international.competitions,]
  #   temp$goals1 <- as.numeric(gsub("^.*?<em>(\\d+)</em>.*?<em>(\\d+)</em>.*?$","\\1",temp$goals1))
  #   temp$goals2 <- as.numeric(gsub("^.*?<em>(\\d+)</em>.*?<em>(\\d+)</em>.*?$","\\2",temp$goals2))
  #   int.res.update <- rbind(int.res.update,temp)
  #   temp <- NULL
  # }
  # int.res.update$date <- as.Date(int.res.update$date)
  # int.res.update$team1 <- tolower(int.res.update$team1)
  # int.res.update$team2 <- tolower(int.res.update$team2)
  # 
  # ##### remove duplicate entries #####
  # int.res.update <- int.res.update[duplicated(int.res.update)==FALSE,]
  # 
  # ##### get rid of any matches that haven't occurred before today (i.e. have NAs in the score) #####
  # int.res.update <- int.res.update[!(is.na(int.res.update$goals1)==TRUE & int.res.update$date<=Sys.Date()),]
  # 
  # #https://www.kaggle.com/datasets/martj42/international-football-results-from-1872-to-2017
  # ###merge datasets together####
  # int.res.update$city <- NA
  # int.res.update$country <- NA
  # int.res.update$neutral <- NA
  # 
  # int.res <- rbind(int.res,int.res.update[int.res.update$date>="2022-09-30",colnames(int.res)])
  # int.res <- int.res[duplicated(int.res)==FALSE,]
  # int.res <- int.res[duplicated(int.res[,c("date","team1","goals1","goals2","team2")])==FALSE,]
  # 
  # int.res$match_id[is.na(int.res$match_id)==TRUE] <- paste0("tgc",1:NROW(int.res$match_id[is.na(int.res$match_id)==TRUE]))
  
  ##give teams without ID an ID
  neededIDs <- c(int.res$team1[is.na(int.res$team1_id==TRUE)],int.res$team2[is.na(int.res$team2_id==TRUE)])
  neededIDs <- neededIDs[duplicated(neededIDs)==FALSE]
  neededIDs <- neededIDs[is.na(neededIDs)==FALSE]
  idcount <- 10000
  for(tt in neededIDs) {
    int.res$team1_id[int.res$team1==tt] <- idcount
    int.res$team2_id[int.res$team2==tt] <- idcount
    idcount <- idcount+1
  }
  
  ###merge datasets together (old)####
  # int.res.update$elostrength10.1 <- NA
  # int.res.update$elostrength10.2 <- NA
  # int.res.update$elo10predict <- NA
  # int.res.update$outcome <- NA
  # int.res <- rbind(full.data.int[,c("match_id","date","team1","team1_id","goals1","goals2","team2","team2_id",
  #                               "elostrength10.1","elostrength10.2","elo10predict","outcome","div_id","division")],
  #                 int.res.update[,c("match_id","date","team1","team1_id","goals1","goals2","team2","team2_id",
  #                               "elostrength10.1","elostrength10.2","elo10predict","outcome","div_id","division")])
  # int.res <- int.res[duplicated(int.res[,c("match_id","date","team1","goals1","goals2","team2")])==FALSE,]
  # 
  
  ###do Elo --- first to determine size of home advantage, second to properly calculate elo ####
  int.res <- int.res[is.na(int.res$date)==FALSE,]
  int.res <- int.res[is.na(int.res$match_id)==FALSE,]
  int.res <- int.res[order(int.res$date),]
  int.res$outcome <- 0.5*as.numeric(int.res$goals1==int.res$goals2) + as.numeric(int.res$goals1>int.res$goals2)
  # elo.date.start <- min(c(1:NROW(int.res))[int.res$date=="2022-06-01"])
  elorank.int <- list()
  # elorank.int.h50 <- list()
  # elorank.int.h60 <- list()
  # elorank.int.h70 <- list()
  # elorank.int.h80 <- list()
  # elorank.int.h90 <- list()
  # elorank.int.h100 <- list()
  int.res$elo.weight <- 10 + 10*as.numeric(int.res$division!="International Friendly")

  int.res$adjustment <- NA
  # int.res$adjustment.h50  <- NA
  # int.res$adjustment.h60  <- NA
  # int.res$adjustment.h70  <- NA
  # int.res$adjustment.h80  <- NA
  # int.res$adjustment.h90  <- NA
  # int.res$adjustment.h100 <- NA
  
  int.res$elostrength1 <- NA
  int.res$elostrength2 <- NA
  int.res$elopredict <- NA
  # int.res$elostrength1h50 <- NA
  # int.res$elostrength2h50 <- NA
  # int.res$elopredicth50 <- NA
  # int.res$elostrength1h60 <- NA
  # int.res$elostrength2h60 <- NA
  # int.res$elopredicth60 <- NA
  # int.res$elostrength1h70 <- NA
  # int.res$elostrength2h70 <- NA
  # int.res$elopredicth70 <- NA
  # int.res$elostrength1h80 <- NA
  # int.res$elostrength2h80 <- NA
  # int.res$elopredicth80 <- NA
  # int.res$elostrength1h90 <- NA
  # int.res$elostrength2h90 <- NA
  # int.res$elopredicth90 <- NA
  # int.res$elostrength1h100 <- NA
  # int.res$elostrength2h100 <- NA
  # int.res$elopredicth100 <- NA
  for(mm in c(1:NROW(int.res))) {
    print(100*mm/NROW(int.res))
    if(!(paste0("id",int.res$team1_id[mm]) %in% names(elorank.int))) {
      elorank.int[[paste0("id",int.res$team1_id[mm])]] <- 1000
      # elorank.int.h50[[paste0("id",int.res$team1_id[mm])]] <- 1000
      # elorank.int.h60[[paste0("id",int.res$team1_id[mm])]] <- 1000
      # elorank.int.h70[[paste0("id",int.res$team1_id[mm])]] <- 1000
      # elorank.int.h80[[paste0("id",int.res$team1_id[mm])]] <- 1000
      # elorank.int.h90[[paste0("id",int.res$team1_id[mm])]] <- 1000
      # elorank.int.h100[[paste0("id",int.res$team1_id[mm])]] <- 1000
    }
    if(!(paste0("id",int.res$team2_id[mm]) %in% names(elorank.int))) {
      elorank.int[[paste0("id",int.res$team2_id[mm])]] <- 1000
      # elorank.int.h50[[paste0("id",int.res$team2_id[mm])]] <- 1000
      # elorank.int.h60[[paste0("id",int.res$team2_id[mm])]] <- 1000
      # elorank.int.h70[[paste0("id",int.res$team2_id[mm])]] <- 1000
      # elorank.int.h80[[paste0("id",int.res$team2_id[mm])]] <- 1000
      # elorank.int.h90[[paste0("id",int.res$team2_id[mm])]] <- 1000
      # elorank.int.h100[[paste0("id",int.res$team2_id[mm])]] <- 1000
    }
    int.res$elostrength1[mm] <- elorank.int[[paste0("id",int.res$team1_id[mm])]]
    int.res$elostrength2[mm] <- elorank.int[[paste0("id",int.res$team2_id[mm])]]
    int.res$elopredict[mm] <- 1/(1+(10^((elorank.int[[paste0("id",int.res$team2_id[mm])]]-elorank.int[[paste0("id",int.res$team1_id[mm])]])/400)))
    int.res$adjustment[mm] <- int.res$outcome[mm] - int.res$elopredict[mm]
    if(is.na(int.res$adjustment[mm])==FALSE) {
      elorank.int[paste0("id",int.res$team1_id[mm])] <- elorank.int[[paste0("id",int.res$team1_id[mm])]] + int.res$elo.weight[mm]*int.res$adjustment[mm]
      elorank.int[paste0("id",int.res$team2_id[mm])] <- elorank.int[[paste0("id",int.res$team2_id[mm])]] - int.res$elo.weight[mm]*int.res$adjustment[mm]
    }
    # int.res$elostrength1h50[mm] <- elorank.int.h50[[paste0("id",int.res$team1_id[mm])]]
    # int.res$elostrength2h50[mm] <- elorank.int.h50[[paste0("id",int.res$team2_id[mm])]]
    # int.res$elopredicth50[mm] <- 1/(1+(10^((elorank.int.h50[[paste0("id",int.res$team2_id[mm])]]-elorank.int.h50[[paste0("id",int.res$team1_id[mm])]]-50*(1-int.res$neutral[mm]))/400)))
    # int.res$adjustment.h50[mm] <- int.res$outcome[mm] - int.res$elopredicth50[mm]
    # if(is.na(int.res$adjustment.h50[mm])==FALSE) {
    #   elorank.int.h50[paste0("id",int.res$team1_id[mm])] <- elorank.int.h50[[paste0("id",int.res$team1_id[mm])]] + int.res$elo.weight[mm]*int.res$adjustment.h50[mm]
    #   elorank.int.h50[paste0("id",int.res$team2_id[mm])] <- elorank.int.h50[[paste0("id",int.res$team2_id[mm])]] - int.res$elo.weight[mm]*int.res$adjustment.h50[mm]
    # }
    # int.res$elostrength1h60[mm] <- elorank.int.h60[[paste0("id",int.res$team1_id[mm])]]
    # int.res$elostrength2h60[mm] <- elorank.int.h60[[paste0("id",int.res$team2_id[mm])]]
    # int.res$elopredicth60[mm] <- 1/(1+(10^((elorank.int.h60[[paste0("id",int.res$team2_id[mm])]]-elorank.int.h60[[paste0("id",int.res$team1_id[mm])]]-60*(1-int.res$neutral[mm]))/400)))
    # int.res$adjustment.h60[mm] <- int.res$outcome[mm] - int.res$elopredicth60[mm]
    # if(is.na(int.res$adjustment.h60[mm])==FALSE) {
    #   elorank.int.h60[paste0("id",int.res$team1_id[mm])] <- elorank.int.h60[[paste0("id",int.res$team1_id[mm])]] + int.res$elo.weight[mm]*int.res$adjustment.h60[mm]
    #   elorank.int.h60[paste0("id",int.res$team2_id[mm])] <- elorank.int.h60[[paste0("id",int.res$team2_id[mm])]] - int.res$elo.weight[mm]*int.res$adjustment.h60[mm]
    # }
    # int.res$elostrength1h70[mm] <- elorank.int.h70[[paste0("id",int.res$team1_id[mm])]]
    # int.res$elostrength2h70[mm] <- elorank.int.h70[[paste0("id",int.res$team2_id[mm])]]
    # int.res$elopredicth70[mm] <- 1/(1+(10^((elorank.int.h70[[paste0("id",int.res$team2_id[mm])]]-elorank.int.h70[[paste0("id",int.res$team1_id[mm])]]-70*(1-int.res$neutral[mm]))/400)))
    # int.res$adjustment.h70[mm] <- int.res$outcome[mm] - int.res$elopredicth70[mm]
    # if(is.na(int.res$adjustment.h70[mm])==FALSE) {
    #   elorank.int.h70[paste0("id",int.res$team1_id[mm])] <- elorank.int.h70[[paste0("id",int.res$team1_id[mm])]] + int.res$elo.weight[mm]*int.res$adjustment.h70[mm]
    #   elorank.int.h70[paste0("id",int.res$team2_id[mm])] <- elorank.int.h70[[paste0("id",int.res$team2_id[mm])]] - int.res$elo.weight[mm]*int.res$adjustment.h70[mm]
    # }
    # int.res$elostrength1h80[mm] <- elorank.int.h80[[paste0("id",int.res$team1_id[mm])]]
    # int.res$elostrength2h80[mm] <- elorank.int.h80[[paste0("id",int.res$team2_id[mm])]]
    # int.res$elopredicth80[mm] <- 1/(1+(10^((elorank.int.h80[[paste0("id",int.res$team2_id[mm])]]-elorank.int.h80[[paste0("id",int.res$team1_id[mm])]]-80*(1-int.res$neutral[mm]))/400)))
    # int.res$adjustment.h80[mm] <- int.res$outcome[mm] - int.res$elopredicth80[mm]
    # if(is.na(int.res$adjustment.h80[mm])==FALSE) {
    #   elorank.int.h80[paste0("id",int.res$team1_id[mm])] <- elorank.int.h80[[paste0("id",int.res$team1_id[mm])]] + int.res$elo.weight[mm]*int.res$adjustment.h80[mm]
    #   elorank.int.h80[paste0("id",int.res$team2_id[mm])] <- elorank.int.h80[[paste0("id",int.res$team2_id[mm])]] - int.res$elo.weight[mm]*int.res$adjustment.h80[mm]
    # }
    # int.res$elostrength1h90[mm] <- elorank.int.h90[[paste0("id",int.res$team1_id[mm])]]
    # int.res$elostrength2h90[mm] <- elorank.int.h90[[paste0("id",int.res$team2_id[mm])]]
    # int.res$elopredicth90[mm] <- 1/(1+(10^((elorank.int.h90[[paste0("id",int.res$team2_id[mm])]]-elorank.int.h90[[paste0("id",int.res$team1_id[mm])]]-90*(1-int.res$neutral[mm]))/400)))
    # int.res$adjustment.h90[mm] <- int.res$outcome[mm] - int.res$elopredicth90[mm]
    # if(is.na(int.res$adjustment.h90[mm])==FALSE) {
    #   elorank.int.h90[paste0("id",int.res$team1_id[mm])] <- elorank.int.h90[[paste0("id",int.res$team1_id[mm])]] + int.res$elo.weight[mm]*int.res$adjustment.h90[mm]
    #   elorank.int.h90[paste0("id",int.res$team2_id[mm])] <- elorank.int.h90[[paste0("id",int.res$team2_id[mm])]] - int.res$elo.weight[mm]*int.res$adjustment.h90[mm]
    # }
    # int.res$elostrength1h100[mm] <- elorank.int.h100[[paste0("id",int.res$team1_id[mm])]]
    # int.res$elostrength2h100[mm] <- elorank.int.h100[[paste0("id",int.res$team2_id[mm])]]
    # int.res$elopredicth100[mm] <- 1/(1+(10^((elorank.int.h100[[paste0("id",int.res$team2_id[mm])]]-elorank.int.h100[[paste0("id",int.res$team1_id[mm])]]-100*(1-int.res$neutral[mm]))/400)))
    # int.res$adjustment.h100[mm] <- int.res$outcome[mm] - int.res$elopredicth100[mm]
    # if(is.na(int.res$adjustment.h100[mm])==FALSE) {
    #   elorank.int.h100[paste0("id",int.res$team1_id[mm])] <- elorank.int.h100[[paste0("id",int.res$team1_id[mm])]] + int.res$elo.weight[mm]*int.res$adjustment.h100[mm]
    #   elorank.int.h100[paste0("id",int.res$team2_id[mm])] <- elorank.int.h100[[paste0("id",int.res$team2_id[mm])]] - int.res$elo.weight[mm]*int.res$adjustment.h100[mm]
    # }
  }

  int.res$total_score <- int.res$goals1+int.res$goals2
  int.res$goal_diff <- abs(int.res$goals1-int.res$goals2)
  int.res$balance <- int.res$elopredict*(1-int.res$elopredict)
  int.res$balance.5 <- abs(0.5-int.res$elopredict)
  
  aggregate(int.res[is.na(int.res$tournament)==FALSE & int.res$tournament=="UEFA Euro",
                    c("total_score","goal_diff","balance","balance.5")],by=list(format(int.res$date[is.na(int.res$tournament)==FALSE & int.res$tournament=="UEFA Euro"],"%Y")),
            FUN=mean,na.rm=TRUE)
  
  ##what should home advantage parameter be?
  #regression approach
  int.res$outcomeH <- as.numeric(int.res$outcome==1)
  int.res$decade <- as.character(10*floor(as.numeric(format(int.res$date,"%Y"))/10))
  summary(had.reg0 <- fixest::feols(outcomeH ~ as.numeric(neutral):decade,data=int.res))
  predict(had.reg0, newdata = int.res[1,c("neutral","decade","team1_id","team2_id")])
  summary(had.reg1 <- lfe::felm(outcomeH ~ elopredict + as.numeric(neutral):decade,data=int.res))
  
  #now look at forecast errors
  int.res$elo.error <- int.res$outcomeH - int.res$elopredict
  # int.res$elo.error.h50 <- int.res$outcomeH - int.res$elopredicth50
  # int.res$elo.error.h60 <- int.res$outcomeH - int.res$elopredicth60
  # int.res$elo.error.h70 <- int.res$outcomeH - int.res$elopredicth70
  # int.res$elo.error.h80 <- int.res$outcomeH - int.res$elopredicth80
  # int.res$elo.error.h90 <- int.res$outcomeH - int.res$elopredicth90
  # int.res$elo.error.h100 <- int.res$outcomeH - int.res$elopredicth100
  
  summary(err.reg0 <- lm(outcomeH ~ elopredict,data=int.res))
  # summary(err.reg0.h50 <- lm(outcomeH ~ elopredicth50,data=int.res))
  # summary(err.reg0.h60 <- lm(outcomeH ~ elopredicth60,data=int.res))
  # summary(err.reg0.h70 <- lm(outcomeH ~ elopredicth70,data=int.res))
  # summary(err.reg0.h80 <- lm(outcomeH ~ elopredicth80,data=int.res))
  # summary(err.reg0.h90 <- lm(outcomeH ~ elopredicth90,data=int.res))
  # summary(err.reg0.h100 <- lm(outcomeH ~ elopredicth100,data=int.res))
  # stargazer::stargazer(err.reg0,err.reg0.h50,err.reg0.h60,err.reg0.h70,err.reg0.h80,err.reg0.h90,err.reg0.h100)
  
  plot(aggregate(int.res$outcomeH,by=list(round(int.res$elopredict,2)),FUN=mean,na.rm=TRUE),cex=0.5,pch=16)
  # lines(aggregate(int.res$outcomeH,by=list(round(int.res$elopredicth50,2)),FUN=mean,na.rm=TRUE),type="p",cex=0.5,pch=16,col=2)
  # lines(aggregate(int.res$outcomeH,by=list(round(int.res$elopredicth60,2)),FUN=mean,na.rm=TRUE),type="p",cex=0.5,pch=16,col=3)
  # lines(aggregate(int.res$outcomeH,by=list(round(int.res$elopredicth70,2)),FUN=mean,na.rm=TRUE),type="p",cex=0.5,pch=16,col=4)
  # lines(aggregate(int.res$outcomeH,by=list(round(int.res$elopredicth80,2)),FUN=mean,na.rm=TRUE),type="p",cex=0.5,pch=16,col=5)
  # lines(aggregate(int.res$outcomeH,by=list(round(int.res$elopredicth90,2)),FUN=mean,na.rm=TRUE),type="p",cex=0.5,pch=16,col=6)
  # lines(aggregate(int.res$outcomeH,by=list(round(int.res$elopredicth100,2)),FUN=mean,na.rm=TRUE),type="p",cex=0.5,pch=16,col=7)
  abline(0,1,lwd=2)
  
  int.res$country2 <- int.res$country
  int.res$country2[int.res$country2=="England"] <- "United Kingdom"
  int.res$country2[int.res$country2=="Scotland"] <- "United Kingdom"
  int.res$country2[int.res$country2=="Wales"] <- "United Kingdom"
  int.res$country2[int.res$country2=="Northern Ireland"] <- "United Kingdom"
  int.res$city.country <- paste0(int.res$city,"-",int.res$country2)
  city.locations <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/worldcities.csv")
  city.locations$city.country <- paste0(city.locations$city,"-",city.locations$country)
  int.res <- merge(int.res,city.locations,by=c("city.country"),all.x=TRUE,suffixes=c("",".cy"))
  
  #  country.locations <- aggregate(city.locations[,c("lat","lng")],by=list(city.locations$country),FUN=mean)
  country.locations <- rbind(city.locations[city.locations$capital=="primary",c("country","city","lat","lng")],
                             city.locations[city.locations$city=="Caerdydd",c("country","city","lat","lng")],
                             city.locations[city.locations$country=="United Kingdom" & city.locations$city=="Belfast",c("country","city","lat","lng")],
                             city.locations[city.locations$country=="United Kingdom" & city.locations$city=="Glasgow",c("country","city","lat","lng")])
  country.locations$country[country.locations$city=="Caerdydd"] <- "Wales"
  country.locations$country[country.locations$city=="Belfast"] <- "Northern Ireland"
  country.locations$country[country.locations$city=="Glasgow"] <- "Scotland"
  country.locations$country[country.locations$country=="United States"] <- "USA"
  country.locations$country[country.locations$country=="United Kingdom"] <- "England"
  country.locations$country[country.locations$country=="United Arab Emirates"] <- "UAE"
  country.locations$country <- tolower(country.locations$country)
  country.locations <- aggregate(country.locations[,c("lat","lng")],by=list(country.locations$country),FUN=mean)
  
  int.res <- merge(int.res,country.locations,by.x=c("team1"),by.y=c("Group.1"),all.x=TRUE,suffixes=c("",".h"))
  int.res <- merge(int.res,country.locations,by.x=c("team2"),by.y=c("Group.1"),all.x=TRUE,suffixes=c("",".a"))
  
  int.res$home_adv1 <- as.numeric(int.res$team1==tolower(int.res$country))
  int.res$home_adv2 <- as.numeric(int.res$team2==tolower(int.res$country))
  
  ##location data#####
  radius = 3958.8 # Radius of the Earth in miles
  int.res$rlat1 = int.res$lat.h * (pi/180); # Convert degrees to radians
  int.res$rlat2 = int.res$lat * (pi/180); # Convert degrees to radians
  int.res$difflat = int.res$rlat2-int.res$rlat1; # Radian difference (latitudes)
  int.res$difflon = (int.res$lng.h-int.res$lng) * (pi/180) # Radian difference (longitudes)
  
  int.res$distance1 = 2 * radius * asin(sqrt(sin(int.res$difflat/2)*sin(int.res$difflat/2)+cos(int.res$rlat1)*cos(int.res$rlat2)*sin(int.res$difflon/2)*sin(int.res$difflon/2)))
  int.res$distance100.1 <- int.res$distance1/100
  
  int.res$rlat1 = int.res$lat.a * (pi/180); # Convert degrees to radians
  int.res$rlat2 = int.res$lat * (pi/180); # Convert degrees to radians
  int.res$difflat = int.res$rlat2-int.res$rlat1; # Radian difference (latitudes)
  int.res$difflon = (int.res$lng.a-int.res$lng) * (pi/180) # Radian difference (longitudes)
  
  int.res$distance2 = 2 * radius * asin(sqrt(sin(int.res$difflat/2)*sin(int.res$difflat/2)+cos(int.res$rlat1)*cos(int.res$rlat2)*sin(int.res$difflon/2)*sin(int.res$difflon/2)))
  int.res$distance100.2 <- int.res$distance2/100
  
  #  int.res$distance1[int.res$home_adv1==1] <- 0
  #  int.res$distance2[int.res$home_adv2==1] <- 0
  
  int.res$outcome <- -1*as.numeric(int.res$goals1<int.res$goals2) + as.numeric(int.res$goals1>int.res$goals2)
  int.res$gdiff <- int.res$goals1-int.res$goals2
  
  int.res$team1_y <- paste0(int.res$team1,"-",format(int.res$date,"%Y"))
  int.res$team2_y <- paste0(int.res$team2,"-",format(int.res$date,"%Y"))
  
  int.res$outcome1 <- as.numeric(int.res$outcome==1)
  int.res$outcomex <- as.numeric(int.res$outcome==0)
  int.res$outcome2 <- as.numeric(int.res$outcome==-1)
  
  write.csv(int.res,paste("/Users/jjreade/Dropbox/Research/Sport/Correct-score-data/all-international-results-",Sys.Date(),".csv"))
  
  int.res$year <- format(int.res$date,"%Y")
  g1.y <- aggregate(int.res$goals1[int.res$home_adv1==0],by=list(int.res$year[int.res$home_adv1==0]),FUN=mean,na.rm=TRUE)
  g2.y <- aggregate(int.res$goals2[int.res$home_adv1==0],by=list(int.res$year[int.res$home_adv1==0]),FUN=mean,na.rm=TRUE)
  g1.h.y <- aggregate(int.res$goals1[int.res$home_adv1==1],by=list(int.res$year[int.res$home_adv1==1]),FUN=mean,na.rm=TRUE)
  g2.h.y <- aggregate(int.res$goals2[int.res$home_adv1==1],by=list(int.res$year[int.res$home_adv1==1]),FUN=mean,na.rm=TRUE)

  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/worldcup-sim-data-N-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(table(int.res$year),type="h",main="Number of international matches each year",
       ylab="Number of matches",xlab="Year (calendar)")
  lines(table(int.res$year[int.res$home_adv1==0]),type="h",col=2)
  abline(h=c(0,200,400,600,800,1000,1200),lty=3)
  legend("topleft",col=1:2,lty=1,legend=c("Home Territory","Neutral Territory"),bty="n",ncol=2)
  dev.off()
  
  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/worldcup-sim-data-goals1-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(g1.y$Group.1,g1.y$x,type="o",main="Goals for each team over time",
       ylab="Goals (mean per year)",xlab="Year (calendar)",pch=16,cex=0.75,
       ylim=range(c(g1.y$x,g1.y$x)))
  lines(g1.h.y$Group.1,g1.h.y$x,type="o",pch=1,cex=0.75,lty=3)
  lines(g2.y$Group.1,g2.y$x,type="o",col=2,pch=16,cex=0.75)
  lines(g2.h.y$Group.1,g2.h.y$x,type="o",col=2,pch=1,cex=0.75,lty=3)
  legend("topright",col=c(1,1,2,2),pch=c(16,1,16,1),lty=c(1,3,1,3),bty="n",
         legend=c("Team1 (neutral)","Team1 (at home)","Team2 (neutral)","Team2 (at home)"),ncol=2)
  abline(0,0,lty=3)
  dev.off()
  
  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/worldcup-sim-data-goals2-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(g1.y$Group.1,g1.y$x-g2.y$x,type="o",main="Goal difference for each match",
       ylab="Goal difference (mean per year)",xlab="Year (calendar)",pch=16,cex=0.75,
       ylim=range(-3,3))
  lines(g1.h.y$Group.1,g1.h.y$x-g2.h.y$x,type="o",col=1,pch=1,cex=0.75,lty=3)
  abline(h=mean(g1.h.y$x-g2.h.y$x),lty=2)
  abline(h=mean(g1.y$x-g2.y$x),lty=2)
  abline(0,0,lty=3)
  legend("topright",col=c(1,1),pch=c(16,1),lty=c(1,3),bty="n",
         legend=c("Neutral","Team 1 at home"),ncol=2)
  dev.off()
  
  stargazer::stargazer(int.res[,c("outcome1","outcomex","outcome2","elopredict","distance1","distance2")])
  
  #pre world cup estimation
  summary(m1d.sim0 <- glm(goals1 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20",], na.action=na.exclude))
  summary(m2d.sim0 <- glm(goals2 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20",], na.action=na.exclude))
  summary(m1d.sim01 <- glm(goals1 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="1930-01-01",], na.action=na.exclude))
  summary(m2d.sim01 <- glm(goals2 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="1930-01-01",], na.action=na.exclude))
  summary(m1d.sim02 <- glm(goals1 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="1950-01-01",], na.action=na.exclude))
  summary(m2d.sim02 <- glm(goals2 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="1950-01-01",], na.action=na.exclude))
  summary(m1d.sim03 <- glm(goals1 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="1970-01-01",], na.action=na.exclude))
  summary(m2d.sim03 <- glm(goals2 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="1970-01-01",], na.action=na.exclude))
  summary(m1d.sim04 <- glm(goals1 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="1990-01-01",], na.action=na.exclude))
  summary(m2d.sim04 <- glm(goals2 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="1990-01-01",], na.action=na.exclude))
  summary(m1d.sim05 <- glm(goals1 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="2010-01-01",], na.action=na.exclude))
  summary(m2d.sim05 <- glm(goals2 ~ elostrength1 + elostrength2, family="poisson", data=int.res[int.res$date<"2022-11-20" & int.res$date>="2010-01-01",], na.action=na.exclude))
  
  summary(m1d.sim1 <- glm(goals1 ~ elostrength1 + elostrength2 + home_adv1 + distance100.1 + distance100.2, family="poisson", data=int.res[int.res$date<"2022-11-20",], na.action=na.exclude))
  summary(m2d.sim1 <- glm(goals2 ~ elostrength1 + elostrength2 + home_adv1 + distance100.1 + distance100.2, family="poisson", data=int.res[int.res$date<"2022-11-20",], na.action=na.exclude))
  
  stargazer(m1d.sim0,m1d.sim1,m2d.sim0,m2d.sim1)
  
  stargazer(m1d.sim0,m1d.sim01,m1d.sim02,m1d.sim03,m1d.sim04,m1d.sim05)
  stargazer(m2d.sim0,m2d.sim01,m2d.sim02,m2d.sim03,m2d.sim04,m2d.sim05)
  
  summary(g1.fe.reg <- felm(goals1 ~ elostrength1 + elostrength2 + home_adv1 + distance100.1 + distance100.2 | team1_y + team2_y | 0 | team1_y + team2_y, data=int.res))
  summary(g2.fe.reg <- felm(goals2 ~ elostrength1 + elostrength2 + home_adv1 + distance100.1 + distance100.2 | team1_y + team2_y | 0 | team1_y + team2_y, data=int.res))
  stargazer(m1d.sim0,m1d.sim1,g1.fe.reg,m2d.sim0,m2d.sim1,g2.fe.reg)
  
  summary(int.fe.reg0 <- felm(outcome ~ distance100.1 + distance100.2, data=int.res))
  summary(int.fe.reg <- felm(outcome ~ distance100.1 + distance100.2 | team1_y + team2_y | 0 | team1_y + team2_y, data=int.res))
  
  summary(int.fe.reg.h0 <- felm(outcome ~ home_adv1*distance100.1 + distance100.2, data=int.res))
  summary(int.fe.reg.h <- felm(outcome ~ home_adv1*distance100.1 + distance100.2 | team1_y + team2_y | 0 | team1_y + team2_y, data=int.res))
  stargazer::stargazer(int.fe.reg0,int.fe.reg,int.fe.reg.h0,int.fe.reg.h)
  
  summary(int.fe.elo.reg0 <- felm(outcome ~ distance100.1 + distance100.2 + elopredict, data=int.res))
  summary(int.fe.elo.reg <- felm(outcome ~ distance100.1 + distance100.2 + elopredict | team1_y + team2_y | 0 | team1_y + team2_y, data=int.res))
  
  summary(int.fe.elo.reg.h <- felm(outcome ~ home_adv1*distance100.1 + distance100.2 | team1_y + team2_y | 0 | team1_y + team2_y, data=int.res))

  stargazer::stargazer(int.fe.elo.reg0,int.fe.elo.reg,int.fe.elo.reg.h)

  int.res$outcome2 <- (int.res$outcome+1)/2
  summary(int.fe.reg0n <- felm(outcome2 ~ distance100.1 + distance100.2, data=int.res[int.res$home_adv1==0 & int.res$home_adv2==0,]))
  summary(int.fe.regn <- felm(outcome2 ~ distance100.1 + distance100.2 | team1_y + team2_y | 0 | team1_y + team2_y, data=int.res[int.res$home_adv1==0 & int.res$home_adv2==0,]))
  summary(int.fe.elo.reg0n <- felm(outcome2 ~ distance100.1 + distance100.2 + elopredict, data=int.res[int.res$home_adv1==0 & int.res$home_adv2==0,]))
  summary(int.fe.elo.regn <- felm(outcome2 ~ distance100.1 + distance100.2 + elopredict | team1_y + team2_y | 0 | team1_y + team2_y, data=int.res[int.res$home_adv1==0 & int.res$home_adv2==0,]))
  
    
  jpeg(paste0("/Users/jjreade/Dropbox/Research/james-and-carl/EC325/2023/Slides-James/world-of-football-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  par(mar=c(1,1,1,1))
  plot(city.locations$lng,city.locations$lat,pch=16,cex=0.5,yaxt="n",xaxt="n",
       main="Global Football",ylab="",xlab="",
       sub="Red dots where international football has happened")
  lines(int.res$lng,int.res$lat,type="p",col=2,pch=16,cex=0.25)
  dev.off()
  
  jpeg(paste0("/Users/jjreade/Dropbox/Research/james-and-carl/EC325/2023/Slides-James/football-distances-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(int.res$distance1,int.res$distance2,pch=16,cex=0.5,
       main="Distance Travelled by International Teams",ylab="Distance by team listed second",
       xlab="Distance by team listed first")
  dev.off()
  
  jpeg(paste0("/Users/jjreade/Dropbox/Research/james-and-carl/EC325/2023/Slides-James/elo-eng-sco-ger-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(int.res$date[int.res$team1=="england"],int.res$elostrength1[int.res$team1=="england"],
       pch=16,cex=0.5,main="Elo ratings through football history",
       ylab="Elo rating",xlab="Date")
  lines(int.res$date[int.res$team2=="england"],int.res$elostrength2[int.res$team2=="england"],pch=16,cex=0.5,
        type="p")
  lines(int.res$date[int.res$team1=="scotland"],int.res$elostrength1[int.res$team1=="scotland"],
        pch=16,cex=0.5,col="blue",type="p")
  lines(int.res$date[int.res$team2=="scotland"],int.res$elostrength2[int.res$team2=="scotland"],
        pch=16,cex=0.5,col="blue",type="p")
  lines(int.res$date[int.res$team1=="germany"],int.res$elostrength1[int.res$team1=="germany"],
        pch=16,cex=0.5,col="orange",type="p")
  lines(int.res$date[int.res$team2=="germany"],int.res$elostrength2[int.res$team2=="germany"],
        pch=16,cex=0.5,col="orange",type="p")
  legend("topleft",col=c("black","blue","orange"),pch=16,legend=c("Eng","Sco","Ger"),bty="n")
  dev.off()
  
  int.res$tgoals <- int.res$goals1 + int.res$goals2
  int.res$gdiff <- abs(int.res$goals1 - int.res$goals2)
  
  ##characterise the history of the euros
  aggregate(int.res[int.res$div_id==68,c("tgoals","gdiff")],by=list(format(int.res$date[int.res$div_id==68],"%Y")),FUN=mean,na.rm=TRUE)
  
  ##create dataset with just tournament matches in
  simdata00 <- int.res[int.res$div_id==68 & int.res$date>="2024-06-14",
                       c("match_id","date","team1","team1_id","goals1","goals2","team2","team2_id",
                         "elostrength1","elostrength2","elopredict","outcome","div_id","division","neutral",
                         "total_score","goal_diff","balance","balance.5")]
  # simdata00 <- simdata00[is.na(simdata00$team1)==FALSE,]
  simdata00 <- simdata00[regexpr("semi-finalist",simdata00$team1)==-1,]
  

  
  #update data
  

  simdata00 <- simdata00[order(simdata00$date),]
  
  ##need match IDs
  # simdata00[simdata00$date=="2024-06-29",c("team1","team2","match_id")] <- rbind(c("group a runner-up","group b runner-up","tgc896869"),#38 L16 2
  #                                                                     c("group a winner","group c runner-up","tgc896870"))#37 L16 1
  # simdata00[simdata00$date=="2024-06-30",c("team1","team2","match_id")] <- rbind(c("group b winner","best 3p a-d-e-f","tgc896871"),#39 L16 3
  #                                                                     c("group c winner","best 3p d-e-f","tgc896872"))#40 L16 4
  # simdata00[simdata00$date=="2024-07-01",c("team1","team2","match_id")] <- rbind(c("group f winner","best 3p a-b-c","tgc896873"),#41 L16 5
  #                                                                     c("group d runner-up","group e runner-up","tgc896874"))#42 L16 6
  # simdata00[simdata00$date=="2024-07-02",c("team1","team2","match_id")] <- rbind(c("group e winner","best 3p a-b-c-d","tgc896875"),#43 L16 7
  #                                                                     c("group d winner","group f runner-up","tgc896876"))#44 L16 8
  # simdata00[simdata00$date=="2024-07-05",c("team1","team2","match_id")] <- rbind(c("winner L16 3","winner L16 1","tgc896873"),#45 qf 1
  #                                                                                c("winner L16 5","winner L16 6","tgc896874"))#46 qf 2
  # simdata00[simdata00$date=="2024-07-06",c("team1","team2","match_id")] <- rbind(c("winner L16 7","winner L16 8","tgc896875"),#47 qf 3
  #                                                                                c("winner L16 4","winner L16 2","tgc896876"))#48 qf 4
  #simdata00[simdata00$date=="2024-07-09",c("team1","team2","match_id")] <- c("winner qf 1","winner qf 2","tgc896877")#49 sf 1
  #simdata00[simdata00$date=="2024-07-10",c("team1","team2","match_id")] <- c("winner qf 3","winner qf 4","tgc896878")#50 sf 2
  simdata00[simdata00$date=="2024-07-14",c("team1","team2","match_id")] <- c("winner sf 1","winner sf 2","tgc896879")#51
  #simdata00 <- simdata00[!(simdata00$team1=="germany" & simdata00$team2=="switzerland"),]
  germany.details <- simdata00[simdata00$team2=="germany",c("team2","team2_id","elostrength2")]
  germany.opp.details <- simdata00[simdata00$team2=="germany",c("team1","team1_id","elostrength1")]
  simdata00[simdata00$team2=="germany",c("team1","team1_id","elostrength1")] <- germany.details
  simdata00[simdata00$team2=="germany",c("team2","team2_id","elostrength2")] <- germany.opp.details
  simdata00$neutral[simdata00$team1=="germany"] <- FALSE
  simdata00$neutral[simdata00$team1!="germany"] <- TRUE
  simdata00 <- simdata00[duplicated(simdata00$match_id)==FALSE,]

  ##estimation model for goal strengths  
  summary(m1d.sim <- glm(goals1 ~ elostrength1 + elostrength2 + neutral, family="poisson", data=int.res, na.action=na.exclude))
  summary(m2d.sim <- glm(goals2 ~ elostrength1 + elostrength2 + neutral, family="poisson", data=int.res, na.action=na.exclude))
  
  stargazer::stargazer(m1d.sim,m2d.sim)
  
  #  summary(m1d <- glm(goals1 ~ goals1.1 + goals2.1.1 + elostrength1 + elostrength2, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  #  summary(m2d <- glm(goals2 ~ goals2.1 + goals1.2.1 + elostrength1 + elostrength2, family="poisson", data=data[data$date>=reg.start.date & data$date<date0,], na.action=na.exclude))
  
  
  simdata00$goals1d.hat <- NA
  simdata00$goals2d.hat <- NA
  simdata00$goals1d.hat <- predict(m1d.sim,type="response",newdata = simdata00[,c("elostrength1",'elostrength2','neutral')])
  simdata00$goals2d.hat <- predict(m2d.sim,type="response",newdata = simdata00[,c("elostrength1",'elostrength2','neutral')])
  
  all.wc.tabs1 <- data.frame(stringsAsFactors = FALSE)
  all.wc.results1 <- data.frame(stringsAsFactors = FALSE)
  
  ##put matches into groups
  simdata00$group <- NA
  simdata00$group[simdata00$team1 %in% groups$A] <- "A"
  simdata00$group[simdata00$team1 %in% groups$B] <- "B"
  simdata00$group[simdata00$team1 %in% groups$C] <- "C"
  simdata00$group[simdata00$team1 %in% groups$D] <- "D"
  simdata00$group[simdata00$team1 %in% groups$E] <- "E"
  simdata00$group[simdata00$team1 %in% groups$F] <- "F"
  # simdata00$group[simdata00$team1 %in% groups$G] <- "G"
  # simdata00$group[simdata00$team1 %in% groups$H] <- "H"
  simdata00$group[simdata00$match_id=="tgc896869"] <- "L16 1"
  simdata00$group[simdata00$match_id=="tgc896870"] <- "L16 2"
  simdata00$group[simdata00$match_id=="tgc912410"] <- "L16 4"
  simdata00$group[simdata00$match_id=="tgc912411"] <- "L16 3"
  simdata00$group[simdata00$match_id=="tgc896871"] <- "L16 6"
  simdata00$group[simdata00$match_id=="tgc912412"] <- "L16 5"
  simdata00$group[simdata00$match_id=="tgc912413"] <- "L16 8"
  simdata00$group[simdata00$match_id=="tgc896872"] <- "L16 7"
  simdata00$group[simdata00$match_id=="tgc896873"] <- "qf 1"
  simdata00$group[simdata00$match_id=="tgc896874"] <- "qf 2"
  simdata00$group[simdata00$match_id=="tgc896875"] <- "qf 3"
  simdata00$group[simdata00$match_id=="tgc896876"] <- "qf 4"
  simdata00$group[simdata00$match_id=="tgc896877"] <- "sf 1"
  simdata00$group[simdata00$match_id=="tgc896878"] <- "sf 2"
  simdata00$group[simdata00$match_id=="tgc896879"] <- "Final"
  
  thirdcombos <- data.frame("ABCD"=c("A","D","B","C"),"ABCE"=c("A","E","B","C"),
                            "ABCF"=c("A","F","B","C"),"ABDE"=c("D","E","A","B"),
                            "ABDF"=c("D","F","A","B"),"ABEF"=c("E","F","B","A"),
                            "ACDE"=c("E","D","C","A"),"ACDF"=c("F","D","C","A"),
                            "ACEF"=c("E","F","C","A"),"ADEF"=c("E","F","D","A"),
                            "BCDE"=c("E","D","B","C"),"BCDF"=c("F","D","C","B"),
                            "BCEF"=c("F","E","C","B"),"BDEF"=c("F","E","D","B"),
                            "CDEF"=c("F","E","D","C"),stringsAsFactors = FALSE)
  # QFs <- data.frame("match_id"=c("tgc849205","tgc849206","tgc849207","tgc849212"),
  #                   "date"=c("2022-12-09","2022-12-09","2022-12-10","2022-12-10"),
  #                   "team1"=paste0(c("L16 5 ","L16 1 ","L16 7 ","L16 3 "),"winner"),
  #                   "team1_id"=c("5772","5774","5776","5778"),
  #                   "goals1"=rep(NA,4),
  #                   "goals2"=rep(NA,4),
  #                   "team2"=paste0(c("L16 6 ","L16 2 ","L16 8 ","L16 4 "),"winner"),
  #                   "team2_id"=c("5773","5775","5777","5779"),
  #                   "elostrength1"=rep(1000,4),
  #                   "elostrength2"=rep(1000,4),
  #                   "elopredict"=rep(0.5,4),
  #                   "outcome"=rep(NA,4),
  #                   "div_id"=rep(73,4),
  #                   "division"=rep("World Cup",4),
  #                   "goals1d.hat"=rep(1,4),
  #                   "goals2d.hat"=rep(1,4),
  #                   "group"=paste0("qf ",1:4))
  # simdata00 <- rbind(simdata00,QFs)
  simdata00 <- simdata00[order(simdata00$date,simdata00$group),]
  
  #get rid of the QFs that have finally appeared on soccerbase's website
  # simdata00 <- simdata00[!(simdata00$match_id %in% c("tgc871577","tgc871595","tgc871578","tgc871596")),]
  
  team_ids <- data.frame("team1"=c(simdata00$team1[duplicated(simdata00$team1_id)==FALSE],
                                   simdata00$team2[duplicated(simdata00$team1_id)==FALSE]),
                         "team1_id"=c(simdata00$team1_id[duplicated(simdata00$team1_id)==FALSE],
                                      simdata00$team2_id[duplicated(simdata00$team1_id)==FALSE]),
                         "elostrength1"=c(simdata00$elostrength1[duplicated(simdata00$team1_id)==FALSE],
                                          simdata00$elostrength2[duplicated(simdata00$team1_id)==FALSE]),
                         stringsAsFactors = FALSE)
  team_ids <- team_ids[duplicated(team_ids$team1)==FALSE,]
  team_ids <- team_ids[regexpr("winner|runner",tolower(team_ids$team1))==-1,]
  team_ids <- team_ids[order(team_ids$elostrength1,decreasing = TRUE),]
  
  counter = 0
  start.date = as.Date("2024-06-19") #Sys.Date()
  RR=10000
  
  for(case in c("","-grprs","-kors","-both")) {
    print(case)
    if(case!="") {
      next
    }
    print("boo")
    basedir = paste0(dbloc,"/euros-sim",case,"/")
    fulldir1 <- paste0(basedir,"sim-",Sys.Date(),"/simno-",counter,"/")
    if (!file.exists(basedir)){
      dir.create(basedir)}
    if (!file.exists(paste0(basedir,"sim-",Sys.Date(),"/"))){
      dir.create(paste0(basedir,"sim-",Sys.Date(),"/"))}
    if (!file.exists(fulldir1)){
      dir.create(fulldir1)}
    
  
    if(case=="") {
      reshuffle=0
      knockout.shuffle=0
    } else if(case=="-grprs") {
      reshuffle=1
      knockout.shuffle=0
    } else if (case=="-kors") {
      reshuffle=0
      knockout.shuffle=1
    } else if (case=="-both") {
      reshuffle=1
      knockout.shuffle=1
    }

    match.dates <- sort(unique(simdata00$date))
    match.dates <- match.dates[match.dates>="2024-07-08"]
    
    for(rr in 1:RR) {
      print(rr)
      elo.sim <- elorank.int
      
      simdata <- simdata00
      
      
      ##reshuffle teams (can add alternative knock out plans later)####
      if(reshuffle==1) {
        simdata01 <- simdata
        team_ids$reshuffle <- sample(1:32)
        movedteams <- c()
        for(tt in 1:NROW(team_ids)) {
          original_team <- team_ids$team1[tt]
          original_team_id <- team_ids$team1_id[tt]
          original_team_elo <- team_ids$elostrength[tt]
          swap_team <- team_ids$team1[team_ids$reshuffle[tt]]
          swap_team_id <- team_ids$team1_id[team_ids$reshuffle[tt]]
          swap_team_elo <- team_ids$elostrength[team_ids$reshuffle[tt]]
          #print(paste0("original team = ",original_team,", swap team = ",swap_team))
          if(!(original_team %in% movedteams | swap_team %in% movedteams))
          {
            simdata01$team1_id[simdata01$team1==original_team] <- "old team id"
            simdata01$team2_id[simdata01$team2==original_team] <- "old team id"
            simdata01$team1[simdata01$team1==original_team] <- "old team"
            simdata01$team2[simdata01$team2==original_team] <- "old team"
            
            simdata01$team1_id[simdata01$team1==swap_team] <- "swap team id"
            simdata01$team2_id[simdata01$team2==swap_team] <- "swap team id"
            simdata01$team1[simdata01$team1==swap_team] <- "swap team"
            simdata01$team2[simdata01$team2==swap_team] <- "swap team"
            
            simdata01$team1_id[simdata01$team1=="old team"] <- swap_team_id
            simdata01$team2_id[simdata01$team2=="old team"] <- swap_team_id
            simdata01$elostrength1[simdata01$team1=="old team"] <- swap_team_elo
            simdata01$elostrength2[simdata01$team2=="old team"] <- swap_team_elo
            simdata01$team1[simdata01$team1=="old team"] <- swap_team
            simdata01$team2[simdata01$team2=="old team"] <- swap_team
            
            simdata01$team1_id[simdata01$team1=="swap team"] <- original_team_id
            simdata01$team2_id[simdata01$team2=="swap team"] <- original_team_id
            simdata01$elostrength1[simdata01$team1=="swap team"] <- original_team_elo
            simdata01$elostrength2[simdata01$team2=="swap team"] <- original_team_elo
            simdata01$team1[simdata01$team1=="swap team"] <- original_team
            simdata01$team2[simdata01$team2=="swap team"] <- original_team
            movedteams <- c(movedteams,original_team,swap_team)
          }
        }
        simdata <- simdata01
        simdata$elopredict <- 1/(1+(10^((simdata$elostrength2-simdata$elostrength1)/400)))
      }
      ##end reshuffle####
      #simdata$goals1d.hat <- simdata00$goals1d.hat/(simdata00$goals1d.hat+simdata01$goals1d.hat)
      #simdata$goals2d.hat <- simdata00$goals2d.hat/(simdata00$goals2d.hat+simdata01$goals2d.hat)
      simdata <- simdata[order(simdata$date),]
      
      
      #now loop through dates
      for(dd0 in match.dates) {
        dd = as.Date(dd0,origin="1970-01-01")
        # print(dd)
        if(dd=="2024-06-29") {##then group stages are over. Need to work out who progresses.
          grA <- league.tab(simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="A"],
                            as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="A"]),
                            simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="A"],
                            simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="A"],
                            simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="A"],
                            simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="A"],3,
                            points.deduct = NA,wc=TRUE)
          grB <- league.tab(simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="B"],
                            as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="B"]),
                            simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="B"],
                            simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="B"],
                            simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="B"],
                            simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="B"],3,
                            points.deduct = NA,wc=TRUE)
          grC <- league.tab(simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="C"],
                            as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="C"]),
                            simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="C"],
                            simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="C"],
                            simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="C"],
                            simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="C"],3,
                            points.deduct = NA,wc=TRUE)
          grD <- league.tab(simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="D"],
                            as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="D"]),
                            simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="D"],
                            simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="D"],
                            simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="D"],
                            simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="D"],3,
                            points.deduct = NA,wc=TRUE)
          grE <- league.tab(simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="E"],
                            as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="E"]),
                            simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="E"],
                            simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="E"],
                            simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="E"],
                            simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="E"],3,
                            points.deduct = NA,wc=TRUE)
          grF <- league.tab(simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="F"],
                            as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="F"]),
                            simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="F"],
                            simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="F"],
                            simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="F"],
                            simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="F"],3,
                            points.deduct = NA,wc=TRUE)
          #need ranking of third placed teams
          
          thirdplaced <- rbind(cbind(grA$final.table[3,],group="A"),cbind(grB$final.table[3,],group="B"),
                               cbind(grC$final.table[3,],group="C"),cbind(grD$final.table[3,],group="D"),
                               cbind(grE$final.table[3,],group="E"),cbind(grF$final.table[3,],group="F"))
          thirdplaced <- merge(thirdplaced,team_ids[,c("team1","team1_id")],by.x="team",by.y="team1",all.x=TRUE)
          thirdplaced <- thirdplaced[order(thirdplaced$points,thirdplaced$goal.diff,thirdplaced$goals.scored,decreasing = TRUE),]
          thirdplaced4 <- thirdcombos[,gsub(", ","",toString(thirdplaced[1:4,"group"][order(thirdplaced[1:4,"group"])]))]
          adef3 <- thirdplaced[thirdplaced$group==thirdplaced4[1],c("team","team1_id")] #vs 1B
          def3 <-  thirdplaced[thirdplaced$group==thirdplaced4[2],c("team","team1_id")] #vs 1C
          abc3 <-  thirdplaced[thirdplaced$group==thirdplaced4[4],c("team","team1_id")] #vs 1F
          abcd3 <- thirdplaced[thirdplaced$group==thirdplaced4[3],c("team","team1_id")] #vs 1E
          # grG <- league.tab(simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="G"],
          #                   as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="G"]),
          #                   simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="G"],
          #                   simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="G"],
          #                   simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="G"],
          #                   simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="G"],3,
          #                   points.deduct = NULL,wc=TRUE)
          # grH <- league.tab(simdata$match_id[is.na(simdata$group)==FALSE & simdata$group=="H"],
          #                   as.Date(simdata$date[is.na(simdata$group)==FALSE & simdata$group=="H"]),
          #                   simdata$team1[is.na(simdata$group)==FALSE & simdata$group=="H"],
          #                   simdata$goals1[is.na(simdata$group)==FALSE & simdata$group=="H"],
          #                   simdata$goals2[is.na(simdata$group)==FALSE & simdata$group=="H"],
          #                   simdata$team2[is.na(simdata$group)==FALSE & simdata$group=="H"],3,
          #                   points.deduct = NULL,wc=TRUE)
          if(knockout.shuffle==0) {
            simdata$elostrength1[simdata$team1=="group a winner"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grA$final.table$team[1]])]]
            simdata$elostrength1[simdata$team1=="group a runner-up"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grA$final.table$team[2]])]]
            simdata$team1_id[simdata$team1=="group a winner"] <- team_ids$team1_id[team_ids$team1==grA$final.table$team[1]]
            simdata$team1_id[simdata$team1=="group a runner-up"] <- team_ids$team1_id[team_ids$team1==grA$final.table$team[2]]
            simdata$team1[simdata$team1=="group a winner"] <- grA$final.table$team[1]
            simdata$team1[simdata$team1=="group a runner-up"] <- grA$final.table$team[2]
            
            simdata$elostrength1[simdata$team1=="group b winner"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grB$final.table$team[1]])]]
            simdata$elostrength2[simdata$team2=="group b runner-up"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grB$final.table$team[2]])]]
            simdata$team1_id[simdata$team1=="group b winner"] <- team_ids$team1_id[team_ids$team1==grB$final.table$team[1]]
            simdata$team2_id[simdata$team2=="group b runner-up"] <- team_ids$team1_id[team_ids$team1==grB$final.table$team[2]]
            simdata$team1[simdata$team1=="group b winner"] <- grB$final.table$team[1]
            simdata$team2[simdata$team2=="group b runner-up"] <- grB$final.table$team[2]
            
            simdata$elostrength1[simdata$team1=="group c winner"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grC$final.table$team[1]])]]
            simdata$elostrength2[simdata$team2=="group c runner-up"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grC$final.table$team[2]])]]
            simdata$team1_id[simdata$team1=="group c winner"] <- team_ids$team1_id[team_ids$team1==grC$final.table$team[1]]
            simdata$team2_id[simdata$team2=="group c runner-up"] <- team_ids$team1_id[team_ids$team1==grC$final.table$team[2]]
            simdata$team1[simdata$team1=="group c winner"] <- grC$final.table$team[1]
            simdata$team2[simdata$team2=="group c runner-up"] <- grC$final.table$team[2]
            
            simdata$elostrength1[simdata$team1=="group d winner"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grD$final.table$team[1]])]]
            simdata$elostrength1[simdata$team1=="group d runner-up"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grD$final.table$team[2]])]]
            simdata$team1_id[simdata$team1=="group d winner"] <- team_ids$team1_id[team_ids$team1==grD$final.table$team[1]]
            simdata$team1_id[simdata$team1=="group d runner-up"] <- team_ids$team1_id[team_ids$team1==grD$final.table$team[2]]
            simdata$team1[simdata$team1=="group d winner"] <- grD$final.table$team[1]
            simdata$team1[simdata$team1=="group d runner-up"] <- grD$final.table$team[2]
            
            simdata$elostrength1[simdata$team1=="group e winner"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grE$final.table$team[1]])]]
            simdata$elostrength2[simdata$team2=="group e runner-up"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grE$final.table$team[2]])]]
            simdata$team1_id[simdata$team1=="group e winner"] <- team_ids$team1_id[team_ids$team1==grE$final.table$team[1]]
            simdata$team2_id[simdata$team2=="group e runner-up"] <- team_ids$team1_id[team_ids$team1==grE$final.table$team[2]]
            simdata$team1[simdata$team1=="group e winner"] <- grE$final.table$team[1]
            simdata$team2[simdata$team2=="group e runner-up"] <- grE$final.table$team[2]
            
            simdata$elostrength1[simdata$team1=="group f winner"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grF$final.table$team[1]])]]
            simdata$elostrength2[simdata$team2=="group f runner-up"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grF$final.table$team[2]])]]
            simdata$team1_id[simdata$team1=="group f winner"] <- team_ids$team1_id[team_ids$team1==grF$final.table$team[1]]
            simdata$team2_id[simdata$team2=="group f runner-up"] <- team_ids$team1_id[team_ids$team1==grF$final.table$team[2]]
            simdata$team1[simdata$team1=="group f winner"] <- grF$final.table$team[1]
            simdata$team2[simdata$team2=="group f runner-up"] <- grF$final.table$team[2]
            
            simdata$elostrength2[simdata$team2=="best 3p a-d-e-f"] <- elo.sim[[paste0("id",adef3$team1_id)]]
            simdata$elostrength2[simdata$team2=="best 3p d-e-f"] <-   elo.sim[[paste0("id",def3$team1_id)]]
            simdata$elostrength2[simdata$team2=="best 3p a-b-c"] <-   elo.sim[[paste0("id",abc3$team1_id)]]
            simdata$elostrength2[simdata$team2=="best 3p a-b-c-d"] <- elo.sim[[paste0("id",abcd3$team1_id)]]
            simdata$team2_id[simdata$team2=="best 3p a-d-e-f"] <- adef3$team1_id
            simdata$team2_id[simdata$team2=="best 3p d-e-f"] <-   def3$team1_id
            simdata$team2_id[simdata$team2=="best 3p a-b-c"] <-   abc3$team1_id
            simdata$team2_id[simdata$team2=="best 3p a-b-c-d"] <- abcd3$team1_id
            simdata$team2[simdata$team2=="best 3p a-d-e-f"] <- adef3$team
            simdata$team2[simdata$team2=="best 3p d-e-f"] <-   def3$team
            simdata$team2[simdata$team2=="best 3p a-b-c"] <-   abc3$team
            simdata$team2[simdata$team2=="best 3p a-b-c-d"] <- abcd3$team
            
            if(any(simdata$team2=="germany" & simdata$group!="A")==TRUE) { #need to make germany `home' team
              germany.details <- simdata[simdata$team2=="germany" & simdata$group!="A",c("team2","team2_id","elostrength2")]
              germany.opp.details <- simdata[simdata$team2=="germany" & simdata$group!="A",c("team1","team1_id","elostrength1")]
              simdata[simdata$team2=="germany" & simdata$group!="A",c("team1","team1_id","elostrength1")] <- germany.details
              simdata[simdata$team2=="germany" & simdata$group!="A",c("team2","team2_id","elostrength2")] <- germany.opp.details
            }
            simdata$neutral[simdata$team1=="germany"] <- FALSE
            # simdata$elostrength1[simdata$team1=="group g winner"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grG$final.table$team[1]])]]
            # simdata$elostrength2[simdata$team2=="group g runner-up"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grG$final.table$team[2]])]]
            # simdata$team1_id[simdata$team1=="group g winner"] <- team_ids$team1_id[team_ids$team1==grG$final.table$team[1]]
            # simdata$team2_id[simdata$team2=="group g runner-up"] <- team_ids$team1_id[team_ids$team1==grG$final.table$team[2]]
            # simdata$team1[simdata$team1=="group g winner"] <- grG$final.table$team[1]
            # simdata$team2[simdata$team2=="group g runner-up"] <- grG$final.table$team[2]
            # 
            # simdata$elostrength1[simdata$team1=="group h winner"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grH$final.table$team[1]])]]
            # simdata$elostrength2[simdata$team2=="group h runner-up"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==grH$final.table$team[2]])]]
            # simdata$team1_id[simdata$team1=="group h winner"] <- team_ids$team1_id[team_ids$team1==grH$final.table$team[1]]
            # simdata$team2_id[simdata$team2=="group h runner-up"] <- team_ids$team1_id[team_ids$team1==grH$final.table$team[2]]
            # simdata$team1[simdata$team1=="group h winner"] <- grH$final.table$team[1]
            # simdata$team2[simdata$team2=="group h runner-up"] <- grH$final.table$team[2]
          } else {
            l16.teams <- data.frame("team"=c(grA$final.table[1:2,1],grB$final.table[1:2,1],
                                             grC$final.table[1:2,1],grD$final.table[1:2,1],
                                             grE$final.table[1:2,1],grF$final.table[1:2,1]),
                                             # grG$final.table[1:2,1],grH$final.table[1:2,1],
                                    stringsAsFactors = FALSE)
            l16.teams <- merge(l16.teams,team_ids[,c("team1","team1_id")],by.x="team",by.y="team1")
            l16.teams$reshuffle <- sample(1:16)
            
            simdata$elostrength1[simdata$team1=="group a winner"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==1])]]
            simdata$elostrength1[simdata$team1=="group a runner-up"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==2])]]
            simdata$team1_id[simdata$team1=="group a winner"] <- l16.teams$team1_id[l16.teams$reshuffle==1]
            simdata$team1_id[simdata$team1=="group a runner-up"] <- l16.teams$team1_id[l16.teams$reshuffle==2]
            simdata$team1[simdata$team1=="group a winner"] <- l16.teams$team[l16.teams$reshuffle==1]
            simdata$team1[simdata$team1=="group a runner-up"] <- l16.teams$team[l16.teams$reshuffle==2]
            
            simdata$elostrength1[simdata$team1=="group b winner"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==3])]]
            simdata$elostrength2[simdata$team2=="group b runner-up"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==4])]]
            simdata$team1_id[simdata$team1=="group b winner"] <- l16.teams$team1_id[l16.teams$reshuffle==3]
            simdata$team2_id[simdata$team2=="group b runner-up"] <- l16.teams$team1_id[l16.teams$reshuffle==4]
            simdata$team1[simdata$team1=="group b winner"] <- l16.teams$team[l16.teams$reshuffle==3]
            simdata$team2[simdata$team2=="group b runner-up"] <- l16.teams$team[l16.teams$reshuffle==4]
            
            simdata$elostrength1[simdata$team1=="group c winner"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==5])]]
            simdata$elostrength2[simdata$team2=="group c runner-up"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==6])]]
            simdata$team1_id[simdata$team1=="group c winner"] <- l16.teams$team1_id[l16.teams$reshuffle==5]
            simdata$team2_id[simdata$team2=="group c runner-up"] <- l16.teams$team1_id[l16.teams$reshuffle==6]
            simdata$team1[simdata$team1=="group c winner"] <- l16.teams$team[l16.teams$reshuffle==5]
            simdata$team2[simdata$team2=="group c runner-up"] <- l16.teams$team[l16.teams$reshuffle==6]
            
            simdata$elostrength1[simdata$team1=="group d winner"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==7])]]
            simdata$elostrength1[simdata$team1=="group d runner-up"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==8])]]
            simdata$team1_id[simdata$team1=="group d winner"] <- l16.teams$team1_id[l16.teams$reshuffle==7]
            simdata$team1_id[simdata$team1=="group d runner-up"] <- l16.teams$team1_id[l16.teams$reshuffle==8]
            simdata$team1[simdata$team1=="group d winner"] <- l16.teams$team[l16.teams$reshuffle==7]
            simdata$team1[simdata$team1=="group d runner-up"] <- l16.teams$team[l16.teams$reshuffle==8]
            
            simdata$elostrength1[simdata$team1=="group e winner"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==9])]]
            simdata$elostrength2[simdata$team2=="group e runner-up"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==10])]]
            simdata$team1_id[simdata$team1=="group e winner"] <- l16.teams$team1_id[l16.teams$reshuffle==9]
            simdata$team2_id[simdata$team2=="group e runner-up"] <- l16.teams$team1_id[l16.teams$reshuffle==10]
            simdata$team1[simdata$team1=="group e winner"] <- l16.teams$team[l16.teams$reshuffle==9]
            simdata$team2[simdata$team2=="group e runner-up"] <- l16.teams$team[l16.teams$reshuffle==10]
            
            simdata$elostrength1[simdata$team1=="group f winner"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==11])]]
            simdata$elostrength2[simdata$team2=="group f runner-up"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==12])]]
            simdata$team1_id[simdata$team1=="group f winner"] <- l16.teams$team1_id[l16.teams$reshuffle==11]
            simdata$team2_id[simdata$team2=="group f runner-up"] <- l16.teams$team1_id[l16.teams$reshuffle==12]
            simdata$team1[simdata$team1=="group f winner"] <- l16.teams$team[l16.teams$reshuffle==11]
            simdata$team2[simdata$team2=="group f runner-up"] <- l16.teams$team[l16.teams$reshuffle==12]
            
            # simdata$elostrength1[simdata$team1=="group g winner"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==13])]]
            # simdata$elostrength2[simdata$team2=="group g runner-up"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==14])]]
            # simdata$team1_id[simdata$team1=="group g winner"] <- l16.teams$team1_id[l16.teams$reshuffle==13]
            # simdata$team2_id[simdata$team2=="group g runner-up"] <- l16.teams$team1_id[l16.teams$reshuffle==14]
            # simdata$team1[simdata$team1=="group g winner"] <- l16.teams$team[l16.teams$reshuffle==13]
            # simdata$team2[simdata$team2=="group g runner-up"] <- l16.teams$team[l16.teams$reshuffle==14]
            # 
            # simdata$elostrength1[simdata$team1=="group h winner"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==15])]]
            # simdata$elostrength2[simdata$team2=="group h runner-up"] <- elo.sim[[paste0("id",l16.teams$team1_id[l16.teams$reshuffle==16])]]
            # simdata$team1_id[simdata$team1=="group h winner"] <- l16.teams$team1_id[l16.teams$reshuffle==15]
            # simdata$team2_id[simdata$team2=="group h runner-up"] <- l16.teams$team1_id[l16.teams$reshuffle==16]
            # simdata$team1[simdata$team1=="group h winner"] <- l16.teams$team[l16.teams$reshuffle==15]
            # simdata$team2[simdata$team2=="group h runner-up"] <- l16.teams$team[l16.teams$reshuffle==16]
          }
          
          simdata$elopredict <- 1/(1+(10^((simdata$elostrength2-simdata$elostrength1)/400)))
        }
        if(dd=="2024-07-05") {##then last 16 is over. Need to work out who progresses.
          #first check for draws and decide winner based on elo predict
          if(simdata$outcome[simdata$group=="L16 1"]==1) {simdata$winner[simdata$group=="L16 1"] <- simdata$team1[simdata$group=="L16 1"]} else if (simdata$outcome[simdata$group=="L16 1"]==0) {simdata$winner[simdata$group=="L16 1"] <- simdata$team2[simdata$group=="L16 1"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="L16 1"])==1) {simdata$winner[simdata$group=="L16 1"] <- simdata$team1[simdata$group=="L16 1"]} else {simdata$winner[simdata$group=="L16 1"] <- simdata$team2[simdata$group=="L16 1"]}
          if(simdata$outcome[simdata$group=="L16 2"]==1) {simdata$winner[simdata$group=="L16 2"] <- simdata$team1[simdata$group=="L16 2"]} else if (simdata$outcome[simdata$group=="L16 2"]==0) {simdata$winner[simdata$group=="L16 2"] <- simdata$team2[simdata$group=="L16 2"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="L16 2"])==1) {simdata$winner[simdata$group=="L16 2"] <- simdata$team1[simdata$group=="L16 2"]} else {simdata$winner[simdata$group=="L16 2"] <- simdata$team2[simdata$group=="L16 2"]}
          if(simdata$outcome[simdata$group=="L16 3"]==1) {simdata$winner[simdata$group=="L16 3"] <- simdata$team1[simdata$group=="L16 3"]} else if (simdata$outcome[simdata$group=="L16 3"]==0) {simdata$winner[simdata$group=="L16 3"] <- simdata$team2[simdata$group=="L16 3"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="L16 3"])==1) {simdata$winner[simdata$group=="L16 3"] <- simdata$team1[simdata$group=="L16 3"]} else {simdata$winner[simdata$group=="L16 3"] <- simdata$team2[simdata$group=="L16 3"]}
          if(simdata$outcome[simdata$group=="L16 4"]==1) {simdata$winner[simdata$group=="L16 4"] <- simdata$team1[simdata$group=="L16 4"]} else if (simdata$outcome[simdata$group=="L16 4"]==0) {simdata$winner[simdata$group=="L16 4"] <- simdata$team2[simdata$group=="L16 4"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="L16 4"])==1) {simdata$winner[simdata$group=="L16 4"] <- simdata$team1[simdata$group=="L16 4"]} else {simdata$winner[simdata$group=="L16 4"] <- simdata$team2[simdata$group=="L16 4"]}
          if(simdata$outcome[simdata$group=="L16 5"]==1) {simdata$winner[simdata$group=="L16 5"] <- simdata$team1[simdata$group=="L16 5"]} else if (simdata$outcome[simdata$group=="L16 5"]==0) {simdata$winner[simdata$group=="L16 5"] <- simdata$team2[simdata$group=="L16 5"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="L16 5"])==1) {simdata$winner[simdata$group=="L16 5"] <- simdata$team1[simdata$group=="L16 5"]} else {simdata$winner[simdata$group=="L16 5"] <- simdata$team2[simdata$group=="L16 5"]}
          if(simdata$outcome[simdata$group=="L16 6"]==1) {simdata$winner[simdata$group=="L16 6"] <- simdata$team1[simdata$group=="L16 6"]} else if (simdata$outcome[simdata$group=="L16 6"]==0) {simdata$winner[simdata$group=="L16 6"] <- simdata$team2[simdata$group=="L16 6"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="L16 6"])==1) {simdata$winner[simdata$group=="L16 6"] <- simdata$team1[simdata$group=="L16 6"]} else {simdata$winner[simdata$group=="L16 6"] <- simdata$team2[simdata$group=="L16 6"]}
          if(simdata$outcome[simdata$group=="L16 7"]==1) {simdata$winner[simdata$group=="L16 7"] <- simdata$team1[simdata$group=="L16 7"]} else if (simdata$outcome[simdata$group=="L16 7"]==0) {simdata$winner[simdata$group=="L16 7"] <- simdata$team2[simdata$group=="L16 7"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="L16 7"])==1) {simdata$winner[simdata$group=="L16 7"] <- simdata$team1[simdata$group=="L16 7"]} else {simdata$winner[simdata$group=="L16 7"] <- simdata$team2[simdata$group=="L16 7"]}
          if(simdata$outcome[simdata$group=="L16 8"]==1) {simdata$winner[simdata$group=="L16 8"] <- simdata$team1[simdata$group=="L16 8"]} else if (simdata$outcome[simdata$group=="L16 8"]==0) {simdata$winner[simdata$group=="L16 8"] <- simdata$team2[simdata$group=="L16 8"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="L16 8"])==1) {simdata$winner[simdata$group=="L16 8"] <- simdata$team1[simdata$group=="L16 8"]} else {simdata$winner[simdata$group=="L16 8"] <- simdata$team2[simdata$group=="L16 8"]}
          # simdata$winner[simdata$group=="L16 1"] <- "netherlands"
          # simdata$winner[simdata$group=="L16 2"] <- "argentina"
          # simdata$winner[simdata$group=="L16 3"] <- "france"
          # simdata$winner[simdata$group=="L16 4"] <- "england"
          # simdata$winner[simdata$group=="L16 5"] <- "croatia"
          # simdata$winner[simdata$group=="L16 6"] <- "brazil"
          # simdata$winner[simdata$group=="L16 7"] <- "morocco"
          # simdata$winner[simdata$group=="L16 8"] <- "portugal"
          ##put winners in as per schedule
          if(knockout.shuffle==0) {
            simdata$elostrength2[simdata$team2=="winner L16 1"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 1"]])]]
            simdata$elostrength2[simdata$team2=="winner L16 2"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 2"]])]]
            simdata$elostrength1[simdata$team1=="winner L16 3"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 3"]])]]
            simdata$elostrength1[simdata$team1=="winner L16 4"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 4"]])]]
            simdata$elostrength1[simdata$team1=="winner L16 5"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 5"]])]]
            simdata$elostrength2[simdata$team2=="winner L16 6"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 6"]])]]
            simdata$elostrength1[simdata$team1=="winner L16 7"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 7"]])]]
            simdata$elostrength2[simdata$team2=="winner L16 8"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 8"]])]]
            simdata$team2_id[simdata$team2=="winner L16 1"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 1"]]
            simdata$team2_id[simdata$team2=="winner L16 2"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 2"]]
            simdata$team1_id[simdata$team1=="winner L16 3"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 3"]]
            simdata$team1_id[simdata$team1=="winner L16 4"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 4"]]
            simdata$team1_id[simdata$team1=="winner L16 5"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 5"]]
            simdata$team2_id[simdata$team2=="winner L16 6"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 6"]]
            simdata$team1_id[simdata$team1=="winner L16 7"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 7"]]
            simdata$team2_id[simdata$team2=="winner L16 8"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="L16 8"]]
            simdata$team2[simdata$team2=="winner L16 1"] <- simdata$winner[simdata$group=="L16 1"]
            simdata$team2[simdata$team2=="winner L16 2"] <- simdata$winner[simdata$group=="L16 2"]
            simdata$team1[simdata$team1=="winner L16 3"] <- simdata$winner[simdata$group=="L16 3"]
            simdata$team1[simdata$team1=="winner L16 4"] <- simdata$winner[simdata$group=="L16 4"]
            simdata$team1[simdata$team1=="winner L16 5"] <- simdata$winner[simdata$group=="L16 5"]
            simdata$team2[simdata$team2=="winner L16 6"] <- simdata$winner[simdata$group=="L16 6"]
            simdata$team1[simdata$team1=="winner L16 7"] <- simdata$winner[simdata$group=="L16 7"]
            simdata$team2[simdata$team2=="winner L16 8"] <- simdata$winner[simdata$group=="L16 8"]
          } else {
            qf.teams <- data.frame("team"=simdata$winner[regexpr("L16",simdata$group)>-1])
            qf.teams <- merge(qf.teams,team_ids[,c("team1","team1_id")],by.x="team",by.y="team1")
            qf.teams$reshuffle <- sample(1:8)
            
            simdata$elostrength1[simdata$team1=="L16 1 winner"] <- elo.sim[[paste0("id",qf.teams$team1_id[qf.teams$reshuffle==1])]]
            simdata$elostrength2[simdata$team2=="L16 2 winner"] <- elo.sim[[paste0("id",qf.teams$team1_id[qf.teams$reshuffle==2])]]
            simdata$elostrength1[simdata$team1=="L16 3 winner"] <- elo.sim[[paste0("id",qf.teams$team1_id[qf.teams$reshuffle==3])]]
            simdata$elostrength2[simdata$team2=="L16 4 winner"] <- elo.sim[[paste0("id",qf.teams$team1_id[qf.teams$reshuffle==4])]]
            simdata$elostrength1[simdata$team1=="L16 5 winner"] <- elo.sim[[paste0("id",qf.teams$team1_id[qf.teams$reshuffle==5])]]
            simdata$elostrength2[simdata$team2=="L16 6 winner"] <- elo.sim[[paste0("id",qf.teams$team1_id[qf.teams$reshuffle==6])]]
            simdata$elostrength1[simdata$team1=="L16 7 winner"] <- elo.sim[[paste0("id",qf.teams$team1_id[qf.teams$reshuffle==7])]]
            simdata$elostrength2[simdata$team2=="L16 8 winner"] <- elo.sim[[paste0("id",qf.teams$team1_id[qf.teams$reshuffle==8])]]
            simdata$team1_id[simdata$team1=="L16 1 winner"] <- qf.teams$team1_id[qf.teams$reshuffle==1]
            simdata$team2_id[simdata$team2=="L16 2 winner"] <- qf.teams$team1_id[qf.teams$reshuffle==2]
            simdata$team1_id[simdata$team1=="L16 3 winner"] <- qf.teams$team1_id[qf.teams$reshuffle==3]
            simdata$team2_id[simdata$team2=="L16 4 winner"] <- qf.teams$team1_id[qf.teams$reshuffle==4]
            simdata$team1_id[simdata$team1=="L16 5 winner"] <- qf.teams$team1_id[qf.teams$reshuffle==5]
            simdata$team2_id[simdata$team2=="L16 6 winner"] <- qf.teams$team1_id[qf.teams$reshuffle==6]
            simdata$team1_id[simdata$team1=="L16 7 winner"] <- qf.teams$team1_id[qf.teams$reshuffle==7]
            simdata$team2_id[simdata$team2=="L16 8 winner"] <- qf.teams$team1_id[qf.teams$reshuffle==8]
            simdata$team1[simdata$team1=="L16 1 winner"] <- qf.teams$team[qf.teams$reshuffle==1]
            simdata$team2[simdata$team2=="L16 2 winner"] <- qf.teams$team[qf.teams$reshuffle==2]
            simdata$team1[simdata$team1=="L16 3 winner"] <- qf.teams$team[qf.teams$reshuffle==3]
            simdata$team2[simdata$team2=="L16 4 winner"] <- qf.teams$team[qf.teams$reshuffle==4]
            simdata$team1[simdata$team1=="L16 5 winner"] <- qf.teams$team[qf.teams$reshuffle==5]
            simdata$team2[simdata$team2=="L16 6 winner"] <- qf.teams$team[qf.teams$reshuffle==6]
            simdata$team1[simdata$team1=="L16 7 winner"] <- qf.teams$team[qf.teams$reshuffle==7]
            simdata$team2[simdata$team2=="L16 8 winner"] <- qf.teams$team[qf.teams$reshuffle==8]
          }
          
          simdata$elopredict <- 1/(1+(10^((simdata$elostrength2-simdata$elostrength1)/400)))
        }
        if(dd=="2024-07-09") {##then quarter finals are over. Need to work out who progresses.
          #first check for draws and decide winner based on elo predict
          if(simdata$outcome[simdata$group=="qf 1"]==1) {simdata$winner[simdata$group=="qf 1"] <- simdata$team1[simdata$group=="qf 1"]} else if (simdata$outcome[simdata$group=="qf 1"]==0) {simdata$winner[simdata$group=="qf 1"] <- simdata$team2[simdata$group=="qf 1"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="qf 1"])==1) {simdata$winner[simdata$group=="qf 1"] <- simdata$team1[simdata$group=="qf 1"]} else {simdata$winner[simdata$group=="qf 1"] <- simdata$team2[simdata$group=="qf 1"]}
          if(simdata$outcome[simdata$group=="qf 2"]==1) {simdata$winner[simdata$group=="qf 2"] <- simdata$team1[simdata$group=="qf 2"]} else if (simdata$outcome[simdata$group=="qf 2"]==0) {simdata$winner[simdata$group=="qf 2"] <- simdata$team2[simdata$group=="qf 2"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="qf 2"])==1) {simdata$winner[simdata$group=="qf 2"] <- simdata$team1[simdata$group=="qf 2"]} else {simdata$winner[simdata$group=="qf 2"] <- simdata$team2[simdata$group=="qf 2"]}
          if(simdata$outcome[simdata$group=="qf 3"]==1) {simdata$winner[simdata$group=="qf 3"] <- simdata$team1[simdata$group=="qf 3"]} else if (simdata$outcome[simdata$group=="qf 3"]==0) {simdata$winner[simdata$group=="qf 3"] <- simdata$team2[simdata$group=="qf 3"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="qf 3"])==1) {simdata$winner[simdata$group=="qf 3"] <- simdata$team1[simdata$group=="qf 3"]} else {simdata$winner[simdata$group=="qf 3"] <- simdata$team2[simdata$group=="qf 3"]}
          if(simdata$outcome[simdata$group=="qf 4"]==1) {simdata$winner[simdata$group=="qf 4"] <- simdata$team1[simdata$group=="qf 4"]} else if (simdata$outcome[simdata$group=="qf 4"]==0) {simdata$winner[simdata$group=="qf 4"] <- simdata$team2[simdata$group=="qf 4"]} else if (rbinom(1,1,simdata$elopredict[simdata$group=="qf 4"])==1) {simdata$winner[simdata$group=="qf 4"] <- simdata$team1[simdata$group=="qf 4"]} else {simdata$winner[simdata$group=="qf 4"] <- simdata$team2[simdata$group=="qf 4"]}
          ##put winners in
          if(knockout.shuffle==0) {
            simdata$elostrength1[simdata$team1=="winner qf 1"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="qf 1"]])]]
            simdata$elostrength2[simdata$team2=="winner qf 2"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="qf 2"]])]]
            simdata$elostrength1[simdata$team1=="winner qf 3"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="qf 3"]])]]
            simdata$elostrength2[simdata$team2=="winner qf 4"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="qf 4"]])]]
            simdata$team1_id[simdata$team1=="winner qf 1"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="qf 1"]]
            simdata$team2_id[simdata$team2=="winner qf 2"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="qf 2"]]
            simdata$team1_id[simdata$team1=="winner qf 3"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="qf 3"]]
            simdata$team2_id[simdata$team2=="winner qf 4"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="qf 4"]]
            simdata$team1[simdata$team1=="winner qf 1"] <- simdata$winner[simdata$group=="qf 1"]
            simdata$team2[simdata$team2=="winner qf 2"] <- simdata$winner[simdata$group=="qf 2"]
            simdata$team1[simdata$team1=="winner qf 3"] <- simdata$winner[simdata$group=="qf 3"]
            simdata$team2[simdata$team2=="winner qf 4"] <- simdata$winner[simdata$group=="qf 4"]
          } else {
            sf.teams <- data.frame("team"=simdata$winner[regexpr("qf",simdata$group)>-1])
            sf.teams <- merge(sf.teams,team_ids[,c("team1","team1_id")],by.x="team",by.y="team1")
            sf.teams$reshuffle <- sample(1:4)
            
            simdata$elostrength1[simdata$team1=="qf 1 winner"] <- elo.sim[[paste0("id",sf.teams$team1_id[sf.teams$reshuffle==1])]]
            simdata$elostrength2[simdata$team2=="qf 2 winner"] <- elo.sim[[paste0("id",sf.teams$team1_id[sf.teams$reshuffle==2])]]
            simdata$elostrength1[simdata$team1=="qf 3 winner"] <- elo.sim[[paste0("id",sf.teams$team1_id[sf.teams$reshuffle==3])]]
            simdata$elostrength2[simdata$team2=="qf 4 winner"] <- elo.sim[[paste0("id",sf.teams$team1_id[sf.teams$reshuffle==4])]]
            simdata$team1_id[simdata$team1=="qf 1 winner"] <- sf.teams$team1_id[sf.teams$reshuffle==1]
            simdata$team2_id[simdata$team2=="qf 2 winner"] <- sf.teams$team1_id[sf.teams$reshuffle==2]
            simdata$team1_id[simdata$team1=="qf 3 winner"] <- sf.teams$team1_id[sf.teams$reshuffle==3]
            simdata$team2_id[simdata$team2=="qf 4 winner"] <- sf.teams$team1_id[sf.teams$reshuffle==4]
            simdata$team1[simdata$team1=="qf 1 winner"] <- sf.teams$team[sf.teams$reshuffle==1]
            simdata$team2[simdata$team2=="qf 2 winner"] <- sf.teams$team[sf.teams$reshuffle==2]
            simdata$team1[simdata$team1=="qf 3 winner"] <- sf.teams$team[sf.teams$reshuffle==3]
            simdata$team2[simdata$team2=="qf 4 winner"] <- sf.teams$team[sf.teams$reshuffle==4]
          }
          simdata$elopredict <- 1/(1+(10^((simdata$elostrength2-simdata$elostrength1)/400)))
        }
        if(dd=="2024-07-14") {##then semi finals are over. Need to work out who progresses.
          #first check for draws and decide winner based on elo predict
          if(simdata$outcome[simdata$group=="sf 1"]==1) 
          {
            simdata$winner[simdata$group=="sf 1"] <- simdata$team1[simdata$group=="sf 1"]
            simdata$loser[simdata$group=="sf 1"] <- simdata$team2[simdata$group=="sf 1"]
          } else if (simdata$outcome[simdata$group=="sf 1"]==0) 
          {
            simdata$winner[simdata$group=="sf 1"] <- simdata$team2[simdata$group=="sf 1"]
            simdata$loser[simdata$group=="sf 1"] <- simdata$team1[simdata$group=="sf 1"]
          } else if (rbinom(1,1,simdata$elopredict[simdata$group=="sf 1"])==1) 
          {
            simdata$winner[simdata$group=="sf 1"] <- simdata$team1[simdata$group=="sf 1"]
            simdata$loser[simdata$group=="sf 1"] <- simdata$team2[simdata$group=="sf 1"]
          } else 
          {
            simdata$winner[simdata$group=="sf 1"] <- simdata$team2[simdata$group=="sf 1"]
            simdata$loser[simdata$group=="sf 1"] <- simdata$team1[simdata$group=="sf 1"]
          }
          if(simdata$outcome[simdata$group=="sf 2"]==1) 
          {
            simdata$winner[simdata$group=="sf 2"] <- simdata$team1[simdata$group=="sf 2"]
            simdata$loser[simdata$group=="sf 2"] <- simdata$team2[simdata$group=="sf 2"]
          } else if (simdata$outcome[simdata$group=="sf 2"]==0) 
          {
            simdata$winner[simdata$group=="sf 2"] <- simdata$team2[simdata$group=="sf 2"]
            simdata$loser[simdata$group=="sf 2"] <- simdata$team1[simdata$group=="sf 2"]
          } else if (rbinom(1,1,simdata$elopredict[simdata$group=="sf 2"])==1) 
          {
            simdata$winner[simdata$group=="sf 2"] <- simdata$team1[simdata$group=="sf 2"]
            simdata$loser[simdata$group=="sf 2"] <- simdata$team2[simdata$group=="sf 2"]
          } else 
          {
            simdata$winner[simdata$group=="sf 2"] <- simdata$team2[simdata$group=="sf 2"]
            simdata$loser[simdata$group=="sf 2"] <- simdata$team1[simdata$group=="sf 2"]
          }
          ##put winners in
          simdata$elostrength1[simdata$team1=="winner sf 1"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="sf 1"]])]]
          simdata$elostrength2[simdata$team2=="winner sf 2"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="sf 2"]])]]
          simdata$team1_id[simdata$team1=="winner sf 1"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="sf 1"]]
          simdata$team2_id[simdata$team2=="winner sf 2"] <- team_ids$team1_id[team_ids$team1==simdata$winner[simdata$group=="sf 2"]]
          simdata$team1[simdata$team1=="winner sf 1"] <- simdata$winner[simdata$group=="sf 1"]
          simdata$team2[simdata$team2=="winner sf 2"] <- simdata$winner[simdata$group=="sf 2"]
          ##put losers in to 3rd place playoff
          # simdata$elostrength1[simdata$team1=="sf 1 loser"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$loser[simdata$group=="sf 1"]])]]
          # simdata$elostrength2[simdata$team2=="sf 2 loser"] <- elo.sim[[paste0("id",team_ids$team1_id[team_ids$team1==simdata$loser[simdata$group=="sf 2"]])]]
          # simdata$team1_id[simdata$team1=="sf 1 loser"] <- team_ids$team1_id[team_ids$team1==simdata$loser[simdata$group=="sf 1"]]
          # simdata$team2_id[simdata$team2=="sf 2 loser"] <- team_ids$team1_id[team_ids$team1==simdata$loser[simdata$group=="sf 2"]]
          # simdata$team1[simdata$team1=="sf 1 loser"] <- simdata$loser[simdata$group=="sf 1"]
          # simdata$team2[simdata$team2=="sf 2 loser"] <- simdata$loser[simdata$group=="sf 2"]
          
          simdata$elopredict <- 1/(1+(10^((simdata$elostrength2-simdata$elostrength1)/400)))
        }
        
        #forecast
        forecast.matches0 <- simdata[simdata$date==dd & is.na(simdata$goals1)==TRUE,]
        
        #forecast matches switching home and away teams to cancel out home advantage
        forecast.matches1 <- forecast.matches0
        forecast.matches1$team1 <- forecast.matches0$team2
        forecast.matches1$team1_id <- forecast.matches0$team2_id
        forecast.matches1$elostrength1 <- forecast.matches0$elostrength2
        forecast.matches1$goals1d.hat <- forecast.matches0$goals2d.hat
        forecast.matches1$team2 <- forecast.matches0$team1
        forecast.matches1$team2_id <- forecast.matches0$team1_id
        forecast.matches1$elostrength2 <- forecast.matches0$elostrength1
        forecast.matches1$goals2d.hat <- forecast.matches0$goals1d.hat
        forecast.matches1$elopredict <- 1-forecast.matches0$elopredict
        
        if(NROW(forecast.matches0)>0) {
          forecasts <- createforecasts(data=forecast.matches0,
                                       home.g.mod = m1d.sim,
                                       away.g.mod = m2d.sim,
                                       date0=dd,
                                       days.ahead=0,save=FALSE)
          # forecasts2 <- createforecasts(data=forecast.matches1,
          #                               home.g.mod = m1d.sim,
          #                               away.g.mod = m2d.sim,
          #                               date0=dd,
          #                               days.ahead=0,save=FALSE)
          #insert outcomes
          forecasts$goals1 <- rpois(NROW(forecasts),lambda=forecasts$lambda1.hat) #+forecasts2$lambda2.hat)/2
          forecasts$goals2 <- rpois(NROW(forecasts),lambda=forecasts$lambda2.hat) #+forecasts2$lambda1.hat)/2
          forecasts$outcome <- 0.5*as.numeric(forecasts$goals1==forecasts$goals2) + as.numeric(forecasts$goals1>forecasts$goals2)
          
          #update data
          updatelist <- quickupdate(data=simdata,elorank.u=elo.sim,results=forecasts,elo.weight = 20,ids=TRUE)
          simdata <- updatelist$res
          elo.sim <- updatelist$elorank
          simdata <- simdata[order(simdata$date),]
          
          # if(dd=="2022-12-17") {##then 3rd place play off is over. Need to work out who was 3rd.
          #   #first check for draws and decide winner based on elo predict
          #   if(simdata$outcome[simdata$group=="3P"]==1) 
          #   {
          #     simdata$winner[simdata$group=="3P"] <- simdata$team1[simdata$group=="3P"]
          #     simdata$loser[simdata$group=="3P"] <- simdata$team2[simdata$group=="3P"]
          #   } else if (simdata$outcome[simdata$group=="3P"]==0) 
          #   {
          #     simdata$winner[simdata$group=="3P"] <- simdata$team2[simdata$group=="3P"]
          #     simdata$loser[simdata$group=="3P"] <- simdata$team1[simdata$group=="3P"]
          #   } else if (rbinom(1,1,simdata$elopredict[simdata$group=="3P"])==1) 
          #   {
          #     simdata$winner[simdata$group=="3P"] <- simdata$team1[simdata$group=="3P"]
          #     simdata$loser[simdata$group=="3P"] <- simdata$team2[simdata$group=="3P"]
          #   } else 
          #   {
          #     simdata$winner[simdata$group=="3P"] <- simdata$team2[simdata$group=="3P"]
          #     simdata$loser[simdata$group=="3P"] <- simdata$team1[simdata$group=="3P"]
          #   }
          # }
          if(dd=="2024-07-14") {##then final is over. Need to work out who won.
            #first check for draws and decide winner based on elo predict
            if(simdata$outcome[simdata$group=="Final"]==1) 
            {
              simdata$winner[simdata$group=="Final"] <- simdata$team1[simdata$group=="Final"]
              simdata$loser[simdata$group=="Final"] <- simdata$team2[simdata$group=="Final"]
            } else if (simdata$outcome[simdata$group=="Final"]==0) 
            {
              simdata$winner[simdata$group=="Final"] <- simdata$team2[simdata$group=="Final"]
              simdata$loser[simdata$group=="Final"] <- simdata$team1[simdata$group=="Final"]
            } else if (rbinom(1,1,simdata$elopredict[simdata$group=="Final"])==1) 
            {
              simdata$winner[simdata$group=="Final"] <- simdata$team1[simdata$group=="Final"]
              simdata$loser[simdata$group=="Final"] <- simdata$team2[simdata$group=="Final"]
            } else 
            {
              simdata$winner[simdata$group=="Final"] <- simdata$team2[simdata$group=="Final"]
              simdata$loser[simdata$group=="Final"] <- simdata$team1[simdata$group=="Final"]
            }
          }
        }
      }
      ##save each rep
      #simdata$replication <- rr
      #    all.season.tabs1 <- rbind(all.season.tabs1,final.lge.tab1)
      #simdata$replication <- rr
      #    all.season.results1 <- rbind(all.season.results1,simdata[simdata$div_id==1,c("match_id","goals1","goals2","replication")])
      rownames(grA$final.table) <- paste0("A",1:4)
      rownames(grB$final.table) <- paste0("B",1:4)
      rownames(grC$final.table) <- paste0("C",1:4)
      rownames(grD$final.table) <- paste0("D",1:4)
      rownames(grE$final.table) <- paste0("E",1:4)
      rownames(grF$final.table) <- paste0("F",1:4)
      # rownames(grG$final.table) <- paste0("G",1:4)
      # rownames(grH$final.table) <- paste0("H",1:4)
      write.csv(rbind(grA$final.table,grB$final.table,grC$final.table,grD$final.table,
                      grE$final.table,grF$final.table),#,grG$final.table,grH$final.table
                paste0(fulldir1,"/groups-rep-",rr,".csv"))
      write.csv(simdata,
                paste0(fulldir1,"/results-rep-",rr,".csv"))
      # write.csv(all.season.tabs1,
      #           paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div1-sim",Sys.Date(),
      #                  "-",counter,"-raw.csv"))
      # write.csv(all.season.results1,
      #           paste0("/Volumes/11330730-dp/Correct-score-data/end-of-season/endof2021season-div1-sim",Sys.Date(),
      #                  "-",counter,"-all-results.csv"))
      # if(rr %in% seq(2000,84000,2000)) {
      #   all.season.tabs1 <- data.frame(stringsAsFactors = FALSE)
      #   all.season.results1 <- data.frame(stringsAsFactors = FALSE)
      #   counter = counter+1
      # }
    }
  }
}

output.wc2022 <- function(date0 = Sys.Date(),counter=0) {
  require(xtable)
  dbloc="/Users/jjreade/Dropbox/Research/Sport/Correct-score"
  basedir = paste0(dbloc,"/euros-sim/")
  date0 = tail(list.files(basedir),1) #"2022-10-31"
  counter=0
  # date0 = "sim-2022-11-03"
  fulldir1 <- paste0(basedir,date0,"/simno-",counter,"/")

  grp.files <- list.files(fulldir1,pattern="^groups-rep")
  res.files <- list.files(fulldir1,pattern="^results-rep")

  all.grps <- data.frame()
  for(ff in grp.files) {
    temp.grp <- read.csv(paste0(fulldir1,ff),stringsAsFactors = FALSE)
    temp.grp$replication <- gsub("^.*?(\\d+).*?$","\\1",ff)
    all.grps <- rbind(all.grps,temp.grp)
  }
  all.sim.res <- data.frame()
  for(ff in res.files) {
    temp.res <- read.csv(paste0(fulldir1,ff),stringsAsFactors = FALSE)
    temp.res$replication <- gsub("^.*?(\\d+).*?$","\\1",ff)
    all.sim.res <- rbind(all.sim.res,temp.res) #[temp.res$date>'2022-12-02',c('date','team1','team2','replication','group','winner')]
  }

  basedir = paste0(dbloc,"/world-cup-sim-grprs/")
  date0 = tail(list.files(basedir),1) #"2022-10-31"
  fulldir1 <- paste0(basedir,date0,"/simno-",counter,"/")

  grp.files <- list.files(fulldir1,pattern="^groups-rep")
  res.files <- list.files(fulldir1,pattern="^results-rep")
  # all.grps.grprs <- data.frame()
  # for(ff in grp.files) {
  #   temp.grp <- read.csv(paste0(fulldir1,ff),stringsAsFactors = FALSE)
  #   temp.grp$replication <- gsub("^.*?(\\d+).*?$","\\1",ff)
  #   all.grps.grprs <- rbind(all.grps.grprs,temp.grp)
  # }
  all.sim.res.grprs <- data.frame()
  for(ff in res.files[1:30000]) {
    print(ff)
    temp.res <- read.csv(paste0(fulldir1,ff),stringsAsFactors = FALSE)
    temp.res$replication <- gsub("^.*?(\\d+).*?$","\\1",ff)
    all.sim.res.grprs <- rbind(all.sim.res.grprs,temp.res[temp.res$date>'2022-12-02',c('date','team1','team2','replication','group','winner')])
  }

  basedir = paste0(dbloc,"/world-cup-sim-kors/")
  date0 = tail(list.files(basedir),1) #"2022-10-31"
  fulldir1 <- paste0(basedir,date0,"/simno-",counter,"/")

  grp.files <- list.files(fulldir1,pattern="^groups-rep")
  res.files <- list.files(fulldir1,pattern="^results-rep")
  all.grps.kors <- data.frame()
  for(ff in grp.files) {
    temp.grp <- read.csv(paste0(fulldir1,ff),stringsAsFactors = FALSE)
    temp.grp$replication <- gsub("^.*?(\\d+).*?$","\\1",ff)
    all.grps.kors <- rbind(all.grps.kors,temp.grp)
  }
  all.sim.res.kors <- data.frame()
  for(ff in res.files) {
    temp.res <- read.csv(paste0(fulldir1,ff),stringsAsFactors = FALSE)
    temp.res$replication <- gsub("^.*?(\\d+).*?$","\\1",ff)
    all.sim.res.kors <- rbind(all.sim.res.kors,temp.res)
  }

  basedir = paste0(dbloc,"/world-cup-sim-both/")
  date0 = tail(list.files(basedir),1) #"2022-10-31"
  fulldir1 <- paste0(basedir,date0,"/simno-",counter,"/")

  grp.files <- list.files(fulldir1,pattern="^groups-rep")
  res.files <- list.files(fulldir1,pattern="^results-rep")
  all.grps.both <- data.frame()
  for(ff in grp.files) {
    temp.grp <- read.csv(paste0(fulldir1,ff),stringsAsFactors = FALSE)
    temp.grp$replication <- gsub("^.*?(\\d+).*?$","\\1",ff)
    all.grps.both <- rbind(all.grps.both,temp.grp)
  }
  all.sim.res.both <- data.frame()
  for(ff in res.files) {
    temp.res <- read.csv(paste0(fulldir1,ff),stringsAsFactors = FALSE)
    temp.res$replication <- gsub("^.*?(\\d+).*?$","\\1",ff)
    all.sim.res.both <- rbind(all.sim.res.both,temp.res)
  }

  elo.ratings.pre.eu0 <- simdata00[simdata00$date<"2024-06-19",c("team2","elostrength2")]
  colnames(elo.ratings.pre.eu0) <- c("team1","elostrength1")
  elo.ratings.pre.eu <- rbind(simdata00[simdata00$date<"2024-06-19",c("team1","elostrength1")],elo.ratings.pre.eu0)
  elo.ratings.pre.eu <- elo.ratings.pre.eu[order(elo.ratings.pre.eu$elostrength1,decreasing=TRUE),]
  elo.ratings.pre.eu <- elo.ratings.pre.eu[duplicated(elo.ratings.pre.eu)==FALSE,]
  elo.ratings.pre.eu <- elo.ratings.pre.eu[regexpr("winner|runner|best",elo.ratings.pre.eu$team1)==-1,]
  
  elonow <- simdata00[regexpr("win|best|run",simdata00$team2)==-1,c("team2","elostrength2","date")]
  colnames(elonow) <- c("team1","elostrength1","date")
  elonow <- rbind(elonow,simdata00[regexpr("win|best|run",simdata00$team1)==-1,c("team1","elostrength1","date")])
  elonow <- elonow[order(elonow$team1,elonow$date),]
  elo.ratings.now <- aggregate(elonow$elostrength1,by=list(elonow$team1),FUN=tail,1)
  colnames(elo.ratings.now) <- c("team1","elo.now")

  elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,elo.ratings.now,by=c("team1"),all.x=TRUE)
  
  past.finals <- data.frame("WC"=c(1930,1934,1938,1950,1954,1958,1962,1966,1970,1974,1978,1982,1986,1990,1994,1998,2002,2006,2010,2014,2018,2022),
                            "match_id"=c("tgc41591","tgc20008","tgc17870",NA,"tgc14665",
                                               "tgc37922","tgc5325","tgc11701","tgc5020",
                                               "tgc14541","tgc1074","tgc20316","tgc1295",
                                               "tgc14619","tgc4859","tgc13461","tgc14596",
                                               "tgc20420","tgc28514","tgc14947","tgc13869","tgc849211"),stringsAsFactors = FALSE)
  past.finals <- merge(past.finals,int.res[,c("match_id","date","team1","goals1","goals2","team2","elostrength1",
                                             "elostrength2","elopredict")],by=c("match_id"),all.x=TRUE)
  past.finals <- past.finals[regexpr("sf",past.finals$team1)==-1,]
  past.finals <- past.finals[duplicated(past.finals)==FALSE,]
  past.finals <- past.finals[is.na(past.finals$match_id)==FALSE,]
  past.finals <- past.finals[order(past.finals$date),]
  past.finals$ET <- c(0,1,0,0,0,0,1,0,0,1,0,0,0,1,0,0,1,1,1,0,NA)
  past.finals$pens <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,1,0,0,0,NA)
  past.finals$closeness <- abs(past.finals$elopredict-0.5)
  past.finals <- past.finals[order(past.finals$closeness),]
  write.csv(past.finals,"/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/world-cup-finals.csv")
  
  int.res$year <- format(int.res$date,"%Y")
  temp0 <- int.res[c("team2","elostrength2","year")]
  colnames(temp0) <- c("team1","elostrength1","year")
  annual.country.elos <- rbind(int.res[c("team1","elostrength1","year")],temp0)
  annual.country.elos <- aggregate(annual.country.elos$elostrength1,
                                   by=list(annual.country.elos$team1,annual.country.elos$year),FUN=mean)
  write.csv(annual.country.elos,"/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/annual-int-elos.csv")

  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/international-elos-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(annual.country.elos$Group.2[annual.country.elos$Group.1=="england"],
       annual.country.elos$x[annual.country.elos$Group.1=="england"],type="o",col=2,
       ylim=range(1000,max(annual.country.elos$x)),pch=16,cex=0.75,
       ylab="Elo rating",xlab="Year",main="Elo ratings for major footballing countries")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="scotland"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="scotland"],type="o",col="darkblue")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="germany"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="germany"],type="o")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="brazil"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="brazil"],type="o",col="yellow")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="argentina"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="argentina"],type="o",col="lightblue")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="france"],cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="france"],type="o",col="darkblue",pch=17)
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="italy"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="italy"],type="o",col="blue")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="spain"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="spain"],type="o",col="darkred")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="uruguay"],pch=17,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="uruguay"],type="o",col="lightblue")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="morocco"],pch=17,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="morocco"],type="o",col="red")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="croatia"],pch=17,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="croatia"],type="o",col="darkred")
  legend("topleft",col=c("red","darkblue","black","yellow","lightblue","darkblue","blue","darkred",
                         "lightblue","red","darkred"),
         lty=1,pch=c(rep(16,5),17,16,16,rep(17,3)),bty="n",legend=c("ENG","SCO","GER","BRA","ARG","FRA","ITA","SPA",
                                                         "URU","MOR","CRO"),ncol = 2)
  dev.off()

  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/home-nations-elos-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(annual.country.elos$Group.2[annual.country.elos$Group.1=="england"],
       annual.country.elos$x[annual.country.elos$Group.1=="england"],type="o",pch=16,cex=0.75,
       ylim=range(annual.country.elos$x[annual.country.elos$Group.1 %in% c("england","scotland","wales","northern ireland")]),
       ylab="Elo rating",xlab="Year",main="Elo ratings for the `home' footballing nations")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="scotland"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="scotland"],type="o",col="darkblue")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="wales"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="wales"],type="o",col="red")
  lines(annual.country.elos$Group.2[annual.country.elos$Group.1=="northern ireland"],pch=16,cex=0.75,
        annual.country.elos$x[annual.country.elos$Group.1=="northern ireland"],type="o",col="green")
  legend("topleft",col=c("black","darkblue","red","green"),
         lty=1,pch=c(rep(16,4)),bty="n",legend=c("ENG","SCO","WAL","NIR"),ncol = 2)
  dev.off()
  
  ##who reaches last 16??
  table(c(all.sim.res$team1[regexpr("^L\\d+",all.sim.res$group)>-1],all.sim.res$team2[regexpr("^L\\d+",all.sim.res$group)>-1]))/(NROW(res.files)/100)
  ##who reaches QFs??
  table(c(all.sim.res$team1[regexpr("^qf",all.sim.res$group)>-1],all.sim.res$team2[regexpr("^qf",all.sim.res$group)>-1]))/(NROW(res.files)/100)

  ##who reaches SFs??
  table(c(all.sim.res$team1[regexpr("^sf",all.sim.res$group)>-1],all.sim.res$team2[regexpr("^sf",all.sim.res$group)>-1]))/(NROW(res.files)/100)

  ##who reaches Final??
  table(c(all.sim.res$team1[regexpr("^Final",all.sim.res$group)>-1],all.sim.res$team2[regexpr("^Final",all.sim.res$group)>-1]))/(NROW(res.files)/100)

  ##who wins?
  wintab <- table(all.sim.res$winner[all.sim.res$group=="Final"])
  wintab <- 100*wintab/sum(wintab)
  wintab <- data.frame(wintab[order(wintab,decreasing = TRUE)])
  # wintab.grprs <- table(all.sim.res.grprs$winner[all.sim.res.grprs$group=="Final"])
  # wintab.grprs <- 100*wintab.grprs/sum(wintab.grprs)
  # wintab.grprs <- data.frame(wintab.grprs[order(wintab.grprs,decreasing = TRUE)])
  # wintab.kors <- table(all.sim.res.kors$winner[all.sim.res.kors$group=="Final"])/(NROW(res.files)/100)
  # wintab.kors <- data.frame(wintab.kors[order(wintab.kors,decreasing = TRUE)])
  # wintab.both <- table(all.sim.res.both$winner[all.sim.res.both$group=="Final"])/(NROW(res.files)/100)
  # wintab.both <- data.frame(wintab.both[order(wintab.both,decreasing = TRUE)])

  ##last 16 qualification ####
  l16tab <- table(c(all.sim.res$team1[regexpr("^L\\d+",all.sim.res$group)>-1],all.sim.res$team2[regexpr("^L\\d+",all.sim.res$group)>-1]))
  l16tab <- 100*l16tab/(sum(l16tab)/16)
  l16tab <- data.frame(l16tab[order(l16tab,decreasing = TRUE)])
  # l16tab.grprs <- table(c(all.sim.res.grprs$team1[regexpr("^L\\d+",all.sim.res.grprs$group)>-1],all.sim.res.grprs$team2[regexpr("^L\\d+",all.sim.res.grprs$group)>-1]))
  # l16tab.grprs <- 100*l16tab.grprs/(sum(l16tab.grprs)/16)
  # l16tab.grprs <- data.frame(l16tab.grprs[order(l16tab.grprs,decreasing = TRUE)])
  # l16tab.kors <- table(c(all.sim.res.kors$team1[regexpr("^L\\d+",all.sim.res.kors$group)>-1],all.sim.res.kors$team2[regexpr("^L\\d+",all.sim.res.kors$group)>-1]))/(NROW(res.files)/100)
  # l16tab.kors <- data.frame(l16tab.kors[order(l16tab.kors,decreasing = TRUE)])
  # l16tab.both <- table(c(all.sim.res.both$team1[regexpr("^L\\d+",all.sim.res.both$group)>-1],all.sim.res.both$team2[regexpr("^L\\d+",all.sim.res.both$group)>-1]))/(NROW(res.files)/100)
  # l16tab.both <- data.frame(l16tab.both[order(l16tab.both,decreasing = TRUE)])
  
  ##QF qualification ####
  qftab <- table(c(all.sim.res$team1[regexpr("^qf \\d+",all.sim.res$group)>-1],all.sim.res$team2[regexpr("^qf \\d+",all.sim.res$group)>-1]))
  qftab <- 100*qftab/(sum(qftab)/8)
  qftab <- data.frame(qftab[order(qftab,decreasing = TRUE)])
  # qftab.grprs <- table(c(all.sim.res.grprs$team1[regexpr("^qf \\d+",all.sim.res.grprs$group)>-1],all.sim.res.grprs$team2[regexpr("^qf \\d+",all.sim.res.grprs$group)>-1]))
  # qftab.grprs <- 100*qftab.grprs/(sum(qftab.grprs)/8)
  # qftab.grprs <- data.frame(qftab.grprs[order(qftab.grprs,decreasing = TRUE)])
  # qftab.kors <- table(c(all.sim.res.kors$team1[regexpr("^qf \\d+",all.sim.res.kors$group)>-1],all.sim.res.kors$team2[regexpr("^qf \\d+",all.sim.res.kors$group)>-1]))/(NROW(res.files)/100)
  # qftab.kors <- data.frame(qftab.kors[order(qftab.kors,decreasing = TRUE)])
  # qftab.both <- table(c(all.sim.res.both$team1[regexpr("^qf \\d+",all.sim.res.both$group)>-1],all.sim.res.both$team2[regexpr("^qf \\d+",all.sim.res.both$group)>-1]))/(NROW(res.files)/100)
  # qftab.both <- data.frame(qftab.both[order(qftab.both,decreasing = TRUE)])

  ##sf qualification ####
  sftab <- table(c(all.sim.res$team1[regexpr("^sf \\d+",all.sim.res$group)>-1],all.sim.res$team2[regexpr("^sf \\d+",all.sim.res$group)>-1]))
  sftab <- 100*sftab/(sum(sftab)/4)
  sftab <- data.frame(sftab[order(sftab,decreasing = TRUE)])
  # sftab.grprs <- table(c(all.sim.res.grprs$team1[regexpr("^sf \\d+",all.sim.res.grprs$group)>-1],all.sim.res.grprs$team2[regexpr("^sf \\d+",all.sim.res.grprs$group)>-1]))
  # sftab.grprs <- 100*sftab.grprs/(sum(sftab.grprs)/4)
  # sftab.grprs <- data.frame(sftab.grprs[order(sftab.grprs,decreasing = TRUE)])
  # sftab.kors <- table(c(all.sim.res.kors$team1[regexpr("^sf \\d+",all.sim.res.kors$group)>-1],all.sim.res.kors$team2[regexpr("^sf \\d+",all.sim.res.kors$group)>-1]))/(NROW(res.files)/100)
  # sftab.kors <- data.frame(sftab.kors[order(sftab.kors,decreasing = TRUE)])
  # sftab.both <- table(c(all.sim.res.both$team1[regexpr("^sf \\d+",all.sim.res.both$group)>-1],all.sim.res.both$team2[regexpr("^sf \\d+",all.sim.res.both$group)>-1]))/(NROW(res.files)/100)
  # sftab.both <- data.frame(sftab.both[order(sftab.both,decreasing = TRUE)])

  ##f qualification ####
  ftab <- table(c(all.sim.res$team1[regexpr("Final",all.sim.res$group)>-1],all.sim.res$team2[regexpr("Final",all.sim.res$group)>-1]))
  ftab <- 100*ftab/(sum(ftab)/2)
  ftab <- data.frame(ftab[order(ftab,decreasing = TRUE)])
  # ftab.grprs <- table(c(all.sim.res.grprs$team1[regexpr("Final",all.sim.res.grprs$group)>-1],all.sim.res.grprs$team2[regexpr("Final",all.sim.res.grprs$group)>-1]))
  # ftab.grprs <- 100*ftab.grprs/(sum(ftab.grprs)/2)
  # ftab.grprs <- data.frame(ftab.grprs[order(ftab.grprs,decreasing = TRUE)])
  # ftab.kors <- table(c(all.sim.res.kors$team1[regexpr("^f \\d+",all.sim.res.kors$group)>-1],all.sim.res.kors$team2[regexpr("^f \\d+",all.sim.res.kors$group)>-1]))/(NROW(res.files)/100)
  # ftab.kors <- data.frame(ftab.kors[order(ftab.kors,decreasing = TRUE)])
  # ftab.both <- table(c(all.sim.res.both$team1[regexpr("^f \\d+",all.sim.res.both$group)>-1],all.sim.res.both$team2[regexpr("^f \\d+",all.sim.res.both$group)>-1]))/(NROW(res.files)/100)
  # ftab.both <- data.frame(ftab.both[order(ftab.both,decreasing = TRUE)])
  
  elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,wintab,by.x="team1",by.y="Var1",all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,wintab.grprs,by.x="team1",by.y="Var1",suffixes=c("",".grp"),all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,wintab.kors,by.x="team1",by.y="Var1",suffixes=c("",".ko"),all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,wintab.both,by.x="team1",by.y="Var1",suffixes=c("",".both"),all.x=TRUE)

  elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,l16tab,by.x="team1",by.y="Var1",suffixes=c("",".l16"),all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,l16tab.grprs,by.x="team1",by.y="Var1",suffixes=c("",".l16.grp"),all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,l16tab.kors,by.x="team1",by.y="Var1",suffixes=c("",".l16.ko"),all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,l16tab.both,by.x="team1",by.y="Var1",suffixes=c("",".l16.both"),all.x=TRUE)

  elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,qftab,by.x="team1",by.y="Var1",suffixes=c("",".qf"),all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,qftab.grprs,by.x="team1",by.y="Var1",suffixes=c("",".qf.grp"),all.x=TRUE)

  elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,sftab,by.x="team1",by.y="Var1",suffixes=c("",".sf"),all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,sftab.grprs,by.x="team1",by.y="Var1",suffixes=c("",".sf.grp"),all.x=TRUE)

  elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,ftab,by.x="team1",by.y="Var1",suffixes=c("",".f"),all.x=TRUE)
  # elo.ratings.pre.eu <- merge(elo.ratings.pre.eu,ftab.grprs,by.x="team1",by.y="Var1",suffixes=c("",".f.grp"),all.x=TRUE)
  
  # elo.ratings.pre.eu[elo.ratings.pre.eu$team1=="austria",4:8] <- elo.ratings.pre.eu[elo.ratings.pre.eu$team1=="austria",4:8]+elo.ratings.pre.eu[elo.ratings.pre.eu$team1=="poland",4:8]
  # elo.ratings.pre.eu[elo.ratings.pre.eu$team1=="poland",4:8] <- c(0,0,0,0,0)

  elo.ratings.pre.eu <- elo.ratings.pre.eu[order(elo.ratings.pre.eu$elostrength1,decreasing = TRUE),]
  
  # elo.ratings.pre.eu$points <- 0
  # elo.ratings.pre.eu$points[elo.ratings.pre.eu$team1 %in% c("wales","germany",
  #                                                           "cameroon","tunisia","denmark",
  #                                                           "south korea","uruguay",
  #                                                           "mexico","serbia")] <- 1
  # elo.ratings.pre.eu$points[elo.ratings.pre.eu$team1 %in% c("usa")] <- 2
  # elo.ratings.pre.eu$points[elo.ratings.pre.eu$team1 %in% c("senegal","iran","argentina","saudi arabia",
  #                                                           "australia","japan","costa rica",
  #                                                           "belgium","switzerland","ghana")] <- 3
  # elo.ratings.pre.eu$points[elo.ratings.pre.eu$team1 %in% c("netherlands","ecuador","england",
  #                                                           "poland","spain","croatia",
  #                                                           "morocco")] <- 4
  # elo.ratings.pre.eu$points[elo.ratings.pre.eu$team1 %in% c("brazil","portugal","france")] <- 6
  
  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/world-cup-sim-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq,
       ylab="Probability of Winning World Cup",type="p",pch=16,
       xlab="Elo rating before tournament",
       main="Team strengths and chances of winning World Cup")#,
       # ylim=range(0,max(elo.ratings.pre.eu[,c(4,8)],na.rm=TRUE)))
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.grp,col=2,type="p",pch=16,cex=0.75)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.ko,col=3,type="p",pch=16,cex=0.75)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.both,col=4,type="p",pch=16,cex=0.75)
  text(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq, labels=to3(elo.ratings.pre.eu$team1), cex=0.5, font=2,pos=4)
  abline(v=elo.ratings.pre.eu$elostrength1,lty=3,col="grey")
  legend("topleft",col=1:4,pch=16,legend=c("As is","Randomised Groups","Randomised Knock Out Stages","Both"))
  dev.off()

  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/world-cup-sim-",Sys.Date(),"-l16.jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq,
       ylab="Probability of Reaching World Cup L16",type="p",pch=16,
       xlab="Elo rating before tournament",
       main="Team strengths and chances of qualifying for World Cup L16",
       ylim=range(0,max(elo.ratings.pre.eu[,3:4],na.rm=TRUE))) #7:10
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.l16.grp,col=2,type="p",pch=16,cex=0.75)
  text(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq, labels=to3(elo.ratings.pre.eu$team1), cex=0.5, font=2,pos=4)
  abline(v=elo.ratings.pre.eu$elostrength1,lty=3,col="grey")
  legend("topleft",col=1:2,pch=16,legend=c("As is","Randomised Groups"))
  dev.off()

  plot(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq,
       ylab="Probability of Winning Euros",type="p",pch=16,
       xlab="Elo rating before tournament",
       main="Team strengths and chances of winning Euros",
       ylim=range(0,100))
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.f,col=2,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.sf,col=3,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.qf,col=4,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.l16,col=5,type="p",pch=16)

  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/euros-sim-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq,
       ylab="Probability",type="p",pch=16,
       xlab="Elo rating before tournament",
       main="Team strengths and chances at the Euros",
       #sub="Circles with actual groups, dots with randomised groups",
       ylim=range(0,100))
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.f,col=2,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.sf,col=3,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.qf,col=4,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.l16,col=5,type="p",pch=16)
  abline(v=elo.ratings.pre.eu$elostrength1,lty=3,col="grey")
  abline(h=c(0,20,40,60,80,100),lty=3,col="grey")
  text(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.l16, labels=to3(elo.ratings.pre.eu$team1), cex=0.5, font=2,pos=4)
  # legend("topleft",pch=16,col=1:2,ncol = 2,
  #        legend=c("Win","Final"),bty="n")
  # legend("topleft",pch=16,col=1:4,ncol = 2,
  #        legend=c("Win","Final","Semi Finals","Quarter Finals"),bty="n")
  legend("topleft",pch=16,col=1:5,ncol = 1,
         legend=c("Win","Final","Semi Finals","Quarter Finals","Last 16"),bty="n")
  dev.off()
  
  write.csv(elo.ratings.pre.eu,paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/euros-sim-summary-",Sys.Date(),".csv"))

  # xtable(elo.ratings.pre.eu[,c("team1","elostrength1","points","Freq.l16","Freq.qf","Freq.sf","Freq.f","Freq")])
  # xtable(elo.ratings.pre.eu[,c("team1","elostrength1","Freq.qf","Freq.sf","Freq.f","Freq")])
  # write.csv(elo.ratings.pre.eu[,c("team1","elostrength1","points","Freq.l16","Freq.qf","Freq.sf","Freq.f","Freq")],
  #           paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/worldcup-sim-summary2-",Sys.Date(),".csv"))
  
  #for paper
  print(xtable(elo.ratings.pre.eu[,c("team1","elostrength1","elo.now","Freq.l16","Freq.qf","Freq.sf","Freq.f","Freq")],digits=c(0,0,0,0,1,1,1,1,1)), include.rownames=FALSE)
  #adjusted for submission
  elo.ratings.pre.eu$P.Grp <- (100-elo.ratings.pre.eu$Freq.l16)/100
  elo.ratings.pre.eu$P.L16 <- (elo.ratings.pre.eu$Freq.l16 - elo.ratings.pre.eu$Freq.qf)/100
  elo.ratings.pre.eu$P.QF <- (elo.ratings.pre.eu$Freq.qf - elo.ratings.pre.eu$Freq.sf)/100
  elo.ratings.pre.eu$P.SF <- (elo.ratings.pre.eu$Freq.sf - elo.ratings.pre.eu$Freq.f)/100
  elo.ratings.pre.eu$P.F <- (elo.ratings.pre.eu$Freq.f - elo.ratings.pre.eu$Freq)/100
  elo.ratings.pre.eu$P.F[is.na(elo.ratings.pre.eu$Freq)==TRUE] <- elo.ratings.pre.eu$Freq.f[is.na(elo.ratings.pre.eu$Freq)==TRUE]/100
  elo.ratings.pre.eu$P.win <- (elo.ratings.pre.eu$Freq)/100
  print(xtable(elo.ratings.pre.eu[,c("team1","elostrength1","elo.now","P.Grp","P.L16","P.QF","P.SF","P.F","Freq")],digits=c(0,0,0,0,1,1,1,1,1,1)), include.rownames=FALSE)
  
  xtable(elo.ratings.pre.eu[,c("team1","elostrength1","elo.now","Freq.sf","Freq.f","Freq")])
  write.csv(elo.ratings.pre.eu[,c("team1","elostrength1","elo.now","Freq.sf","Freq.f","Freq")],
            paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/worldcup-sim-summary2-",Sys.Date(),".csv"))
  
  gc.mod.comp <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/worldcup-sim-summary2-2022-11-29.csv",stringsAsFactors = FALSE)
  gc.mod.comp$Gracenote <- as.numeric(gsub("%","",gc.mod.comp$Gracenote))

  plot(gc.mod.comp$Freq.l16,gc.mod.comp$Gracenote,pch=16,col=4,ylim=range(0,105),
       main="Comparison of Gracenote and Model",
       ylab="Gracenote Probability",xlab="Model Probability")
  text(gc.mod.comp$Freq.l16,gc.mod.comp$Gracenote, labels=to3(gc.mod.comp$team1), cex=0.5, font=2,pos=3)
  abline(0,1,lty=3)
  
  jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Correct-score/tex/worldcup-sim-random-",Sys.Date(),".jpg"),
       width = 8, height = 5, units = "in", res=300)
  plot(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.grp,
       ylab="Probability",type="p",pch=16,
       xlab="Elo rating before tournament",
       main="Team strengths and chances at the World Cup",
       sub="Circles with actual groups, dots with randomised groups",
       ylim=range(0,100))
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq,type="p",pch=1)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.f.grp,col=2,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.f,col=2,type="p",pch=1)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.sf.grp,col=3,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.sf,col=3,type="p",pch=1)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.qf.grp,col=4,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.qf,col=4,type="p",pch=1)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.l16.grp,col=5,type="p",pch=16)
  lines(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.l16,col=5,type="p",pch=1)
  abline(v=elo.ratings.pre.eu$elostrength1,lty=3,col="grey")
  abline(h=c(0,20,40,60,80,100),lty=3,col="grey")
  text(elo.ratings.pre.eu$elostrength1,elo.ratings.pre.eu$Freq.l16, labels=to3(elo.ratings.pre.eu$team1), cex=0.5, font=2,pos=3)
  legend("topleft",pch=16,col=1:5,legend=c("Win","Final","Semi Finals","Quarter Finals","Last 16"),bty="n")
  dev.off()
  
  all.grps.grprs <- merge(all.grps.grprs,elo.ratings.pre.eu[,c("team1","elostrength1")],
                          by.x=c("team"),by.y=c("team1"),all.x=TRUE)
  ecuador.grps0 <- all.grps.grprs[all.grps.grprs$team=="ecuador",c("X","replication","elostrength1")]
  ecuador.grps0$group <- substr(ecuador.grps0$X,1,1)
  ecuador.grps0$group.rep <- paste0(ecuador.grps0$group,ecuador.grps0$replication)
  all.grps.grprs$group <- substr(all.grps.grprs$X,1,1)
  all.grps.grprs$group.rep <- paste0(all.grps.grprs$group,all.grps.grprs$replication)
  ecuador.grps <- all.grps.grprs[all.grps.grprs$group.rep %in% ecuador.grps0$group.rep,]
  ecuador.grps.elo <- aggregate(ecuador.grps$elostrength1,by=list(ecuador.grps$replication),FUN=mean)
  all.grps.grprs.elo <- aggregate(all.grps.grprs$elostrength1,by=list(all.grps.grprs$group.rep),FUN=mean)
}

