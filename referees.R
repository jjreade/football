sb.matches0 <- load.sb.detail()
efl.nl <- read.csv("~/Dropbox/Research/Sport/OAFC/data/results-updated.csv",stringsAsFactors = FALSE)

sb.matches0$yellow1 <- as.numeric(regexpr("yellow",sb.matches0$player4)>-1 & regexpr("team1",sb.matches0$info.type)>-1)
sb.matches0$yellow2 <- as.numeric(regexpr("yellow",sb.matches0$player1)>-1 & regexpr("team2",sb.matches0$info.type)>-1)
sb.matches0$red <- as.numeric(regexpr("sending",sb.matches0$player0)>-1)
sb.matches0$red1 <- as.numeric(regexpr("sending",sb.matches0$player0)>-1 & regexpr("team1",sb.matches0$info.type)>-1)
sb.matches0$red2 <- as.numeric(regexpr("sending",sb.matches0$player0)>-1 & regexpr("team2",sb.matches0$info.type)>-1)
sb.matches0$red2.timing <- as.numeric(gsub("^.*?[(].*?s/o (\\d+)[)].*?$","\\1",sb.matches0$player4))
sb.matches0$red1.timing <- as.numeric(gsub("^.*?[(].*?s/o (\\d+)[)].*?$","\\1",sb.matches0$player1))
sb.matches0$red1.h1 <- as.numeric(sb.matches0$red1 & sb.matches0$red1.timing<46)
sb.matches0$red2.h1 <- as.numeric(sb.matches0$red2 & sb.matches0$red2.timing<46)

sb.matches0$referee.name <- gsub("^.*?Go to (.*?) referee profile.*?$","\\1",sb.matches0$referee)
sb.matches0$referee.id <- gsub("^.*?eferee_id=(\\d+) title.*?$","\\1",sb.matches0$referee)

##how often does a team have 1, 2, etc reds
reds.by.match <- aggregate(sb.matches0[,c("red1","red2","red1.h1","red2.h1")],by=list(match_id=sb.matches0$match.id),FUN=sum)
reds.by.match <- merge(reds.by.match,res0[,c("match_id","date","div_id","division","season","team1","team1_id","goals1","goals2","team2_id","team2","outcomeH","outcomeA")],
                       by="match_id",all.x=TRUE)

summary(reds.by.match$outcomeH[reds.by.match$red1==0])
summary(reds.by.match$outcomeH[reds.by.match$red1==1])
summary(reds.by.match$outcomeH[reds.by.match$red1.h1==1])
summary(reds.by.match$outcomeH[reds.by.match$red1==2])
summary(reds.by.match$outcomeH[reds.by.match$red1.h1==2])
summary(reds.by.match$outcomeA[reds.by.match$red1==0])
summary(reds.by.match$outcomeA[reds.by.match$red1==1])
summary(reds.by.match$outcomeA[reds.by.match$red1.h1==1])
summary(reds.by.match$outcomeA[reds.by.match$red1==2])
summary(reds.by.match$outcomeA[reds.by.match$red1.h1==2])

summary(reds.by.match$outcomeA[reds.by.match$red2==0])
summary(reds.by.match$outcomeA[reds.by.match$red2==1])
summary(reds.by.match$outcomeA[reds.by.match$red2.h1==1])


##want timings also
reds.by.time <- sb.matches0[sb.matches0$red1>0 | sb.matches0$red2>0,c("match.id","red1","red2","red2.timing","red1.timing")]
reds.by.time <- reds.by.time[order(reds.by.time$match.id,reds.by.time$red1.timing),]
reds.by.time$cred1 <- ave(reds.by.time$red1,by=reds.by.time$match.id,FUN=cumsum)
reds.by.time <- reds.by.time[order(reds.by.time$match.id,reds.by.time$red2.timing),]
reds.by.time$cred2 <- ave(reds.by.time$red2,by=reds.by.time$match.id,FUN=cumsum)
reds.by.time <- merge(reds.by.time,res0[,c("match_id","date","div_id","division","season","team1","team1_id","goals1","goals2","team2_id","team2","outcomeH","outcomeA")],
                      by.x="match.id",by.y="match_id",all.x=TRUE)

reds.by.time$resource.loss1 <- 90 - reds.by.time$red1.timing
reds.by.time$resource.loss2 <- 90 - reds.by.time$red2.timing

red.resource <- aggregate(reds.by.time[,c("resource.loss1","resource.loss2")],by=list(match.id=reds.by.time$match.id),FUN=sum,na.rm=TRUE)

red.resource <- merge(red.resource,res0[,c("match_id","date","div_id","division","season","team1","team1_id","goals1","goals2","team2_id","team2","outcomeH","outcomeA")],
                      by.x="match.id",by.y="match_id",all.x=TRUE)

summary(felm(outcomeH ~ resource.loss1 + resource.loss2 | team1 + team2 + season,data=red.resource))

red.resource <- merge(red.resource,efl.nl[,c("match_id","elorating1","elorating2","elopredict")],
                      by.x="match.id",by.y="match_id",all.x=TRUE)

summary(felm(outcomeH ~ elopredict + resource.loss1 + resource.loss2 | team1 + team2 + season,data=red.resource))
summary(felm(outcomeH ~ elorating1 + resource.loss1 + elorating2 + resource.loss2 | team1 + team2 + season,data=red.resource))

write.csv(reds.by.match,"~/Dropbox/Research/Sport/github/football/reds-by-match.csv")
write.csv(reds.by.time,"~/Dropbox/Research/Sport/github/football/reds-by-time.csv")
write.csv(red.resource,"~/Dropbox/Research/Sport/github/football/red-resource.csv")

##match summaries for cards
match.card.summary <- aggregate(sb.matches0[,c("yellow1","yellow2","red1","red2")],by=list(match_id=sb.matches0$match.id,
                                                                                           referee.name=sb.matches0$referee.name,
                                                                                           referee.id=sb.matches0$referee.id),
                                FUN=sum)
match.card.summary <- merge(match.card.summary,res0[,c("match_id","date","div_id","division","season","team1","team1_id","goals1","goals2","team2_id","team2","outcomeH","outcomeA")],
                            by="match_id",all.x=TRUE)

match.card.summary <- merge(match.card.summary,efl.nl[,c("match_id","elorating1","elorating2","elopredict")],
                            by="match_id",all.x=TRUE)

summary(ref.yellow1.reg0 <- lm(yellow1 ~ elopredict,data=match.card.summary))
summary(ref.yellow2.reg0 <- lm(yellow2 ~ elopredict,data=match.card.summary))
summary(ref.yellow1.reg <- lfe::felm(yellow1 ~ elopredict | referee.name + team1 + team2 + season,data=match.card.summary))
summary(ref.yellow2.reg <- lfe::felm(yellow2 ~ elopredict | referee.name + team1 + team2 + season,data=match.card.summary))

summary(ref.red1.reg0 <- lm(red1 ~ elopredict,data=match.card.summary))
summary(ref.red2.reg0 <- lm(red2 ~ elopredict,data=match.card.summary))
summary(ref.red1.reg <- lfe::felm(red1 ~ elopredict | referee.name + team1 + team2 + season,data=match.card.summary))
match.card.summary$referee.name.team1 <- paste0(match.card.summary$team1,"-",match.card.summary$referee.name)
summary(ref.red1.reg <- lfe::felm(red1 ~ elopredict | referee.name.team1 + team2 + season,data=match.card.summary))
fixed.effects <- getfe(ref.red1.reg)
summary(ref.red2.reg <- lfe::felm(red2 ~ elopredict | referee.name + team1 + team2 + season,data=match.card.summary))

##want by team (with column for home/away)
match.card.summaryh <- match.card.summary[,1:22]
match.card.summaryh$venue <- "home"
colnames(match.card.summaryh) <- gsub("2","opp",gsub("1","",colnames(match.card.summaryh)))
match.card.summarya <- match.card.summary[,1:22]
match.card.summarya$venue <- "away"
colnames(match.card.summarya) <- gsub("1","opp",gsub("2","",colnames(match.card.summarya)))

match.card.summary.teams <- rbind(match.card.summaryh,match.card.summarya)

summary(lm(yellow ~ venue,data=match.card.summary.teams))
summary(lm(red ~ venue,data=match.card.summary.teams))

match.card.summary.teams$referee.team <- paste0(match.card.summary.teams$team,"-",match.card.summary.teams$referee.name)

summary(red.team.reg <- felm(red ~ venue + elopredict | referee.team + season,data=match.card.summary.teams))

