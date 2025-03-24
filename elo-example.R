##download the data from: https://www.dropbox.com/scl/fi/7it51bp114cv2w33y4wif/eloratings-1888-2025.csv?rlkey=8rkcs868csrstxkjljxc7nl0m&dl=0
elo <- read.csv("/Users/jjreade/Dropbox/Research/Sport/Correct-score/data/eloratings-1888-2025.csv",stringsAsFactors = FALSE)


jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Attendance/tex/elo-oldham-man-city-",Sys.Date(),".jpg"),
     width = 8, height = 5, units = "in", res=300)
plot(elo$date[elo$team=="Manchester City" & elo$date!="2023-06-10"],
     elo$elostrength[elo$team=="Manchester City" & elo$date!="2023-06-10"],
     col="skyblue",type="o",pch=16,cex=0.5,ylab="Elo rating",xlab="Date")
lines(elo$date[elo$team=="Manchester United"],elo$elostrength[elo$team=="Manchester United"],
      col="red",type="o",pch=16,cex=0.5)
lines(elo$date[elo$team=="Oldham Athletic"],elo$elostrength[elo$team=="Oldham Athletic"],
      col="blue",type="o",pch=16,cex=0.5)
lines(elo$date[elo$team=="Everton"],elo$elostrength[elo$team=="Everton"],
      col="darkblue",type="o",pch=16,cex=0.5)
lines(elo$date[elo$team=="Chelsea"],elo$elostrength[elo$team=="Chelsea"],
      col="slateblue",type="o",pch=16,cex=0.5)
lines(elo$date[elo$team=="Sutton United"],elo$elostrength[elo$team=="Sutton United"],
      col="yellow",type="o",pch=16,cex=0.5)
legend("topleft",col=c("skyblue","red","blue","darkblue","slateblue","yellow"),lty=1,pch=16,
       bty="n",legend=c("Man City","Man United","Oldham","Everton","Chelsea","Sutton"))
dev.off()


jpeg(paste0("/Users/jjreade/Dropbox/Research/Sport/Elo-ratings/tex/oafc-elo-plot-1908-",Sys.Date(),".jpg"),
     width = 8, height = 5, units = "in", res=300)
plot(elo$date[elo$date<"1908-08-01" & elo$team=="Oldham Athletic"],
     elo$elostrength[elo$date<"1908-08-01" & elo$team=="Oldham Athletic"],type="l",col="blue",
     ylab="Date",xlab="Elo rating",
     main="OAFC Elo rating Oct 1905 to Apr 1908",
     sub=paste0("Played: ",NROW(elo[elo$date<"1908-08-01" & elo$team=="Oldham Athletic",]),
                ", W",sum(as.numeric(elo$date<"1908-08-01" & elo$outcome==1 & elo$team=="Oldham Athletic")),
                ", D",sum(as.numeric(elo$date<"1908-08-01" & elo$outcome==0.5 & elo$team=="Oldham Athletic")),
                ", W",sum(as.numeric(elo$date<"1908-08-01" & elo$outcome==0 & elo$team=="Oldham Athletic"))))
lines(elo$date[elo$date<"1908-08-01" & elo$outcome==1 & elo$team=="Oldham Athletic"],
      elo$elostrength[elo$date<"1908-08-01" & elo$outcome==1 & elo$team=="Oldham Athletic"],type="p",col="blue",pch=15,cex=0.75)
lines(elo$date[elo$date<"1908-08-01" & elo$outcome==0.5 & elo$team=="Oldham Athletic"],
      elo$elostrength[elo$date<"1908-08-01" & elo$outcome==0.5 & elo$team=="Oldham Athletic"],type="p",col="darkgreen",pch=16,cex=0.75)
lines(elo$date[elo$date<"1908-08-01" & elo$outcome==0 & elo$team=="Oldham Athletic"],
      elo$elostrength[elo$date<"1908-08-01" & elo$outcome==0 & elo$team=="Oldham Athletic"],type="p",col="red",pch=17,cex=0.75)
legend("topleft",pch=15:17,col=c("blue","darkgreen","red"),bty="n",legend=c("Win","Draw","Defeat","League"),ncol=1)
# legend("topleft",pch=15:18,col=c("blue","darkgreen","red","darkblue"),bty="n",legend=c("Win","Draw","Defeat","League"),ncol=1)
# par(new=T)
# plot(elo$date[elo$date<"1908-08-01" & elo$team=="Oldham Athletic"],
#      elo$full.pos[elo$date<"1908-08-01" & elo$team=="Oldham Athletic"],col="darkblue",
#      ylab="",xlab="",yaxt="n",xaxt="n",type="p",pch=18)
# axis(side = 4)
dev.off()
