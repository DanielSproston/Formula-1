


library(ggplot2)
library(readr)
library("Hmisc")
library(Rmisc)
library(pgirmess)
setwd("~/R")
Constructors <- read.csv("F1 Constructors.csv")
Constructors$Nationality <- as.factor(Constructors$Nationality)
Constructors$Races.Entered <- as.numeric(Constructors$Races.Entered)
Constructors$Seasons.Competed <- as.numeric(Constructors$Seasons.Competed)
Constructors$No.Starts <- as.numeric(Constructors$No.Starts)
Constructors$X..No.Start <- as.numeric(Constructors$X..No.Start)
Constructors$Engine.Builders <- as.numeric(Constructors$Engine.Builders)
Constructors$X..Engines..E.per.Season. <- as.numeric(Constructors$X..Engines..E.per.Season.)
Constructors$Drivers <- as.numeric(Constructors$Drivers)
Constructors$X..Drivers.per.race <- as.numeric(Constructors$X..Drivers.per.race)
Constructors$Models <- as.numeric(Constructors$Models)
Constructors$X..Models.per.season <- as.numeric(Constructors$X..Models.per.season)
Constructors$Wins <- as.numeric(Constructors$Wins)
Constructors$X..Wins.per.race <- as.numeric(Constructors$X..Wins.per.race)
Constructors$Podiums <- as.numeric(Constructors$Podiums)
Constructors$X..Podiums.per.race <- as.numeric(Constructors$X..Podiums.per.race)
Constructors$Pole.Positions <- as.numeric(Constructors$Pole.Positions)
Constructors$X..Pole.per.race <- as.numeric(Constructors$X..Pole.per.race)
Constructors$X1.2.Finishes <- as.numeric(Constructors$X1.2.Finishes)
Constructors$X..1.2.per.race <- as.numeric(Constructors$X..1.2.per.race)
Constructors$Fastest.Laps <- as.numeric(Constructors$Fastest.Laps)
Constructors$X..Fastest.lap.per.race <- as.numeric(Constructors$X..Fastest.lap.per.race)
Constructors$Nominal.Points <- as.numeric(Constructors$Nominal.Points)
Constructors$X..nominal.Points.per.race <- as.numeric(Constructors$X..nominal.Points.per.race)
Constructors$Standardised.Points <- as.numeric(Constructors$Standardised.Points)
Constructors$X..Standard.points.per.race <- as.numeric(Constructors$X..Standard.points.per.race)
Constructors$Km.raced <- as.numeric(Constructors$Km.raced)
Constructors$Km.led <- as.numeric(Constructors$Km.led)
Constructors$X..Km.led <- as.numeric(Constructors$X..Km.led)
Constructors$Average.Start.Position <- as.numeric(Constructors$Average.Start.Position)
Constructors$Average.Finishing.position <- as.numeric(Constructors$Average.Finishing.position)
Constructors$Activity <- as.factor(Constructors$Activity)
Constructors$..Constructor <- as.factor(Constructors$..Constructor)

ggplot(Constructors, aes(x = Constructors$X..Standard.points.per.race, y = Constructors$Average.Finishing.position, color = Constructors$..Constructor)) +
geom_point(size = 2, stat = "identity") +
geom_text(aes(label=Constructors$..Constructor),hjust=0, vjust=0, size = 3) +
scale_y_reverse() +
scale_color_manual(values = c("#99CCFF", "#990000","#000033", "#FF9933", "#FFFF99", "white", "green", "#000066", "dark blue",
"#CCFF33", "dark green", "#CCFFCC", "#009900", "#336600", "#FF0033", "dark blue", "orange", "red", "yellow", "#990066",
"#FF66CC", "#33CCFF", "#996600", "white", "white", "#666600", "#FF3333", "#006633", "yellow", "#CC0000", "#000099",
"#0099FF", "#0033CC", "white", "black", "#0099CC", "#990000", "#0033CC", "#FF6600", "#00FF99", "#333333", "#FF00CC",
"#3399FF", "#000033", "#666666", "#CCCCCC", "#003399", "pink", "dark blue", "yellow", "#6699FF", "#333333", "black",
"#3399FF", "white", "orange", "#3399CC", "red", "white", "#000099", "red", "#0000FF", "dark green", "white",
"gold", "red", "red")) +
theme(panel.background = element_rect(fill = "light grey",colour = "white",
size = 0.5, linetype = "solid")) +
theme( axis.line = element_line(colour = "black", size = 1, linetype = "solid")) +
theme(text = element_text(size = 5), axis.text.y = element_text(size = 10),
axis.text.x = element_text(size = 10), legend.title = element_text(size = 15),
legend.text = element_text(size = 5), plot.title = element_text(size = 10),
axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10)) +
labs(x = "Points per race",
y = "Finishing Position",
title = "Whose the best",
color = "constructor")

ggplot(Constructors, aes(x="", y=Constructors$Standardised.Points, fill= Constructors$..Constructor)) +
geom_bar(stat="identity", width=1) +
scale_fill_manual(values = c("#99CCFF", "#990000","#000033", "#FF9933", "#FFFF99", "white", "green", "#000066", "dark blue",
"#CCFF33", "dark green", "#CCFFCC", "#009900", "#336600", "#FF0033", "dark blue", "orange", "red", "yellow", "#990066",
"#FF66CC", "#33CCFF", "#996600", "white", "white", "#666600", "#FF3333", "#006633", "yellow", "#CC0000", "#000099",
"#0099FF", "#0033CC", "white", "black", "#0099CC", "#990000", "#0033CC", "#FF6600", "#00FF99", "#333333", "#FF00CC",
"#3399FF", "#000033", "#666666", "#CCCCCC", "#003399", "pink", "dark blue", "yellow", "#6699FF", "#333333", "black",
"#3399FF", "white", "orange", "#3399CC", "red", "white", "#000099", "red", "#0000FF", "dark green", "white",
"gold", "red", "red")) +
coord_polar("y", start=0)

