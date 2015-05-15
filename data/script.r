setwd("~/Homework/Winter 2015/Capestone/assignment/data")
politics<-read.csv("politics.csv")
head((politics$party=="independent")&
        (politics$sex!="female"))
