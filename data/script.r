setwd("~/Homework/Winter 2015/Capestone/assignment/data")
politics<-read.csv("politics.csv")
head((politics$party=="independent")&
        (politics$sex!="female"))


```{r}
pols<-read.csv("politics.csv")
```


```{r}
str(pols)
```

hist(incomes)
politics<-read.csv("politics.csv")
str(politics)

head(politics[,2])
tail(politics$income)

politics[28,7]
politics$income[28]

a<-c(1:3,12,61, (length(politics$income)-2):length(politics$income))
politics$income[a]

head(politics$party)
head(politics$party=="independent")

head((politics$party=="independent")&(politics$sex!="female"))

head(politics$income[politics$testtime=="pre"])

head(politics$subject[politics$testtime=="pre"]==politics$subject[politics$testtime=="post"])
sum(1:3)
sum(!c(TRUE,TRUE,FALSE,FALSE,FALSE))
trues<-politics$testtime=="pre"
sum(!(politics$subject[trues]==politics$subject[!trues]))

sum(!(politics$party[trues]==politics$party[!trues]))
sum(!(politics$subject[trues]==politics$subject[!trues]))
sum(!(politics$sex[trues]==politics$sex[!trues]))
sum(!(politics$income[trues]==politics$income[!trues]))
politics$subject<-factor(politics$subject)

politics$testtime<-factor(politics$testtime, levels=c("pre", "post"))

str(politics)

incomes<-politics$income[trues]
summary(incomes)


var(incomes)
sd(incomes)
sd(incomes)==sqrt(var(incomes))
biased<-sum((incomes-mean(incomes))^2)/length(incomes)
unbiased<-sum((incomes-mean(incomes))^2)/(length(incomes)-1)
var(incomes)==biased

var(incomes)==unbiased

sem<-sd(incomes)/sqrt(length(incomes))

hist(incomes)
install.packages("dplyr")
install.packages("ggplot2")
install.packages("gplots")

library("dplyr")
library("ggplot2")
library("gplots")

pre=politics[trues,]
post=politics[!trues,]

mytable<-table(pre$party,pre$minwage)
mytable

write.table(mytable,"clipboard",sep="\t",col.names=NA)
margin.table(mytable,1) #Row margins
margin.table(mytable,2) #Column margins
chisq.test(pre$party,pre$minwage)

Test name/symbol(degrees of freedom) = test value, p=p-value
cs<-chisq.test(pre$party,pre$minwage)
t.test(pre$optimism[pre$sex=="male"]),pre$optimism[pre$sex=="female"],paired-FALSE, var.equal-FALSE,alternative="two.sided",conf.level=.95)
t.test(pre$optimism[pre$sex=="male"],
        pre$optimism[pre$sex=="female"],
        paired=FALSE, var.equal=FALSE,
        alternative="two.sided",
        conf.level=.95)

t.test(politics$optimism[(politics$testtime=="pre")&
                            (politics$party=="republican")],
        politics$optimism[(politics$testtime=="post")&
                              (politics$party=="republican")],
        paired=TRUE, conf.level=1-.05/3)
