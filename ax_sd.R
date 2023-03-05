
library(readxl)
library(data.table)
library(ggplot2)
library(lmerTest)

dat1 <- read_excel(path = "AX bread_Petros.xlsx")
setDT(dat1)
dat1 <- melt(dat1, id.vars = c("Loaf", "iD", "TS (as is)", "Moisture %"), variable.name = "Time", value.name = "sd")
dat1[, t := as.numeric(substr(Time,0,2))]
dat1[, lid := paste(iD, Loaf)]
dat1[, dough := ifelse(Loaf %in% c("L1","L2","L3"),"D1","D2")]
ggplot(dat1, aes(x=t, y=sd,col=iD, group=lid)) + geom_line(aes(group=Loaf)) + facet_wrap(~iD)
ggplot(dat1, aes(x=t, y=sd,col=iD, group=lid)) + geom_line(aes(group=lid))



ggplot(dat1[t==90], aes(x=iD, y=sd,col=iD,shape=dough)) + geom_point(size=3)

       
(lmer0 <- lmer(data=dat1[t==90] , sd ~ (1|iD/dough))) |> summary()

(lmer1 <- lmer(data=dat1[t==90] , sd ~ iD + (1|iD:dough))) |> summary()
anova(lmer1)
ranef(lmer1)

(ranef(lmer0, ) |> as.data.frame() |> as.data.table())[grpvar=="iD"]

lattice::dotplot(ranef(lmer0))

emmeans::emmeans(lmer1 , ~ iD) |> 
  as.data.table() |> 
  ggplot(aes(y=emmean,ymin=lower.CL,ymax=upper.CL,x=iD)) + 
  geom_pointrange()
emmeans::emmeans( lmer1 , trt.vs.ctrlk ~ iD , ref="Control" )


library(nlme)

SSasymp()
datNM <- remove_missing(dat1) |> as.data.frame()
nlslist1 <- nlsList( sd ~ SSasymp(t, Asym, R0, lrc), data=groupedData(sd ~ t|iD/dough/Loaf, datNM)  )

nls1 <- (coef(nlslist1) |> as.data.table(keep.rownames = "ID"))[, cbind("iD", "dough", "Loaf") := tstrsplit(ID, "/")]

ggplot(nls1, aes(x=iD, y=Asym,col=iD,shape=dough)) + geom_point(size=3)

(lmer0 <- lmer(data=nls1 , Asym ~ (1|iD/dough))) |> summary()

(lmer2 <- lmer(data=nls1 , Asym ~ iD + (1|iD:dough))) |> summary()
anova(lmer2)

lattice::dotplot(ranef(lmer0))


(lmer2 <- lmer(data=nls1[iD!=61] , Asym ~ iD + (1|iD:dough))) |> summary()
anova(lmer2)
