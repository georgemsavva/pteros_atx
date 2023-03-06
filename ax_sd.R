
library(readxl)
library(data.table)
library(ggplot2)
library(lmerTest)
library(patchwork)

# Load and encode data
dat1 <- read_excel(path = "AX bread_Petros.xlsx")
setDT(dat1)
dat1 <- melt(dat1, id.vars = c("Loaf", "iD", "TS (as is)", "Moisture %"), variable.name = "Time", value.name = "sd")
dat1[, t := as.numeric(substr(Time,0,2))]
dat1[, lid := paste(iD, Loaf)]
dat1[, dough := ifelse(Loaf %in% c("L1","L2","L3"),"D1","D2")]

#*  Plot each line separate
ggplot(dat1, aes(x=t, y=sd,col=iD, group=lid)) + geom_line(aes(group=Loaf)) + facet_wrap(~iD)

#* Plot all together
ggplot(dat1, aes(x=t, y=sd,col=iD, group=lid)) + geom_line(aes(group=lid))

#* Plot 90 minute data
(plot90 <- ggplot(dat1[t==90], aes(x=iD, y=sd,col=iD,shape=dough)) + geom_point(size=3))

#* It looks like there may be a 'dough' effect.

#* Yes, if we look at the 'dough' random effect estimate it has a positive variance.
(lmer0 <- lmer(data=dat1[t==90] , sd ~ (1|iD/dough))) |> summary()

#* Some models for testing
#* First no 'dough' effect
(lmer0b <- lmer(data=dat1[t==90] , sd ~ (1|iD)))
#* Second all doughs equal (no 'iD' effect)
(lmer0c <- lmer(data=dat1[t==90] , sd ~ (1|iD:dough)))


pbkrtest::PBmodcomp(largeModel = lmer0, smallModel = lmer0b, nsim=100)
## The effect of id is borderline significant here.
pbkrtest::PBmodcomp(largeModel = lmer0, smallModel = lmer0c, nsim=1000)


#* Fixed effect of iD, 90 min data only, random effect of dough within iD.
#* Only just statistically significant ANOVA p=0.02.
(lmer1 <- lmer(data=dat1[t==90] , sd ~ iD + (1|iD:dough))) |> summary()
anova(lmer1)


#* We can extract the random effects into a table if we want..
ranefs <- (ranef(lmer0, ) |> as.data.frame() |> as.data.table())[grpvar=="iD"]


#* This is a plot of the fixed effects and the 'shrunk' random effects.
emmeans::emmeans(lmer1 , ~ iD) |> 
  as.data.table() |> 
  ggplot(aes(y=emmean,ymin=lower.CL,ymax=upper.CL,x=iD)) + 
  geom_pointrange() + 
  geom_pointrange(data=ranefs, aes(y=64.55+condval, 
                                   x=grp, 
                                   ymin=64.55+condval-2*condsd, 
                                   ymax=64.55+condval+2*condsd), col="red")
emmeans::emmeans( lmer1 , trt.vs.ctrlk ~ iD , ref="Control" )

## Non-linear modelling

#* Now lets fix mixed models to non-linear models.
library(nlme)

#* There's a missing point that needs to be removed for some reason.
datNM <- remove_missing(dat1) |> as.data.frame()

#* Estimate the models.
nlslist1 <- nlsList( sd ~ SSasymp(t, Asym, R0, lrc), data=groupedData(sd ~ t|iD/dough/Loaf, datNM)  )

#* Add the coefficients to a data table.
nls1 <- (coef(nlslist1) |> as.data.table(keep.rownames = "ID"))[, cbind("iD", "dough", "Loaf") := tstrsplit(ID, "/")]

#* Plot the asymtotes against the 90 minute values:
asym <- ggplot(nls1, aes(x=iD, y=Asym,col=iD,shape=dough)) + geom_point(size=3)
asym + plot90 + plot_layout(guides="collect")

#* Redo the linear models:
(lmerNL_0 <- lmer(data=nls1 , Asym ~ (1|iD/dough))) |> summary()
(lmerNL_0b <- lmer(data=nls1 , Asym ~ (1|iD:dough))) |> summary()

(lmerNL_2 <- lmer(data=nls1 , Asym ~ iD + (1|iD:dough))) |> summary()

#* More statistical significance for this model..
anova(lmerNL_2)

#* More clearly significant here as well (via testing random effects).
pbkrtest::PBmodcomp(largeModel = lmerNL_0, smallModel = lmerNL_0b, nsim=1000)


#* So it looks like using the non-linear modelling might be better?
#* How could we make this judgement?  The reliability?
#* 

#* Anyway, lets look at the relationship with other parameters:
#* 

dat2 <- read_excel(path="ATC baking DATA for 23_XXXX (Great British High Fibre Loaf 2022 crop wheat trials) AL.xlsx", sheet="forR", skip=3)

library(psych)

pca1 <- principal(dat2[,4:30],nfactors = 4, rotate="varimax")
plot(pca1)
summary(pca1)
pca1$values |> plot()

pca1$loadings

fa1 <- fa(dat2[,4:30],nfactors = 4, rotate="varimax")
