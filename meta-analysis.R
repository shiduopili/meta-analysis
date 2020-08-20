##data about author, ID, outcome, comparison, effect size etc. were previously calculated using Comprehensive Meta-Analysis

set.seed(101000)
names(data)
##change the column names to R-required format; 
names(data)[1:12] <-
  c(
    "id",
    "comparison",
    "outcome",
    "Type",
    "seTE",
    "TE",
    "##could also add some moderators here if needed"
  )


#big group: six outcomes
data_socialsupport <-
  data %>% filter(bigbigoutcome == "Social support")
data_generalhealth <-
  data %>% filter(bigbigoutcome == "General health")
data_hblifestyle <-
  data %>% filter(bigbigoutcome == "HB and Lifestyle")
data_mentaldisorder <-
  data %>% filter(bigbigoutcome == "Negative affect")
data_physicalillness <-
  data %>% filter(bigbigoutcome == "Physical illness")
data_swb <- data1 %>% filter(bigbigoutcome == "SWB")

install.packages("metafor")
install.packages("meta")
library(meta)
library(metafor)
library(dplyr)


#effect size six outcomes----I just put two examples here. "data" can be subsitituted to other outcomes. 
##social support
metagen_fitss <-
  metagen(
    TE,
    seTE,
    data = data_socialsupport,
    studlab = paste(id),
    tau.common = TRUE,
    method.tau = "REML",
    sm = "SMD",
    hakn = TRUE,
    comb.fixed = FALSE,
    comb.random = TRUE
  )

##lifestyle and health behaviors    
metagen_fithbls <-
  metagen(
    TE,
    seTE,
    data = data_hblifestyle,
    studlab = paste(id),
    tau.common = TRUE,
    method.tau = "REML",
    sm = "SMD",
    hakn = TRUE,
    comb.fixed = FALSE,
    comb.random = TRUE
  )



##forest plots of the three significant outcomes (one example here)
metagen_plot <-
  metagen(
    TE,
    seTE,
    data = ##significant outcome(s),
    studlab = paste(Author.y, Year, sep = ", "),
    tau.common = TRUE,
    method.tau = "REML",
    sm = "SMD",
    hakn = TRUE,
    comb.fixed = FALSE,
    comb.random = TRUE
  )


## plot the publication bias of the three outcomes(trim and fill method)
par(mfrow = c(2, 2), mar = c(5, 4, 3, 3))
tar_1 <- trimfill(metagen_##significant outcome)
funnel(tar_1, legend = TRUE)


##moderator analysis (k>=10)
###I only analyzed those outcomes with the number of studies greater than 10
modana<-list(metagen_fitss,metagen_fitsubs,metagen_fitos,metagen_fitus,
             metagen_fitna,metagen_fitpi,metagen_fitswb,metagen_fitswbhappi)
names(modana)<-c("Social support","Subjective social support","Objective social support",
                 "Utilization of social support","Negative affect","Fewer physical illness",
                 "Subjective well-being","Happiness")


egger<-lapply(all,function(x) metabias(x,k.min = 3))  ##publication bias 
trifil<-lapply(all,function(x) trimfill(x))
trifil_te<-stack(sapply(trifil,"[","TE.random"))
trifil_ll<-stack(sapply(trifil,"[","lower.random"))
trifil_hl<-stack(sapply(trifil,"[","upper.random"))
trifil_p<-stack(sapply(trifil,"[","pval.random"))
trifil_Q<-stack(sapply(trifil,"[","Q"))
trifil_i2<-stack(sapply(trifil,"[","I2"))
trimfill_result<-data.frame(trifil_te[2],trifil_te[1],trifil_ll[1],trifil_hl[1],
                            trifil_p[1],trifil_Q[1],trifil_i2[1])
trimfill_result[,2:7]<-round(trimfill_result[,2:7],3)
colnames(trimfill_result)[1:7]<-c("Outcome","Effectsize","Lowerlimit","Upperlimit","P-value","Q","I-square")
round(trifil_result[1],3)

##only studies with k>=10
###categorical using subgroup,continuous using meta-regression
mod_hukou1<-lapply(modana,function(x) update(x,byvar=hukou))
mod_region1<-lapply(modana,function(x) update(x,byvar=Region))
mod_treat1<-lapply(modana,function(x) update(x,byvar=treatment))
mod_language1<-lapply(modana,function(x) update(x,byvar=lang))
mod_popu1<-lapply(modana,function(x) update(x,byvar=Population))
mod_age1<-lapply(modana,function(x) metareg(x,agemean,intercept = T))
mod_quality1<-lapply(modana,function(x) metareg(x,quality,intercept = T))
mod_female1<-lapply(modana,function(x) metareg(x,femalepercent,intercept = T))
egger1<-lapply(modana,function(x) metabias(x,k.min = 3))  ##publication bias 

##only studies with k>=10
###categorical using subgroup,continuous using meta-regression
temp11<-stack(sapply(mod_hukou1,"[[","pval.Q.b.random"))
hukou_mod1<-round(temp11[1],3)

temp21<-stack(sapply(mod_region1,"[[","pval.Q.b.random"))
region_mod1<-round(temp21[1],3)

temp31<-stack(sapply(mod_treat1,"[[","pval.Q.b.random"))
treat_mod1<-round(temp31[1],3)

temp41<-stack(sapply(mod_language1,"[[","pval.Q.b.random"))
lang_mod1<-round(temp41[1],3)

temp51<-stack(sapply(mod_popu1,"[[","pval.Q.b.random"))
popu_mod1<-round(temp51[1],3)

temp61<-stack(sapply(mod_age1,"[[","pval")[2,])
age_mod1<-round(temp61[1],3)

temp71<-stack(sapply(mod_quality1,"[[","pval")[2,])
quality_mod1<-round(temp71[1],3)

temp81<-stack(sapply(mod_female1,"[[","pval")[2,])
female_mod1<-round(temp81[1],3)

temp91<-stack(sapply(egger1,"[","p.value"))
publibias1<-round(temp91[1],3)


##put the results together
mods1<-data.frame("Outcomes"=names(modana),"Hukou"=hukou_mod1,"Region"=region_mod1,"Treat"= treat_mod1,"Language"=lang_mod1,
                  "Population"=popu_mod1,"Age"=age_mod1,"Female"=female_mod1,"Quality"=quality_mod1)
colnames(mods1)<-c("Outcomes","Hukou","Region","Treat","Language","Population","Age","Female","Quality")






