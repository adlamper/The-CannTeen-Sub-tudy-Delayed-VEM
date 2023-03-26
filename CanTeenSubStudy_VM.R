#install packages

install.packages("haven") 
install.packages("tidyverse")
install.packages("dplyr")
install.packages("lme4")
install.packages("lmerTest")
install.packages("viridis")
install.packages("merTools")
install.packages("car")
install.packages("mlmRev")
install.packages("plm")
install.packages("tzdb")
install.packages("tidyr")
install.packages("sjstats")
install.packages("labelled")
install.packages("flextable")
install.packages("officer")
install.packages("lsmeans")
install.packages("emmeans")
install.packages("ggplot2")
install.packages("tigerstats")
install.packages("tcltk")
install.packages("ranger")
install.packages("ggfortify")
install.packages("broom")
install.packages("tinytex")
install.packages("readxl")
install.packages("gghalves")
install.packages("reshape2")
install.packages("bayestestR")
install.packages("DescTools")
install.packages("performace")
install.packages("graphics")
install.packages("EnvStats")
install.packages("nparLD")
install.packages("ggpubr")




library(haven)        
library(tidyverse)       
library(dplyr)
library(lme4)         
library(lmerTest)   
library(viridis)      
library(merTools)
library(car)
library(mlmRev)
library(plm) 
library(tzdb)
library(tidyr)
library(sjstats)
library(labelled)
library(flextable)
library(officer)
library(lsmeans)
library(emmeans)

CT_VM_long<-read_sav("CannTeen_vm_longitudinal_long.sav")

CT_VM_long$Age_Group_Fac <- factor(CT_VM_long$Age_Group)
CT_VM_long$User_Group_Fac <- factor(CT_VM_long$User_Group)
CT_VM_long$Gender_Fac <- factor(CT_VM_long$Gender)
CT_VM_long$Gender_Fac <- factor(CT_VM_long$Gender)
CT_VM_long$Occasion_Fac <- factor(CT_VM_long$Occasion_1_3_5)
CT_VM_long$Category_Fac <- factor(CT_VM_long$Category)

CT_VM_long$Occasion_Fac_Hel <- CT_VM_long$Occasion_Fac
contrasts(CT_VM_long$Occasion_Fac_Hel)= contr.Helmert(3)

CT_VM_long$Age_Group_Fac_Hel <- CT_VM_long$Age_Group_Fac
contrasts(CT_VM_long$Age_Group_Fac_Hel) = contr.Helmert(2)

CT_VM_long$User_Group_Fac_Hel <- CT_VM_long$User_Group_Fac
contrasts(CT_VM_long$User_Group_Fac_Hel) = contr.Helmert(2)



levels(CT_VM_long$Age_Group_Fac) <- c("adult", "adolescent")
levels(CT_VM_long$User_Group_Fac) <- c("control", "user")
levels(CT_VM_long$Category_Fac) <- c("teen user", "teen control", "adult user", "adult control")
levels(CT_VM_long$Occasion_Fac) <- c("base", "occ3", "occ5")


ICC(outcome="PR_Del",group="P_Num_numerical",data=CT_VM_long)
ICC(outcome="Pr_IM",group="P_Num_numerical",data=CT_VM_long)

boxplot(CT_VM_long$PR_Del)
test <- rosnerTest (CT_VM_long$PR_Del, k=10)


m1 <- lmer(data = CT_VM_long, PR_Del ~ 1 + (1 | P_Num_numerical))
summary(m1)

m2 <- lmer(data = CT_VM_long, PR_Del ~ 1 + (1|P_Num_numerical) + Occasion_Fac_Hel)
summary(m2)
Anova(m2, type = 3)
m3 <- lmer(data = CT_VM_long, PR_Del ~ 1 + (1|P_Num_numerical) + Occasion_Fac_Hel*User_Group_Fac_Hel)
Anova(m3, type = 3)

#linear mixed effect model for delayed VEM with fixed effects of occasion, user-group, and age, with a random intercept for participant number

m4 <- lmer(data = CT_VM_long, PR_Del ~ 1 + (1|P_Num_numerical) + Occasion_Fac_Hel*User_Group_Fac_Hel*Age_Group_Fac_Hel)
summary(m4)
Anova(m4, type = 3)
check_model(m4)

#linear mixed effect model for immediate VEM with fixed effects of occasion, user-group, and age, with a random intercept for participant number

m5 <- lmer(data = CT_VM_long, PR_IM ~ 1 + (1|P_Num_numerical) + Occasion_Fac_Hel*User_Group_Fac_Hel*Age_Group_Fac_Hel)
summary(m5)
Anova(m5, type = 3)
check_model(m5)

#estimated marginal means for significant findings

emmeans(m4, specs = pairwise ~ User_Group_Fac_Hel:Age_Group_Fac_Hel )
emmeans(m4, specs = pairwise ~ Occasion_Fac_Hel)
emmeans(m4, ~ User_Group_Fac_Hel)

emmeans(m5, specs = pairwise ~ User_Group_Fac_Hel:Age_Group_Fac_Hel )
emmeans(m5, specs = pairwise ~ Occasion_Fac_Hel)
emmeans(m5, specs = pairwise ~ User_Group_Fac_Hel)

#interaction term for user and age

CT_VM_long$User_Age <- interaction(CT_VM_long$User_Group_Fac_Hel,CT_VM_long$Age_Group_Fac_Hel)

#plot 

ggplot(data=CT_VM_long, aes(fill=User_Age))+
  
  geom_line(data=CT_VM_long %>% filter(User_Age=="0.0"),aes(group=c(P_Num_numerical), x=Occasion_Fac, y=PR_Del), color = "darkorange", size = 1, alpha = 0.1)+
  geom_line(data=CT_VM_long %>% filter(User_Age=="0.1"),aes(group=c(P_Num_numerical), x=Occasion_Fac, y=PR_Del), color = "darkgreen", size = 1, alpha = 0.1)+
  geom_line(data=CT_VM_long %>% filter(User_Age=="1.0"),aes(group=c(P_Num_numerical), x=Occasion_Fac, y=PR_Del), color = "salmon", size = 1, alpha = 0.1)+
  geom_line(data=CT_VM_long %>% filter(User_Age=="1.1"),aes(group=c(P_Num_numerical), x=Occasion_Fac, y=PR_Del), color = "dodgerblue", size = 1, alpha = 0.1)+

  # scale_linetype_binned() +
  # scale_shape_binned() +
  
  geom_ribbon(data=mean_task %>% filter(User_Age=="0.0"),aes(x=c(1,2,3), ymin=lower.CL, y=emmean, ymax=upper.CL), fill = "darkorange", alpha=0.5) +
  geom_ribbon(data=mean_task %>% filter(User_Age=="0.1"),aes(x=c(1,2,3), ymin=lower.CL, y=emmean, ymax=upper.CL), fill = "darkgreen",alpha=0.5) +
  geom_ribbon(data=mean_task %>% filter(User_Age=="1.0"),aes(x=c(1,2,3), ymin=lower.CL, y=emmean, ymax=upper.CL), fill = "salmon", alpha=0.5) +
  geom_ribbon(data=mean_task %>% filter(User_Age=="1.1"),aes(x=c(1,2,3), ymin=lower.CL, y=emmean, ymax=upper.CL), fill = "dodgerblue",alpha=0.5) +
  
  geom_line(data=mean_task %>% filter(User_Age=="0.0"),aes(x=c(1,2,3), y=emmean), linetype=1, size=2, color = "darkorange") +
  geom_line(data=mean_task %>% filter(User_Age=="0.1"),aes(x=c(1,2,3), y=emmean), linetype=1, size=2, color = "darkgreen") +
  geom_line(data=mean_task %>% filter(User_Age=="1.0"),aes(x=c(1,2,3), y=emmean), linetype=1, size=2, color = "salmon") +
  geom_line(data=mean_task %>% filter(User_Age=="1.1"),aes(x=c(1,2,3), y=emmean), linetype=1, size=2, color = "dodgerblue") +
  
  geom_point(data=mean_task %>% filter(User_Age=="0.0"),aes(x=Occasion_Fac, y=emmean), fill = "darkorange",  shape = 23, size = 4, stroke = 1.5) +
  geom_point(data=mean_task %>% filter(User_Age=="0.1"),aes(x=Occasion_Fac, y=emmean), fill = "darkgreen",  shape = 23, size = 4, stroke = 1.5) +
  geom_point(data=mean_task %>% filter(User_Age=="1.0"),aes(x=Occasion_Fac, y=emmean), fill = "salmon", shape = 23, size = 4, stroke = 1.5) +
  geom_point(data=mean_task %>% filter(User_Age=="1.1"),aes(x=Occasion_Fac, y=emmean), fill = "dodgerblue", shape = 23, size = 4, stroke = 1.5) +

xlab("Occasion") + ylab("Delayed Prose Recall Performance") + scale_fill_manual(name='Group',
                                                                                 breaks=c('Adolescent User', 'Adolescent Control', 'Adult User', 'Adult Control'),
                                                                                 values=c('Adolescent User' = "dodgerblue", 'Adolescent Control' = "darkgreen", 'Adult User' = "salmon", 'Adult Control' ="darkorange"))+ 
    theme(legend.position="right") + theme_classic()
