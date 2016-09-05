library(datasets)
library(ggplot2)

TG<-as.data.frame(ToothGrowth)
head(TG)
summary(TG)

#plotting tooth growth vs dose+supp
LenBox <- ggplot(data = TG,aes(x = supp,y = len))+
    geom_boxplot(aes(fill = supp))+
    facet_wrap(~dose)+
    labs(title ="Tooth Growth vs Dose+Supp",x = "Dosage",y = "Length")

print(LenBox)

#plotting tooth growth vs dose
LenDoseBox<- ggplot(TG,aes(x = TG$dose, y = TG$len ,group = TG$dose))+
    geom_boxplot(aes(fill= factor(TG$dose)))+
    labs(title = "Tooth Growth vs Dose",x = "Dosage",y = "Length")

print(LenDoseBox)

#plotting tooth growth vs supp
LenSuppBox<- ggplot(TG,aes(x = TG$supp, y = TG$len ,group = TG$supp))+
    geom_boxplot(aes(fill=supp))+
    labs(title ="Tooth Growth vs Supplement",x = "Supplement",y = "Length")

print(LenSuppBox)


#--------------- Teeth Growth Vs Supplement -----------------------#

#Assumptions - teeth growth distribution population variance is same as sample variance
# The distribution is normally distributed iid

# Null Hypothesis that there is no difference in teeth growth due to Supplement
# Alternate Hypothesis there exists a significant relationship between teeth growth and Supplement at 95% conf level

lenOJ <- TG$len[TG$supp=='OJ']
lenVC <- TG$len[TG$supp=='VC']

t.test(x = lenOJ,y = lenVC, paired = FALSE,var.equal = TRUE)$conf
t.test(x = lenOJ,y = lenVC, paired = FALSE,var.equal = TRUE)$p.value

#Since P value is >6% we fail to reject null hypothesis at 95% confidence interval level

# Null Hypothesis that there is no difference in teeth growth due to Supplement
# Alternate Hypothesis that OJ supplement leads to higher teeth growth

t.test(x = lenOJ,y = lenVC, paired = FALSE,var.equal = TRUE,alternative = 'greater')$conf
t.test(x = lenOJ,y = lenVC, paired = FALSE,var.equal = TRUE, alternative = 'greater')$p.value

#Since P value is <5% for the one sided t test we can reject the null hypothesis at 95% confidence interval level




#--------------- Teeth Growth Vs Dosage -----------------------#

#Assumptions - teeth growth distribution population variance is same as sample variance

# Null Hypothesis that there is no difference in teeth growth due to Dosage
# Alternate Hypothesis there exists a significant relationship between teeth growth and Dosage at 95% conf level
len_0.5 <- TG$len[TG$dose==0.5]
len_1 <-  TG$len[TG$dose==1]
len_2 <-  TG$len[TG$dose==2]


#Dose 0.5 vs Dose 1
test_0.5_1 <- t.test(x = len_1,y = len_0.5, paired = FALSE,var.equal = TRUE)
test_0.5_1$p.value
test_0.5_1$conf.int
#Null Hypothesis rejected at 95% confidence level for two sided t-test

#Dose 1 vs Dose 2
test_1_2 <- t.test(x = len_2,y = len_1, paired = FALSE,var.equal = TRUE)
test_1_2$p.value
test_1_2$conf.int
#Null Hypothesis rejected at 95% confidence level for two sided t-test

#Dose 2 vs Dose 0.5
test_2_0.5 <- t.test(x = len_2,y = len_0.5, paired = FALSE,var.equal = TRUE)
test_2_0.5$p.value
test_2_0.5$conf.int
#Null Hypothesis rejected at 95% confidence level for two sided t-test
