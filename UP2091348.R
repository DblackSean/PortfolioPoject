library(tidyverse)
library(lubridate)
library(splitstackshape)
library(ggplot2)
library(scales)
library(Hmisc)
library(polycor)
library(Rcmdr)
library(ggm)
library(skimr)
library(dplyr)
library(apaTables)
library(funModeling)
library(dlookr)
library(gtsummary)
library(vtable)
library(rstatix)
library(correlation)
library(QuantPsyc)
library(PKNCA)
library(regclass)
library(scatterplot3d)
library(rgl)
library(ggpubr)
library(flextable)
library(officer)
library(stargazer)

insuranceDetails <- read.csv("Assignment 1/Assignment 1 - data set.csv")

#Set Up Factors
insuranceDetails$Kilometres <- as.factor(insuranceDetails$Kilometres)
insuranceDetailsv2$Zone <- as.factor(insuranceDetailsv2$Zone)
insuranceDetailsv2$Make <- as.factor(insuranceDetailsv2$Make)

summary(insuranceDetails)
describe(insuranceDetails)

#Summary Categorical Variable
#Factor Summary <- 
  insuranceDetails %>%
  dplyr::select(Kilometres, Zone, Make) %>%
  tbl_summary(digits = list(all_categorical() ~ c(0, 2)))
  
factorSummary %>%
  as_flex_table() %>%
  save_as_docx(path = "Categorical Variables Summary.docx")

insuranceDetails %>%
  dplyr::select(Bonus, Insured, Claims, Payment) %>%
  stargazer(median = TRUE,out="Continuos Variables Summary.html")

insuranceDetails %>%
  group_by(Kilometres) %>%
  

#Payments and Average Payments per Kilometers
paymentKmBarChart <- 
  insuranceDetails %>%
  ggplot(aes(Kilometres,Payment, fill = Kilometres)) + 
    geom_bar(stat = "identity") + 
    scale_y_continuous(labels = comma) + 
    ylab("Payments in Swedish Krona (SEK)") +
    ggtitle("Total Payments per Kilometres") +
    scale_fill_brewer(labels = c("1: < 1000 Km", "2: 1000 Km - 15000 Km","3: 15000 Km - 20000 Km","4: 20000 Km - 25000 Km","5: > 25000 Km"),palette="Set3")

meanpaymentKmBarChart <- 
  insuranceDetails %>%
  ggplot(aes(Kilometres,Payment, fill = Kilometres)) + 
  stat_summary(fun = "mean", geom = "bar", position = "dodge") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2) + 
  scale_y_continuous(labels = comma) + 
    ylab("Payments in Swedish Krona (SEK)") +
    ggtitle("Mean Payments per Kilometres") +
    scale_fill_brewer(labels = c("1: < 1000 Km", "2: 1000 Km - 15000 Km","3: 15000 Km - 20000 Km","4: 20000 Km - 25000 Km","5: > 25000 Km"),palette="Set3")

ggarrange(paymentKmBarChart, meanpaymentKmBarChart, align = "h", common.legend = TRUE, legend = "right")

#Payments and Mean Payments per Make

paymentsMakebarChart <-
  insuranceDetails %>%
  ggplot(aes(Make,Payment, fill = Make)) + geom_bar(stat = "identity")  + 
  scale_y_continuous(labels = comma) + 
  ylab("Payments in Swedish Krona (SEK)") +
  ggtitle("Total Payments per Car Make") +
  scale_fill_brewer(palette="Spectral")

meanPaymentMake <- 
  insuranceDetails %>%
  ggplot(aes(Make,Payment, fill = Make)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2) + 
  scale_y_continuous(labels = comma) +
  ylab("Payments in Swedish Krona (SEK)") +
  ggtitle("Average Payments per Car Make") +
  scale_fill_brewer(palette="Spectral")

ggarrange(paymentsMakebarChart, meanPaymentMake, align = "h", common.legend = TRUE, legend = "right")

#Payment and Mean Payment per Zone

paymentsZonebarChart <- 
  insuranceDetails %>%
  ggplot(aes(Zone,Payment, fill=Zone)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = comma)+
    ylab("Payments in Swedish Krona (SEK)") +
    ggtitle("Total Payments per Geographical Area") +
    scale_fill_hue(labels = c("1: Stockholm, Göteborg, and Malmö with surroundings", "2: Other large cities with surroundings","3: Smaller cities with surroundings in southern Sweden","4: Rural areas in southern Sweden","5: Smaller cities with surroundings in northern Sweden","6: Rural areas in northern Sweden", "7: Gotland"))

meanPaymentZone <- 
  insuranceDetails %>%
  ggplot(aes(Zone,Payment, fill = Zone)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2) +
  scale_y_continuous(labels = comma) +
  ylab("Payments in Swedish Krona (SEK)") +
  ggtitle("Average Payments per Geographical Area") +
  scale_fill_hue(labels = c("1: Stockholm, Göteborg, and Malmö with surroundings", "2: Other large cities with surroundings","3: Smaller cities with surroundings in southern Sweden","4: Rural areas in southern Sweden","5: Smaller cities with surroundings in northern Sweden","6: Rural areas in northern Sweden", "7: Gotland"))

ggarrange(paymentsZonebarChart, meanPaymentZone, align = "h", common.legend = TRUE, legend = "right")

#Claims and Mean Claims per Kilometers 
claimsKmBar <- insuranceDetails %>%
  ggplot(aes(Kilometres,Claims, fill = Kilometres)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = comma) + 
  ylab("Number of Claims") +
  ggtitle("Total Number of Claims per Kilometres") +
  scale_fill_brewer(labels = c("1: < 1000 Km", "2: 1000 Km - 15000 Km","3: 15000 Km - 20000 Km","4: 20000 Km - 25000 Km","5: > 25000 Km"),palette="Set2")

meanclaimsKmBar <- insuranceDetails %>%
  ggplot(aes(Kilometres,Claims, fill = Kilometres)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2)+
  ylab("Number of Claims") +
  ggtitle("Average Number of Claims per Kilometres") +
  scale_fill_brewer(labels = c("1: < 1000 Km", "2: 1000 Km - 15000 Km","3: 15000 Km - 20000 Km","4: 20000 Km - 25000 Km","5: > 25000 Km"),palette="Set2")

ggarrange(claimsKmBar, meanclaimsKmBar, align = "h", common.legend = TRUE, legend = "right")

#Number of Claims per Zone
claimsZonebar <- insuranceDetails %>%
  ggplot(aes(Zone,Claims, fill = Zone)) +
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = comma) + 
  ylab("Number of Claims") +
  ggtitle("Total Number of Claims per Geographical Area") +
  scale_fill_hue(labels = c("1: Stockholm, Göteborg, and Malmö with surroundings", "2: Other large cities with surroundings","3: Smaller cities with surroundings in southern Sweden","4: Rural areas in southern Sweden","5: Smaller cities with surroundings in northern Sweden","6: Rural areas in northern Sweden", "7: Gotland"))

meanclaimsZoneBar <- insuranceDetails %>%
  ggplot(aes(Zone,Claims, fill = Zone)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2) +
  ylab("Number of Claims") +
  ggtitle("Average Number of Claims per Geographical Area") +
  scale_fill_hue(labels = c("1: Stockholm, Göteborg, and Malmö with surroundings", "2: Other large cities with surroundings","3: Smaller cities with surroundings in southern Sweden","4: Rural areas in southern Sweden","5: Smaller cities with surroundings in northern Sweden","6: Rural areas in northern Sweden", "7: Gotland"))

ggarrange(claimsZonebar, meanclaimsZoneBar, align = "h", common.legend = TRUE, legend = "right")

#Claims per Car Make
claimsMakeBar <-
insuranceDetails %>%
  ggplot(aes(Make,Claims, fill = Make)) + 
  geom_bar(stat = "identity")  + 
  scale_y_continuous(labels = comma) + 
  ylab("Number of Claims") +
  xlab("Car Makes")+
  ggtitle("Total Number of Claims per Car Make") +
  scale_fill_brewer(palette="Spectral")

meanclaimsMake <- 
insuranceDetails %>%
  ggplot(aes(Make,Claims, fill = Make)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2) +
  scale_y_continuous(labels = comma) +
  ylab("Number of Claims") +
  xlab("Car Makes") +
  ggtitle("Average Number of Claims per Car Make") +
  scale_fill_brewer(palette="Spectral")

ggarrange(claimsMakeBar, meanclaimsMake, align = "h", common.legend = TRUE, legend = "right")

#Insured per Kilometers
insuranceKmBar <- 
  insuranceDetails %>%
  ggplot(aes(Kilometres,Insured, fill=Kilometres)) + 
  geom_bar(stat = "identity") + 
  scale_y_continuous(labels = comma) +
  ylab("Years Insured") +
  ggtitle("Total Number of Insured (Years) per Kilometres") +
  scale_fill_brewer(labels = c("1: < 1000 Km", "2: 1000 Km - 15000 Km","3: 15000 Km - 20000 Km","4: 20000 Km - 25000 Km","5: > 25000 Km"),palette="Set2")

meaninsuranceKmBar <- 
  insuranceDetails %>%
  ggplot(aes(Kilometres,Insured, fill = Kilometres)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2) + 
  scale_y_continuous(labels = comma) +
    ylab("Years Insured") +
    ggtitle("Average Number of Insured (Years) per Kilometres") +
  scale_fill_brewer(labels = c("1: < 1000 Km", "2: 1000 Km - 15000 Km","3: 15000 Km - 20000 Km","4: 20000 Km - 25000 Km","5: > 25000 Km"),palette="Set2")

ggarrange(insuranceKmBar, meaninsuranceKmBar, align = "h", common.legend = TRUE, legend = "right")

#Insured per Make
insuredMakeBar <-
  insuranceDetails %>%
  ggplot(aes(Make,Insured, fill = Make)) + 
  geom_bar(stat = "identity")  + 
  scale_y_continuous(labels = comma) + 
  ylab("Years Insured") +
  ggtitle("Total Number of Insured (Years) per Car Make") +
  scale_fill_brewer(palette="Spectral")

meaninsuredMakeBar <- 
  insuranceDetails %>%
  ggplot(aes(Make,Insured, fill = Make)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2) + 
  scale_y_continuous(labels = comma) +
  ylab("Years Insured") +
  ggtitle("Average Number of Insured per Car Make") +
  scale_fill_brewer(palette="Spectral")

ggarrange(insuredMakeBar, meaninsuredMakeBar, align = "h", common.legend = TRUE, legend = "right")

#Insured and Mean Insured per Zone
insuredZoneBar <-
  insuranceDetails %>%
  ggplot(aes(Zone,Insured, fill = Zone)) + 
  geom_bar(stat = "identity")  + 
  scale_y_continuous(labels = comma) + 
  ylab("Years Insured") +
  ggtitle("Total Number of Insured (Years) per Geographical Area")+
  scale_fill_hue(labels = c("1: Stockholm, Göteborg, and Malmö with surroundings", "2: Other large cities with surroundings","3: Smaller cities with surroundings in southern Sweden","4: Rural areas in southern Sweden","5: Smaller cities with surroundings in northern Sweden","6: Rural areas in northern Sweden", "7: Gotland"))

meaninsuredZoneBar <- 
  insuranceDetails %>%
  ggplot(aes(Zone,Insured, fill = Zone)) + 
  stat_summary(fun = "mean", geom = "bar") + 
  stat_summary(fun.data = "mean_cl_normal", geom = "errorbar",position = position_dodge(width = 0.90), width = 0.2) + 
  scale_y_continuous(labels = comma) +
  ylab("Years Insured") +
  ggtitle("Average Number of Insured per Geographical Area") +
  scale_fill_hue(labels = c("1: Stockholm, Göteborg, and Malmö with surroundings", "2: Other large cities with surroundings","3: Smaller cities with surroundings in southern Sweden","4: Rural areas in southern Sweden","5: Smaller cities with surroundings in northern Sweden","6: Rural areas in northern Sweden", "7: Gotland"))

ggarrange(insuredZoneBar, meaninsuredZoneBar, align = "h", common.legend = TRUE, legend = "right")

insuranceDetails %>%
  ggplot(aes(Zone, Bonus, fill = Zone)) + geom_bar(stat = "identity")

#Create Make group variable
insuranceDetails <- insuranceDetails %>%
  mutate(Make_Group = if_else(Make == 9,"Not Popular","Popular"))
insuranceDetails$Make_Group <- as.factor(insuranceDetails$Make_Group)

#Scatter Plot Between Payment and Claim (Grouped by Kilometers)
insuranceDetails %>%
  ggplot(aes(Claims, Payment)) + 
  geom_point(aes(color = Kilometres)) + 
  geom_smooth(method = "lm") + 
  scale_x_continuous(labels = comma)+ 
  scale_y_continuous(labels = comma)+
  ylab("Payment Amount") +
  xlab("Number of Claims") + 
  ggtitle("Scatter plot between Payments and Claims, grouped by Kilometres")

#Scatter Plot Between Payment and Claim (Grouped by Make Group, Popular or Not Popular)
insuranceDetails %>%
  ggplot(aes(Claims, Payment)) +
  geom_point(aes(color = Make_Group)) +
  geom_smooth(method = "lm") + 
  scale_y_continuous(labels = comma)+
  scale_x_continuous(labels = comma)+ 
  ylab("Payment Amount") +
  xlab("Number of Claims") + 
  ggtitle("Scatter plot between Payments and Claims, grouped by Car Make")


#Scatter Plot Between Payment and Insured
insuranceDetails %>%
  ggplot(aes(Insured, Payment)) +
  geom_point(aes(color = Zone)) +
  geom_smooth(method = "lm") + 
  scale_x_continuous(labels = comma)+ 
  scale_y_continuous(labels = comma)+
  ylab("Payment Amount") +
  xlab("Number of Claims") + 
  ggtitle("Scatter plot between Payments and Insured, grouped by Geographical Area")


#Scatter Plot Between Payment and Insured
insuranceDetails %>%
  ggplot(aes(Claims, Insured)) +
  geom_point(aes(color = Kilometres)) +
  geom_smooth(method = "lm") + 
  scale_x_continuous(labels = comma)+ 
  scale_y_continuous(labels = comma)+
  ylab("Years Insured") +
  xlab("Number of Claims") + 
  ggtitle("Scatter plot between Years Insured and Claims, grouped by Kilometres")


#Scatter Plot Between Payment and Bonus
insuranceDetails %>%
  ggplot(aes(Payment, Kilometres)) + geom_point()

#Correlation Matrix pearson
insuranceDetailsv2 %>%
  dplyr::select(Kilometres, Bonus, Insured, Claims, Payment) %>%
  correlation(method = "pearson")

#Correlation Pearson
insuranceDetailsv2 %>%
  dplyr::select(Kilometres, Bonus, Insured, Claims, Payment) %>%
  apa.cor.table(.,filename = "Correlation Matrix.doc", table.number = 4)
  
#Linear Regression Model to predict Payments - Models using insured variable instead of claims
paymentModel <- lm(Payment ~ Insured + Bonus + Make + Kilometres + Zone, data = insuranceDetails)
summary(paymentModel)

paymentModel2 <- lm(Payment ~ Insured + Bonus + Make + Zone, data = insuranceDetails)
summary(paymentModel2)

paymentModel3 <- lm(Payment ~ Insured + Bonus + Zone, data = insuranceDetails)
summary(paymentModel3)

paymentModel4 <- lm(Payment ~ Insured + Bonus + Kilometres, data = insuranceDetails)
summary(paymentModel4)

paymentModel5 <- lm(Payment ~ Insured + Bonus, data = insuranceDetails)
summary(paymentModel5)

#Analysis of Variance for Insured Models
anova(paymentModel5,paymentModel2, paymentModel3, paymentModel4, paymentModel)

#Linear Regression Model to predict Payments - Models using Claims
paymentModelC <- lm(Payment ~ Claims + Bonus + Make + Kilometres + Zone, data = insuranceDetails)
summary(paymentModelC) #R2 0.9916

paymentModelC2 <- lm(Payment ~ Claims + Bonus + Kilometres + Zone, data = insuranceDetails)
summary(paymentModelC2) #R2 0.9913

paymentModelC3 <- lm(Payment ~ Claims + Bonus + Zone, data = insuranceDetails)
summary(paymentModelC3) #R2 0.9913

paymentModelC4 <- lm(Payment ~ Claims + Bonus + Kilometres, data = insuranceDetails)
summary(paymentModelC4)#R2 0.991

paymentModelC5 <- lm(Payment ~ Claims + Bonus, data = insuranceDetails)
summary(paymentModelC5)#R2 0.991

#Analysis of Variance for Models including Claims
anova(paymentModelC5,paymentModelC2, paymentModelC3, paymentModelC4, paymentModelC)

anova(paymentModelC3, paymentModelC2, paymentModelC)

#AIC Test for lowest AIC value
modelists <- list(paymentModelC, paymentModelC2,paymentModelC3,paymentModelC4,paymentModelC5, paymentModel, paymentModel2, paymentModel3, paymentModel4,paymentModel5)
AIC(modelists, assess.best = TRUE) #paymentModelC best fit lowest AIC

apa.reg.table(paymentModelC, filename = "Payments regression.doc",table.number = 6, prop.var.conf.level = 0.95)

#3D Scatter Plot with Payments as y
scatter3d(x=insuranceDetails$Claims ,
          y = insuranceDetails$Payment, 
          z = insuranceDetails$Insured, 
          groups = insuranceDetails$Zone,
          grid = FALSE,
          axis.scales = FALSE,
          fit = "linear",
          xlab = "Number of Claims",
          ylab = "Payment Amount",
          zlab = "Years Insured")
rgl.snapshot(filename = "3d Payment.png")

#Residual Test of Payment Model C
insuranceDetails$residualPM3 <- rstandard(paymentModelC)
insuranceDetails$cookDistance <- cooks.distance(paymentModelC)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  

#Cook Test
insuranceDetails <- insuranceDetails %>%
  mutate(outlier = if_else(cookDistance > 1, "Outlier", "Good"))

insuranceDetails %>%
  dplyr::filter(outlier == "Outlier")
#only 1 outlier therefore the cook test confirms that there's isn't a problem with outliers

#Multicollinearity - No collinearity detected following VIF test as the mean is not far from 1
regclass::VIF(paymentModelC)

mean(1.482632,1.018511,1.399052,1.033227,1.042593)

#Linear Regression Model to predict Claims - Models using insured
claimsModelI <- lm(Claims ~ Insured + Bonus + Make + Kilometres + Zone, data = insuranceDetails)
summary(claimsModelI) #R2 0.871

#Lowest AIC
step (claimsModelI , direction ="backward")

apa.reg.table(claimsModelI, filename = "Claims regression.doc",table.number = 2, prop.var.conf.level = 0.95)

#Residual Test of Payment Model C
insuranceDetails$residualCM <- rstandard(claimsModelI)
insuranceDetails$cookDistanceCM <- cooks.distance(claimsModelI)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  

#Cook Test
insuranceDetails <- insuranceDetails %>%
  mutate(outlierCM = if_else(cookDistance > 1, "Outlier", "Good"))

insuranceDetails %>%
  dplyr::filter(outlierCM == "Outlier")
#only 1 outlier therefore the cook test confirms that there's isn't a problem with outliers

#Multicollinearity - No collinearity detected following VIF test as the mean is not far from 1
regclass::VIF(claimsModelI)

mean(1.256114,1.036498,1.180304,1.019923,1.027816)

#3D Scatter Plot with claims as y 

scatterplot3d(x=insuranceDetails$Claims, y = insuranceDetails$Payment, z=insuranceDetails$Insured)

scatter3d(x = insuranceDetails$Payment,
          y = insuranceDetails$Claims, 
          z = insuranceDetails$Insured, 
          groups = insuranceDetails$Make_Group,
          fit = "linear",
          xlab = "Payment Amount",
          ylab = "Number of Claims",
          zlab = "Years Insured")

#Using Claims Model I for Claims Prediction

#case 1 - Model predicts claim of 145
predict(claimsModelI, data.frame(Zone = as.factor(5), Insured = 4621,Kilometres = as.factor(2), Bonus = 3, Make =as.factor(3)))
#case2 - Model predicts claims of 444
predict(claimsModelI, data.frame(Zone = as.factor(3), Insured = 9500,Kilometres = as.factor(2), Bonus = 1, Make =as.factor(9)))
#case3 - Model predicts claims of 516 - 750
predict(claimsModelI, data.frame(Zone = as.factor(2), Insured = 17500,Kilometres = as.factor(4), Bonus = 5, Make =as.factor(3)))

#Using Payment Model C for Payment prediction

#case1 - Model predicts Payments of 731,283.30 skr
predict(paymentModelC, data.frame(Zone = as.factor(5), Claims = 145,Kilometres = as.factor(2), Bonus = 3, Make =as.factor(3)))
#case2 - Model predicts Payments of 2,161,435.00 skr
predict(paymentModelC, data.frame(Zone = as.factor(3), Claims = 444,Kilometres = as.factor(2), Bonus = 1, Make =as.factor(9)))
#case3 - Model predicts Payments of 2,616,242.00 - 3,801,384.00 skr
predict(paymentModelC, data.frame(Zone = as.factor(2), Claims = 750,Kilometres = as.factor(4), Bonus = 5, Make =as.factor(3)))