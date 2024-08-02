library(ggplot2)
library(tidyverse)

# This code creates Figures and Supplemental Figures as found in the manuscript and supplemental appendix.

output_all <- read.csv(file="results/vacciation_results_72824.csv")
output_all <- output_all[which(output_all$Vax_Age_u5=="0.5"|output_all$Vax_Age_u5=="0.8"|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR=="0.8"|output_all$Vax_Age_u5==1.0|output_all$Vax_Age_o15HR==1.0),2:19]

output_all$VaxStrategy <- ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 & output_all$Vax_Age_o15HR==1.0 & output_all$Vax_Age_o15LR=="1.0", "100% all vaccinated",
                            ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" & output_all$Vax_Age_o15HR=="0.8"& output_all$Vax_Age_o15LR=="0.8", "80% all vaccinated",
                              ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5" & output_all$Vax_Age_o15HR=="0.5"& output_all$Vax_Age_o15LR=="0.5", "50% all vaccinated",
                                ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" & output_all$Vax_Age_o15HR=="0.8", "80% age <=15 and high risk adults",
                                  ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 & output_all$Vax_Age_o15HR==1.0, "100% age <=15 and high risk adults",
                                     ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5" & output_all$Vax_Age_o15HR=="0.5", "50% age <=15 and high risk adults",
                                        ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" , "80% age <=15 vaccinated",
                                               ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 ,"100% age <=15 vaccinated",
                                                ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5", "50% age <=15 vaccinated",
                                                      ifelse(output_all$Vax_Age_u5=="0.5", "50% age <5 vaccinated",
                                                          ifelse(output_all$Vax_Age_u5=="0.8", "80% age <5 vaccinated", 
                                                                 ifelse(output_all$Vax_Age_o15HR=="0.8", "80% high risk adults vaccinated",
                                                                        ifelse(output_all$Vax_Age_u5==1.0, "100% age <5 vaccinated",
                                                                               ifelse(output_all$Vax_Age_o15HR==1.0, "100% high risk adults vaccinated",NA))))))))))))))


output_all$VaxStrategy = factor(output_all$VaxStrategy, levels=c('50% all vaccinated','50% age <=15 and high risk adults','50% age <=15 vaccinated','50% age <5 vaccinated',
                                                                 '80% all vaccinated','80% age <=15 and high risk adults','80% age <=15 vaccinated','80% age <5 vaccinated',
                                                                 '100% all vaccinated','100% age <=15 and high risk adults','100% age <=15 vaccinated','100% age <5 vaccinated',
                                                                  '80% high risk adults vaccinated','100% high risk adults vaccinated'))


output_all <- output_all[-c(10:12,26:28,42:44),]
data_cases <- output_all[which(output_all$Vax_Age_u5=="0.5"|output_all$Vax_Age_u5=="0.8"|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR=="0.8"),c(1,6:9,18,19)]
data_cases <- data_cases[-c(2,7:8,11,16:17,20,25:26),]
data_deaths <- output_all[which(output_all$Vax_Age_u5=="0.5"|output_all$Vax_Age_u5=="0.8"|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR=="0.8"),c(1,10:13,18,19)]
data_deaths <- data_deaths[-c(2,7:8,11,16:17,20,25:26),]
data_vaccinations <- output_all[which(output_all$Vax_Age_u5=="0.5"|output_all$Vax_Age_u5=="0.8"|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR=="0.8"),c(1,14:18,19)]
data_vaccinations <-data_vaccinations[-c(2,7:8,11,16:17,20,25:26),]

data_HR_cases <- output_all[which(output_all$Vax_Age_o15HR!=0.0|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR!=0.0),c(1,6:9,18,19)]
data_HR_cases <- data_HR_cases[-c(6,7,13,14,20,21),]

data_HR_deaths <- output_all[which(output_all$Vax_Age_o15HR!=0.0|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR!=0.0),c(1,10:13,18,19)]
data_HR_deaths <- data_HR_deaths[-c(6,7,13,14,20,21),]

data_HR_vaccinations <- output_all[which(output_all$Vax_Age_o15HR!=0.0|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR!=0.0),c(1,14:18,19)]
data_HR_vaccinations <-data_HR_vaccinations[-c(6,7,13,14,20,21),]

data_100_cases <- output_all[which(output_all$Vax_Age_u5==1.0|output_all$Vax_Age_u15==1.0|output_all$Vax_Age_o15LR==1.0|output_all$Vax_Province=="None"),c(1,6:9,18,19)]
data_100_deaths <- output_all[which(output_all$Vax_Age_u5==1.0|output_all$Vax_Age_u15==1.0|output_all$Vax_Age_o15LR==1.0|output_all$Vax_Province=="None"),c(1,10:13,18,19)]
data_100_vaccinations <- output_all[which(output_all$Vax_Age_u5==1.0|output_all$Vax_Age_u15==1.0|output_all$Vax_Age_o15LR==1.0|output_all$Vax_Province=="None"),c(1,14:18,19)]


# Cases

data_cases$cases_averted_u5 <- as.numeric(data_cases[which(data_cases$Vax_Province=="None"),"Cases_Age_u5"])-as.numeric(data_cases$Cases_Age_u5)
data_cases$cases_averted_u15 <- as.numeric(data_cases[which(data_cases$Vax_Province=="None"),"Cases_Age_u15"])-as.numeric(data_cases$Cases_Age_u15)
data_cases$cases_averted_o15LR <- as.numeric(data_cases[which(data_cases$Vax_Province=="None"),"Cases_Age_o15LR"])-as.numeric(data_cases$Cases_Age_o15LR)
data_cases$cases_averted_o15HR <- as.numeric(data_cases[which(data_cases$Vax_Province=="None"),"Cases_Age_o15HR"])-as.numeric(data_cases$Cases_Age_o15HR)

data_HR_cases$cases_averted_u5 <- as.numeric(data_HR_cases[which(data_HR_cases$Vax_Province=="None"),"Cases_Age_u5"])-as.numeric(data_HR_cases$Cases_Age_u5)
data_HR_cases$cases_averted_u15 <- as.numeric(data_HR_cases[which(data_HR_cases$Vax_Province=="None"),"Cases_Age_u15"])-as.numeric(data_HR_cases$Cases_Age_u15)
data_HR_cases$cases_averted_o15LR <- as.numeric(data_HR_cases[which(data_HR_cases$Vax_Province=="None"),"Cases_Age_o15LR"])-as.numeric(data_HR_cases$Cases_Age_o15LR)
data_HR_cases$cases_averted_o15HR <- as.numeric(data_HR_cases[which(data_HR_cases$Vax_Province=="None"),"Cases_Age_o15HR"])-as.numeric(data_HR_cases$Cases_Age_o15HR)


data_100_cases$cases_averted_u5 <- as.numeric(data_100_cases[which(data_100_cases$Vax_Province=="None"),"Cases_Age_u5"])-as.numeric(data_100_cases$Cases_Age_u5)
data_100_cases$cases_averted_u15 <- as.numeric(data_100_cases[which(data_100_cases$Vax_Province=="None"),"Cases_Age_u15"])-as.numeric(data_100_cases$Cases_Age_u15)
data_100_cases$cases_averted_o15LR <- as.numeric(data_100_cases[which(data_100_cases$Vax_Province=="None"),"Cases_Age_o15LR"])-as.numeric(data_100_cases$Cases_Age_o15LR)
data_100_cases$cases_averted_o15HR <- as.numeric(data_100_cases[which(data_100_cases$Vax_Province=="None"),"Cases_Age_o15HR"])-as.numeric(data_100_cases$Cases_Age_o15HR)



data_cases_long <- gather(data_cases,age,cases_averted,cases_averted_u5:cases_averted_o15HR)
data_cases_long2 <- data_cases_long[which(data_cases_long$Vax_Province!="None"),c(1,6:9)]

data_HR_cases_long <- gather(data_HR_cases,age,cases_averted,cases_averted_u5:cases_averted_o15HR)
data_HR_cases_long2 <- data_HR_cases_long[which(data_HR_cases_long$Vax_Province!="None"),c(1,6:9)]

data_100_cases_long <- gather(data_100_cases,age,cases_averted,cases_averted_u5:cases_averted_o15HR)
data_100_cases_long2 <- data_100_cases_long[which(data_100_cases_long$Vax_Province!="None"),c(1,6:9)]


cases_plot <- ggplot(data=data_cases_long2, aes(y=cases_averted,  x=Vax_Province, fill=Vax_Province)) + geom_bar(stat="identity", show.legend = FALSE) +
  xlab("") + ylab("Number cases averted") +
  facet_wrap(~VaxStrategy, ncol=3) + theme_classic() + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
    size = 13))

cases_HR_plot <- ggplot(data=data_HR_cases_long2, aes(y=cases_averted,  x=Vax_Province, fill=age)) + geom_bar(stat="identity") +
  xlab("") + ylab("Number cases averted") +
  facet_wrap(~VaxStrategy, ncol=3) + theme_classic() + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
    size = 10)) +
  scale_fill_discrete(labels=c('High risk adult', 'Low risk adult','Child 5-15','Child under 5')) +
  labs(fill="Cases averted by age")

cases_100_plot <- ggplot(data=data_100_cases_long2, aes(y=cases_averted,  x=Vax_Province, fill=age)) + geom_bar(stat="identity") +
  xlab("") + ylab("Number cases averted") +
  facet_wrap(~VaxStrategy, ncol=3) + theme_classic() + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
    size = 10)) +
  scale_fill_discrete(labels=c('High risk adult', 'Low risk adult','Child 5-15','Child under 5')) +
  labs(fill="Cases averted by age")

# Deaths

data_deaths$deaths_averted_u5 <- as.numeric(data_deaths[which(data_deaths$Vax_Province=="None"),"Deaths_Age_u5"])-as.numeric(data_deaths$Deaths_Age_u5)
data_deaths$deaths_averted_u15 <- as.numeric(data_deaths[which(data_deaths$Vax_Province=="None"),"Deaths_Age_u15"])-as.numeric(data_deaths$Deaths_Age_u15)
data_deaths$deaths_averted_o15LR <- as.numeric(data_deaths[which(data_deaths$Vax_Province=="None"),"Deaths_Age_o15LR"])-as.numeric(data_deaths$Deaths_Age_o15LR)
data_deaths$deaths_averted_o15HR <- as.numeric(data_deaths[which(data_deaths$Vax_Province=="None"),"Deaths_Age_o15HR"])-as.numeric(data_deaths$Deaths_Age_o15HR)

data_HR_deaths$deaths_averted_u5 <- as.numeric(data_HR_deaths[which(data_HR_deaths$Vax_Province=="None"),"Deaths_Age_u5"])-as.numeric(data_HR_deaths$Deaths_Age_u5)
data_HR_deaths$deaths_averted_u15 <- as.numeric(data_HR_deaths[which(data_HR_deaths$Vax_Province=="None"),"Deaths_Age_u15"])-as.numeric(data_HR_deaths$Deaths_Age_u15)
data_HR_deaths$deaths_averted_o15LR <- as.numeric(data_HR_deaths[which(data_HR_deaths$Vax_Province=="None"),"Deaths_Age_o15LR"])-as.numeric(data_HR_deaths$Deaths_Age_o15LR)
data_HR_deaths$deaths_averted_o15HR <- as.numeric(data_HR_deaths[which(data_HR_deaths$Vax_Province=="None"),"Deaths_Age_o15HR"])-as.numeric(data_HR_deaths$Deaths_Age_o15HR)

data_100_deaths$deaths_averted_u5 <- as.numeric(data_100_deaths[which(data_100_deaths$Vax_Province=="None"),"Deaths_Age_u5"])-as.numeric(data_100_deaths$Deaths_Age_u5)
data_100_deaths$deaths_averted_u15 <- as.numeric(data_100_deaths[which(data_100_deaths$Vax_Province=="None"),"Deaths_Age_u15"])-as.numeric(data_100_deaths$Deaths_Age_u15)
data_100_deaths$deaths_averted_o15LR <- as.numeric(data_100_deaths[which(data_100_deaths$Vax_Province=="None"),"Deaths_Age_o15LR"])-as.numeric(data_100_deaths$Deaths_Age_o15LR)
data_100_deaths$deaths_averted_o15HR <- as.numeric(data_100_deaths[which(data_100_deaths$Vax_Province=="None"),"Deaths_Age_o15HR"])-as.numeric(data_100_deaths$Deaths_Age_o15HR)


data_deaths_long <- gather(data_deaths,age,deaths_averted,deaths_averted_u5:deaths_averted_o15HR)
data_deaths_long2 <- data_deaths_long[which(data_deaths_long$Vax_Province!="None"),c(1,6:9)]

data_HR_deaths_long <- gather(data_HR_deaths,age,deaths_averted,deaths_averted_u5:deaths_averted_o15HR)
data_HR_deaths_long2 <- data_HR_deaths_long[which(data_HR_deaths_long$Vax_Province!="None"),c(1,6:9)]

data_100_deaths_long <- gather(data_100_deaths,age,deaths_averted,deaths_averted_u5:deaths_averted_o15HR)
data_100_deaths_long2 <- data_100_deaths_long[which(data_100_deaths_long$Vax_Province!="None"),c(1,6:9)]


deaths_plot <- ggplot(data=data_deaths_long2, aes(y=deaths_averted,  x=Vax_Province, fill=Vax_Province)) + geom_bar(stat="identity", show.legend = FALSE) + 
  xlab("") + ylab("Number deaths averted") +
 facet_wrap(~VaxStrategy, ncol=3) + theme_classic() + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
   size = 13))  

deaths_HR_plot <- ggplot(data=data_HR_deaths_long2, aes(y=deaths_averted,  x=Vax_Province, fill=age)) + geom_bar(stat="identity") + 
  xlab("") + ylab("Number deaths averted") +
  facet_wrap(~VaxStrategy, ncol=3) + theme_classic() + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
    size = 10))  +
  scale_fill_discrete(labels=c('High risk adult', 'Low risk adult','Child 5-15','Child under 5')) +
  labs(fill="Deaths averted by age")


deaths_100_plot <- ggplot(data=data_100_deaths_long2, aes(y=deaths_averted,  x=Vax_Province, fill=age)) + geom_bar(stat="identity") + 
  xlab("") + ylab("Number deaths averted") +
  facet_wrap(~VaxStrategy, ncol=3) + theme_classic() + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
    size = 10))  +
  scale_fill_discrete(labels=c('High risk adult', 'Low risk adult','Child 5-15','Child under 5')) +
  labs(fill="Deaths averted by age")



# Vaccine Doses

data_vaccinations_long <- gather(data_vaccinations,age,vaccinations,Vax_Doses_Age_u5:Vax_Doses_Age_o15HR)
data_vaccinations_long2 <- data_vaccinations_long[which(data_vaccinations_long$Vax_Province!="None"),]

data_vaccinations_long2$vaccinations <- as.numeric(data_vaccinations_long2$vaccinations)

data_HR_vaccinations_long <- gather(data_HR_vaccinations,age,vaccinations,Vax_Doses_Age_u5:Vax_Doses_Age_o15HR)
data_HR_vaccinations_long2 <- data_HR_vaccinations_long[which(data_HR_vaccinations_long$Vax_Province!="None"),]

data_HR_vaccinations_long2$vaccinations <- as.numeric(data_HR_vaccinations_long2$vaccinations)

data_100_vaccinations_long <- gather(data_100_vaccinations,age,vaccinations,Vax_Doses_Age_u5:Vax_Doses_Age_o15HR)
data_100_vaccinations_long2 <- data_100_vaccinations_long[which(data_100_vaccinations_long$Vax_Province!="None"),]

data_100_vaccinations_long2$vaccinations <- as.numeric(data_100_vaccinations_long2$vaccinations)


vaccinations_plot <- ggplot(data=data_vaccinations_long2, aes(y=vaccinations/1000000,  x=Vax_Province, fill=Vax_Province)) + geom_bar(stat="identity", show.legend = FALSE) +
  facet_wrap(~VaxStrategy, ncol=3) + theme_classic() +xlab("") + ylab("Number vaccine doses, millions") + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
    size = 13))

vaccinations_100_plot <- ggplot(data=data_100_vaccinations_long2, aes(y=vaccinations/1000000,  x=Vax_Province, fill=age)) + geom_bar(stat="identity") +
  facet_wrap(~VaxStrategy, ncol=3) + theme_classic() +xlab("") + ylab("Number vaccine doses,millions") + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
    size = 9)) +
  scale_fill_discrete(labels=c('High risk adult', 'Low risk adult','Child 5-15','Child under 5')) +
  labs(fill="Vaccine doses by age, millions")

vaccinations_HR_plot <- ggplot(data=data_HR_vaccinations_long2, aes(y=vaccinations/1000,  x=Vax_Province, fill=age)) + geom_bar(stat="identity") +
  facet_wrap(~VaxStrategy, ncol=3) + theme_classic() +xlab("") + ylab("Number vaccine doses, thousands") + theme(axis.text.x = element_text(angle = 70,  hjust=1), text=element_text(size=20, family="serif"),strip.text = element_text(
    size = 9)) +
  scale_fill_discrete(labels=c('High risk adult', 'Low risk adult','Child 5-15','Child under 5')) +
  labs(fill="Vaccine doses by age, millions")

# Print plots of cases, deaths, and vaccinations

pdf(file = "results/Figure2_Cases_Averted_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(cases_plot)
# Step 3: Run dev.off() to create the file!
dev.off()

pdf(file = "results/Figure4_Vaccine_Doses_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(vaccinations_plot)
# Step 3: Run dev.off() to create the file!
dev.off()

pdf(file = "results/Figure3_Deaths_Averted_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(deaths_plot)
# Step 3: Run dev.off() to create the file!
dev.off()


###### Supplemental figures

pdf(file = "results/SupplementalFigure1_Cases_Averted_100pVax_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(cases_100_plot)
# Step 3: Run dev.off() to create the file!
dev.off()

pdf(file = "results/SupplementalFigure3_Vaccine_Doses_100pVax_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(vaccinations_100_plot)
# Step 3: Run dev.off() to create the file!
dev.off()

pdf(file = "results/SupplementalFigure2_Deaths_Averted_100pVax_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(deaths_100_plot)
# Step 3: Run dev.off() to create the file!
dev.off()

pdf(file = "results/SupplementalFigure4_Cases_Averted_HRVax_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(cases_HR_plot)
# Step 3: Run dev.off() to create the file!
dev.off()

pdf(file = "results/SupplementalFigure6_Vaccine_Doses_HRVax_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(vaccinations_HR_plot)
# Step 3: Run dev.off() to create the file!
dev.off()

pdf(file = "results/SupplementalFigure5_Deaths_Averted_HRVax_72624.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(deaths_HR_plot)
# Step 3: Run dev.off() to create the file!
dev.off()


