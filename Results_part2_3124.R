library(ggplot2)

output_all <- read.csv(file="results/vacciation_results_22924.csv")
output_all <- output_all[which(output_all$Vax_Age_u5=="0.5"|output_all$Vax_Age_u5=="0.8"|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR=="0.8"|output_all$Vax_Age_u5==1.0|output_all$Vax_Age_o15HR==1.0),2:19]

output_all$VaxStrategy <- ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 & output_all$Vax_Age_o15HR==1.0 & output_all$Vax_Age_o15LR=="1.0", "100% all vaccinated",
                                 ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" & output_all$Vax_Age_o15HR=="0.8"& output_all$Vax_Age_o15LR=="0.8", "80% all vaccinated",
                                        ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5" & output_all$Vax_Age_o15HR=="0.5"& output_all$Vax_Age_o15LR=="0.5", "50% all vaccinated",
                                               ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" & output_all$Vax_Age_o15HR=="0.8", "80% kids age <=15 and high risk adults",
                                                      ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 & output_all$Vax_Age_o15HR==1.0, "100% kids age <=15 and high risk adults",
                                                             ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5" & output_all$Vax_Age_o15HR=="0.5", "50% kids age <=15 and high risk adults",
                                                                    ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" , "80% age <=15 vaccinated",
                                                                           ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 ,"100% age <=15 vaccinated",
                                                                                  ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5", "50% age <=15 vaccinated",
                                                                                         ifelse(output_all$Vax_Age_u5=="0.5", "50% age <5 vaccinated",
                                                                                                ifelse(output_all$Vax_Age_u5=="0.8", "80% age <5 vaccinated", 
                                                                                                       ifelse(output_all$Vax_Age_o15HR=="0.8", "80% high risk adults vaccinated",
                                                                                                              ifelse(output_all$Vax_Age_u5==1.0, "100% age <5 vaccinated",
                                                                                                                     ifelse(output_all$Vax_Age_o15HR==1.0, "100% high risk adults vaccinated",NA))))))))))))))


output_all$VaxStrategy = factor(output_all$VaxStrategy, levels=c('50% all vaccinated','50% kids age <=15 and high risk adults','50% age <=15 vaccinated','50% age <5 vaccinated',
                                                                 '80% all vaccinated','80% kids age <=15 and high risk adults','80% age <=15 vaccinated','80% age <5 vaccinated',
                                                                 '100% all vaccinated','100% kids age <=15 and high risk adults','100% age <=15 vaccinated','100% age <5 vaccinated',
                                                                 '80% high risk adults vaccinated','100% high risk adults vaccinated'))


output_all$cases_averted_u5 <- as.numeric(output_all[which(output_all$Vax_Province=="None"),"Cases_Age_u5"])-as.numeric(output_all$Cases_Age_u5)
output_all$cases_averted_u15 <- as.numeric(output_all[which(output_all$Vax_Province=="None"),"Cases_Age_u15"])-as.numeric(output_all$Cases_Age_u15)
output_all$cases_averted_o15LR <- as.numeric(output_all[which(output_all$Vax_Province=="None"),"Cases_Age_o15LR"])-as.numeric(output_all$Cases_Age_o15LR)
output_all$cases_averted_o15HR <- as.numeric(output_all[which(output_all$Vax_Province=="None"),"Cases_Age_o15HR"])-as.numeric(output_all$Cases_Age_o15HR)
output_all$deaths_averted_u5 <- as.numeric(output_all[which(output_all$Vax_Province=="None"),"Deaths_Age_u5"])-as.numeric(output_all$Deaths_Age_u5)
output_all$deaths_averted_u15 <- as.numeric(output_all[which(output_all$Vax_Province=="None"),"Deaths_Age_u15"])-as.numeric(output_all$Deaths_Age_u15)
output_all$deaths_averted_o15LR <- as.numeric(output_all[which(output_all$Vax_Province=="None"),"Deaths_Age_o15LR"])-as.numeric(output_all$Deaths_Age_o15LR)
output_all$deaths_averted_o15HR <- as.numeric(output_all[which(output_all$Vax_Province=="None"),"Deaths_Age_o15HR"])-as.numeric(output_all$Deaths_Age_o15HR)

output_all$total_vaccine_doses <- output_all$Vax_Doses_Age_u5+ output_all$Vax_Doses_Age_u15+ output_all$Vax_Doses_Age_o15LR+ output_all$Vax_Doses_Age_o15HR 
output_all$total_deaths_averted <- output_all$deaths_averted_u5 + output_all$deaths_averted_u15 + output_all$deaths_averted_o15LR + output_all$deaths_averted_o15HR   
output_all$total_cases_averted <- output_all$cases_averted_u5 + output_all$cases_averted_u15 + output_all$cases_averted_o15LR + output_all$cases_averted_o15HR
  
output_all$vaxdoses_per_caseaverted <- output_all$total_vaccine_doses/output_all$total_cases_averted 
output_all$vaxdoses_per_deathaverted <- output_all$total_vaccine_doses/output_all$total_deaths_averted 


output_all <- output_all[-c(10:12,26:28,42:44),]

output_all_deaths <- output_all[which(output_all$VaxStrategy=="50% age <5 vaccinated"|output_all$VaxStrategy=="50% age <=15 vaccinated"|output_all$VaxStrategy=="50% all vaccinated"|
                                        output_all$VaxStrategy=="80% age <5 vaccinated"|output_all$VaxStrategy=="80% age <=15 vaccinated"|output_all$VaxStrategy=="80% all vaccinated"), c(1,19,32)]
output_all_cases <- output_all[which(output_all$VaxStrategy=="50% age <5 vaccinated"|output_all$VaxStrategy=="50% age <=15 vaccinated"|output_all$VaxStrategy=="50% all vaccinated"|
                                       output_all$VaxStrategy=="80% age <5 vaccinated"|output_all$VaxStrategy=="80% age <=15 vaccinated"|output_all$VaxStrategy=="80% all vaccinated"), c(1,19,31)]

output_HR_all_deaths <- output_all[which(output_all$Vax_Age_o15HR!=0.0 & output_all$Vax_Age_o15LR==0.0 & output_all$Vax_Province!="None"), c(1,19,32)]
output_HR_all_cases <- output_all[which(output_all$Vax_Age_o15HR!=0.0 & output_all$Vax_Age_o15LR==0.0 & output_all$Vax_Province!="None"), c(1,19,31)]

output_100_all_deaths <- output_all[which(output_all$Vax_Age_u5==1.0|output_all$Vax_Age_u15==1.0|output_all$Vax_Age_o15LR==1.0), c(1,19,32)]
output_100_all_cases <- output_all[which(output_all$Vax_Age_u5==1.0|output_all$Vax_Age_u15==1.0|output_all$Vax_Age_o15LR==1.0), c(1,19,31)]


outcomes_plot_deaths <- ggplot(data=output_all_deaths, aes(x=VaxStrategy, y=vaxdoses_per_deathaverted/1000, fill=VaxStrategy)) + geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Vax_Province, ncol=1) + theme_classic() + theme(axis.text.x = element_blank(),text=element_text(size=20),strip.text = element_text(
    size = 20)) +
  xlab("") + ylab("Number vaccine doses per death averted, thousands") + labs(fill="Vaccine strategy")

outcomes_plot_cases <- ggplot(data=output_all_cases, aes(x=VaxStrategy, y=vaxdoses_per_caseaverted/1000, fill=VaxStrategy)) + geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Vax_Province, ncol=1) + theme_classic() + theme(axis.text.x = element_blank(), text=element_text(size=20),strip.text = element_text(
    size = 20))+
  xlab("") + ylab("Number vaccine doses per case averted, thousands") + labs(fill="Vaccine strategy")

outcomes_HR_plot_deaths <- ggplot(data=output_HR_all_deaths, aes(x=VaxStrategy, y=vaxdoses_per_deathaverted/1000, fill=VaxStrategy)) + geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Vax_Province, ncol=1) + theme_classic() + theme(axis.text.x = element_blank(),text=element_text(size=20),strip.text = element_text(
    size = 20)) +
  xlab("") + ylab("Number vaccine doses per death averted, thousands") + labs(fill="Vaccine strategy")

outcomes_HR_plot_cases <- ggplot(data=output_HR_all_cases, aes(x=VaxStrategy, y=vaxdoses_per_caseaverted/1000, fill=VaxStrategy)) + geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Vax_Province, ncol=1) + theme_classic() + theme(axis.text.x = element_blank(),text=element_text(size=20),strip.text = element_text(
    size = 20))+
  xlab("") + ylab("Number vaccine doses per case averted, thousands") + labs(fill="Vaccine strategy")

outcomes_100_plot_deaths <- ggplot(data=output_100_all_deaths, aes(x=VaxStrategy, y=vaxdoses_per_deathaverted/1000, fill=VaxStrategy)) + geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Vax_Province, ncol=1) + theme_classic() + theme(axis.text.x = element_blank(), text=element_text(size=20),strip.text = element_text(
    size = 20)) +
  xlab("") + ylab("Number vaccine doses per death averted, thousands") + labs(fill="Vaccine strategy")

outcomes_100_plot_cases <- ggplot(data=output_100_all_cases, aes(x=VaxStrategy, y=vaxdoses_per_caseaverted/1000, fill=VaxStrategy)) + geom_bar(stat="identity", position="dodge") + 
  facet_wrap(~Vax_Province, ncol=1) + theme_classic() + theme(axis.text.x = element_blank(), text=element_text(size=20),strip.text = element_text(
    size = 20))+
  xlab("") + ylab("Number vaccine doses per case averted, thousands") + labs(fill="Vaccine strategy")


pdf(file = "results/Figure5_VaxDose_Per_Cases_Averted_3424.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(outcomes_plot_cases)
# Step 3: Run dev.off() to create the file!
dev.off()


pdf(file = "results/Figure6_VaxDose_Per_Death_Averted_3424.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(outcomes_plot_deaths)
# Step 3: Run dev.off() to create the file!
dev.off()

### Supplemental Figures

pdf(file = "results/SupplementalFigure7_VaxDose_Per_Cases_Averted_100Vax_3424.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(outcomes_100_plot_cases)
# Step 3: Run dev.off() to create the file!
dev.off()


pdf(file = "results/SupplementalFigure8_VaxDose_Per_Deaths_Averted_100Vax_3424.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(outcomes_100_plot_deaths)
# Step 3: Run dev.off() to create the file!
dev.off()

pdf(file = "results/SupplementalFigure9_VaxDose_Per_Cases_Averted_HRVax_3424.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(outcomes_HR_plot_cases)
# Step 3: Run dev.off() to create the file!
dev.off()


pdf(file = "results/SupplementalFigure10_VaxDose_Per_Deaths_Averted_HRVax_3424.pdf",   # The directory you want to save the file in
    width = 12, # The width of the plot in inches
    height = 12) # The height of the plot in inches

# Step 2: Create the plot with R code
plot(outcomes_HR_plot_deaths)
# Step 3: Run dev.off() to create the file!
dev.off()