# Code for table of results 
# 
# 
output_all <- read.csv(file="results/SA/vacciation_results_3724.csv")
output_all <- output_all[which(output_all$Vax_Age_u5=="0.5"|output_all$Vax_Age_u5=="0.8"|output_all$Vax_Province=="None"|output_all$Vax_Age_o15HR=="0.8"|output_all$Vax_Age_u5==1.0|output_all$Vax_Age_o15HR==1.0),2:19]

output_all$VaxStrategy <- ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 & output_all$Vax_Age_o15HR==1.0 & output_all$Vax_Age_o15LR=="1.0", "100% all vaccinated",
                                 ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" & output_all$Vax_Age_o15HR=="0.8"& output_all$Vax_Age_o15LR=="0.8", "80% all vaccinated",
                                        ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5" & output_all$Vax_Age_o15HR=="0.5"& output_all$Vax_Age_o15LR=="0.5", "50% all vaccinated",
                                               ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" & output_all$Vax_Age_o15HR=="0.8", "80% kids under 15 and high risk adults",
                                                      ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 & output_all$Vax_Age_o15HR==1.0, "100% kids under 15 and high risk adults",
                                                             ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5" & output_all$Vax_Age_o15HR=="0.5", "50% kids under 15 and high risk adults",
                                                                    ifelse(output_all$Vax_Age_u5=="0.8" & output_all$Vax_Age_u15=="0.8" , "80% under 15s vaccinated",
                                                                           ifelse(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 ,"100% under 15s vaccinated",
                                                                                  ifelse(output_all$Vax_Age_u5=="0.5" & output_all$Vax_Age_u15=="0.5", "50% under 15s vaccinated",
                                                                                         ifelse(output_all$Vax_Age_u5=="0.5", "50% under 5s vaccinated",
                                                                                                ifelse(output_all$Vax_Age_u5=="0.8", "80% under 5s vaccinated", 
                                                                                                       ifelse(output_all$Vax_Age_o15HR=="0.8", "80% high risk adults vaccinated",
                                                                                                              ifelse(output_all$Vax_Age_u5==1.0, "100% under 5s vaccinated",
                                                                                                                     ifelse(output_all$Vax_Age_o15HR==1.0, "100% high risk adults vaccinated",NA))))))))))))))



output_all <- output_all[-which(output_all$Vax_Age_u5==0.5 & output_all$Vax_Age_u15==0.5 & output_all$Vax_Age_o15LR==0.5 & output_all$Vax_Age_o15HR==0),]
output_all <- output_all[-which(output_all$Vax_Age_u5==0.8 & output_all$Vax_Age_u15==0.8 & output_all$Vax_Age_o15LR==0.8 & output_all$Vax_Age_o15HR==0),]
output_all <- output_all[-which(output_all$Vax_Age_u5==1.0 & output_all$Vax_Age_u15==1.0 & output_all$Vax_Age_o15LR==1.0 & output_all$Vax_Age_o15HR==0),]

output_all$cases <- output_all$Cases_Age_u5 + output_all$Cases_Age_u15 + output_all$Cases_Age_o15LR + output_all$Cases_Age_o15HR 
output_all$deaths <- output_all$Deaths_Age_u5 + output_all$Deaths_Age_u15 + output_all$Deaths_Age_o15LR + output_all$Deaths_Age_o15HR 
output_all$vaccines <- output_all$Vax_Doses_Age_u5 + output_all$Vax_Doses_Age_u15 + output_all$Vax_Doses_Age_o15LR + output_all$Vax_Doses_Age_o15HR 

output_all$VaxStrategy <- ifelse(is.na(output_all$VaxStrategy), "No vaccination", output_all$VaxStrategy)

output_all_base <- output_all[-grep("high risk adults",output_all$VaxStrategy),]
output_all_HR <- output_all[grep("high risk adults",output_all$VaxStrategy),]


output_all_table <- output_all_base[,c(1,19:22)]
output_all_table_HR <- output_all_HR[,c(1,19:22)]



library(dplyr)
output_all_table_rounded <- output_all_table %>% 
                              mutate_if(is.numeric, round, digits=-2)

output_all_table_HR_rounded <- output_all_table_HR %>% 
  mutate_if(is.numeric, round, digits=-2)


write.csv(output_all_table_rounded, file="results/SA/results_table_3724.csv")
write.csv(output_all_table_HR_rounded, file="results/SA/results_table_HR_3724.csv")
