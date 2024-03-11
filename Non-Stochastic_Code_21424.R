
library(shiny)
library(deSolve)
library(ggplot2)
library(reshape2)
library(tidyverse)

demographics <- read.csv("Demographic_data_2.4.24.csv")

output_all <- data.frame(matrix(0, nrow=49, ncol=18))
colnames(output_all) <- c("Vax_Province", "Vax_Age_u5","Vax_Age_u15","Vax_Age_o15LR","Vax_Age_o15HR",
                          "Cases_Age_u5","Cases_Age_u15","Cases_Age_o15LR","Cases_Age_o15HR",
                          "Deaths_Age_u5","Deaths_Age_u15","Deaths_Age_o15LR","Deaths_Age_o15HR",
                          "Vax_Doses_Age_u5","Vax_Doses_Age_u15","Vax_Doses_Age_o15LR","Vax_Doses_Age_o15HR","Run")

output_all$Vax_Province <- c("None",rep(list("All"),16),rep(list("Endemic"),16),rep(list("Historic"),16))
output_all$Vax_Age_u5 <- c(0, rep(list(0,0,.50,.80,1.00,.50,.80,1.00,0.5,0.8,1,.80,.50,0.5,0.8,1.0),3))
output_all$Vax_Age_u15 <- c(0, rep(list(0,0,0,0,0,.50,.80,1.00,0.5, 0.8,1.0, .80,.50,0.5,0.8,1.0),3))
output_all$Vax_Age_o15LR <- c(0, rep(list(0,0,0,0,0,0,0,0,0.5, 0.8, 1.0,0, 0,0.5,0.8,1.0),3))
output_all$Vax_Age_o15HR <- c(0, rep(list(1.00,.80,0,0,0,0,0,0,0,0,0,.80,.50,0.5,0.8,1.0),3))
output_all$Run <- c(1,rep(list(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17),3))


for (p in 1:nrow(output_all)) {
  

results_all <- data.frame(matrix(0, nrow=0, ncol=22))
colnames(results_all) <- c( "time","S_u5","I_u5","V_u5","D_u5",            
                                          "R_u5","S_u15", "I_u15","V_u15","D_u15",           
                                          "R_u15" ,"S_o15LR", "I_o15LR","V_o15LR","D_o15LR",         
                                          "R_o15LR","S_o15HR","I_o15HR","V_o15HR","D_o15HR",           
                                          "R_o15HR","province")
    
    # Set initial conditions for model
    
    initial_inf_u5 = 0
    initial_inf_u15 = 0
    initial_inf_o15LR = 0
    initial_inf_o15HR = 0
    
    # Set vaccination strategy
    # 
    vax_effect <- 0.75 #input$vax_effect
  
    
    
    #proportion of infections that are sylvatic
    
    prop_sylvatic = 0.4
    prop_sylvatic_adult = 0.6 #proportion of sylvatic infections in those 5-15
    
    
    
 for (i in 1:nrow(demographics)) {
   
   vax_S_u5 <- as.numeric(output_all[p,"Vax_Age_u5"])* vax_effect
   vax_S_u15 <- as.numeric(output_all[p,"Vax_Age_u15"])* vax_effect
   vax_S_o15LR <- as.numeric(output_all[p,"Vax_Age_o15LR"])* vax_effect
   vax_S_o15HR <- as.numeric(output_all[p,"Vax_Age_o15HR"])* vax_effect
   
      province = demographics[i,1]
      
      
      
      # this assumes that half of all sylvatic transmission happens in adults, and 1/4 each in children's age groups
      exogshock_u5= (demographics[i,9]*prop_sylvatic)*(1-prop_sylvatic_adult)/2
      exogshock_u15= (demographics[i,9]*prop_sylvatic)*prop_sylvatic_adult
      exogshock_o15= (demographics[i,9]*prop_sylvatic)*(1-prop_sylvatic_adult)/2
      
      population = as.numeric(demographics[i,2]) + as.numeric(demographics[i,3]) + as.numeric(demographics[i,4]) #population of region
      exograte_u5 = exogshock_u5/365  # rate of exogenous shocks, under 5
      exograte_u15 = exogshock_u15/365  # rate of exogenous shocks, under 15
      exograte_o15 = exogshock_o15/365  # rate of exogenous shocks, over 15
      recoveryrate = 1/21 # recovery rate
      
      
      u5_pop = as.numeric(demographics[i,2]) # proportion of pop under 5
      o15_pop = as.numeric(demographics[i,3]) # proportion of pop over  15
      u15_pop = as.numeric(demographics[i,4]) # proportion of pop over 5 and under 15
      propHR = 0.05#input$propHR #as.numeric(demographics[i,5])
      
      contacts_u5 = 5.44 # number of daily physical contacts, under 5
      contacts_u15 = 6.88 # number of daily physical contacts, under 15
      contacts_o15 = 4.788 # number of daily physical contacts, over 15
      contacts_sex = 10/30 # sexual contacts for high risk, daily (average 15 a month- made up)
      
      contactprop_u5u5 = 0.235 # proportion of physical contacts for under 5 with under 5
      contactprop_u5u15 = 0.235 # proportion of physical contacts for under 5 with under 15
      contactprop_u5o15 = 1-contactprop_u5u5-contactprop_u5u15 # proportion of physical contacts for under 5 with over 15
      
      contactprop_u15u5 = 0.235# proportion of physical contacts for under 15 with under 5
      contactprop_u15u15 = 0.235 # proportion of physical contacts for under 15 with under 15
      contactprop_u15o15 = 1-contactprop_u15u5-contactprop_u15u15 # proportion of physical contacts for under 15 with over 15
      
      contactprop_o15u5 = 0.135 # proportion of physical contacts for over 15 with under 5
      contactprop_o15u15 = 0.135 # proportion of physical contacts for over 15 with under 15
      contactprop_o15o15 = 1-contactprop_o15u5-contactprop_o15u15 # proportion of physical contacts for over 15 with over 15
      
      SA_u5 = 0.1168/4 # secondary attack rate when infected person is under 5, halved to account for data coming from household transmission
      SA_u15 = 0.0803/4 # secondary attack rate when infected person is under 15, halved to account for data coming from household transmission
      SA_o15 = 0.0133/3 # secondary attack rate when infected person is over 15, halved to account for data coming from household transmission
      SA_sex = 0.15 # secondary attack rate from sex, made up for now
      
      
      mortality_u5 = 0.08 #input$cfr_u5 #0.12 # mortality rate under 5
      mortality_u15 = 0.05 #input$cfr_u15 #0.09 # mortality rate under 15
      mortality_o15 = 0.03 #input$cfr_o15 #0.035 # mortality rate over 15
      
      
      historical= as.numeric(demographics[i,"Historical"])
      endemic= as.numeric(demographics[i,"Endemic"])
      
      
      if (output_all[p,"Vax_Province"]=="None"|output_all[p,"Vax_Province"]=="All") {
        
        vax_S_u52 <- vax_S_u5
        vax_S_u152 <- vax_S_u15
        vax_S_o15LR2 <- vax_S_o15LR
        vax_S_o15HR2 <- vax_S_o15HR
        
      } else if (output_all[p,"Vax_Province"]=="Historic") {
        
        vax_S_u52 <- ifelse(historical==1, vax_S_u5, 0)
        vax_S_u152 <- ifelse(historical==1, vax_S_u15, 0)
        vax_S_o15LR2 <- ifelse(historical==1, vax_S_o15LR, 0)
        vax_S_o15HR2 <- ifelse(historical==1, vax_S_o15HR, 0)  
        
       } else if (output_all[p,"Vax_Province"]=="Endemic") {
         
         vax_S_u52 <- ifelse(endemic==1, vax_S_u5, 0)
         vax_S_u152 <- ifelse(endemic==1, vax_S_u15, 0)
         vax_S_o15LR2 <- ifelse(endemic==1, vax_S_o15LR, 0)
         vax_S_o15HR2 <- ifelse(endemic==1, vax_S_o15HR, 0)
         
       } 
      

  
        vax_S_u5 <- vax_S_u52
        vax_S_u15 <- vax_S_u152
        vax_S_o15LR <- vax_S_o15LR2
        vax_S_o15HR <-  vax_S_o15HR2
      
      
      
      yinit = c(
        S_u5 = as.integer(u5_pop-initial_inf_u5-u5_pop*vax_S_u5),  
        I_u5 = as.integer(initial_inf_u5), 
        V_u5=as.integer(u5_pop*vax_S_u5), 
        D_u5=0, 
        R_u5=0,
        S_u15 = as.integer(u15_pop-initial_inf_u15-u15_pop*vax_S_u15),
        I_u15 = as.integer(initial_inf_u15),
        V_u15=as.integer(u15_pop*vax_S_u15),
        D_u15=0,
        R_u15=0,
        S_o15LR = as.integer(o15_pop*(1-propHR)-initial_inf_o15LR-o15_pop*(1-propHR)*vax_S_o15LR), 
        I_o15LR = as.integer(initial_inf_o15LR), 
       # V_o15LR=as.integer(o15_pop*propHR*0.21+(o15_pop-o15_pop*0.21)*propHR*vax_S_o15LR), 
        V_o15LR= as.integer(o15_pop*(1-propHR)*vax_S_o15LR), 
        D_o15LR=0, 
        R_o15LR=0,
        S_o15HR = as.integer(o15_pop*propHR-initial_inf_o15HR-(o15_pop*propHR*vax_S_o15HR)), 
        I_o15HR = as.integer(initial_inf_o15HR), 
        V_o15HR= as.integer(o15_pop*propHR*vax_S_o15HR),
       # V_o15HR=as.integer(o15_pop*propHR*0.21+(o15_pop-o15_pop*0.21)*propHR*vax_S_o15HR), 
        D_o15HR=0 , 
        R_o15HR=0
      )
      
      
      vac_model <- function(times,yinit,pars){
        
        with(as.list(c(yinit,pars)), {
          
          N_u5 = S_u5 + V_u5 + I_u5 + R_u5
          N_u15 = S_u15 + V_u15 + I_u15 + R_u15
          N_o15LR = S_o15LR + V_o15LR + I_o15LR + R_o15LR
          N_o15HR = S_o15HR + V_o15HR + I_o15HR + R_o15HR
          
          
          dS_u5 <- -beta_u5u5*S_u5*(I_u5/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_u5u15*S_u5*(I_u15/(N_u5+N_u15+N_o15LR+N_o15HR)) -   
            beta_u5o15*S_u5*(I_o15LR/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_u5o15*S_u5*(I_o15HR/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            (theta_u5-theta_u5*(V_u5/N_u5))
          dI_u5 <- beta_u5u5*S_u5*(I_u5/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_u5u15*S_u5*(I_u15/(N_u5+N_u15+N_o15LR+N_o15HR)) +   
            beta_u5o15*S_u5*(I_o15LR/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_u5o15*S_u5*(I_o15HR/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            (theta_u5-theta_u5*(V_u5/N_u5))-
            mu_u5*I_u5 - rho*I_u5
          dV_u5 <- 0
          dD_u5 <- mu_u5*I_u5
          dR_u5 <- rho*I_u5 
          
          
          
          dS_u15 <- -beta_u15u5*S_u15*(I_u5/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_u15u15*S_u15*(I_u15/(N_u5+N_u15+N_o15LR+N_o15HR)) -   
            beta_u15o15*S_u15*(I_o15LR/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_u15o15*S_u15*(I_o15HR/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            (theta_u15-theta_u15*(V_u15/N_u15))
          dI_u15 <- beta_u15u5*S_u15*(I_u5/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_u15u15*S_u15*(I_u15/(N_u5+N_u15+N_o15LR+N_o15HR)) +   
            beta_u15o15*S_u15*(I_o15LR/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_u15o15*S_u15*(I_o15HR/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            (theta_u15-theta_u15*(V_u15/N_u15))-
            mu_u15*I_u15 - rho*I_u15 
          dV_u15 <- 0
          dD_u15 <- mu_u15*I_u15
          dR_u15 <- rho*I_u15 
         
          
          dS_o15LR <- -beta_o15u5*S_o15LR*(I_u5/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_o15u15*S_o15LR*(I_u15/(N_u5+N_u15+N_o15LR+N_o15HR)) -   
            beta_o15o15*S_o15LR*(I_o15LR/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_o15o15*S_o15LR*(I_o15HR/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            (theta_o15LR-theta_o15LR*(V_o15LR/N_o15LR)) 
          dI_o15LR <- beta_o15u5*S_o15LR*(I_u5/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_o15u15*S_o15LR*(I_u15/(N_u5+N_u15+N_o15LR+N_o15HR)) +   
            beta_o15o15*S_o15LR*(I_o15LR/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_o15o15*S_o15LR*(I_o15HR/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            (theta_o15LR-theta_o15LR*(V_o15LR/N_o15LR))-
            mu_o15*I_o15LR - rho*I_o15LR 
          dV_o15LR <- 0
          dD_o15LR <- mu_o15*I_o15LR
          dR_o15LR <- rho*I_o15LR 

          
          
          dS_o15HR <- -beta_o15u5*S_o15HR*(I_u5/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_o15u15*S_o15HR*(I_u15/(N_u5+N_u15+N_o15LR+N_o15HR)) -   
            beta_o15o15*S_o15HR*(I_o15LR/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_o15o15*S_o15HR*(I_o15HR/(N_u5+N_u15+N_o15LR+N_o15HR)) -
            beta_o15o15HR*S_o15HR/N_o15HR-
            (theta_o15HR-theta_o15HR*(V_o15HR/N_o15HR)) 
          dI_o15HR <- beta_o15u5*S_o15HR*(I_u5/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_o15u15*S_o15HR*(I_u15/(N_u5+N_u15+N_o15LR+N_o15HR)) +   
            beta_o15o15*S_o15HR*(I_o15LR/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_o15o15*S_o15HR*(I_o15HR/(N_u5+N_u15+N_o15LR+N_o15HR)) +
            beta_o15o15HR*S_o15HR/N_o15HR +
            (theta_o15HR-theta_o15HR*(V_o15HR/N_o15HR)) -
            mu_o15*I_o15HR - rho*I_o15HR 
          dV_o15HR <- 0
          dD_o15HR <- mu_o15*I_o15HR
          dR_o15HR <- rho*I_o15HR 

          
          
          return(list(c(dS_u5, dI_u5, dV_u5, dD_u5,dR_u5, 
                        dS_u15, dI_u15, dV_u15,dD_u15,dR_u15, 
                        dS_o15LR, dI_o15LR, dV_o15LR,dD_o15LR, dR_o15LR, 
                        dS_o15HR, dI_o15HR, dV_o15HR, dD_o15HR, dR_o15HR)))})	
        
      }
      
      
      
      # Setting parameters
      pars = list(
        beta_u5u5= SA_u5*contacts_u5*contactprop_u5u5, #beta for transmission between u5 and u5
        beta_u5u15= SA_u5*contacts_u5*contactprop_u5u15, #beta for transmission between u5 and u15
        beta_u5o15= SA_u5*contacts_u5*contactprop_u5o15, #beta for transmission between u5 and o15
        
        beta_u15u5= SA_u15*contacts_u15*contactprop_u15u5, #beta for transmission between u15 and u5
        beta_u15u15= SA_u15*contacts_u15*contactprop_u15u15, #beta for transmission between u15 and u15
        beta_u15o15= SA_u15*contacts_u15*contactprop_u15o15, #beta for transmission between u15 and o15
        
        beta_o15u5= SA_o15*contacts_o15*contactprop_o15u5, #beta for transmission between o15 and u5
        beta_o15u15= SA_o15*contacts_o15*contactprop_o15u15, #beta for transmission between o15 and u15
        beta_o15o15= SA_o15*contacts_o15*contactprop_o15o15, #beta for transmission between o15 and o15
        
        beta_o15o15HR = SA_sex * contacts_sex,
        
        #  (u5_pop/(population)*(beta_u5u5+beta_u5u15+beta_u5o15)/3 + u15_pop/(population)*(beta_u15u5+beta_u15u15+beta_u15o15)/3 +
        #   o15_pop/(population)*(beta_o15u5+beta_o15u15+beta_o15o15)/3)*21 # Estimate of R0
        
        rho= recoveryrate, #recovery rate
        mu_u5 = mortality_u5/21, #death rate, under 5
        mu_u15 = mortality_u15/21, #death rate, under 15
        mu_o15 = mortality_o15/21, #death rate, over 15
        theta_u5= exograte_u5, #rate of exogenous shocks
        theta_u15= exograte_u15, #rate of exogenous shocks
        theta_o15HR= exograte_o15*propHR, #rate of exogenous shocks
        theta_o15LR = exograte_o15*(1-propHR),
        omega_u5= 0, #vaccination rate, under 5
        omega_u15= 0, #vaccination rate, under 15
        omega_o15LR= 0, #vaccination rate, over 15 low risk
        omega_o15HR= 0  #vaccination rate, over 15 high risk 
        
      )
      
      start_time = 0 # start date
      end_time = 365 # end date 
      times <- seq(start_time, end_time, by = 1) 
      
      results <- as.data.frame(ode(y=yinit, times=times, func=vac_model,parms=pars,
                                   method='lsoda'))
      

  results$province <- province
  
  results_all <- rbind(results_all, results)
  
  #dataset for final deaths statistics

 }
    
        
  Data_Summary_Overall_365 <- results_all %>%
          filter(time==365) %>%
          mutate(total_u5 = S_u5 + I_u5 + V_u5 + D_u5 + R_u5,
                 total_u15 = S_u15 + I_u15 + V_u15 + D_u15 + R_u15,
                 total_o15LR = S_o15LR + I_o15LR + V_o15LR + D_o15LR + R_o15LR,
                 total_o15HR = S_o15HR + I_o15HR + V_o15HR + D_o15HR + R_o15HR,
                 total_o15 = S_o15LR + I_o15LR + V_o15LR + D_o15LR + R_o15LR + S_o15HR + I_o15HR + V_o15HR + D_o15HR + R_o15HR,
                 total_infected_u5 = I_u5 + D_u5 + R_u5,
                 total_infected_u15 = I_u15 + D_u15 + R_u15,
                 total_infected_o15 = I_o15LR + D_o15LR + R_o15LR + I_o15HR + D_o15HR + R_o15HR,
                 total_infected_o15LR = I_o15LR + D_o15LR + R_o15LR ,
                 total_infected_o15HR = I_o15HR + D_o15HR + R_o15HR ,
                 total_infected = I_u5 + D_u5 + R_u5 + I_u15 + D_u15 + R_u15 + I_o15LR + D_o15LR + R_o15LR + I_o15HR + D_o15HR + R_o15HR,
                 total_deaths = D_u5 + D_u15 + D_o15LR + D_o15HR,
                 total_vaccinated = V_u5 + V_u15 + V_o15LR + V_o15HR) %>%
          summarize(infections1 = sum(total_infected),
                    infections_u51 = sum(total_infected_u5),
                    infections_u151 = sum(total_infected_u15),
                    infections_o151 = sum(total_infected_o15),
                    infections_o15LR1 = sum(total_infected_o15LR),
                    infections_o15HR1 = sum(total_infected_o15HR),
                    deaths_u51 = sum(D_u5),
                    deaths_u151 = sum(D_u15),
                    deaths_o15LR1 = sum(D_o15LR),
                    deaths_o15HR1 = sum(D_o15HR),
                    deaths1 = sum(total_deaths),
                    vaccines1 = sum(total_vaccinated)/vax_effect,
                    vaccines_u5 = sum(V_u5)/vax_effect,
                    vaccines_u15 = sum(V_u15)/vax_effect,
                    vaccines_o15LR = sum(V_o15LR)/vax_effect,
                    vaccines_o15HR = sum(V_o15HR)/vax_effect)
      
      Data_Summary_Overall_365 <- list(
        infections = Data_Summary_Overall_365$infections1,
        infections_u5 = Data_Summary_Overall_365$infections_u51,
        infections_u15 = Data_Summary_Overall_365$infections_u151,
        infections_o15LR = Data_Summary_Overall_365$infections_o15LR1,
        infections_o15HR = Data_Summary_Overall_365$infections_o15HR1,
        
        deaths_u5 = Data_Summary_Overall_365$deaths_u51,
        deaths_u15 = Data_Summary_Overall_365$deaths_u151,
        deaths_o15LR = Data_Summary_Overall_365$deaths_o15LR1,
        deaths_o15HR = Data_Summary_Overall_365$deaths_o15HR1,
        deaths = Data_Summary_Overall_365$deaths1,
        
        vaccines = Data_Summary_Overall_365$vaccines1,
        vaccines_u5 = Data_Summary_Overall_365$vaccines_u5,
        vaccines_u15 = Data_Summary_Overall_365$vaccines_u15,
        vaccines_o15LR = Data_Summary_Overall_365$vaccines_o15LR,
        vaccines_o15HR = Data_Summary_Overall_365$vaccines_o15HR
        )
      
      
  output_all[p,"Cases_Age_u5"] <- Data_Summary_Overall_365$infections_u5
  output_all[p,"Cases_Age_u15"] <- Data_Summary_Overall_365$infections_u15
  output_all[p,"Cases_Age_o15LR"] <- Data_Summary_Overall_365$infections_o15LR
  output_all[p,"Cases_Age_o15HR"] <- Data_Summary_Overall_365$infections_o15HR
  
  output_all[p,"Deaths_Age_u5"] <- Data_Summary_Overall_365$deaths_u5
  output_all[p,"Deaths_Age_u15"] <- Data_Summary_Overall_365$deaths_u15
  output_all[p,"Deaths_Age_o15LR"] <- Data_Summary_Overall_365$deaths_o15LR
  output_all[p,"Deaths_Age_o15HR"] <- Data_Summary_Overall_365$deaths_o15HR
  
  output_all[p,"Vax_Doses_Age_u5"] <- Data_Summary_Overall_365$vaccines_u5*2
  output_all[p,"Vax_Doses_Age_u15"] <- Data_Summary_Overall_365$vaccines_u15*2
  output_all[p,"Vax_Doses_Age_o15LR"] <- Data_Summary_Overall_365$vaccines_o15LR*2
  output_all[p,"Vax_Doses_Age_o15HR"] <- Data_Summary_Overall_365$vaccines_o15HR*2
      
  }

getwd()

output_all1 <- t(apply(output_all, 1, unlist))

output_all1 <- as.data.frame(output_all1)

write.csv(output_all1, file="results/SA/vacciation_results_3724.csv")
  
  