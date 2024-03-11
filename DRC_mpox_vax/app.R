#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(adaptivetau)
library(ggplot2)
library(tidyverse)
library(scales)
library(gridExtra)
library(shiny)
library(shinydashboard)
library(shinycssloaders)


demographics <- read.csv("Demographic_data_2.4.24.csv")
province_list <- demographics$Province


# Define UI for application that draws a histogram


ui <- shinyUI(fluidPage(
    
    # Application title
    titlePanel("DRC Mpox Vaccine Model"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        
        h3("Vaccine Inputs"),
        selectInput("vaccination", "Vaccine Strategy", c("No vaccine", 
                                                         "All under 5s",
                                                         "80% under 5s",
                                                         "50% under 5s",
                                                         "20% under 5s",
                                                         "All under 15s",
                                                         "80% under 15s",
                                                         "50% under 15s",
                                                         "20% under 15s",
                                                         "All high-risk adults",
                                                         "80% high-risk adults",
                                                         "80% under 5s and 80% high-risk adults")),
        
        sliderInput("vax_effect", 
                    "Vaccine Effectiveness (2 dose)",
                    min=0,
                    max=1,
                    step=0.05,
                    value=0.75),
        
        sliderInput("prop_sylvatic", 
                    "Proportion of infections coming from sylvatic transmission",
                    min=0,
                    max=1,
                    step=0.05,
                    value=0.1),
        
        sliderInput("prop_sylvatic_adult", 
                    "Proportion of sylvatic infections in adults (vs children)",
                    min=0,
                    max=1,
                    step=0.05,
                    value=0.9),
        
        sliderInput("cfr_u5", 
                    "Mortality proportion in children under 5 (CFR)",
                    min=0,
                    max=0.5,
                    step=0.01,
                    value=0.12),
        
        sliderInput("cfr_u15", 
                    "Mortality proportion in children under 15 (CFR)",
                    min=0,
                    max=0.5,
                    step=0.01,
                    value=0.09),
        
        sliderInput("cfr_o15", 
                    "Mortality proportion in adults over 15 (CFR)",
                    min=0,
                    max=0.5,
                    step=0.01,
                    value=0.035),
        
        sliderInput("propHR", 
                    "Proportion of adult population at high risk of sexual transmission",
                    min=0,
                    max=0.5,
                    step=0.01,
                    value=0.05),
        
        
        h3("Model Runs"),
        sliderInput("runs",
                    "Number of model runs:",
                    min = 1,
                    max = 1000,
                    value = 5),
        
       # submitButton(text = "Apply Changes", icon = NULL, width = NULL)),
       # 
       actionButton("update" ,"Update View", icon("refresh"),
                    class = "btn btn-primary")),
      
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(title = "Total infections",icon = icon("database"), 
                  shinycssloaders::withSpinner(tableOutput("total_infections_province")),
                  plotOutput("I_plot")
                   ), 
          tabPanel(title = "Total deaths",icon = icon("eye"),
                   shinycssloaders::withSpinner(tableOutput("total_deaths_province"))), 
          tabPanel(title = "Total vaccinations",icon = icon("chart-bar"),
                   shinycssloaders::withSpinner(tableOutput("number_vaccinated_province")))
        )
      )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

  output$I_plot <- renderPlot({

    province_list[27] <- "All"
    
    # for the model, general inputs
    # 
    
    runs=input$runs
    
    set.seed(1)
    
    # create datasets to capture data in model
    # 
    # dataset for all infected over time
    province_all_infected_bytime <- list()
    # dataset for total infections over time
    province_totalinfections_bytime <- list()
    #dataset for final infection statistics
    total_infections_province <- data.frame(matrix(0, nrow=0, ncol=8))
    colnames(total_infections_province) <- c("province", "n_min","Q1", "Q95l","median", "Q3","Q95u","max")
    #dataset for final deaths statistics
    total_deaths_province <- data.frame(matrix(0, nrow=0, ncol=8))
    colnames(total_deaths_province) <- c("province", "n_min","Q1", "Q95l","median", "Q3","Q95u","max")
    #dataset for final infections statistics, bye age (medial only)
    total_infections_age_province <- data.frame(matrix(0, nrow=0, ncol=6))
    colnames(total_infections_age_province) <- c("province", "total_cases_u5_median","total_cases_u15_median",  "total_cases_o15LR_median","total_cases_o15HR_median", "total_cases_o15_median")
    #dataset for final deaths statistics, bye age (medial only)
    total_deaths_age_province <- data.frame(matrix(0, nrow=0, ncol=5))
    colnames(total_deaths_age_province) <- c("province", "D_u5_median","D_u15_median",  "D_o15LR_median",
                                             "D_o15HR_median")  
    
    
    #dataset for total vaccinated and vaccine doses
    number_vaccinated_province <- data.frame(matrix(0, nrow=0, ncol=3))
    colnames(number_vaccinated_province) <- c("province", "n_vaccinated", "n_doses")
    
    # Set initial conditions for model
    
    initial_inf_u5 = 0
    initial_inf_u15 = 0
    initial_inf_o15LR = 0
    initial_inf_o15HR = 0
    
    # Set vaccination strategy
    # 
    vax_effect <- input$vax_effect
    
    vax_S_u51 <- ifelse(input$vaccination=="All under 5s"|input$vaccination=="All under 15s", 1, 
                       ifelse(input$vaccination=="80% under 5s"|input$vaccination=="80% under 15s"|input$vaccination=="80% under 5s and 80% high-risk adults", 0.8,
                              ifelse(input$vaccination=="50% under 5s"|input$vaccination=="50% under 15s",0.5,
                                     ifelse(input$vaccination=="20% under 5s"|input$vaccination=="20% under 15s", 0.2,0))))
    vax_S_u151 <-  ifelse(input$vaccination=="All under 15s", 1, 
                          ifelse(input$vaccination=="80% under 15s", 0.8,
                                 ifelse(input$vaccination=="50% under 15s",0.5,
                                        ifelse(input$vaccination=="20% under 15s", 0.2,0))))
    vax_S_o15LR1 <- 0
    vax_S_o15HR1 <-ifelse(input$vaccination=="All high-risk adults", 1, 
                          ifelse(input$vaccination=="80% high-risk adults"|input$vaccination=="80% under 5s and 80% high-risk adults", 0.8, 0))
   
    
    vax_S_u5 <- as.numeric(vax_S_u51)* vax_effect
    vax_S_u15 <- as.numeric(vax_S_u151)* vax_effect
    vax_S_o15LR <- as.numeric(vax_S_o15LR1)* vax_effect
    vax_S_o15HR <- as.numeric(vax_S_o15LR1)* vax_effect
    
    
    #proportion of infections that are sylvatic
    
    prop_sylvatic = input$prop_sylvatic
    prop_sylvatic_adult = input$prop_sylvatic_adult
    
    
    for (i in 1:nrow(demographics)) {
      
      province = demographics[i,1]
      
      # this assumes that half of all sylvatic transmission happens in adults, and 1/4 each in children's age groups
      exogshock_u5= (demographics[i,6]*prop_sylvatic)*(1-prop_sylvatic_adult)/2
      exogshock_u15= (demographics[i,6]*prop_sylvatic)*(1-prop_sylvatic_adult)/2
      exogshock_o15= (demographics[i,6]*prop_sylvatic)*prop_sylvatic_adult
      
      population = as.numeric(demographics[i,2]) + as.numeric(demographics[i,3]) + as.numeric(demographics[i,4]) #population of region
      exograte_u5 = exogshock_u5/365  # rate of exogenous shocks, under 5
      exograte_u15 = exogshock_u15/365  # rate of exogenous shocks, under 15
      exograte_o15 = exogshock_o15/365  # rate of exogenous shocks, over 15
      recoveryrate = 1/21 # recovery rate
      
      
      u5_pop = as.numeric(demographics[i,2]) # proportion of pop under 5
      o15_pop = as.numeric(demographics[i,3]) # proportion of pop over  15
      u15_pop = as.numeric(demographics[i,4]) # proportion of pop over 5 and under 15
      propHR = input$propHR #as.numeric(demographics[i,5])
      
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
      
      SA_u5 = 0.1168/2 # secondary attack rate when infected person is under 5, halved to account for data coming from household transmission
      SA_u15 = 0.0803/2 # secondary attack rate when infected person is under 15, halved to account for data coming from household transmission
      SA_o15 = 0.0133/2 # secondary attack rate when infected person is over 15, halved to account for data coming from household transmission
      SA_sex = 0.15 # secondary attack rate from sex, made up for now
      
      
      mortality_u5 = input$cfr_u5 #0.12 # mortality rate under 5
      mortality_u15 = input$cfr_u15 #0.09 # mortality rate under 15
      mortality_o15 = input$cfr_o15 #0.035 # mortality rate over 15
      
      
      
      init.values = c(
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
        S_o15LR = as.integer(o15_pop-initial_inf_o15LR-o15_pop*vax_S_o15LR), 
        I_o15LR = as.integer(initial_inf_o15LR), 
        V_o15LR= as.integer(o15_pop*vax_S_o15LR), 
        D_o15LR=0, 
        R_o15LR=0,
        S_o15HR = as.integer(o15_pop*propHR-initial_inf_o15HR-(o15_pop*propHR*vax_S_o15HR+o15_pop*propHR*vax_S_o15LR)), 
        I_o15HR = as.integer(initial_inf_o15HR), 
        V_o15HR=as.integer(o15_pop*propHR*vax_S_o15HR+o15_pop*propHR*vax_S_o15LR), 
        D_o15HR=0 , 
        R_o15HR=0
      )
      
      
      # Specify all transitions
      
      transitions = list(
        # under 5
        c(S_u5 = -1, I_u5 = +1), # movement from susceptible to infected, endogenous infection
        c(S_u5 = -1, I_u5 = +1), # movement from susceptible to infected, exogenous infection 
        c(S_u5= -1, V_u5= +1), #movement from susceptible to vaccinated
        c(I_u5 = -1, D_u5 = +1), #movement from infected to dead
        c(I_u5 = -1, R_u5 = +1), #movement from infected to recovered
        
        # 5-15
        c(S_u15 = -1, I_u15 = +1), # movement from susceptible to infected, endogenous infection
        c(S_u15 = -1, I_u15 = +1), # movement from susceptible to infected, exogenous infection 
        c(S_u15= -1, V_u15= +1), #movement from susceptible to vaccinated
        c(I_u15 = -1, D_u15 = +1), #movement from infected to dead
        c(I_u15 = -1, R_u15 = +1), #movement from infected to recovered
        
        # 15+, LR
        c(S_o15LR = -1, I_o15LR = +1), # movement from susceptible to infected, endogenous infection
        c(S_o15LR = -1, I_o15LR = +1), # movement from susceptible to infected, exogenous infection 
        c(S_o15LR= -1, V_o15LR= +1), #movement from susceptible to vaccinated
        c(I_o15LR = -1, D_o15LR = +1), #movement from infected to dead
        c(I_o15LR = -1, R_o15LR = +1), #movement from infected to recovered
        
        # 15+, HR
        c(S_o15HR = -1, I_o15HR = +1), # movement from susceptible to infected, endogenous infection
        c(S_o15HR = -1, I_o15HR = +1), # movement from susceptible to infected, exogenous infection 
        c(S_o15HR= -1, V_o15HR= +1), #movement from susceptible to vaccinated
        c(I_o15HR = -1, D_o15HR = +1), #movement from infected to dead
        c(I_o15HR = -1, R_o15HR = +1) #movement from infected to recovered
        
      )
      
      # Rates for all transitions
      # (In same order as "transitions")
      RateF <- function(x, pars, times) {
        
        # under 5
        return(c(
          pars$beta_u5u5*x["S_u5"]*x["I_u5"]/(x["S_u5"] + x["I_u5"] + x["R_u5"] + x["V_u5"] + x["S_u15"] + x["I_u15"] + x["R_u15"] + x["V_u15"]+
                                                x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]+
                                                x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+
            pars$beta_u5u15*x["S_u5"]*x["I_u15"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] + x["V_u15"]+
                                                    x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]+
                                                    x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+
            pars$beta_u5o15*x["S_u5"]*x["I_o15LR"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] +  x["V_u15"] +
                                                      x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"] +
                                                      x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]) +
            pars$beta_u5o15*x["S_u5"]*x["I_o15HR"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] +  x["V_u15"] +
                                                      x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"] +
                                                      x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]), # movement from susceptible to pre-symptomatic, due to endogenous infection
          #for exogenous shocks- we assume a standard amount annually, broken up into daily rates
          #this is assumed to happen by age, an homogenously across vaccination status, though only those
          #unvaccinated will be infected
          pars$theta_u5-pars$theta_u5*x["V_u5"]/(x["S_u5"]  + x["I_u5"] + x["R_u5"] + x["V_u5"]), # movement from susceptible to infected, exogenous infection
          pars$omega_u5*x["S_u5"], #movement from susceptible to vaccinated
          pars$mu_u5*x["I_u5"], #movement from infected to dead
          pars$rho*x["I_u5"], #movement from infected to recovered
          
          
          # 5- 15
          pars$beta_u15u5*x["S_u15"]*x["I_u5"]/(x["S_u5"] + x["I_u5"] + x["R_u5"] + x["V_u5"] + x["S_u15"] + x["I_u15"] + x["R_u15"] + x["V_u15"]+
                                                  x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]+
                                                  x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+
            pars$beta_u15u15*x["S_u15"]*x["I_u15"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] + x["V_u15"]+
                                                      x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]+
                                                      x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+
            pars$beta_u15o15*x["S_u15"]*x["I_o15LR"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] +  x["V_u15"] +
                                                        x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"] +
                                                        x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]) +
            pars$beta_u15o15*x["S_u15"]*x["I_o15HR"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] +  x["V_u15"] +
                                                        x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"] +
                                                        x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]), # movement from susceptible to pre-symptomatic, due to endogenous infection
          pars$theta_u15-pars$theta_u15*x["V_u15"]/(x["S_u15"]  + x["I_u15"] + x["R_u15"] + x["V_u15"]), # movement from susceptible to infected, exogenous infection
          pars$omega_u15*x["S_u15"], #movement from susceptible to vaccinated
          pars$mu_u15*x["I_u15"], #movement from infected to dead
          pars$rho*x["I_u15"], #movement from infected to recovered
          
          # 15+ LR
          pars$beta_o15u5*x["S_o15LR"]*x["I_u5"]/(x["S_u5"] + x["I_u5"] + x["R_u5"] + x["V_u5"] + x["S_u15"] + x["I_u15"] + x["R_u15"] + x["V_u15"]+
                                                    x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]+
                                                    x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+
            pars$beta_o15u15*x["S_o15LR"]*x["I_u15"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] + x["V_u15"]+
                                                        x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]+
                                                        x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+
            pars$beta_o15o15*x["S_o15LR"]*x["I_o15LR"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] +  x["V_u15"] +
                                                          x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"] +
                                                          x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]) +
            pars$beta_o15o15*x["S_o15LR"]*x["I_o15HR"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] +  x["V_u15"] +
                                                          x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"] +
                                                          x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]), # movement from susceptible to pre-symptomatic, due to endogenous infection
          pars$theta_o15LR-pars$theta_o15LR*x["V_o15LR"]/(x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]), # movement from susceptible to infected, exogenous infection
          pars$omega_o15*x["S_o15LR"], #movement from susceptible to vaccinated
          pars$mu_o15*x["I_o15LR"], #movement from infected to dead
          pars$rho*x["I_o15LR"], #movement from infected to recovered
          
          # 15+ HR
          pars$beta_o15u5*x["S_o15HR"]*x["I_u5"]/(x["S_u5"] + x["I_u5"] + x["R_u5"] + x["V_u5"] + x["S_u15"] + x["I_u15"] + x["R_u15"] + x["V_u15"]+
                                                    x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]+
                                                    x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+
            pars$beta_o15u15*x["S_o15HR"]*x["I_u15"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] + x["V_u15"]+
                                                        x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"]+
                                                        x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+
            pars$beta_o15o15*x["S_o15HR"]*x["I_o15LR"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] +  x["V_u15"] +
                                                          x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"] +
                                                          x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]) +
            pars$beta_o15o15*x["S_o15HR"]*x["I_o15HR"]/(x["S_u5"]  + x["I_u5"]  + x["R_u5"] + x["V_u5"]+ x["S_u15"]  + x["I_u15"] + x["R_u15"] +  x["V_u15"] +
                                                          x["S_o15LR"]  + x["I_o15LR"] + x["R_o15LR"] + x["V_o15LR"] +
                                                          x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"])+      
            pars$beta_o15o15HR*x["S_o15HR"]*x["I_o15HR"]/ (x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]), # movement from susceptible to pre-symptomatic, due to endogenous infection
          pars$theta_o15HR - pars$theta_o15HR*x["V_o15HR"]/(x["S_o15HR"]  + x["I_o15HR"] + x["R_o15HR"] + x["V_o15HR"]), # movement from susceptible to infected, exogenous infection
          pars$omega_o15HR*x["S_o15HR"] +  pars$omega_o15*x["S_o15HR"], #movement from susceptible to vaccinated
          pars$mu_o15*x["I_o15HR"], #movement from infected to dead
          pars$rho*x["I_o15HR"] #movement from infected to recovered
        ))
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
        omega_o15= 0, #vaccination rate, over 15 low risk
        omega_o15HR= 0  #vaccination rate, over 15 high risk 
        
      )
      
      
      #  Create dataset
      
      results_all_susceptible <- data.frame(matrix(0, nrow=0, ncol=6))
      colnames(results_all_susceptible) <- c("time", "S_u5","S_u15", "S_o15LR","S_o15HR", "run")
      
      results_all_vaccinated <- data.frame(matrix(0, nrow=0, ncol=6))
      colnames(results_all_vaccinated) <- c("time", "V_u5","V_u15", "V_o15LR", "V_o15HR", "run")
      
      results_all_infected <- data.frame(matrix(0, nrow=0, ncol=6))
      colnames(results_all_infected) <- c("time", "I_u5","I_u15", "I_o15LR","I_o15HR","run")
      
      results_all_recovered <- data.frame(matrix(0, nrow=0, ncol=6))
      colnames(results_all_recovered) <- c("time", "R_u5","R_u15", "R_o15LR", "R_o15HR","run")
      
      results_all_dead<- data.frame(matrix(0, nrow=0, ncol=6))
      colnames(results_all_dead) <- c("time", "D_u5", "D_u15", "D_o15LR","D_o15HR", "run")
      
      
      for (k in 1:runs) {
        
        
        results = as.data.frame(ssa.adaptivetau(init.values, 
                                                transitions, 
                                                RateF, 
                                                pars, 
                                                tf=365))
        
        
        results_susceptible_k <- results[,c(1,2,7,12,17)]
        results_susceptible_k$run <- paste0(k)
        
        results_all_susceptible <- rbind(results_all_susceptible, results_susceptible_k)
        
        results_infected_k <-  results[,c(1,3,8,13,18)]
        results_infected_k$run <- paste0(k)
        
        results_all_infected <- rbind(results_all_infected, results_infected_k)
        
        results_vaccinated_k <-  results[,c(1,4,9,14,19)]
        results_vaccinated_k$run <- paste0(k)
        
        results_all_vaccinated <- rbind(results_all_vaccinated, results_vaccinated_k)
        
        results_dead_k <-  results[,c(1,5,10, 15,20)]
        results_dead_k$run <- paste0(k)
        
        results_all_dead <- rbind(results_all_dead, results_dead_k)
        
        results_recovered_k <- results[,c(1,6,11, 16,21)]
        results_recovered_k$run <- paste0(k)
        
        results_all_recovered <- rbind(results_all_recovered, results_recovered_k)
        
        
      }
      
      # Total active infections over time
      # 
      # 
      # 
      
      results_all_infected$time2 <- round(results_all_infected$time)
      
      results_all_infected$I <- results_all_infected$I_u5 + results_all_infected$I_u15 + results_all_infected$I_o15LR + results_all_infected$I_o15HR
      
      results_all_infected2 <- results_all_infected %>%
        group_by(run,time2) %>%
        summarise_at(vars(I), list(maxI = max))
      
      results_all_infected3 <- results_all_infected2 %>%
        group_by(time2) %>%
        summarise_at(vars(maxI), list(nmin=min, Q1=~quantile(., probs = 0.25), Q95l=~quantile(., probs = 0.05),
                                      median=median, Q3=~quantile(., probs = 0.75),Q95u=~quantile(., probs = 0.95),
                                      max=max))
      
      results_all_infected3$province <- province
      
      
      province_all_infected_bytime[[length(province_all_infected_bytime)+1]] <- results_all_infected3
      
      
      # Total deaths over time 
      # 
      
      total_deaths <- results_all_dead
      
      total_deaths <- total_deaths[which(total_deaths$time==365),]
      
      total_deaths$total_deaths <- total_deaths$D_u5 + total_deaths$D_u15 + total_deaths$D_o15LR + total_deaths$D_o15HR 
      
      total_deaths <- total_deaths[, c(1,7)]
      
      total_deaths2 <- total_deaths %>%
        summarise_at(vars(total_deaths), list(nmin=min, Q1=~quantile(., probs = 0.25), Q95l=~quantile(., probs = 0.05),
                                              median=median, Q3=~quantile(., probs = 0.75),Q95u=~quantile(., probs = 0.95),
                                              max=max))
      
      total_deaths_province[i,1] <- province
      total_deaths_province[i,2:8] <- total_deaths2[1,1:7]
      
      
      
      # Total infections over time (counting recovered)
      
      total_infections1 <- cbind(results_all_recovered, results_all_dead, results_all_infected)
      
      total_infections <- total_infections1[which(total_infections1$time==365),]
      
      total_infections$total_cases <- total_infections$R_u5 + total_infections$R_u15 + total_infections$R_o15LR + total_infections$R_o15HR +
        total_infections$D_u5 + total_infections$D_u15 + total_infections$D_o15LR + total_infections$D_o15HR +
        total_infections$I_u5 + total_infections$I_u15 + total_infections$I_o15LR + total_infections$I_o15HR 
      
      
      total_infections <- total_infections[,c(6,21)]
      
      total_infections2 <- total_infections %>%
        summarise_at(vars(total_cases), list(nmin=min, Q1=~quantile(., probs = 0.25), Q95l=~quantile(., probs = 0.05),
                                             median=median, Q3=~quantile(., probs = 0.75),Q95u=~quantile(., probs = 0.95),
                                             max=max))
      
      total_infections_province[i,1] <- province
      total_infections_province[i,2:8] <- total_infections2[1,1:7]
      
      #total infections by age overall
      
      total_infections_age <- total_infections1[which(total_infections1$time==365),]
      
      total_infections_age$total_cases_u5 <- total_infections_age$R_u5 + total_infections_age$D_u5 +total_infections_age$I_u5  
      total_infections_age$total_cases_u15 <- total_infections_age$R_u15 + total_infections_age$D_u15 +total_infections_age$I_u15  
      total_infections_age$total_cases_o15LR <- total_infections_age$R_o15LR + total_infections_age$D_o15LR +total_infections_age$I_o15LR  
      total_infections_age$total_cases_o15HR <- total_infections_age$R_o15HR + total_infections_age$D_o15HR +total_infections_age$I_o15HR  
      total_infections_age$total_cases_o15 <- total_infections_age$total_cases_o15HR + total_infections_age$total_cases_o15LR
      
      total_infections_age <- total_infections_age[,c(21:25)]
      
      # total_infections_age2 <- total_infections_age %>%
      #   summarise_at(vars(total_cases_u5, total_cases_u15, total_cases_o15HR, total_cases_o15LR, total_cases_o15), list(nmin=min, Q1=~quantile(., probs = 0.25), Q95l=~quantile(., probs = 0.05),
      #                                        median=median, Q3=~quantile(., probs = 0.75),Q95u=~quantile(., probs = 0.95),
      #                                        max=max))
      
      total_infections_age2 <- total_infections_age %>%
        summarise_at(vars(total_cases_u5, total_cases_u15, total_cases_o15HR, total_cases_o15LR, total_cases_o15), list(median=median))
      
      total_infections_age_province[i,1] <- province
      total_infections_age_province[i,2:6] <- total_infections_age2[1,1:5]
      
      # longitudinal over time calculation
      
      total_infections_long <- total_infections1
      total_infections_long$total_cases <-  total_infections1$R_u5 + total_infections1$R_u15 + total_infections1$R_o15LR + total_infections1$R_o15HR +
        total_infections1$D_u5 + total_infections1$D_u15 + total_infections1$D_o15LR + total_infections1$D_o15HR +
        total_infections1$I_u5 + total_infections1$I_u15 + total_infections1$I_o15LR + total_infections1$I_o15HR 
      
      total_infections_long <- total_infections_long[,c(1,6,21)]
      
      
      total_infections_long$time2 <- round(total_infections_long$time)
      
      total_infections_long2 <- total_infections_long %>%
        group_by(run,time2) %>%
        summarise_at(vars(total_cases), list(maxTotalCases = max))
      
      total_infections_long3 <- total_infections_long2 %>%
        group_by(time2) %>%
        summarise_at(vars(maxTotalCases), list(nmin=min, Q1=~quantile(., probs = 0.25), Q95l=~quantile(., probs = 0.05),
                                               median=median, Q3=~quantile(., probs = 0.75),Q95u=~quantile(., probs = 0.95),
                                               max=max)) 
      
      total_infections_long3$province <- province
      
      province_totalinfections_bytime[[length(province_totalinfections_bytime)+1]] <- total_infections_long3
      
      
      
      # Total vaccinated people/ total vaccines needed
      # 
      
      results_all_vaccinated$total_vaccines <- results_all_vaccinated$V_u5 + results_all_vaccinated$V_u15 + results_all_vaccinated$V_o15LR + results_all_vaccinated$V_o15HR 
      
      results_all_vaccinated <- results_all_vaccinated[which(results_all_vaccinated$time==365,),7]
      
      number_vaccinated_province[i,1] <- province
      number_vaccinated_province[i,2] <- mean(results_all_vaccinated)/vax_effect
      number_vaccinated_province[i,3] <- (mean(results_all_vaccinated)/vax_effect)*2
      
      #total deaths by age overall
      
      total_deaths_age <- results_all_dead
      total_deaths_age <- total_deaths_age[which(total_deaths_age$time==365),]
      
      # Calculate summary stats by age, only median for now
      # total_deaths_age2 <- total_deaths_age %>%
      #   summarise_at(vars(D_u5, D_u15, D_o15LR, D_o15HR), list(nmin=min, Q1=~quantile(., probs = 0.25), Q95l=~quantile(., probs = 0.05),
      #                                         median=median, Q3=~quantile(., probs = 0.75),Q95u=~quantile(., probs = 0.95),
      #                                         max=max))
      
      total_deaths_age2 <- total_deaths_age %>%
        summarise_at(vars(D_u5, D_u15, D_o15LR, D_o15HR), list(median=median))
      
      total_deaths_age_province[i,1] <- province
      total_deaths_age_province[i,2:5] <- total_deaths_age2[1,1:4]
      
      # total_infections_age2 <- total_infections_age %>%
      #   summarise_at(vars(total_cases_u5, total_cases_u15, total_cases_o15HR, total_cases_o15LR, total_cases_o15), list(nmin=min, Q1=~quantile(., probs = 0.25), Q95l=~quantile(., probs = 0.05),
      #                                        median=median, Q3=~quantile(., probs = 0.75),Q95u=~quantile(., probs = 0.95),
      #                                        max=max))
      
      
    }
    
    total_infections_province[27,2:8] <- colSums(total_infections_province[,2:8])
    total_infections_age_province[27,2:6] <- colSums(total_infections_age_province[,2:6])
    total_deaths_province[27,2:8] <- colSums(total_deaths_province[,2:8])
    total_deaths_age_province[27,2:5] <- colSums(total_deaths_age_province[,2:5])
    number_vaccinated_province[27,2:3] <- colSums(number_vaccinated_province[,2:3])
    
    total_infections_province[27,1] <- "Total"
    total_infections_age_province[27,1] <- "Total"
    total_deaths_province[27,1] <- "Total"
    total_deaths_age_province[27,1] <- "Total"
    number_vaccinated_province[27,1] <- "Total"
    
   # total_infections_province <- datatable(total_infections_province)
  
    output$total_infections_province <- renderTable({input$update
      total_infections_province})
    output$total_deaths_province <- renderTable({input$update
      total_deaths_province})
    output$number_vaccinated_province <- renderTable({input$update
                                                     number_vaccinated_province})
    
    })

    



}


# Run the application 
shinyApp(ui = ui, server = server)
