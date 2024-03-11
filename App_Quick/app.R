#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(scales)
library(gridExtra)
library(shiny)
library(shinydashboard)
library(kableExtra)
library(priceR)
library(deSolve)
library(ggplot2)
library(reshape2)
library(tidyverse)

input_element_color <- "primary" 
highlight_color <- "red" 
regular_color <- "red"


demographics <- read.csv("Demographic_data_2.4.24.csv")
province_list <- demographics$Province
province_list <- c("All", province_list)

# Define UI for application that draws a histogram


header <- dashboardHeader(
  tags$li(
    class = "dropdown"),
  title = "DRC Mpox Vaccine Model", titleWidth = 500
)

sidebar <- dashboardSidebar(
  tags$head(tags$style(HTML('.content-wrapper { height: 1800px !important;}'))),
  sidebarMenu(
    id = "sidebar",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    #menuItem("Source Code", icon = icon("file-code-o")),
    menuItem("References", tabName = "references", icon = icon("book"))
  )
)

body <- dashboardBody(
 
  
  tabItems(
    tabItem(
      # MAIN DASHBOARD ---------------------------------------------------
      tabName = "dashboard",
      ## INPUTS --------
      column(width = 4,
             ## Population
             box(title = "Inputs", width = NULL, solidHeader = TRUE, status = input_element_color,
                 collapsible = TRUE, collapsed = FALSE,
                 h3("Time Period and Province"),
                 sliderInput("time_period", 
                             "Run model for how many days?",
                             min=0,
                             max=1000,
                             step=1,
                             value=365),
                 
                 selectInput("province_select", "Select province",
                             choices = province_list),
                 
                 selectInput("year_select", "Mpox outbreak severity",
                             choices = list("outbreak year"=9,"low endemic year"=5,"high endemic year"=7)),
                 
                 selectInput("underreport", "Account for underreporting",
                             choices = list("Reported cases"=1,"Estimated actual cases"=2)),
                 
                 h3("Vaccine Inputs"),
                 selectInput("vaccination", "Vaccine Strategy, age", c("No vaccine", 
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
                 
                 h3("Vaccine Inputs"),
                 selectInput("vaccination_province", "Vaccine Strategy, provinces vaccinated", c("All","Endemic Cases","Historical Cases")),
                 
                 sliderInput("vax_effect", 
                             "Vaccine Effectiveness (2 dose)",
                             min=0,
                             max=1,
                             step=0.05,
                             value=0.75),
                 
                 h3("Disease Inputs"), 
                 
                 sliderInput("prop_sylvatic", 
                             "Proportion of infections coming from sylvatic transmission",
                             min=0,
                             max=1,
                             step=0.05,
                             value=0.4),
                 
                 sliderInput("prop_sylvatic_u15", 
                             "Proportion of sylvatic infections in those 5-15",
                             min=0,
                             max=1,
                             step=0.05,
                             value=0.6),
                 
                 sliderInput("cfr_u5", 
                             "Mortality proportion in children under 5 (CFR)",
                             min=0,
                             max=0.5,
                             step=0.01,
                             value=0.08),
                 
                 sliderInput("cfr_u15", 
                             "Mortality proportion in children under 15 (CFR)",
                             min=0,
                             max=0.5,
                             step=0.01,
                             value=0.05),
                 
                 sliderInput("cfr_o15", 
                             "Mortality proportion in adults over 15 (CFR)",
                             min=0,
                             max=0.5,
                             step=0.01,
                             value=0.03),
                 
                 sliderInput("propHR", 
                             "Proportion of adult population at high risk of sexual transmission",
                             min=0,
                             max=0.5,
                             step=0.01,
                             value=0.05),
                 
                 
                  #submitButton(text = "Apply Changes", icon = NULL, width = NULL)
                 # 
                #  actionButton("update" ,"Update View", icon("refresh"),
                 #              class = "btn btn-primary")
             
                )
      ),
      
      
      ## OUTPUT: plot and metrics --------
      
      column(width=8,

              fluidRow(h3("Cases")),
                fluidRow(valueBoxOutput("Cases", width=3),
                         valueBoxOutput("Cases_u5", width=3),
                         valueBoxOutput("Cases_u15", width=3),
                         valueBoxOutput("Cases_o15", width=3)),
              fluidRow(h3("Deaths")),
                fluidRow(valueBoxOutput("Deaths", width=3),
                        valueBoxOutput("Deaths_u5", width=3),
                        valueBoxOutput("Deaths_u15", width=3),
                        valueBoxOutput("Deaths_o15", width=3)),
              fluidRow(h3("Vaccines")),
                fluidRow(valueBoxOutput("Vaccines", width=3),
                        valueBoxOutput("Doses", width=3))
      )),
    
    # ,
    ## References ----------------------------------------------------------
    tabItem(
      tabName = "references"
  )
)
)
  

  
ui <- dashboardPage(header, sidebar, body)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  model <- reactive({
    req(input$time_period,
        input$vaccination,
        input$vax_effect,
        input$prop_sylvatic,
        input$prop_sylvatic_u15,
        input$cfr_u5,
        input$cfr_u15,
        input$cfr_o15,
        input$propHR,
        input$year_select,
        input$underreport,
        input$vaccination_province,
        cancelOutput = FALSE
    )
    
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
    vax_S_u51 <- ifelse(input$vaccination=="No vaccination", 0,
                      ifelse(input$vaccination=="All under 5s"|input$vaccination=="All under 15s", 1,
                       ifelse(input$vaccination=="80% under 5s"|input$vaccination=="80% under 15s"|input$vaccination=="80% under 5s and 80% high-risk adults", 0.8,
                              ifelse(input$vaccination=="50% under 5s"|input$vaccination=="50% under 15s",0.5,
                                     ifelse(input$vaccination=="20% under 5s"|input$vaccination=="20% under 15s", 0.2,0)))))
    vax_S_u151 <- ifelse(input$vaccination=="No vaccination", 0,
                        ifelse(input$vaccination=="All under 15s", 1,
                        ifelse(input$vaccination=="80% under 15s", 0.8,
                               ifelse(input$vaccination=="50% under 15s",0.5,
                                      ifelse(input$vaccination=="20% under 15s",0.2,0)))))
    vax_S_o15LR1 <- 0
    vax_S_o15HR1 <- ifelse(input$vaccination=="No vaccination", 0,
                          ifelse(input$vaccination=="All high-risk adults", 1,
                          ifelse(input$vaccination=="80% high-risk adults"|input$vaccination=="80% under 5s and 80% high-risk adults", 0.8,0)))


    vax_effect <- as.numeric(input$vax_effect)
    
    

    
    
    #proportion of infections that are sylvatic
    
    prop_sylvatic = input$prop_sylvatic
    prop_sylvatic_u15 = input$prop_sylvatic_u15
    
    j <- as.numeric(input$year_select)
    
    underreport <- ifelse(input$underreport==1, 1, 0.41)
  
    
    for (i in 1:nrow(demographics)) {
      
      vax_S_u5 <- as.numeric(vax_S_u51)* vax_effect
      vax_S_u15 <- as.numeric(vax_S_u151)* vax_effect
      vax_S_o15LR <- as.numeric(vax_S_o15LR1)* vax_effect
      vax_S_o15HR <- as.numeric(vax_S_o15HR1)* vax_effect
      
      province = demographics[i,1]
      endemic = as.numeric(demographics[i,13])
      historical = as.numeric(demographics[i,14])

      # this assumes that half of all sylvatic transmission happens in adults, and 1/4 each in children's age groups
      exogshock_u5= ((demographics[i,j]*prop_sylvatic)*(1-prop_sylvatic_u15)/2)/underreport
      exogshock_u15= ((demographics[i,j]*prop_sylvatic)*(prop_sylvatic_u15))/underreport
      exogshock_o15= ((demographics[i,j]*prop_sylvatic)*(1-prop_sylvatic_u15)/2)/underreport
      
      population = as.numeric(demographics[i,2]) + as.numeric(demographics[i,3]) + as.numeric(demographics[i,4]) #population of region
      exograte_u5 = exogshock_u5/365  # rate of exogenous shocks, under 5
      exograte_u15 = exogshock_u15/365  # rate of exogenous shocks, under 15
      exograte_o15 = exogshock_o15/365  # rate of exogenous shocks, over 15
      recoveryrate = 1/21 # recovery rate
      
      
      u5_pop = as.numeric(demographics[i,2]) # proportion of pop under 5
      o15_pop = as.numeric(demographics[i,3]) # proportion of pop over  15
      u15_pop = as.numeric(demographics[i,4]) # proportion of pop over 5 and under 15
      propHR =input$propHR #as.numeric(demographics[i,5])
      
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
      
      
      mortality_u5 = input$cfr_u5 #0.12 # mortality rate under 5
      mortality_u15 = input$cfr_u15 #0.09 # mortality rate under 15
      mortality_o15 = input$cfr_o15 #0.035 # mortality rate over 15

      
      vaccination_province = input$vaccination_province
      
      
      if (vaccination_province=="All") {
        
        vax_S_u52 <- vax_S_u5
        vax_S_u152 <- vax_S_u15
        vax_S_o15LR2 <- vax_S_o15LR
        vax_S_o15HR2 <- vax_S_o15HR
        
      } 
      
      if (vaccination_province=="Historical Cases") {
        
        vax_S_u52 <- ifelse(historical==1, vax_S_u5, 0)
        vax_S_u152 <- ifelse(historical==1, vax_S_u15, 0)
        vax_S_o15LR2 <- ifelse(historical==1, vax_S_o15LR, 0)
        vax_S_o15HR2 <- ifelse(historical==1, vax_S_o15HR, 0)  
        
      } 
      
      if (vaccination_province=="Endemic Cases") {
        
        vax_S_u52 <- ifelse(endemic==1, vax_S_u5, 0)
        vax_S_u152 <- ifelse(endemic==1, vax_S_u15, 0)
        vax_S_o15LR2 <- ifelse(endemic==1, vax_S_o15LR, 0)
        vax_S_o15HR2 <- ifelse(endemic==1, vax_S_o15HR, 0)
        
      } 
      
      
      yinit = c(
        S_u5 = as.integer(u5_pop-initial_inf_u5-u5_pop*vax_S_u52),  
        I_u5 = as.integer(initial_inf_u5), 
        V_u5= as.integer(u5_pop*vax_S_u52),
        D_u5=0, 
        R_u5=0,
        S_u15 = as.integer(u15_pop-initial_inf_u15-u15_pop*vax_S_u152),
        I_u15 = as.integer(initial_inf_u15),
        V_u15=as.integer(u15_pop*vax_S_u152),
        D_u15=0,
        R_u15=0,
        S_o15LR = as.integer(o15_pop-initial_inf_o15LR-o15_pop*vax_S_o15LR2), 
        I_o15LR = as.integer(initial_inf_o15LR), 
        V_o15LR= as.integer(o15_pop*vax_S_o15LR2), 
        D_o15LR=0, 
        R_o15LR=0,
        S_o15HR = as.integer(o15_pop*propHR-initial_inf_o15HR-(o15_pop*propHR*vax_S_o15HR2+o15_pop*propHR*vax_S_o15LR2)), 
        I_o15HR = as.integer(initial_inf_o15HR), 
        V_o15HR=as.integer(o15_pop*propHR*vax_S_o15HR2+o15_pop*propHR*vax_S_o15LR2), 
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
      end_time = input$time_period #365 # end date 
      times <- seq(start_time, end_time, by = 1) 
      
      results <- as.data.frame(ode(y=yinit, times=times, func=vac_model,parms=pars,
                                   method='lsoda'))
      
      
      results$province <- province
      
      results_all <- rbind(results_all, results)
      
      
    }
    
    model <- results_all
    
    


  })
    
# Data_Summary_Province <- reactive({
# 
#       Data_Summary_Province <- model() %>%
#         mutate(total_u5 = rowSums(results_all[,c(2:6)]),
#                total_u15 = rowSums(results_all[,c(7:11)]),
#                total_o15 = rowSums(results_all[,c(12:21)]),
#                total_infected_u5 = total_u5 - rowSums(results_all[,c(2,4)]),
#                total_infected_u15 = total_u15 - rowSums(results_all[,c(7,9)]),
#                total_infected_o15 = total_o15 - rowSums(results_all[,c(12,14,17,19)]),
#                total_infected = total_infected_u5 +   total_infected_u15  + total_infected_o15,
#                total_deaths = D_u5 + D_u15 + D_o15LR + D_o15HR,
#                total_vaccinated = V_u5 + V_u15 + V_o15LR + V_o15HR)
#     })


    # Data_Summary_Overall <- reactive({
    # 
    #   Data_Summary_Overall <- results_all() %>%
    #         group_by(time) %>%
    #         summarize(infections = sum(total_infected),
    #             infections_u5 = sum(total_infected_u5),
    #             infections_u15 = sum(total_infected_u15),
    #             infections_o15 = sum(total_infected_o15),
    #             deaths_u5 = sum(D_u5),
    #             deaths_u15 = sum(D_u15),
    #             deaths_o15LR = sum(D_o15LR),
    #             deaths_o15HR = sum(D_o15HR),
    #             deaths = sum(total_deaths),
    #             vaccines = sum(total_vaccinated))
    # })
    
    Data_Summary_Overall_365 <- reactive({
      
      
      if (input$province_select=="All") {
        
        Data_Summary_Overall_365 <- model() %>%
          filter(time==input$time_period) %>%
          mutate(total_u5 = S_u5 + I_u5 + V_u5 + D_u5 + R_u5,
                 total_u15 = S_u15 + I_u15 + V_u15 + D_u15 + R_u15,
                 total_o15LR = S_o15LR + I_o15LR + V_o15LR + D_o15LR + R_o15LR,
                 total_o15HR = S_o15HR + I_o15HR + V_o15HR + D_o15HR + R_o15HR,
                 total_o15 = S_o15LR + I_o15LR + V_o15LR + D_o15LR + R_o15LR + S_o15HR + I_o15HR + V_o15HR + D_o15HR + R_o15HR,
                 total_infected_u5 = I_u5 + D_u5 + R_u5,
                 total_infected_u15 = I_u15 + D_u15 + R_u15,
                 total_infected_o15 = I_o15LR + D_o15LR + R_o15LR + I_o15HR + D_o15HR + R_o15HR,
                 total_infected = I_u5 + D_u5 + R_u5 + I_u15 + D_u15 + R_u15 + I_o15LR + D_o15LR + R_o15LR + I_o15HR + D_o15HR + R_o15HR,
                 total_deaths = D_u5 + D_u15 + D_o15LR + D_o15HR,
                 total_vaccinated = V_u5 + V_u15 + V_o15LR + V_o15HR) %>%
          summarize(infections1 = sum(total_infected),
                    infections_u51 = sum(total_infected_u5),
                    infections_u151 = sum(total_infected_u15),
                    infections_o151 = sum(total_infected_o15),
                    deaths_u51 = sum(D_u5),
                    deaths_u151 = sum(D_u15),
                    deaths_o15LR1 = sum(D_o15LR),
                    deaths_o15HR1 = sum(D_o15HR),
                    deaths1 = sum(total_deaths),
                    vaccines1 = sum(total_vaccinated)/input$vax_effect)
        
      } else if (input$province_select != "All") {
        
        Data_Summary_Overall_365 <- model() %>%
          filter(time==input$time_period & province==input$province_select) %>%
          mutate(total_u5 = S_u5 + I_u5 + V_u5 + D_u5 + R_u5,
                 total_u15 = S_u15 + I_u15 + V_u15 + D_u15 + R_u15,
                 total_o15LR = S_o15LR + I_o15LR + V_o15LR + D_o15LR + R_o15LR,
                 total_o15HR = S_o15HR + I_o15HR + V_o15HR + D_o15HR + R_o15HR,
                 total_o15 = S_o15LR + I_o15LR + V_o15LR + D_o15LR + R_o15LR + S_o15HR + I_o15HR + V_o15HR + D_o15HR + R_o15HR,
                 total_infected_u5 = I_u5 + D_u5 + R_u5,
                 total_infected_u15 = I_u15 + D_u15 + R_u15,
                 total_infected_o15 = I_o15LR + D_o15LR + R_o15LR + I_o15HR + D_o15HR + R_o15HR,
                 total_infected = I_u5 + D_u5 + R_u5 + I_u15 + D_u15 + R_u15 + I_o15LR + D_o15LR + R_o15LR + I_o15HR + D_o15HR + R_o15HR,
                 total_deaths = D_u5 + D_u15 + D_o15LR + D_o15HR,
                 total_vaccinated = V_u5 + V_u15 + V_o15LR + V_o15HR) %>%
          summarize(infections1 = sum(total_infected),
                    infections_u51 = sum(total_infected_u5),
                    infections_u151 = sum(total_infected_u15),
                    infections_o151 = sum(total_infected_o15),
                    deaths_u51 = sum(D_u5),
                    deaths_u151 = sum(D_u15),
                    deaths_o15LR1 = sum(D_o15LR),
                    deaths_o15HR1 = sum(D_o15HR),
                    deaths1 = sum(total_deaths),
                    vaccines1 = sum(total_vaccinated)/input$vax_effect)
        
        
      }
      
        Data_Summary_Overall_365 <- list(
        infections = Data_Summary_Overall_365$infections1,
        infections_u5 = Data_Summary_Overall_365$infections_u51,
        infections_u15 = Data_Summary_Overall_365$infections_u151,
        infections_o15 = Data_Summary_Overall_365$infections_o151,
        deaths_u5 = Data_Summary_Overall_365$deaths_u51,
        deaths_u15 = Data_Summary_Overall_365$deaths_u151,
        deaths_o15LR = Data_Summary_Overall_365$deaths_o15LR1,
        deaths_o15HR = Data_Summary_Overall_365$deaths_o15HR1,
        deaths = Data_Summary_Overall_365$deaths1,
        vaccines = Data_Summary_Overall_365$vaccines1)

    })
  
    
    output$Cases <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$infections), big.mark = ","), "Total mpox infections",
               color="maroon")
    })
    
    output$Cases_u5 <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$infections_u5), big.mark = ","), "Total mpox infections, under 5s",
               color="maroon")
    })
    
    output$Cases_u15 <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$infections_u15), big.mark = ","), "Total mpox infections, 5-15",
               color="maroon")
    })
    
    output$Cases_o15 <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$infections_o15), big.mark = ","), "Total mpox infections, adults",
               color="maroon")
    })
    
    output$Deaths <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$deaths), big.mark = ","), "Total mpox deaths",
               color="navy")
    })

    output$Deaths_u5 <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$deaths_u5), big.mark = ","), "Total mpox deaths, under 5s",
               color="navy")
    })

    output$Deaths_u15 <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$deaths_u15), big.mark = ","), "Total mpox deaths, 5-15",
               color="navy")
    })
    
    output$Deaths_o15 <- renderValueBox({
      valueBox(prettyNum(trunc(sum(Data_Summary_Overall_365()$deaths_o15LR,Data_Summary_Overall_365()$deaths_o15HR)), big.mark = ","), "Total mpox deaths, adults",
               color="navy")
    })


    output$Vaccines <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$vaccines), big.mark = ","), "Total number vaccinated",
               color="teal")
    })
    
    
    output$Doses <- renderValueBox({
      valueBox(prettyNum(trunc(Data_Summary_Overall_365()$vaccines*2), big.mark = ","), "Total number vaccine doses",
               color="teal")
    })
    
  }
  
  
# Run the application 
shinyApp(ui = ui, server = server)
