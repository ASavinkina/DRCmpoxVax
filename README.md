Code to recreate analyses for DRC Mpox Vaccination Manuscript.

Description of files:

Data:
Demographic_data_2.4.24.csv: to be read-in, contains province-level data on population by age, and number of mpox cases in 2023.

Model Code:
Non-Stochastic_Code_7824.R: Code which creates and runs the DRC mpox vaccination model. Requires Demographic_data_2.4.24.csv to run. This runs the model for all vaccination scenarios in all ages and provinces, and then outputs a large output file of model results including number of Susceptible, Infected, Recovered, Dead, and Vaccinated at each time step in each age group and province. 

Results Code:
Results_and_Graphs_72624.R: outputs main text and supplement figures using output from Non-Stochastic_Code_7824.R.

Results_part2_72624.R : outputs vaccine doses per case/death averted plots and data using output from Non-Stochastic_Code_7824.R.

Additionally, shiny app which also runs this model and can allow for variation of major model parameters can be found here:
https://savinkina.shinyapps.io/App_Quick/



