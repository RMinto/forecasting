############################################################
## shiny user interface function
############################################################
source('Home_Module.R')
source('APEX_Module.R')
source('FIFA_Module.R')
source('Sims4_Module.R')
source('ORIGIN_Module.R')
source('pkgsndata.R')

shinyUI(
  
  navbarPage(
    
    title = "WWCE DATA SCIENCE",
    
    collapsible = TRUE, inverse = TRUE, theme = shinytheme("simplex"),
    
    tabPanel("HOME", home_UI("home")),
    tabPanel("FIFA", fifa_UI("fifa")),
    tabPanel("APEX", apex_UI("apex")),
    tabPanel("Sims4", sims4_UI("sims4")),
    tabPanel("ORIGIN", origin_UI("origin"))
  )
)

