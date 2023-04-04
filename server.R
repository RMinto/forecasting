################# 2. SHINY SERVER FUNCTION #################
source('Home_Module.R')
source('APEX_Module.R')
source('FIFA_Module.R')
source('Sims4_Module.R')
source('ORIGIN_Module.R')
source('pkgsndata.R')

server <- function(input, output) {
  callModule(home_server, "home")
  callModule(fifa_server, "fifa")
  callModule(apex_server, "apex")
  callModule(sims4_server, "sims4")
  callModule(origin_server, "origin")
}

############################################################

############### End of Server Script #######################

############################################################
