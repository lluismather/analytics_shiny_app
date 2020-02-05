
library(shiny)
library(shinyWidgets)
library(shinythemes)

# lists ======================

# list of the regions
region_options <- c("Eastern", "East Midlands", "London", "North", "North West", 
                    "Scotland", "South East", "South West", "Wales", 
                    "West Midlands", "Yorkshire and Humber")

# list of the target seat categories
seat_category <- c("Attack", "Defense", "Other")

# list of the current parties holding seats
seat_holder <- c("Green", "Labour", "LibDem", "Other", "Plaid", "Tory", "SNP")

# list of the different levels of activist
activist_type <- c("None", "Donor", "Dialogue", "Doorstep", "Chatter", "Event", "Roleholder", "Signup")

# list of the different types of membership
membership_type <- c("member", "registered", "affiliated")

# list of the membership status types
membership_status <- c("ACCEPTED", "ARREARS", "CANCELLED", "DELETED", "LAPSED", "PART-ARREARS", "PRE-LAPSE", "RESIGNED")

# to turn the clp selector on or off
clp_override <- c("On", "Off")


# ui ========================
shinyUI(navbarPage(
  
                   windowTitle="Membership and Activism",
                   title=div(#div(img(src="logo.svg", height=35)), 
                             div(p("Analytics App", class="app-title")), 
                             a("Help?", href="mailto:lluismather@gmail.com?subject=membership%20shinyapp%20help", class="help-link")),
                   theme=shinytheme("lumen"),
                   
                   tags$head(
                     #includeHTML(("ga.html")),
                     #tags$link(rel="shortcut icon", href="favicon.ico"),
                     tags$link(rel="stylesheet", type="text/css", href="lbranalytics.css?version=51")
                   ),
                   
                   fluidRow(
                     # left side bar ===============
                     column(2,
                            
                            uiOutput("picker"),
                            
                            pickerInput(
                              inputId = "seat_category",
                              label = "Seat Type:",
                              choices = seat_category,
                              selected = seat_category,
                              multiple = TRUE,
                              options = list(
                                `actions-box` = TRUE,
                                `selected-text-format` = "count > 2",
                                `count-selected-text` = "{0} of {1} categories"
                              )
                            ),
                            
                            pickerInput(
                              inputId = "region_groups",
                              label = "Regions:",
                              choices = region_options,
                              selected = region_options,
                              multiple = TRUE,
                              options = list(
                                `actions-box` = TRUE,
                                `selected-text-format` = "count > 2",
                                `count-selected-text` = "{0} of {1} regions"
                              )
                            ),
                          
                            pickerInput(
                              inputId = "seat_held",
                              label = "Seat Held By:",
                              choices = seat_holder,
                              selected = seat_holder,
                              multiple = TRUE,
                              options = list(
                                `actions-box` = TRUE,
                                `selected-text-format` = "count > 2",
                                `count-selected-text` = "{0} of {1} parties"
                              )
                            ),
                            
                            pickerInput(
                              inputId = "activist_type",
                              label = "Activist Type:",
                              choices = activist_type,
                              selected = activist_type,
                              multiple = TRUE,
                              options = list(
                                `actions-box` = TRUE,
                                `selected-text-format` = "count > 2",
                                `count-selected-text` = "{0} of {1} activist types"
                              )
                            ),
                            
                            pickerInput(
                              inputId = "membership_type",
                              label = "Membership Type:",
                              choices = membership_type,
                              selected = membership_type,
                              multiple = TRUE,
                              options = list(
                                `actions-box` = TRUE,
                                `selected-text-format` = "count > 2",
                                `count-selected-text` = "{0} of {1} memberships"
                              )
                            ),
                            
                            pickerInput(
                              inputId = "membership_status",
                              label = "Membership Status:",
                              choices = membership_status,
                              selected = membership_status,
                              multiple = TRUE,
                              options = list(
                                `actions-box` = TRUE,
                                `selected-text-format` = "count > 2",
                                `count-selected-text` = "{0} of {1} statuses"
                              )
                            ),
                            
                            pickerInput(
                              inputId = "clp_override",
                              label = "CLP Override:",
                              choices = clp_override,
                              selected = "Off",
                              multiple = FALSE,
                              options = list(
                                `actions-box` = TRUE,
                                `selected-text-format` = "count > 2",
                                `count-selected-text` = "{0} of {1} parties"
                              )
                            )
                            ),
                     
                     # display the tabs and plots in the middle seciton ===============
                     column(7,
                            tabsetPanel(id="main_view",
                              tabPanel("Age"),
                              tabPanel("Membership Length"),
                              tabPanel("Donors"),
                              tabPanel("Activists"),
                              tabPanel("Issues")
                              ),
                            
                            # main plot ==============
                            plotOutput("main_plot", width="95%", height="550px"),
                            
                            # display the grouping buttons under the main plots ================
                            radioGroupButtons("graph_break", label = "View data by:",
                                              choices = c("Overall"="all",
                                                          "Region"="region_name",
                                                          "Seat Held"="seat_held",
                                                          "Membership Type"="mem_type",
                                                          "Activism Type"="activism_type"),
                                              selected = "all",
                                              direction = "horizontal",
                                              justified = TRUE,
                                              status = "primary"
                            ),
                            radioGroupButtons("graph_type", label = "View plot as:",
                                              choices = c("Histogram"="histogram",
                                                          "Density"="density"),
                                              selected = "histogram",
                                              direction = "horizontal",
                                              justified = TRUE,
                                              status = "primary"
                            )
                            ),
                     
                     # display the data table clp picker at the right of the screen =========
                     column(3,
                            DT::dataTableOutput("geographies_picker")
                            )
                     ),
                   
                   # for under the main plot section ===========
                   fluidRow(column(10)),
                   
                   fluidRow(column(10))
  
))

