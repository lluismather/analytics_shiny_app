library(shiny)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(scales)
library(RColorBrewer)

#===styles========================>>
theme1 <- theme(plot.title = element_text(size=12, 
                                          face="bold", 
                                          margin = margin(20, 0, 20, 0)),
                legend.title = element_blank(), 
                legend.background = element_rect(fill="grey97", 
                                                 size=0.5, 
                                                 linetype="solid"),
                legend.key=element_rect(fill= "white"), 
                axis.text.x=element_text(angle = 310, 
                                         hjust = 0, 
                                         size = 8),
                axis.text.y=element_text(hjust = 0, 
                                         size = 8),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(), 
                panel.background = element_blank(),
                axis.line = element_line(colour = "grey"), 
                axis.title = element_text(size=11, 
                                          face="bold"))
#axes as numeric
options(scipen = 10000)
plotColor <- brewer.pal(n=12, name="Set3")

# ==================================

# members
mem <- read.csv('mems.csv', stringsAsFactors=F)
vec_incl <- names(mem) %in% c("Sex", "Mosaic", "StatusCode", "source", "dialogue_user", "events_attendee", "chatter_lead", "is_donor",
                                  "donations_count", "donations_sum", "doorstep_user", "cc_user", "wpc_name", "region_name", "age", "mem_len", "age_cut",
                                  "wpc", "seat_17", "is_key_seat")
mem <- mem[mem$source=="member", vec_incl]
names(mem)[20] <- "key_seat"
mem$activism_type <- ifelse(mem$is_donor==T, 'Donor',
                     ifelse(mem$dialogue_user==T, 'Dialogue',
                            ifelse(mem$events_attendee==T, 'Event',
                                   ifelse(mem$chatter_lead==T, 'Chatter', sample(c('None', 'Doorstep', 'Roleholder', 'Signup'), 1)))))
mem <- mem[, -c(5:12, 18)]
mem$key_seat <- ifelse(mem$key_seat==TRUE, ifelse(mem$seat_17=="Labour", "Defense", "Attack"), "Other")
names(mem)[10] <- 'seat_held'
mem$mem_type <- sample(c('member', 'registered', 'affiliated'), nrow(mem), replace=T, prob=c(0.7, 0.1, 0.2))

# # members
# mem <- data.frame(Sex=sample(c('F', 'M', 'O', 'U'), 
#                              size=500000, replace=T, prob=c(0.4276, 0.5684, 0.0028, 0.0012)),
#                   Mosaic=sample(c('A', 'B', 'C', 'D', 'E', 'F', 'G'), 
#                                 size=500000, replace=T, prob=c(0.4, 0.2, 0.1, 0.05, 0.05, 0.1, 0.1)),
#                   StatusCode=sample(c('ACCEPTED', 'ARREARS', 'PART-ARREARS', 'PRE-LAPSE'), 
#                                     size=500000, replace=T, prob=c(0.9109, 0.0504, 0.027, 0.0118)), 
#                   source=sample('membership', size=500000, replace=T), 
#                   wpc_name=sample(geo$wpc_name, size=500000, replace=T),
#                   region_name=sample(NA, size=500000, replace=T), 
#                   age=sample(floor(rnorm(500000, mean=47, sd=17))), 
#                   mem_len=floor(abs(rnorm(500000, mean=5, sd=6))), 
#                   age_cut=sample(NA, size=500000, replace=T),
#                   seat_held=sample(c('Green', 'Labour', 'LibDem', 'Other', 'Plaid', 'SNP', 'Tory'),
#                                    size=500000, replace=T, prob=c(0.0061, 0.5712, 0.0154, 0.0011, 0.0047, 0.0233, 0.3782)),
#                   key_seat=sample(c('Defense', 'Attack', 'Other'),
#                                   size=500000, replace=T, prob=c(0.2, 0.2, 0.6)), 
#                   activism_type=sample(c('Donor', 'Dialogue', 'Event', 'Chatter', 'None', 'Doorstep', 'Roleholder', 'Signup'), 
#                                        size=500000, replace=T, prob=c(0.1, 0.025, 0.025, 0.05, 0.5, 0.1, 0.025, 0.175)), 
#                   mem_type=sample(c('member', 'registered', 'affiliated'), 
#                                   size=500000, replace=T, prob=c(0.7, 0.1, 0.2)))
# mem$region_name <- geo$region_name[match(mem1$wpc_name, geo$wpc_name)]
# 
# # wpcs and other data
# wpcs_table <- geo %>%
#   mutate(seat_held=sample(c('Green', 'Labour', 'LibDem', 'Other', 'Plaid', 'SNP', 'Tory'),
#                           size=630, replace=T, prob=c(0.0061, 0.5712, 0.0154, 0.0011, 0.0047, 0.0233, 0.3782)),
#          key_seat=sample(c('Defense', 'Attack', 'Other'),
#                          size=630, replace=T, prob=c(0.2, 0.2, 0.6))) %>%
#   arrange(wpc_name) %>%
#   filter(region_name!='')

# wpcs and other data
wpcs_table <- mem %>% 
  select(wpc_name, region_name, seat_held, key_seat) %>% 
  distinct %>% 
  arrange(wpc_name) %>%
  filter(region_name!="")

# ===================================

# server
shinyServer(function(input, output) {
  
  # create the reactives with filters =========
  # clp selected
  selected_clp <- reactive({
    if (input$clp_override == "On" & length(input$geographies_picker_rows_selected > 0)) {
      print(TRUE)
      wpcs_table[input$geographies_picker_rows_selected,]$wpc_name
    } else { NA }
  })

  # filtered_clps for the right hand table
  filtered_clps <- reactive({
    wpcs_table %>%
      filter(region_name %in% input$region_groups) %>%
      filter(key_seat %in% input$seat_category) %>%
      filter(seat_held %in% input$seat_held)
  })

  # panel for constituencies
  output$geographies_picker <- DT::renderDataTable(DT::datatable(data.frame(wpc=filtered_clps()[, 1]),
                                                                 extensions = "Scroller",
                                                                 filter = "bottom",
                                                                 selection = list(mode = 'single', selected=NULL),
                                                                 rownames = FALSE,
                                                                 options = list(
                                                                   scroller = TRUE,
                                                                   scrollY = "500px",
                                                                   columnDefs = list(
                                                                     list(
                                                                       visible = FALSE,
                                                                       targets = FALSE
                                                                     )
                                                                   ),
                                                                   columns = list(
                                                                     list(title = "clp")
                                                                   ),
                                                                   autoWidth = TRUE,
                                                                   sDom  = '<"top">rt<"bottom">ipl'
                                                                   )
                                                                 ),
                                                  server = FALSE
  )
  
  # main plot
  output$main_plot <- renderPlot({
    
    if (input$clp_override == "Off") {
      
      # if no clp override
      plot_data <- reactive({
        mem %>%
          filter(wpc_name %in% filtered_clps()$wpc_name) %>%
          filter(region_name %in% filtered_clps()$region_name) %>%
          filter(activism_type %in% input$activist_type) %>%
          filter(mem_type %in% input$membership_type) %>%
          filter(StatusCode %in% input$membership_status)
      })

    } else {
      
      # select clp
      plot_data <- reactive({
        mem %>%
          filter(wpc_name %in% selected_clp()) %>%
          filter(activism_type %in% input$activist_type) %>%
          filter(source %in% input$membership_type) %>%
          filter(StatusCode %in% input$membership_status)
      })

    }
    
    # only display plots for some of the data
    if (!input$main_view %in% c('Membership Length', 'Age')) {
      ggplot() + labs(title='There is no plot to show here yet...') + theme1
    } else {
      
      # which x value do we show
      if (input$main_view == 'Age') {
        x_var <- sym('age')
        xlim_val2 <- 100
        hist_bins <- 5
      } else if (input$main_view == 'Membership Length') {
        x_var <- sym('mem_len')
        xlim_val2 <- 40
        hist_bins <- 1
      }
      
      # which filter is selected
      if (input$graph_break == "all") {
        gg <- ggplot(plot_data(), aes(x=!!x_var, fill=plotColor[4])) + theme(legend.position='none')
      } else {
        gg <- ggplot(plot_data(), aes(x=!!x_var, fill=!!sym(input$graph_break)))
      } 
      
      # which type of plot
      if (input$graph_type=='histogram') {
        gg <- gg + geom_histogram(binwidth=hist_bins, color='black')
      } else if (input$graph_type=='density') {
        gg <- gg + geom_density(alpha=0.5)
      }
      
      gg +
        xlim(0, xlim_val2) +
        labs(title="", x=x_var, y="") +
        theme1
      
    }

  })
  
})

