library('shiny')
library("plotly")
library("ggplot2")
library("ggthemes")
library("DT")
library("dplyr")
library("bdvis")

mammal_data <- read.csv("./mammal_data.csv", encoding = 'latin1')

shinyServer(function(input, output, session){
    
    remove_na_cols <- function(test) {
        remove_columns <- c()
        for(column in 1:length(colnames(test))) {
            if(sum(!is.na(test[,column])) != nrow(test)) {
                remove_columns <- append(column, remove_columns)
            }
        }
        test <- test[, -remove_columns]
        test
    }

######################################################################################################################################
## View Dataset    
######################################################################################################################################
    output$mammal_data_table <- renderDT({remove_na_cols(as.data.frame(mammal_data))})
    
######################################################################################################################################
## Chronohogram    
######################################################################################################################################
    output$mammal_chronohorogram <- renderPlot({bdvis::chronohorogram(indf = as.data.frame(mammal_data),
                                                                        title = "US Mammal Data Chronohorogram")})
    
    output$chronohorogram_params <- renderValueBox({
        valueBox(
            value = tags$p("bdvis::chronohorogram() params",tags$br(), tags$br()),
            subtitle = tags$p("indf = Dataset from View Dataset Tab",tags$br(),
                              "startyear = 1980(default)", tags$br(),
                              "colors = c(red, blue)",tags$br(),
                              "ptsize = 1",tags$br(),
                              style = "font-size: 150%;"),
            icon = icon("hippo"),
            color = "light-blue"
        )
    })
    
    
######################################################################################################################################
## Calendar Heat  
######################################################################################################################################
    output$mammal_heatmap <- renderPlot({bdvis::bdcalendarheat(indf = as.data.frame(mammal_data),
                                                               title = "US Mammal Data Calendar Heat Map")})
    
    output$heatmap_params <- renderValueBox({
        valueBox(
            value = tags$p("bdvis::bdcalendarheat() params",tags$br(), tags$br()),
            subtitle = tags$p("indf = Dataset from View Dataset Tab",tags$br(),
                              style = "font-size: 150%;"),
            icon = icon("dog"),
            color = "light-blue"
        )
    })
    
######################################################################################################################################
## Calendar Heat  
######################################################################################################################################
    
    output$mammal_tempolar_daily <- renderPlot({tempolar(as.data.frame(mammal_data), 
                                                           color="red", 
                                                           title="US Mammals daily", 
                                                           plottype="r", 
                                                           timescale="d"
                                                           )})

    
    output$mammal_tempolar_weekly <- renderPlot({tempolar(as.data.frame(mammal_data), 
                                                            color="green", 
                                                            title="US Mammals weekly", 
                                                            plottype="r", 
                                                            timescale="w"
                                                            )})

    
    output$mammal_tempolar_monthly <- renderPlot({tempolar(as.data.frame(mammal_data), 
                                                          color="blue", 
                                                          title="US Mammals monthly", 
                                                          plottype="r", 
                                                          timescale="m"
                                                          )})
    output$tempolar_weekly <- renderValueBox({
        valueBox(
            value = tags$p("Tempolar (Weekly): ",tags$br(), tags$br()),
            subtitle = tags$p("indf = Dataset from View Dataset Tab",tags$br(),
                              "plottype = r", tags$br(),
                              "timescale = w", tags$br(),
                              style = "font-size: 150%;"),
            icon = icon("globe"),
            color = "light-blue"
        )
    })
    output$tempolar_monthly <- renderValueBox({
        valueBox(
            value = tags$p("Tempolar (Monthly): ",tags$br(), tags$br()),
            subtitle = tags$p("indf = Dataset from View Dataset Tab",tags$br(),
                              "plottype = r", tags$br(),
                              "timescale = m", tags$br(),
                              style = "font-size: 150%;"),
            icon = icon("map-marker-alt"),
            color = "light-blue"
        )
    })
    output$tempolar_daily <- renderValueBox({
        valueBox(
            value = tags$p("Tempolar (Daily): ",tags$br(), tags$br()),
            subtitle = tags$p("indf = Dataset from View Dataset Tab",tags$br(),
                              "plottype = r", tags$br(),
                              "timescale = d", tags$br(),
                              style = "font-size: 150%;"),
            icon = icon("location-arrow"),
            color = "light-blue"
        )
    })

######################################################################################################################################
## Map Grid  
######################################################################################################################################
    
    output$map_record <- renderPlotly({
        mapgrid(as.data.frame(mammal_data),
                ptype="records",
                region = "US") 
    })
    
    output$map_presence <- renderPlotly({
        mapgrid(as.data.frame(mammal_data),
                ptype="presence",
                region="US"
                )
    })
    output$map_complete <- renderPlotly({
        mapgrid(as.data.frame(mammal_data),
                comp = bdcomplete(indf = as.data.frame(mammal_data)),
                ptype="complete",
                bbox=c(60,100,5,40),
                region="US") 
    })
    
    output$record_param <- renderValueBox({
        valueBox(
            value = tags$p("Record-Density Map",tags$br(), tags$br()),
            subtitle = tags$p("indf = Dataset from View Dataset Tab",tags$br(),
                              "ptype = records", tags$br(),
                              style = "font-size: 150%;"),
            icon = icon("chart-area"),
            color = "light-blue"
        )
    })
    output$presence_param <- renderValueBox({
        valueBox(
            value = tags$p("Presence/Absence Map",tags$br(), tags$br()),
            subtitle = tags$p("indf = Dataset from View Dataset Tab",tags$br(),
                              "ptype = presence", tags$br(),
                              style = "font-size: 150%;"),
            icon = icon("chart-pie"),
            color = "light-blue"
        )
    })
    output$complete_param <- renderValueBox({
        valueBox(
            value = tags$p("Completeness Map",tags$br(), tags$br()),
            subtitle = tags$p("indf = Dataset from View Dataset Tab",tags$br(),
                              "comp = completeness matrix", tags$br(),
                              "ptype = complete", tags$br(),
                              style = "font-size: 150%;"),
            icon = icon("project-diagram"),
            color = "light-blue"
        )
    })
            
})