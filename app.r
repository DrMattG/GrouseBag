# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(dashboardthemes)
library(devtools)
source_url("https://raw.githubusercontent.com/DrMattG/ShinyNINA/master/Shinytheme_NINA.R")
###########################
options(encoding="UTF-8")
library(httr)
library(rjstat)
##Get data from the stats agency
# url for POST
url <- "https://data.ssb.no/api/v0/en/table/03886/"
# Query, copied from API Console
# Run by highlighting all of this function and ctrl enter/r
data <- '{  "query": [  {"code": "Region",      "selection": {        "filter": "vs:FylkerJakt2",        "values": [          "01",          "02",          "03",          "04",          "05",          "06",          "07",          "08",          "09",          "10",          "11",          "12",          "14",          "15",          "50",          "16",          "17",          "18",          "19",          "20",          "AA"          ]      }    },    {      "code": "Smaviltjakt",      "selection": {        "filter": "item",        "values": [          "01",          "02",          "03",          "04",          "05"]}}],"response": {"format": "json-stat2"}}'

# post query
d.tmp <- POST(url , body = data, encode = "json", verbose())
# Get content from d.tmp as text, using fromJSONstat
dattable <- fromJSONstat(content(d.tmp, "text"))
head(dattable)
names(dattable)
#Preprocess data
# group Trøndelag
dattable<-dattable %>% 
  mutate(region=case_when
         (region %in%  c("Trøndelag","Sør-Trøndelag (-2017)" , "Nord-Trøndelag (-2017)") ~ "Trøndelag",
           region %in%  c("Østfold")~"Østfold",
           region %in%  c("Akershus")~"Akershus",
           region %in%  c("Oslo")~"Oslo",
           region %in%  c("Hedmark")~"Hedmark",
           region %in%  c("Oppland")~"Oppland",
           region %in%  c("Rogaland")~"Rogaland",
           region %in%  c("Buskerud")~"Buskerud",
           region %in%  c("Vestfold") ~"Vestfold",
           region %in%  c("Telemark")~"Telemark",
           region %in%  c("Aust-Agder")~"Aust-Agder",
           region %in%  c("Vest-Agder")~"Vest-Agder",
           region %in%  c("Hordaland")~"Hordaland",
           region %in%  c("Sogn og Fjordane")~"Sogn og Fjordane",
           region %in%  c("Møre og Romsdal"  )~"Møre og Romsdal"  ,
           region %in%  c("Finnmark - Finnmárku")~"Finnmark - Finnmárku",
           region %in%  c("Troms - Romsa")~"Troms - Romsa",
           region %in%  c("Nordland")~"Nordland",
           region %in%  c("Unknown hunting county")~"Unknown hunting county"))

###App starts here 
#set up title
title <- tags$a(href='https://www.nina.no',
                'Norwegian Grouse hunting data', target="_blank")

#Build shinyapp
#UI
ui <- dashboardPage(
  
  dashboardHeader(
    
    title = title, titleWidth=600),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
                         href = "https://www.nina.no"),
                menuItem(selectInput("season", "season", choices=c(
                  "1991"= "1991-1992",
                  "1992"= "1992-1993",
                  "1993"= "1993-1994",
                  "1994"= "1994-1995",
                  "1995"= "1995-1996",
                  "1996"= "1996-1997" ,
                  "1997"= "1997-1998",
                  "1998"= "1998-1999",
                  "1999"= "1999-2000",
                  "2000"= "2000-2001" ,
                  "2001"= "2001-2002",
                  "2002"= "2002-2003",
                  "2003"= "2003-2004",
                  "2004"= "2004-2005",
                  "2005"= "2005-2006",
                  "2006"= "2006-2007",
                  "2007"= "2007-2008",
                  "2008"= "2008-2009" ,
                  "2009"= "2009-2010" ,
                  "2010"= "2010-2011" ,
                  "2011"= "2011-2012" ,
                  "2012"= "2012-2013",
                  "2013"= "2013-2014",
                  "2014"= "2014-2015" ,
                  "2015"= "2015-2016",
                  "2016"= "2016-2017",
                  "2017"= "2017-2018",
                  "2018"= "2018-2019",
                  "2019"= "2019-2020",
                  "2020"= "2020-2021"
                ), selected = "2018-2019"
                ))
    )
  ),
  
  dashboardBody(theme_nina,
                tags$head(
                  tags$link(rel="stylesheet", type= "text/css", href="custom1.css")),
                
                tabsetPanel(
                  
                  id = "tabs",
                  
                  tabPanel(
                    
                    title = "Grouse Bag Statistics",
                    
                    value = "page1",
                    fluidRow(
                      valueBoxOutput("value1"),
                      valueBoxOutput("value2"),
                      valueBoxOutput("value3")),
                    fluidRow(
                      valueBoxOutput("value4"),
                      valueBoxOutput("value5")),
                    fluidRow(
                      fluidRow(
                        box(
                          title = "All Grouse per year"
                          ,status = "primary"
                          ,solidHeader = TRUE 
                          ,collapsible = TRUE 
                          ,plotOutput("Allbyyr", height = "600px")
                        ),
                        box(
                          title = "All Grouse by Region"
                          ,status = "primary"
                          ,solidHeader = TRUE 
                          ,collapsible = TRUE
                          ,selectInput("regionall", "Choose the region"
                                       ,choices=c(
                                         "Østfold" = "Østfold",
                                         "Akershus"= "Akershus",
                                         "Oslo"="Oslo",
                                         "Hedmark"="Hedmark",
                                         "Oppland"="Oppland",
                                         "Buskerud"="Buskerud",
                                         "Vestfold"="Vestfold",
                                         "Telemark"="Telemark" ,
                                         "Aust-Agder"="Aust-Agder",
                                         "Vest-Agder"="Vest-Agder",
                                         "Rogaland"="Rogaland",
                                         "Hordaland"="Hordaland",
                                         "Sogn og Fjordane"="Sogn og Fjordane",
                                         "Møre og Romsdal"="Møre og Romsdal",
                                         "Trøndelag"="Trøndelag",
                                         "Nordland"="Nordland",
                                         "Troms - Romsa"="Troms - Romsa",
                                         "Finnmark - Finnmárku"="Finnmark - Finnmárku",
                                         "Unknown hunting county"="Unknown hunting county")
                                       ,multiple = FALSE)
                          ,plotOutput("AllbyRegion", height = "600px")
                        )
                        
                      ))
                  ),
                  
                  tabPanel(
                    
                    title = "Capercaillie",
                    
                    value = "page2",
                    fluidRow(
                      box(
                        title = "Capercaillie per year"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,plotOutput("Capercailliebyyr", height = "600px")
                      ),
                      box(
                        title = "Capercaillie by Region"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE
                        ,selectInput("region1", "Choose the region"
                                     ,choices=c(
                                       "Østfold" = "Østfold",
                                       "Akershus"= "Akershus",
                                       "Oslo"="Oslo",
                                       "Hedmark"="Hedmark",
                                       "Oppland"="Oppland",
                                       "Buskerud"="Buskerud",
                                       "Vestfold"="Vestfold",
                                       "Telemark"="Telemark" ,
                                       "Aust-Agder"="Aust-Agder",
                                       "Vest-Agder"="Vest-Agder",
                                       "Rogaland"="Rogaland",
                                       "Hordaland"="Hordaland",
                                       "Sogn og Fjordane"="Sogn og Fjordane",
                                       "Møre og Romsdal"="Møre og Romsdal",
                                       "Trøndelag"="Trøndelag",
                                       "Nordland"="Nordland",
                                       "Troms - Romsa"="Troms - Romsa",
                                       "Finnmark - Finnmárku"="Finnmark - Finnmárku",
                                       "Unknown hunting county"="Unknown hunting county")
                                     ,multiple = FALSE)
                        ,plotOutput("CapercailliebyRegion", height = "600px")
                      )
                    )
                  ),
                  tabPanel(
                    
                    title = "Black Grouse",
                    
                    value = "page3",
                    fluidRow(
                      box(
                        title = "Black Grouse per year"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,plotOutput("BlackGrousebyyr", height = "600px")
                      ),
                      box(
                        title = "Black Grouse by Region"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE
                        ,selectInput("region2", "Choose the region"
                                     ,choices=c(
                                       "Østfold" = "Østfold",
                                       "Akershus"= "Akershus",
                                       "Oslo"="Oslo",
                                       "Hedmark"="Hedmark",
                                       "Oppland"="Oppland",
                                       "Buskerud"="Buskerud",
                                       "Vestfold"="Vestfold",
                                       "Telemark"="Telemark" ,
                                       "Aust-Agder"="Aust-Agder",
                                       "Vest-Agder"="Vest-Agder",
                                       "Rogaland"="Rogaland",
                                       "Hordaland"="Hordaland",
                                       "Sogn og Fjordane"="Sogn og Fjordane",
                                       "Møre og Romsdal"="Møre og Romsdal",
                                       "Trøndelag"="Trøndelag",
                                       "Nordland"="Nordland",
                                       "Troms - Romsa"="Troms - Romsa",
                                       "Finnmark - Finnmárku"="Finnmark - Finnmárku",
                                       "Unknown hunting county"="Unknown hunting county")
                                     ,multiple = FALSE)
                        ,plotOutput("BlackGrousebyRegion", height = "600px")
                      )
                    )
                  ),
                  tabPanel(
                    
                    title = "Willow Ptarmigan",
                    
                    value = "page4",
                    fluidRow(
                      box(
                        title = "Willow Ptarmigan per year"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE
                        ,plotOutput("WillowPtarmiganbyyr", height = "600px")
                      ),
                      box(
                        title = "Willow Ptarmigan by Region"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,selectInput("region3", "Choose the region"
                                     ,choices=c(
                                       "Østfold" = "Østfold",
                                       "Akershus"= "Akershus",
                                       "Oslo"="Oslo",
                                       "Hedmark"="Hedmark",
                                       "Oppland"="Oppland",
                                       "Buskerud"="Buskerud",
                                       "Vestfold"="Vestfold",
                                       "Telemark"="Telemark" ,
                                       "Aust-Agder"="Aust-Agder",
                                       "Vest-Agder"="Vest-Agder",
                                       "Rogaland"="Rogaland",
                                       "Hordaland"="Hordaland",
                                       "Sogn og Fjordane"="Sogn og Fjordane",
                                       "Møre og Romsdal"="Møre og Romsdal",
                                       "Trøndelag"="Trøndelag",
                                       "Nordland"="Nordland",
                                       "Troms - Romsa"="Troms - Romsa",
                                       "Finnmark - Finnmárku"="Finnmark - Finnmárku",
                                       "Unknown hunting county"="Unknown hunting county")
                                     ,multiple = FALSE)
                        ,plotOutput("WillowPtarmiganbyRegion", height = "600px")
                      )
                    )
                  ),
                  tabPanel(
                    
                    title = "Ptarmigan",
                    
                    value = "page5",
                    fluidRow(
                      box(
                        title = "Ptarmigan per year"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,plotOutput("Ptarmiganbyyr", height = "600px")
                      ),
                      box(
                        title = "Ptarmigan by Region"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,selectInput("region4", "Choose the region"
                                     ,choices=c(
                                       "Østfold" = "Østfold",
                                       "Akershus"= "Akershus",
                                       "Oslo"="Oslo",
                                       "Hedmark"="Hedmark",
                                       "Oppland"="Oppland",
                                       "Buskerud"="Buskerud",
                                       "Vestfold"="Vestfold",
                                       "Telemark"="Telemark" ,
                                       "Aust-Agder"="Aust-Agder",
                                       "Vest-Agder"="Vest-Agder",
                                       "Rogaland"="Rogaland",
                                       "Hordaland"="Hordaland",
                                       "Sogn og Fjordane"="Sogn og Fjordane",
                                       "Møre og Romsdal"="Møre og Romsdal",
                                       "Trøndelag"="Trøndelag",
                                       "Nordland"="Nordland",
                                       "Troms - Romsa"="Troms - Romsa",
                                       "Finnmark - Finnmárku"="Finnmark - Finnmárku",
                                       "Unknown hunting county"="Unknown hunting county")
                                     ,multiple = FALSE)
                        ,plotOutput("PtarmiganbyRegion", height = "600px")
                      )
                    )
                  ),
                  tabPanel(
                    
                    title = "Hazel Grouse",
                    
                    value = "page6",
                    fluidRow(
                      box(
                        title = "Hazel Grouse per year"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,plotOutput("HazelGrousebyyr", height = "600px")
                      ),
                      box(
                        title = "Hazel Grouse by Region"
                        ,status = "primary"
                        ,solidHeader = TRUE 
                        ,collapsible = TRUE 
                        ,selectInput("region5", "Choose the region"
                                     ,choices=c(
                                       "Østfold" = "Østfold",
                                       "Akershus"= "Akershus",
                                       "Oslo"="Oslo",
                                       "Hedmark"="Hedmark",
                                       "Oppland"="Oppland",
                                       "Buskerud"="Buskerud",
                                       "Vestfold"="Vestfold",
                                       "Telemark"="Telemark" ,
                                       "Aust-Agder"="Aust-Agder",
                                       "Vest-Agder"="Vest-Agder",
                                       "Rogaland"="Rogaland",
                                       "Hordaland"="Hordaland",
                                       "Sogn og Fjordane"="Sogn og Fjordane",
                                       "Møre og Romsdal"="Møre og Romsdal",
                                       "Trøndelag"="Trøndelag",
                                       "Nordland"="Nordland",
                                       "Troms - Romsa"="Troms - Romsa",
                                       "Finnmark - Finnmárku"="Finnmark - Finnmárku",
                                       "Unknown hunting county"="Unknown hunting county")
                                     ,multiple = FALSE)
                        ,plotOutput("HazelGrousebyRegion", height = "600px")
                      )
                    )
                  )
                  
                )
  )
)

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values of boxes (take last year in the dataset)
  Capercaillie<-reactive({dattable %>%
      filter(`small game`=="Capercaillie" & `interval (year)`== input$season) %>% 
      summarise(sum=sum(value, na.rm = TRUE))})
  
  BlackGrouse<-reactive({dattable %>%
      filter(`small game`=="Black Grouse" & `interval (year)`== input$season) %>% 
      summarise(sum=sum(value, na.rm = TRUE))})
  
  WillowPtarmigan<-reactive({dattable %>%
      filter(`small game`=="Willow Ptarmigan" & `interval (year)`== input$season) %>% 
      summarise(sum=sum(value, na.rm = TRUE))})
  
  Ptarmigan<-reactive({dattable %>%
      filter(`small game`=="Ptarmigan" & `interval (year)`== input$season) %>% 
      summarise(sum=sum(value, na.rm = TRUE))})
  
  HazelGrouse<-reactive({dattable %>%
      filter(`small game`=="Hazel Grouse" & `interval (year)`== input$season) %>% 
      summarise(sum=sum(value, na.rm = TRUE))})
  
  
  #datasets for plots
  #Can change this to here later when adding iteractive data
  
  Region1<-reactive({
    dattable %>% 
      filter(region==input$region1)
  })
  Region2<-reactive({
    dattable %>% 
      filter(region==input$region2)
  })
  Region3<-reactive({
    dattable %>% 
      filter(region==input$region3)
  })
  Region4<-reactive({
    dattable %>% 
      filter(region==input$region4)
  })
  Region5<-reactive({
    dattable %>% 
      filter(region==input$region5)
  })
  RegionAll<-reactive({
    dattable %>% 
      filter(region==input$regionall)
  })
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(Capercaillie()$sum, format="d", big.mark=',')
      ,paste('Capercaillie:',input$season)
      ,icon = icon("list-ol",lib='font-awesome')
      ,color = "purple")})
  output$value2 <- renderValueBox({
    valueBox(
      formatC(BlackGrouse()$sum, format="d", big.mark=',')
      ,paste('Black Grouse:',max(dattable$`interval (year)`))
      ,icon = icon("list-ol",lib='font-awesome')
      ,color = "green")})
  output$value3 <- renderValueBox({
    valueBox(
      formatC(WillowPtarmigan()$sum, format="d", big.mark=',')
      ,paste('Willow Ptarmigan:',max(dattable$`interval (year)`))
      ,icon = icon("list-ol",lib='font-awesome')
      ,color = "yellow") })
  output$value4 <- renderValueBox({
    valueBox(
      formatC(Ptarmigan()$sum, format="d", big.mark=',')
      ,paste('Ptarmigan:',max(dattable$`interval (year)`))
      ,icon = icon("list-ol")
      ,color = "purple")})
  output$value5 <- renderValueBox({
    valueBox(
      formatC(HazelGrouse()$sum, format="d", big.mark=',')
      ,paste('Hazel Grouse:',max(dattable$`interval (year)`))
      ,icon = icon("list-ol")
      ,color = "green")})
  
  #creating the plotOutput content
  output$Allbyyr <- renderPlot({
    dattable %>% 
      group_by(`interval (year)`) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$AllbyRegion <- renderPlot({
    RegionAll() %>% 
      group_by(`interval (year)`, region) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$Capercailliebyyr <- renderPlot({
    dattable %>% 
      filter(`small game`== "Capercaillie") %>% 
      group_by(`interval (year)`) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$CapercailliebyRegion <- renderPlot({
    Region1() %>% 
      filter(`small game`== "Capercaillie") %>% 
      group_by(`interval (year)`, region) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$BlackGrousebyyr <- renderPlot({
    dattable %>% 
      filter(`small game`== "Black Grouse") %>% 
      group_by(`interval (year)`) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$BlackGrousebyRegion <- renderPlot({
    Region2() %>% 
      filter(`small game`== "Black Grouse") %>% 
      group_by(`interval (year)`, region) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$WillowPtarmiganbyyr <- renderPlot({
    dattable %>% 
      filter(`small game`== "Willow Ptarmigan") %>% 
      group_by(`interval (year)`) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$WillowPtarmiganbyRegion <- renderPlot({
    Region3() %>% 
      filter(`small game`== "Willow Ptarmigan") %>% 
      group_by(`interval (year)`, region) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$Ptarmiganbyyr <- renderPlot({
    dattable %>% 
      filter(`small game`== "Ptarmigan") %>% 
      group_by(`interval (year)`) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$PtarmiganbyRegion <- renderPlot({
    Region4() %>% 
      filter(`small game`== "Ptarmigan") %>% 
      group_by(`interval (year)`, region) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$HazelGrousebyyr <- renderPlot({
    dattable %>% 
      filter(`small game`== "Hazel Grouse") %>% 
      group_by(`interval (year)`) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  
  output$HazelGrousebyRegion <- renderPlot({
    Region5() %>% 
      filter(`small game`== "Hazel Grouse") %>% 
      group_by(`interval (year)`, region) %>% 
      summarise(sum=sum(value, na.rm=TRUE)) %>% 
      ggplot(aes(x=`interval (year)`, y=sum)) + 
      geom_bar(position = "dodge", stat = "identity", fill="#2DCCD3") +
      ylab("Number of birds") + 
      xlab("Season") +
      theme_classic()+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
}

#Launch the App
shinyApp(ui, server)
