library(tidyr)
library(dplyr)
library(network)
library(stringr)
library(igraph)
library(visNetwork)
library(shiny)
library(shinythemes)
library(DT)
source("./func.R")

# R-shiny
fluidPage(
    theme = shinytheme("journal"),
    titlePanel("Chemical Mixture Effects"),
    sidebarLayout(
      sidebarPanel(
        conditionalPanel(condition="input.tabselected==1",
                         fluidRow(
                           shinydashboard::box(
                             width = NULL,
                             title = "Model Selection",
                             radioButtons("model", "Select the model of interest", choices = c("Multiple Informant Model", "Quantile G-computation Model"), selected = "Multiple Informant Model"),
                           )
                         ),
                         fluidRow(
                           shinydashboard::box(
                             width = NULL,
                             title = "Window Selection",
                             uiOutput("effect_type"),
                             uiOutput("window_option")
                           )
                         )
                         #radioButtons("window", "Select the window of interests", choices = c("prenatal", "birth", "three_year", "five_eight_year (reference window)"), selected = "prenatal"),
                         #radioButtons("effect_type", "Select the type of effects", choices = c("main & interaction", "total"), selected = "total"),
                         #textOutput("note")
        ),
        conditionalPanel(condition="input.tabselected==2",
                         fluidRow(
                           shinydashboard::box(
                             width = NULL,
                             title = "Effect Selection",
                             radioButtons("agree_effect", "Select the effect of interest", choices = c("Single OPE Effect", "Combined Effect"), selected = "Single OPE Effect"),
                           )
                         ),
                         fluidRow(
                           shinydashboard::box(
                             width = NULL,
                             title = "Window Selection",
                             radioButtons("window_me", "Select the window of interest", choices = c("prenatal", "birth", "3y", "5-8y"), selected = "prenatal")
                           )
                         ),
                         fluidRow(
                           shinydashboard::box(
                             width = NULL,
                             title = "Brain Region Selection",
                             radioButtons("brain_me", "Select the type of brain regions to look at", choices = c("individual", "anatomy", "function"), selected = "individual")
                           )
                         )
        ),
        conditionalPanel(condition="input.tabselected==3",
                         fluidRow(
                           shinydashboard::box(
                             width = NULL,
                             title = "Effect Selection",
                             radioButtons("agree_effect_me", "Select the effect of interest", choices = c("Single OPE Effect", "Combined Effect"), selected = "Single OPE Effect"),
                           )
                         ),
                         fluidRow(
                           shinydashboard::box(
                             width = NULL,
                             title = "Mediation Analysis Stage Selection",
                             radioButtons("mediation_selection", "Select the stage of mediation analysis", choices = c("pre", "post"), selected = "pre"),
                           )
                         )
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Exploratory Analysis", value = 1, 
                   fluidRow(uiOutput("vis_output")),
                   fluidRow(
                     shinydashboard::box(
                       width = NULL,
                       title = "Exploratory Analysis Result Table",
                     DTOutput("explore_table")))),
          tabPanel("Model Agreement", value = 2, 
                   fluidRow(visNetworkOutput("agree_plot", height = 600)),
                   fluidRow(
                     shinydashboard::box(
                       width = NULL,
                       title = "Model Agreement Result Table",
                     DTOutput("agree_table")))),
          tabPanel("Mediation Analysis", value = 3, 
                   fluidRow(visNetworkOutput("mediation_plot", height = 600)),
                   fluidRow(
                     shinydashboard::box(
                       width = NULL,
                       title = "Mediation Analysis Result Table",
                     DTOutput("mediation_table"))),
                   fluidRow(uiOutput("better_interpretation")),
                   fluidRow(DTOutput("mediation_table_int"))),
          id = "tabselected"
        )
      )
    )
)
