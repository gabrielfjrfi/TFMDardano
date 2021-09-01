if(!require("shinydashboard")) {
    install.packages("shinydashboard")
    library("shinydashboard")
}

if(!require("shinyWidgets")) {
    install.packages("shinyWidgets")
    library("shinyWidgets")
}

if(!require("shinycssloaders")) {
    install.packages("shinycssloaders")
    library("shinycssloaders")
}

library(shiny)


shinyUI( dashboardPage(skin = "blue",
                       dashboardHeader(title = "Dardano"), 
                       dashboardSidebar(
                           sidebarMenu(
                               
                               menuItem("CEO", icon = icon("poll"), startExpanded = FALSE,
                                        menuSubItem("Clustering", tabName = "panel_clustering"),
                                        menuSubItem("Exploration", tabName = "panel_exploration")
                               ),
                               menuItem("Twitter", icon = icon("twitter-square"), startExpanded = FALSE,
                                        menuSubItem("Life", tabName = "panel_life"),
                                        menuSubItem("Parties", tabName = "panel_parties"),
                                        menuSubItem("Exploration", tabName = "panel_tw_exploration")
                               ),
                               menuItem("Facebook Ads", icon = icon("facebook-square"), startExpanded = FALSE,
                                        menuSubItem("G1", tabName = "panelg1")
                               ),
                               menuItem("Reports", icon = icon('file'), tabName = "reports"),
                               menuItem("FAQs", icon = icon('question-circle'), tabName = "info")
                               )
                           ),
                       dashboardBody(
                           tabItems(
                               tabItem("panel_clustering", "Comparations",uiOutput("general_render"),textOutput('G1_data_info'),
                                       fluidRow(
                                           box(title = "Inputs 1", status = "warning", solidHeader = TRUE,width=3,
                                               
                                               uiOutput("G1_param"),
                                               
                                               uiOutput("G1_limits")),
                                           
                                           box(title = "Inputs 2", status = "warning", solidHeader = TRUE,width=3,
                                               
                                               uiOutput("G1_2param"),
                                               
                                               uiOutput("G1_2limits")),
                                           
                                           box(title = "G1", status = "primary", solidHeader = TRUE,
                                               collapsible = FALSE,
                                               withSpinner(
                                                   plotOutput("Chart_g1", height = 300),
                                                   type = 4,
                                                   color = "#d33724",
                                                   size = 0.7),
                                               div(
                                                   style = "position: absolute; left: 1.5em; bottom: 1.5em;",
                                                   dropdown(
                                                       downloadButton(outputId = "save_g1", label = "Download on JPEG"),
                                                       size = "xs",
                                                       icon = icon("download", class = "opt"), 
                                                       up = TRUE
                                                   )
                                               )),
                                           tabBox(width=12,
                                                  tabPanel(
                                                      title = "Table G1.1",
                                                      DT::dataTableOutput('G1_Table')),
                                                  tabPanel(
                                                      title = "Table G1.2",
                                                      DT::dataTableOutput('G1_Table2'))))),
                               
                               tabItem("panel_exploration", "G2 Ranking",
                                       textOutput('G2_data_info'),
                                       fluidRow(
                                           
                                           tabBox(title = "G2 Block 1",
                                                  tabPanel(
                                                      title = "Plot G2B1",
                                                      withSpinner(
                                                          plotOutput("Chart_g2_b1", height = 300),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7
                                                      ),
                                                      div(
                                                          style = "position: absolute; left: 1.5em; bottom: 1.75em;",
                                                          dropdown(
                                                              downloadButton(outputId = "save_g2_b1", label = "Download on JPEG"),
                                                              size = "xs",
                                                              icon = icon("download", class = "opt"), 
                                                              up = TRUE
                                                          )
                                                      )),
                                                  tabPanel(
                                                      title = "Table G2B1",
                                                      withSpinner(
                                                          DT::dataTableOutput('G2B1_Table'),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7)
                                                  )),
                                           tabBox(title = "G2 Block 2",
                                                  tabPanel(
                                                      title = "Plot G2B2",
                                                      withSpinner(
                                                          plotOutput("Chart_g2_b2", height = 300),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7
                                                      ),
                                                      div(
                                                          style = "position: absolute; left: 1.5em; bottom: 1.75em;",
                                                          dropdown(
                                                              downloadButton(outputId = "save_g2_b2", label = "Download on JPEG"),
                                                              size = "xs",
                                                              icon = icon("download", class = "opt"), 
                                                              up = TRUE
                                                          )
                                                      )),
                                                  tabPanel(
                                                      title = "Table G2B2",
                                                      withSpinner(
                                                          DT::dataTableOutput('G2B2_Table'),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7)
                                                  )),
                                           tabBox(title = "G2 Block 3",
                                                  tabPanel(
                                                      title = "Plot G2B3",
                                                      withSpinner(
                                                          plotOutput("Chart_g2_b3", height = 300),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7
                                                      ),
                                                      div(
                                                          style = "position: absolute; left: 1.5em; bottom: 1.75em;",
                                                          dropdown(
                                                              downloadButton(outputId = "save_g2_b3", label = "Download on JPEG"),
                                                              size = "xs",
                                                              icon = icon("download", class = "opt"), 
                                                              up = TRUE
                                                          )
                                                      )),
                                                  tabPanel(
                                                      title = "Table G2B3",
                                                      withSpinner(
                                                          DT::dataTableOutput('G2B3_Table'),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7)
                                                  )),
                                           tabBox(title = "G2 Block 4",
                                                  tabPanel(
                                                      title = "Plot G2B4",
                                                      withSpinner(
                                                          plotOutput("Chart_g2_b4", height = 300),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7
                                                      ),
                                                      div(
                                                          style = "position: absolute; left: 1.5em; bottom: 1.75em;",
                                                          dropdown(
                                                              downloadButton(outputId = "save_g2_b4", label = "Download on JPEG"),
                                                              size = "xs",
                                                              icon = icon("download", class = "opt"), 
                                                              up = TRUE
                                                          )
                                                      )),
                                                  tabPanel(
                                                      title = "Table G2B4",
                                                      withSpinner(
                                                          DT::dataTableOutput('G2B4_Table'),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7)
                                                  )),
                                           tabBox(title = "G2 Block 5",
                                                  tabPanel(
                                                      title = "Plot G2B5",
                                                      withSpinner(
                                                          plotOutput("Chart_g2_b5", height = 300),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7
                                                      ),
                                                      div(
                                                          style = "position: absolute; left: 1.5em; bottom: 1.75em;",
                                                          dropdown(
                                                              downloadButton(outputId = "save_g2_b5", label = "Download on JPEG"),
                                                              size = "xs",
                                                              icon = icon("download", class = "opt"), 
                                                              up = TRUE
                                                          )
                                                      )),
                                                  tabPanel(
                                                      title = "Table G2B5",
                                                      withSpinner(
                                                          DT::dataTableOutput('G2B5_Table'),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7)
                                                  )),
                                           
                                           tabBox(title = "G2 Block 6",
                                                  tabPanel(
                                                      title = "Plot G2B6",
                                                      withSpinner(
                                                          plotOutput("Chart_g2_b6", height = 300),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7
                                                      ),
                                                      div(
                                                          style = "position: absolute; left: 1.5em; bottom: 1.75em;",
                                                          dropdown(
                                                              downloadButton(outputId = "save_g2_b6", label = "Download on JPEG"),
                                                              size = "xs",
                                                              icon = icon("download", class = "opt"), 
                                                              up = TRUE
                                                          )
                                                      )),
                                                  tabPanel(
                                                      title = "Table G2B6",
                                                      withSpinner(
                                                          DT::dataTableOutput('G2B6_Table'),
                                                          type = 4,
                                                          color = "#d33724",
                                                          size = 0.7)
                                                  )))),
                               
                               tabItem("panel_life", "G3 and G5 Saveings",
                                       fluidRow(
                                           
                                           box(title = "G3",status = "primary", solidHeader = TRUE,
                                               collapsible = FALSE,
                                               withSpinner(
                                                   plotOutput("Chart_g3", height = 400),
                                                   type = 4,
                                                   color = "#d33724",
                                                   size = 0.7),
                                               div(style = "position: absolute; left: 1.5em; bottom: 1.75em;",
                                                   dropdown(
                                                       downloadButton(outputId = "save_g3", label = "Download on JPEG"),
                                                       size = "xs",
                                                       icon = icon("download", class = "opt"), 
                                                       up = TRUE))
                                           ),
                                           
                                           box(title = "G5", status = "primary", solidHeader = TRUE,
                                               collapsible = FALSE,
                                               withSpinner(
                                                   plotOutput("Chart_g5", height = 400),
                                                   type = 4,
                                                   color = "#d33724",
                                                   size = 0.7),
                                               div(style = "position: absolute; left: 1.5em; bottom: 1.75em;",
                                                   dropdown(
                                                       downloadButton(outputId = "save_g5", label = "Download on JPEG"),
                                                       size = "xs",
                                                       icon = icon("download", class = "opt"), 
                                                       up = TRUE))
                                           ),
                                           
                                           tabBox(width=12,
                                                  tabPanel(
                                                      title = "Table G3",
                                                      dataTableOutput('G3_Table')),
                                                  tabPanel(
                                                      title = "Table G5",
                                                      dataTableOutput('G5_Table'))))),
                               
                               tabItem("panel_parties",
                                       box(title = "Inputs 1", status = "warning", solidHeader = TRUE,
                                           actionButton("CorrGener","Generate correltions list"),
                                           uiOutput("g10selector")),
                                       box(title = "Plot G10", status = "primary", solidHeader = TRUE,
                                           withSpinner(
                                               plotOutput("Chart_g10", height = 400),
                                               type = 4,
                                               color = "#d33724",
                                               size = 0.7),
                                           div(style = "position: absolute; left: 1em; bottom: 1em;",
                                               dropdown(
                                                   downloadButton(outputId = "save_g10", label = "Download on JPEG"),
                                                   size = "xs",
                                                   icon = icon("download", class = "opt"), 
                                                   up = TRUE))
                                       )),
                               
                               tabItem("reports", "Report",
                                       fluidRow(
                                           uiOutput("report_limits"),
                                           column(width=6,
                                                  box(title = "Report G1", status = "success", solidHeader = TRUE,width = NULL,
                                                      
                                                      radioButtons(
                                                          "r_g1_selector", "Report by:",
                                                          choices = c("Complete parameter block" = "block_select",
                                                                      "Single parameter" =  "parameter_selector"),
                                                          selected = "block_select"),
                                                      uiOutput("r_g1"),
                                                      textOutput('G1_r')),
                                                  
                                                  box(title = "Report G2", status = "success", solidHeader = TRUE,width = NULL,
                                                      uiOutput("r_g2"),
                                                      textOutput('G2_r')),
                                                  
                                                  box(title = "Report G3", status = "success", solidHeader = TRUE,width = NULL,
                                                      uiOutput("G3_r")),
                                                  
                                                  box(title = "Report G5", status = "success", solidHeader = TRUE,width = NULL,
                                                      uiOutput("G5_r"))
                                           ),
                                           column(width=6,
                                                  box(title = "Report G8", status = "success", solidHeader = TRUE,width = NULL,
                                                      'This report does not use the company selected in the side menu.',
                                                      uiOutput("r_g8_info"),
                                                      textOutput('G8_r'),
                                                      uiOutput("r1_g8"),
                                                      uiOutput("G8_limits"),
                                                      uiOutput("r2_g8"),
                                                      uiOutput("G8_2limits"),
                                                      uiOutput("G8_company"),
                                                      uiOutput("G8_company2"),
                                                      uiOutput("G8_ppt"))
                                           )
                                       )
                               ),
                               
                               tabItem("info", "Info")
                           )
                       )
))

