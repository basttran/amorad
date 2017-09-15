#ascii_
#assembly
#erlang
#jack
#jade
#livescript
#tcl

library(shiny)
library(leaflet)
library(shinyjs)
library(shinyAce)
library(colourpicker)
library(DT)
mapPalette<-readRDS("mapPalette")
shinyUI(
  fluidPage(
    useShinyjs(),
####Main panel####
    mainPanel(h3("divRs"),
              tags$div(id="cite",
                       'Collaborative mapping for ',
                       tags$em('AMORAD 2015'),
                       ' by Bastien Tran (REEDS-OVSQ, 2015).'),
              div(class="outer",
                  tags$head(
                    # Include superzip's custom CSS
                    includeCSS("styles.css"),
                    includeCSS("shinychat.css"),
                    # And custom JavaScript -- just to send a message when a user hits "enter"
                    # and automatically scroll the chat window for us. Totally optional.
                    includeScript("sendOnEnter.js")),
                  leafletOutput("map", width="100%", height="100%"))
              
    ),
    absolutePanel(id = "datatable", class = "panel panel-default", fixed = TRUE,
                  draggable = FALSE, top = 0, left = 0, right = "auto", 
                  bottom = 30, width = '25%', height = '100%',
                  tabsetPanel(
####Entity panel####
                    tabPanel("Entity",
                             textInput("entityLabel","Entity label",value=""),
                             selectInput("entityType","Element's type",c("Select" = 0,
                                                                                "Draw points" = "point",
                                                                                "Draw lines" = "line",
                                                                                "Draw polygons" = "polygon"),0),
                             actionButton("entityNew", "Create entity"),
                             DT::dataTableOutput("collection"),
                             aceEditor("selectedEntity", value= "", mode="jade", theme="textmate", readOnly = TRUE,
                                              height = "150px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
                                              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
                    ),
####Note panel####
                    tabPanel("Note",
                             textInput("noteLabel","Note label",value=""),
                             aceEditor("noteBody", value= "", mode="text", theme="textmate", readOnly = FALSE,
                                              height = "90px", fontSize = 12, wordWrap = TRUE,
                                              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab"))),
                             actionButton("noteNew", "Create note"),
                             dataTableOutput("corpus"),
                             ###This for displaying last clicked note's body
                             aceEditor("noteView", value= "", mode="text", theme="textmate", readOnly = TRUE,
                                              height = "150px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
                                              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab"))),
                             aceEditor("selectedNote", value= "", mode="jade", theme="textmate", readOnly = TRUE,
                                              height = "150px", fontSize = 12, wordWrap = TRUE, showLineNumbers = FALSE,
                                              hotkeys = list(nextInputKey = list(win="Tab",mac="Tab")))
                      
                    ),
#####Tag panel#####
                    tabPanel("Tag",
                             textInput("tagLabel","Tag label",value=""),
                             uiOutput("tagEntity"),
                             uiOutput("tagNote"),
                             uiOutput("tagTag"),
                             DT::dataTableOutput("cloud"),
                             actionButton("tagNew", "Create tag")
                             )
,
                    tabPanel("Map",
                             fluidRow(
                               column(8,uiOutput("tag1")),column(4,colourpicker::colourInput("legend1", "legend n°1", value = "white", showColour = "both", palette = "limited", allowedCols = mapPalette, allowTransparent = FALSE, returnName = FALSE)),
                               column(8,uiOutput("tag2")),column(4,colourpicker::colourInput("legend2", "legend n°2", value = "white", showColour = "both", palette = "limited", allowedCols = mapPalette, allowTransparent = FALSE, returnName = FALSE)),
                               column(8,uiOutput("tag3")),column(4,colourpicker::colourInput("legend3", "legend n°3", value = "white", showColour = "both", palette = "limited", allowedCols = mapPalette, allowTransparent = FALSE, returnName = FALSE)),
                               column(8,uiOutput("tag4")),column(4,colourpicker::colourInput("legend4", "legend n°4", value = "white", showColour = "both", palette = "limited", allowedCols = mapPalette, allowTransparent = FALSE, returnName = FALSE)),
                               column(8,uiOutput("tag5")),column(4,colourpicker::colourInput("legend5", "legend n°5", value = "white", showColour = "both", palette = "limited", allowedCols = mapPalette, allowTransparent = FALSE, returnName = FALSE))
                               ),
                             tabsetPanel(
                               tabPanel("n°1",
                                        uiOutput("stamp1"),
                                        uiOutput("theme1")),
                               tabPanel("n°2",
                                        uiOutput("stamp2"),
                                        uiOutput("theme2")),
                               tabPanel("n°3",
                                        uiOutput("stamp3"),
                                        uiOutput("theme3")),
                               tabPanel("n°4",
                                        uiOutput("stamp4"),
                                        uiOutput("theme4")),
                               tabPanel("n°5",
                                        uiOutput("stamp5"),
                                        uiOutput("theme5"))
                             )
                    )
                  )
    ),

#####Chat panel#####
absolutePanel(id = "collaboration", class = "panel panel-default", fixed = TRUE, draggable = FALSE, top = 0, left = "auto", right = 0, bottom = "auto", width = '25%', height = '100%',
              h2("Chat"),
####Chatbox####
              uiOutput("chat"),
              # Create the bottom bar to allow users to chat.
####InputBox####
              br(),
textInput("entry", ""),
actionButton("send", "Send"),
br(),
br(),
aceEditor("brouillon", value= "", mode="text", theme="textmate", readOnly = FALSE,
          height = "450", fontSize = 12, wordWrap = TRUE,
          hotkeys = list(nextInputKey = list(win="Tab",mac="Tab"))),
br(),
br(),

#####Sources####
              tags$hr(),
              h6(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>.
                              <p>Source code available <a href =\"https://github.com/basttran/divRs\">on GitHub</a>.
                              <p>Chat based on <a href =\"https://github.com/trestletech/ShinyChat\">ShinyChat</a>.
                              <p>Map based on <a href =\"https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example\">SuperZIP example</a>."))
  )
))


