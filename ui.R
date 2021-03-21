
library(shiny)
library(shinyjs)
library(shinyTree) # Need version 0.2.7
library(rclipboard)



shinyUI(navbarPage(
  "Page",
  position = "fixed-top",
  tabPanel(
    "Explore",
    sidebarLayout(
      sidebarPanel(
        tags$head(
          tags$style(HTML(".red {color: red} .blue {color: blue}")) # CSS for colored node : red = LI, blue = RA
        ),
        # checkboxInput(
        #   inputId = "fetch_children",
        #   label = "Append children on click",
        #   value = T
        # ),
        # checkboxInput(
        #   inputId = "open_children",
        #   label = "Open children on click",
        #   value = T
        # ),
        conditionalPanel(
          condition = "output.selected == true",
          h1(),
          h3("Selected"),
          h4("C_NAME :"),
          textOutput('node_name'),
          h2(),
          h4("C_BASECODE :"),
          textOutput('code'),
          h2(),
          h4("C_FULLNAME : "),
          div(style="display: inline-block;width:45px;", uiOutput("clip")),
          div(style="display: inline-block;width: 80%;margin: 0 auto;", verbatimTextOutput('fullpath')),
          h1(),
          h4("Matching Codes : "),
          tableOutput("codes")
        ),
        style = "position:fixed;top:70px;width:inherit;overflow-y:scroll;max-height: 600px;margin-right:20px"
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Tree", 
            shinyTree("explore_tree", 
                      sort=T, 
                      animation=F
            )
          ),
          tabPanel("Matching codes", 
                   actionButton("open_matching_code", 'OPEN'), 
                   shinyTree(
                     "matching_code_tree", 
                     sort=T, 
                     animation=F, 
                     types = "{
                     'default': {'icon': 'glyphicon glyphicon-folder-open' },
                     'file':{'icon': 'glyphicon glyphicon-file'},
                     'FA': {'icon': 'glyphicon glyphicon-folder-open'},
                     'LA': {'icon' : 'glyphicon glyphicon-file'},
                     'RA': {'icon' : 'glyphicon glyphicon-file blue'},
                     'LI': {'icon' : 'glyphicon glyphicon-file red'}}"
                   ))
        ),
        style = "top:70px;"
      )
    )
  ),
  tabPanel(
    "Search",
    sidebarLayout(
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        textInput(
          inputId = "text_to_search",
          label = "Search Text",
          value = "hyp(o|er)gly"
        ),
        h1(),
        checkboxInput(
          inputId = "use_regex",
          label = "Use Regex",
          value = T
        ),
        h1(),
        selectInput(
          inputId = "search_on",
          label = "Search On",
          choices = c("C_FULLNAME", "C_NAME", "C_BASECODE"),
          selected = "C_NAME"
        ),
        h1(),
        checkboxInput(
          inputId = "dynamic_search",
          label = "Dynamic Search",
          value = F
        ),
        h1(),
        actionButton(
          inputId = "perform_search",
          label = "Launch Search"
        ),
        h1(),
        actionButton(
          "open_all",
          "Open the Tree"
        ),
        conditionalPanel(
          condition = "output.s_selected == true",
          h1(),
          h3("Selected :"),
          h4("C_NAME :"),
          textOutput('s_node_name'),
          h1(),
          h4("C_BASECODE :"),
          textOutput('s_code'),
          h1(),
          h4("C_FULLNAME :"),
          div(style="display: inline-block;width:45px;", uiOutput("s_clip")),
          div(style="display: inline-block;width: 80%;margin: 0 auto;", verbatimTextOutput('s_fullname'))
        ),
        style = "position:fixed;top:70px;"
        
      ),
      mainPanel(
        style = "top:70px;",
        useShinyjs(),
        rclipboardSetup(),
        shinyTree("search_tree", 
                  sort=T, 
                  animation=F, 
                  types = "{
                           'default': {'icon': 'glyphicon glyphicon-folder-open' },
                           'file':{'icon': 'glyphicon glyphicon-file'},
                           'FA': {'icon': 'glyphicon glyphicon-folder-open'},
                           'LA': {'icon' : 'glyphicon glyphicon-file'},
                           'RA': {'icon' : 'glyphicon glyphicon-file blue'},
                           'LI': {'icon' : 'glyphicon glyphicon-file red'}}"
        )
      )
    )
  )
))
