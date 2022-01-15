library(shiny)
library(bslib)
library(bs4Dash)
library(shinyWidgets)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    title = "Refugee Statistics Dashboard",
    fullscreen = TRUE,
    
    header = dashboardHeader(
      "Refugee Analysis",
      
      title = dashboardBrand(
        title = "Refugee Analysis",
        color = "gray-dark",
        href = "https://www.unhcr.org/refugee-statistics/",
        #image = "https://www.itu.int/net4/wsis/ungis/Content/img/logos/uniform/unhcr.png",
        opacity = 0.5
      ),
      skin = "light",
      status = "white",
      border = TRUE,
      sidebarIcon = icon("bars"),
      controlbarIcon = icon("th"),
      fixed = FALSE,
      
      rightUi = dropdownMenu(
        badgeStatus = "info",
        type = "notifications",
        notificationItem(
          inputId = "triggerCredits",
          text = "Credits",
          status = "info",
          icon = icon("info"),
        )
      )
    ),
    
    sidebar = dashboardSidebar(
      sidebarMenu(id = "tabs",
        menuItem(
          text = "Home",
          tabName = "home",
          icon = icon("home")
        ),
        menuItem(
          text = "Overview",
          tabName = "overview",
          icon = icon("lightbulb")
        ),
        menuItem(
          text = "Population",
          tabName = "population",
          icon = icon("people-arrows")
        ),
        menuItem(
          text = "Demographics",
          tabName = "demographics",
          icon = icon("child")
        ),
        menuItem(
          text = "Attacks",
          tabName = "attacks",
          icon = icon("procedures")
        )
      )
    ),
    
    body = dashboardBody(
      tags$style(HTML(".jumbotron {background: white !important; color: black !important;padding-bottom: 1rem;padding-top: 2rem;}")),
      tags$style(HTML("#big-heading{color: black;}")),
      tags$style(HTML("img {opacity: 0.7; ")),
      tags$style(HTML(".text-block {
                        position: absolute !important; 
                        bottom: 20px;
                        padding-top: 10px;
                        right: 20px;
                        background-color: rgba(0, 0, 0, 0.5);
                        color: white;
                        padding-left: 20px;
                        padding-right: 20px;
                      }")),
      tags$style(HTML(".carousel-caption {
                        position: unset;
                        right: 15%;
                        bottom: 20px;
                        left: 15%;
                        z-index: 10;
                        padding-top: 0;
                        padding-bottom: 0;
                        color: #fff;
                        text-align: inherit;
                      }")),
      tags$style(HTML("#learnmore {background: rgba(0,0,0,0.5) !important; margin-bottom: 10px;font-size: large}")),
      tags$style('#welcome-header .card-header{ display: none !important}'),
      tags$style('#welcome-header .card-body{ padding:2rem}'),
      
      # SWEET ALERT #
      useSweetAlert(),
      
      # BODY CONTENT #
      tabItems(
        # HOME TAB #
        
        # HOME CONTENT #
        tabItem(
          tabName = "home",

          box(width = 12, 
              title = NULL, 
              headerBorder = FALSE, 
              id = 'welcome-header',
              h1("Welcome!"),
              p("Refugee Analysis Dashboard..."),
              hr(),
              actionBttn(
                inputId = "learnmore",
                label = "Click to Learn More", 
                style = "material-flat",
                color = "danger"
              )
              ),
          
          box(width = 12,
              bs4Carousel(
              id = "refugee-carousell",
              bs4CarouselItem(
                caption = div(class="text-block", h3("Refugees"), p("Who are they?")),
                tags$img(src = "https://assets.ey.com/content/dam/ey-sites/ey-com/en_gl/topics/purpose/ey-family-refugees-travels-desert.jpg", width = "100%")
              ),
              bs4CarouselItem(
                caption = div(class="text-block", h3("Refugees"), p("Where are they?")),
                tags$img(src = "https://www.nrc.no/image/100838/coronavirus_refugees.jpg?width=1200&height=800", width = "100%")
              ),
              bs4CarouselItem(
                caption = div(class="text-block", h3("Refugees"), p("How many are they?")),
                tags$img(src = "https://www.brookings.edu/wp-content/uploads/2016/07/jordan_refugeechildrenposing001.jpg", width = "100%")
              ),
              bs4CarouselItem(
                caption = div(class="text-block", actionBttn(
                  inputId = "learnmore",
                  label = "Click to Learn More", 
                  style = "material-flat",
                  color = "danger"
                )),
                tags$img(src = "https://www.borgenmagazine.com/wp-content/uploads/2014/02/Refugees.jpg", width = "100%")
              )
            )
          )
        
                
          
        ),
        
        tabItem(
          tabName = "overview",
          box(title = "Overview Map",
              
              sidebar = boxSidebar(
                icon = icon("question"),
                id = "map_info",
                background = "gray-dark",
                h4("How to use?"),
                p(code("Type"), ": Country of Origin (where they come from) or Asylum (where they went to)"),
                p(code("Year Range"), ": The range of years to display"),
                p(code("Population Type"), ": Different refugee population types. Allowed to select multiple at once."),
                ),
              
              width = 12,
              maximizable = TRUE,
              uiOutput("casesMap")),
          
          box(title = "", icon = icon("sliders-h"),
            chooseSliderSkin(skin = "Shiny",
                             color = NULL),
            
            width = 12,
            
            radioGroupButtons(
              inputId = "map_type",
              justified = TRUE,
              label = "Type:",
              choices = list("Origin" = 1, "Asylum" = 2),
              individual = TRUE,
              checkIcon = list(
                yes = tags$i(class = "fa fa-circle", 
                             style = "color: steelblue"),
                no = tags$i(class = "fa fa-circle-o", 
                            style = "color: steelblue"))
            ),
            
            sliderTextInput(
              inputId = "map_year",
              label = "Year Range:",
              choices = c(2010:2021),
              selected = c(2010, 2021),
              #width = "30%"
            ),
            
            pickerInput(
              inputId = "map_refugee_type",
              label = "Population Types:",
              choices = list("Refugees" = "refugees", "Asylum Seekers" = "asylum_seekers", "IDPs of concern to UNHCR" = "idps",
                             "Venezuelans displaced abroad" = "venz", "Stateless Persons" = "stateless",
                             "Others of concern" = "others"),
              options = list(`actions-box` = TRUE),
              multiple = TRUE,
              selected = "refugees"
            )
          )
          
        ),
        
        tabItem(
          tabName = "population",
          fluidRow(
            controlbarMenu(
              id = "controlbarmenu",
              controlbarItem("Item 1",
                             "Simple text 1"),
              controlbarItem("Item 2",
                             "Simple text")
            )
          )
        )
        
        
      ),
      # END OF BODY CONTENT #),
      
    ),
    
    controlbar = dashboardControlbar(
      collapsed = TRUE,
      div(class = "p-3", skinSelector()),
      pinned = FALSE
    ),
  )
)
