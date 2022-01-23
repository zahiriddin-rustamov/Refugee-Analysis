library(shiny)
library(bslib)
library(bs4Dash)
library(shinyWidgets)

country_names <- read.csv("country-names.csv")
countries <- country_names[["name"]]

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
        bs4SidebarHeader("Stats"),
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
        ),
        bs4SidebarHeader("Documentation"),
        menuItem(
          text = "User Manual",
          tabName = "documentation",
          icon = icon("file")
        ),
        menuItem(
          text = "Deon",
          tabName = "deon",
          icon = icon("check")
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
      tags$style('#welcome-header .card-body{ padding:2rem;padding-top: 1.5rem;padding-bottom: 1.5rem;}'),
      tags$style(HTML(".awesome-checkbox{padding-bottom: 10px;}")),
      tags$style(HTML(".nav-pills .nav-link.active{background-color: #606060;}")),
      tags$style('#gender-settings li .nav-link { background-color: transparent !important}'),
      tags$style('.btn-group-toggle { margin-right: 1rem !important}'),
      tags$style('.radiobtn, .checkbtn { padding-right: 2rem !important; padding-left: 2rem !important}'),
      
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
                caption = div(class="text-block", h5("Refugees"), p("Who are they?")),
                tags$img(src = "https://assets.ey.com/content/dam/ey-sites/ey-com/en_gl/topics/purpose/ey-family-refugees-travels-desert.jpg", width = "100%")
              ),
              bs4CarouselItem(
                caption = div(class="text-block", p("Where are they?")),
                tags$img(src = "https://www.nrc.no/image/100838/coronavirus_refugees.jpg?width=1200&height=800", width = "100%")
              ),
              bs4CarouselItem(
                caption = div(class="text-block", h5("Refugees"), p("How many are they?")),
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
          box(width = 12, id = 'welcome-header',
            HTML("<h4><center>Overview of Refugee Statistics</center></h4><center>View the country of origin & asylum of refugees from 2010 to 2021.</center>")
          ),
          
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
          tabName = "demographics",
          
          box(width = 12, id = 'welcome-header',
              HTML("<h4><center>Demographics of Refugees</center></h4><center>View the distribution of gender & age across different years, type (origin, asylum) and countries.</center>")
          ),
          
          fluidRow(
            controlbarMenu(
              id = "demographics-controlbarmenu",
              vertical=T,
              type = "pills",
              side = "left",
              
              controlbarItem(title = "Gender",
                             box(title = "Gender Distribution",
                                 
                                 sidebar = boxSidebar(
                                   icon = icon("question"),
                                   id = "gender_info",
                                   background = "gray-dark",
                                   h4("How to use?"),
                                   p(code("Type"), ": Only choose one of them for pie chart. It will work once you select one or more countries together.
                                     Country of Origin (where they come from) or Asylum (where they went to)."),
                                   p(code("Year Range"), ": The range of years to display"),
                                   p(code("Age Groups"), ": Different age group types. You can select multiple at once."),
                                   p(code("Countries"), ": List of countries"),
                                 ),
                                 
                               width = 12,
                               maximizable = TRUE,
                               uiOutput("gender")
                             ),
                             
                             box(title = "", icon = icon("sliders-h"),
                                 chooseSliderSkin(skin = "Shiny",
                                                  color = NULL),
                                 
                                 width = 12, elevation = 1,
                                 
                                 checkboxGroupButtons(
                                   inputId = "demo_type",
                                   justified = TRUE,
                                   label = "Type:",
                                   choices = list("Origin" = 1, "Asylum" = 2),
                                   individual = TRUE,
                                   checkIcon = list(
                                     yes = tags$i(class = "fa fa-check-square", 
                                                  style = "color: steelblue"),
                                     no = tags$i(class = "fa fa-square-o", 
                                                 style = "color: steelblue"))
                                 ),
                                 
                                 sliderTextInput(
                                   inputId = "demo_year",
                                   label = "Year Range:",
                                   choices = c(2010:2021),
                                   selected = c(2010, 2021),
                                   #width = "30%"
                                 ),
                                 
                                 pickerInput(
                                   inputId = "demo_age_type",
                                   label = "Age Groups:",
                                   choices = list("0 - 4" = "age0to4", "5 - 11" = "age5to11", "12 - 17" = "age12to17",
                                                  "18 - 59" = "age18to59", "60+" = "age60",
                                                  "Unknown" = "unknown"),
                                   options = list(`actions-box` = TRUE),
                                   multiple = TRUE,
                                   selected = "refugees"
                                 ),
                                 
                                 multiInput(
                                   inputId = "demo_countries",
                                   label = "Countries:", 
                                   choices = NULL,
                                   choiceNames = lapply(seq_along(countries), 
                                                        function(i) tagList(#tags$img(src = flags[i],
                                                          #width = 20, height = 15),
                                                          countries[i])),
                                   choiceValues = countries
                                 )
                             )
                             
                             ),
              controlbarItem("Age",
                             "Simple text")
            )
          )
        ),
        
        tabItem(
          tabName = "documentation",
          box(width = 12,
              title = "User Manual",
              span("Each graph has a "), icon("question"), span(" on the top right corner."),
              p(),
              p("Click the icon to see how to configure individual graphs."),
              p("Contact - Zahir (s2106642@siswa.um.edu.my) - if you have any issues/questions.")
            
          )
        ),
        
        tabItem(
          tabName = "deon",
          box(width = 12,
              title = "Data Science Ethics Checklist",
              HTML("<a href='http://deon.drivendata.org/'><img src='https://img.shields.io/badge/ethics%20checklist-deon-brightgreen.svg?style=popout-square' alt='Deon badge' /></a><br><br/>"),
              
              awesomeCheckboxGroup(
                inputId = "deon-data-collection",
                label = h5("A. Data Collection"), 
                choices = list("A.1 Informed consent: If there are human subjects, have they given informed consent, where subjects affirmatively opt-in and have a clear understanding of the data uses to which they consent?" = "A1",
                               "A.2 Collection bias: Have we considered sources of bias that could be introduced during data collection and survey design and taken steps to mitigate those?" = "A2",
                               "A.3 Limit PII exposure: Have we considered ways to minimize exposure of personally identifiable information (PII) for example through anonymization or not collecting information that isn't relevant for analysis?" = "A3",
                               "A.4 Downstream bias mitigation: Have we considered ways to enable testing downstream results for biased outcomes (e.g., collecting data on protected group status like race or gender)?" = "A4"),
                selected = c("")
              ),
              
              awesomeCheckboxGroup(
                inputId = "deon-data-storage",
                label = h5("B. Data Storage"), 
                choices = list("B.1 Data security: Do we have a plan to protect and secure data (e.g., encryption at rest and in transit, access controls on internal users and third parties, access logs, and up-to-date software)?" = "B1",
                               "B.2 Right to be forgotten: Do we have a mechanism through which an individual can request their personal information be removed?" = "B2",
                               "B.3 Data retention plan: Is there a schedule or plan to delete the data after it is no longer needed?" = "B3"),
                selected = c("")
              ),
              
              awesomeCheckboxGroup(
                inputId = "deon-analysis",
                label = h5("C. Analysis"), 
                choices = list("C.1 Missing perspectives: Have we sought to address blindspots in the analysis through engagement with relevant stakeholders (e.g., checking assumptions and discussing implications with affected communities and subject matter experts)?" = "C1",
                               "C.2 Dataset bias: Have we examined the data for possible sources of bias and taken steps to mitigate or address these biases (e.g., stereotype perpetuation, confirmation bias, imbalanced classes, or omitted confounding variables)?" = "C2",
                               "C.3 Honest representation: Are our visualizations, summary statistics, and reports designed to honestly represent the underlying data?" = "C3",
                               "C.4 Privacy in analysis: Have we ensured that data with PII are not used or displayed unless necessary for the analysis?" = "C4",
                               "C.5 Auditability: Is the process of generating the analysis well documented and reproducible if we discover issues in the future?" = "C5"),
                selected = c("C3", "C4")
              ),
              
              awesomeCheckboxGroup(
                inputId = "deon-modeling",
                label = h5("D. Modeling"), 
                choices = list("D.1 Proxy discrimination: Have we ensured that the model does not rely on variables or proxies for variables that are unfairly discriminatory?" = "D1",
                               "D.2 Fairness across groups: Have we tested model results for fairness with respect to different affected groups (e.g., tested for disparate error rates)?" = "D2",
                               "D.3 Metric selection: Have we considered the effects of optimizing for our defined metrics and considered additional metrics?" = "D3",
                               "D.4 Explainability: Can we explain in understandable terms a decision the model made in cases where a justification is needed?" = "D4",
                               "D.5 Communicate bias: Have we communicated the shortcomings, limitations, and biases of the model to relevant stakeholders in ways that can be generally understood?" = "D5"),
                selected = c("")
              ),
              
              awesomeCheckboxGroup(
                inputId = "deon-deployment",
                label = h5("E. Deployment"), 
                choices = list("E.1 Redress: Have we discussed with our organization a plan for response if users are harmed by the results (e.g., how does the data science team evaluate these cases and update analysis and models to prevent future harm)?" = "E1",
                               "E.2 Roll back: Is there a way to turn off or roll back the model in production if necessary?" = "E2",
                               "E.3 Concept drift: Do we test and monitor for concept drift to ensure the model remains fair over time?" = "E3",
                               "E.4 Unintended use: Have we taken steps to identify and prevent unintended uses and abuse of the model and do we have a plan to monitor these once the model is deployed?" = "E4"),
                selected = c("")
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
