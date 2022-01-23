library(dplyr)
library(shiny)
library(highcharter)
library(viridisLite)
library(countrycode)
library(plotly)

population <- read.csv("population-cleaned.csv")
demo_gender <- read.csv("demographics-gender-cleaned.csv")
demo_age <- read.csv("demographics-age-cleaned.csv")

#Colors for the map
data(worldgeojson, package = "highcharter")
dshmstops <- data.frame(q = c(0, exp(1: 5) / exp(5)),
                        c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>% list_parse2()

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  print(input)
  
  observeEvent(input$learnmore, {
    updateTabItems(session, "tabs", "overview")
    }
  )
  
  observeEvent(input$triggerCredits, {
    sendSweetAlert(
      session = session,
      title = "Credits",
      text = "Zahiriddin Rustamov, ...",
      type = "info"
    )
  })

  output$casesMap <-renderUI({
    
    year_range = input$map_year
    refugee_type = input$map_refugee_type
    type = as.integer(input$map_type)
    
    if (is.null(refugee_type)) {
      refugee_type = "refugees"
    }
    
    if (type == 1) {
      pop <- population %>% 
        group_by(origin, origin_iso) %>%
        rename("Country" = origin, "iso3" = origin_iso)
      
      title = "Refugee Country of Origin in "
    } else {
      pop <- population %>% 
        group_by(asylum, asylum_iso) %>%
        rename("Country" = asylum, "iso3" = asylum_iso)
      
      title = "Refugee Country of Asylum in "
    }
      
    pop <- pop %>%
      filter(year >= year_range[1], year <= year_range[2]) %>% 
      summarise(across(c(refugee_type), sum), .groups = 'drop') %>%
      mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE))
      
    
    if (year_range[1] == year_range[2]) {
      title = paste(title, year_range[1], collapse = " ")
    } else {
      title = paste(title, paste(year_range, collapse = " - "), collapse = " ")
    }

    #Build the map
    highchart() %>% #from highchart package
      hc_add_series_map(worldgeojson, df = pop,
                        value = "total", joinBy = "iso3") %>%
      hc_colorAxis(stops = dshmstops) %>%
      hc_legend(enabled = TRUE) %>%
      hc_mapNavigation(enabled = TRUE) %>%
      hc_title(text = title, style = list(fontSize = "20px", fontWeight = "bold", color = "grey")) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_credits(enabled = TRUE,
                 text = "Source: UNHCR Refugee Data",
                 style = list(fontSize = "10px"))%>%
      hc_chart(events = list(load = JS("function() {
      var chart = this; chart.update({
        chart: {
          backgroundColor: 'transparent'
        }
      }); 
    }
    ")
      ))
    
  })
  
  output$gender <- renderUI({
    colors <- c('#084f80', '#800813', "#800813")
    
    countries = input$demo_countries
    age_type = input$demo_age_type
    year_range = input$demo_year
    type = as.integer(input$demo_type)
    
    print(countries)
    print(age_type)
    print(year_range)
    #print(type)
    
    # YEAR RANGE #
    demo_gender <- demo_gender %>% filter(year >= year_range[1], year <= year_range[2])
    
    # AGE TYPE #
    if (!is.null(age_type)) {
      demo_gender$total <- demo_gender %>% select(age_type) %>% rowSums(.)
    }
    
    if (length(type) != 0 && length(type) != 2 && !is.null(countries)) {
      type <- ifelse(type == 1, "origin", "asylum")
      demo_gender <- demo_gender[demo_gender[type] == countries,]
    }
    
    demo_gender <- demo_gender %>% mutate(gender = ifelse(gender == 1, "Male","Female"))
    plot_ly(demo_gender, labels = ~gender, values = ~total, type = 'pie',
            textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>%
      layout(title = 'Gender Distribution', paper_bgcolor='transparent')
  })

})
