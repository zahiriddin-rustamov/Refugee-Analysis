library(dplyr)
library(shiny)
library(highcharter)
library(viridisLite)
library(countrycode)
library(plotly)
library(randomcoloR)
library(scales)
library(networkD3)

population <- read.csv("population-cleaned.csv")
demo_gender <- read.csv("demographics-gender-cleaned.csv")
demo_age <- read.csv("demographics-age-cleaned.csv")
gtd <- read.csv("gtd_cleaned.csv")
asylum_countries <- read.csv("asylum-country-names.csv")
asylum_countries <- asylum_countries[["asylum"]]
origin_countries <- read.csv("origin-country-names.csv")
origin_countries <- origin_countries[["origin"]]

#Colors for the map
data(worldgeojson, package = "highcharter")
dshmstops <- data.frame(q = c(0, exp(1:5) / exp(5)),
                        c = substring(viridis(5 + 1, option = "D"), 0, 7)) %>% list_parse2()

#Random Colors
#randomColor = ""
#for (i in 1:100) {
  #randomColor = rbind(randomColor, randomColor(count = 1, hue = c("random"), luminosity = c("dark")))
#}
#randomColor <- randomColor[-1]

randomColor = randomColor(count = 100, hue = c("random"), luminosity = c("dark"))

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
      title = "Elpizo - Refugee Analysis Dashboard",
      text = "Elpizo means Hope in Greek. We hope that refugees one day will be treated as humans. Until that day comes, we hope this dashboard broadens your views.",
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
                 style = list(fontSize = "10px")) %>%
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
    
    #print(asylum_countries)
    #print(age_type)
    #print(origin_countries)
    #print(type)
    
    demo_gender_c <- data.frame(demo_gender)
    
    # YEAR RANGE #
    demo_gender <- demo_gender %>% filter(year >= year_range[1], year <= year_range[2])
    
    # AGE TYPE #
    if (!is.null(age_type)) {
      demo_gender$total <- demo_gender %>% select(age_type) %>% rowSums(.)
    }
    
    if (length(type) != 0 && length(type) != 2 && !is.null(countries)) {
      if (type == 1) {
        countries_to_include <- countries %in% origin_countries
      } else {
        countries_to_include <- countries %in% asylum_countries
      }
      
      type <- ifelse(type == 1, "origin", "asylum")
      
      for (num in 1:length(countries[countries_to_include])) {
        if (num == 1) {
          demo_gender_c <- demo_gender[demo_gender[type] == countries[countries_to_include][num],]
        } else {
          demo_gender_c <- rbind(demo_gender_c, demo_gender[demo_gender[type] == countries[countries_to_include][num],])
        }
      }
    }
    
    #demo_gender <- demo_gender %>% mutate(gender = ifelse(gender == 1, "Male","Female"))
    plot_ly(demo_gender_c, labels = ~gender, values = ~total,
            textposition = 'inside', textinfo = 'label+percent', insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = colors, line = list(color = '#FFFFFF', width = 1)), showlegend = FALSE) %>% 
      add_pie(hole = 0.4) %>%
      layout(title = '', paper_bgcolor='transparent')
  })
  
  output$age <- renderUI({
    countries = input$age_demo_countries
    age_type = input$age_demo_age_type
    year_range = input$age_demo_year
    type = as.integer(input$age_demo_type)
    
    demo_gender_c <- data.frame(demo_gender)
    
    if (length(type) != 0 && length(type) != 2 && !is.null(countries)) {
      if (type == 1) {
        countries_to_include <- countries %in% origin_countries
      } else {
        countries_to_include <- countries %in% asylum_countries
      }
      
      type <- ifelse(type == 1, "origin", "asylum")
      
      for (num in 1:length(countries[countries_to_include])) {
        if (num == 1) {
          demo_gender_c <- demo_gender[demo_gender[type] == countries[countries_to_include][num],]
        } else {
          demo_gender_c <- rbind(demo_gender_c, demo_gender[demo_gender[type] == countries[countries_to_include][num],])
        }
      }
    }
    
    age_demo <- demo_gender_c %>% filter(year >= year_range[1], year <= year_range[2]) %>% group_by(gender) %>% summarise(age0to4 = sum(age0to4), age5to11 = sum(age5to11), age12to17 = sum(age12to17), age18to59 = sum(age18to59), age60 = sum(age60))
    age_demo <- age_demo %>% rename("0 - 4" = age0to4, "5 - 11" = age5to11, "12 - 17" = age12to17, "18 - 59" = age18to59, "60+" = age60)
    
    if (!is.null(age_type)) {
      age_demo <- age_demo %>% select(gender, age_type)
    }
    
    n <- age_demo$gender
    df.aree <- as.data.frame(t(age_demo[,-1]))
    colnames(df.aree) <- n
    df.aree$Age <- rownames(df.aree)
    rownames(df.aree) <- NULL
    df.aree <- df.aree %>% select(Age, Male, Female)
    df.aree <- df.aree %>% arrange(desc(row_number()))
    df.aree$Age <- factor(df.aree$Age, levels = df.aree$Age)
    
    fig <- plot_ly(df.aree, color = I("gray80"))
    fig <- fig %>% add_segments(x = ~Female, xend = ~Male, y = ~Age, yend = ~Age, showlegend = FALSE)
    fig <- fig %>% add_markers(x = ~Female, y = ~Age, name = "Female", marker = list(color = '#c80d1e', size = 12, line = list(color = '#800813', width = 0)))
    fig <- fig %>% add_markers(x = ~Male, y = ~Age, name = "Male", marker = list(color = '#084f8080', size = 15, line = list(color = '#084f80', width = 1)))
    fig <- fig %>% layout(
      title = "Comparison between Age Groups for Gender",
      xaxis = list(title = ""),
      margin = list(l = 0),
      yaxis = list(title = ""),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor='transparent'
    )
    
    fig
  })
  
  output$gender_country <- renderUI({
    
    countries = input$gender_country_demo_countries
    age_type = input$gender_country_demo_age_type
    year_range = input$gender_country_demo_year
    type = as.integer(input$gender_country_demo_type)
    
    if (length(countries) == 0) {
      output$gender_country_no_input <- renderText({ 
        "Please select a country."
      })
      return()
      
    } else {
      output$gender_country_no_input <- renderText({ 
        ""
      })
    }
    
    if (year_range[1] == year_range[2]) {
      return ()
    }
    
    demo_gender <- demo_gender %>% filter(year >= year_range[1], year <= year_range[2])
    
    if (!is.null(age_type)) {
      demo_gender$total <- demo_gender %>% select(age_type) %>% rowSums(.)
    }
    
    if (type == 1) {
      demo_gender %>% group_by(year, origin, gender) %>%
        summarise(total = sum(total), .groups = 'drop') %>%filter(origin == countries, gender == "Female") -> females
      demo_gender %>% group_by(year, origin, gender) %>%
        summarise(total = sum(total), .groups = 'drop') %>% filter(origin == countries, gender == "Male") -> males
    } else {
      demo_gender %>% group_by(year, asylum, gender) %>%
        summarise(total = sum(total), .groups = 'drop') %>% filter(asylum == countries, gender == "Female") -> females
      demo_gender %>% group_by(year, asylum, gender) %>%
        summarise(total = sum(total), .groups = 'drop') %>% filter(asylum == countries, gender == "Male") -> males
    }
    
    females$year <- factor(females$year, levels = females[["year"]])
    males$year <- factor(males$year, levels = males[["year"]])
    
    fig <- plot_ly(females, x = ~year, y = ~total, type = 'scatter', name = 'Female', mode = 'lines+markers',
                   line = list(color = 'rgb(205, 12, 24)', width = 3), marker = list(color = '#800813', size = 8)) 
    fig <- fig %>% add_trace(y = males$total, name = 'Male', line = list(color = 'rgb(22, 96, 167)', width = 3),
                             marker = list(color = '#084f8080', size = 10, line = list(color = '#084f80', width = 1))) 
    fig <- fig %>% layout(
      title = paste("Yearly Gender Comparison for ", countries, sep = ""),
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor='transparent'
    )
    
    fig
  })
  
  observeEvent(input$gender_country_demo_year,{
    if(input$gender_country_demo_year[1] == input$gender_country_demo_year[2]){
      if (input$gender_country_demo_year[1] != 2010) {
        updateSliderTextInput(session,"gender_country_demo_year",selected = c((input$gender_country_demo_year[1]-1),input$gender_country_demo_year[2]))
      } else {
        updateSliderTextInput(session,"gender_country_demo_year",selected = c((input$gender_country_demo_year[1]),input$gender_country_demo_year[2]+1))
      }
    }
  })
  
  output$age_country <- renderUI({
    
    countries = input$age_country_demo_countries
    age_type = input$age_country_demo_age_type
    year_range = input$age_country_demo_year
    type = as.integer(input$age_country_demo_type)
    gender_type = as.integer(input$age_country_gender_type)
    
    if (length(countries) == 0) {
      output$age_country_no_input <- renderText({ 
        "Please select a country."
      })
      return()
      
    } else {
      output$age_country_no_input <- renderText({ 
        ""
      })
    }
    
    demo_gender_age <- data.frame(demo_gender)
    
    if (!is.null(countries)) {
      if (type == 1) {
        countries_to_include <- countries %in% origin_countries
      } else {
        countries_to_include <- countries %in% asylum_countries
      }
      
      typeChecker <- ifelse(type == 1, "origin", "asylum")
      
      for (num in 1:length(countries[countries_to_include])) {
        if (num == 1) {
          demo_gender_age <- demo_gender[demo_gender[typeChecker] == countries[countries_to_include][num],]
        } else {
          demo_gender_age <- rbind(demo_gender_age, demo_gender[demo_gender[typeChecker] == countries[countries_to_include][num],])
        }
      }
    }
    
    if (!is.null(gender_type) && length(gender_type) == 1) {
      gender_ <- ifelse(gender_type == 1, "Male", "Female")
      print(gender_)
      demo_gender_age %>% filter(gender == gender_) -> demo_gender_age
    }
    
    if (type == 1) {
      demo_age_country <- demo_gender_age %>% group_by(year, origin) %>%
        summarise(age0to4 = sum(age0to4), age5to11 = sum(age5to11), age12to17 = sum(age12to17), age18to59 = sum(age18to59), age60 = sum(age60), unknown = sum(unknown), .groups = 'drop') %>%
        filter(year >= year_range[1], year <= year_range[2])
    } else {
      demo_age_country <- demo_gender_age %>% group_by(year, asylum) %>%
        summarise(age0to4 = sum(age0to4), age5to11 = sum(age5to11), age12to17 = sum(age12to17), age18to59 = sum(age18to59), age60 = sum(age60), unknown = sum(unknown), .groups = 'drop') %>%
        filter(year >= year_range[1], year <= year_range[2])
    }
    
    demo_age_country <- demo_age_country %>% select(-year)

    if (type == 1) {
      n <- demo_age_country$origin
    } else {
      n <- demo_age_country$asylum
    }
    demo_age_country <- as.data.frame(t(demo_age_country[,-1]))
    colnames(demo_age_country) <- n
    demo_age_country$age <- rownames(demo_age_country)
    rownames(demo_age_country) <- NULL
    demo_age_country$age <- factor(demo_age_country$age, levels = demo_age_country[["age"]])
    
    fig <- plot_ly(x = demo_age_country$age, y = demo_age_country[[countries[1]]], type = 'scatter', name = countries[1], mode = 'lines+markers',
                   line = list(color = randomColor[[1]], width = 3), marker = list(color = randomColor[[1]], size = 10))
    
    if (length(countries) > 1) {
      for (i in 2:length(countries)) {
        fig <- fig %>% add_trace(demo_age_country, y = demo_age_country[[countries[i]]], name = countries[i], line = list(color = randomColor[[i]], width = 3),
                                 marker = list(color = randomColor[[i]], size = 10))
      }
    }
    #fig <- fig %>% add_trace(demo_age_country, y = ~Myanmar, name = 'Albania', line = list(color = randomColor[[2]], width = 3),
    #                         marker = list(color = randomColor[[2]], size = 10)) 
    #fig <- fig %>% add_trace(demo_age_country, y = ~Uzbekistan, name = 'Uzbekistan', line = list(color = randomColor[[3]], width = 3),
    #                         marker = list(color = randomColor[[3]], size = 10)) 
    
    country_names <- countries[1]
    
    if (length(countries) != 1) {
      for (i in 2:length(countries)) {
        country_names <- paste(country_names, countries[i], sep = ", ")
      }
    }
    
    fig <- fig %>% layout(
      title = paste("Yearly Age Comparison for ", country_names, sep = ""),
      xaxis = list(title = ""),
      yaxis = list(title = ""),
      plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor='transparent'
    )
    
    fig
  })
  
  observeEvent(input$age_country_gender_type, {
    if (is.null(input$age_country_gender_type)) {
      updateCheckboxGroupButtons(session, "age_country_gender_type", selected = 1:2)
    }
  }, ignoreNULL=FALSE)
  
  output$refugeeAttacks <- renderUI({
    
    if (input$attacks_all) {
      gtd -> gtd_country
      input_country = "All Countries"
      
      output$attacks_no_input <- renderText({ 
        ""
      })
    } else {
      input_country <- input$attack_countries
      if (length(input_country) == 0) {
        output$attacks_no_input <- renderText({ 
          "Please select a country."
        })
        return()
        
      } else {
        output$attacks_no_input <- renderText({ 
          ""
        })
      }
      
      gtd %>% filter(country == input_country) -> gtd_country
    }
    
    gtd_country$size <- rescale(gtd_country$attacks, to=c(0,80))
    
    fig <- plot_ly(gtd_country, x = ~refugees, y = ~attacks, text = ~year, color = ~size, type = 'scatter', mode = 'markers', name = 'year',
                   marker = list(size = ~size, opacity = 0.5))
    fig <- fig %>% layout(title = paste('Terrorist Attacks & Refugees in ', input_country, sep = ""),
                          paper_bgcolor='transparent',
                          plot_bgcolor  = "rgba(0, 0, 0, 0)",
                          xaxis = list(title = "No. of Refugees", showgrid = FALSE),
                          yaxis = list(title = "No. of Attacks", showgrid = FALSE))
    
    fig %>% hide_colorbar()
  }) 
  
  output$top10origin <- renderUI({
    
    type <- as.integer(input$racebar_pop_type)
    
    if (input$racebar_lowres) {
      if (type == 1) {
        imgSrc = 'http://www.zahirsher.com/images/origin_top10.gif'
      } else {
        imgSrc = 'http://www.zahirsher.com/images/asylum_top10.gif'
      }
    } else {
      if (type == 1) {
        imgSrc = 'http://www.zahirsher.com/images/origin_top10hd.gif'
      } else {
        imgSrc = 'http://www.zahirsher.com/images/asylum_top10hd.gif'
      }
    }
    
    img(src = imgSrc, class="img-fluid", style = "opacity:1")
  })
  
  output$country_comparison <- renderUI({
    
    countries <- input$pop_countries
    refugee_type <- input$pop_refugee_type
    year_range <- as.integer(input$pop_country_year)
    type <- as.integer(input$country_pop_type)
    
    if (length(countries) == 0) {
      output$pop_country_no_input <- renderText({ 
        "Please select a country."
      })
      return()
      
    } else {
      output$pop_country_no_input <- renderText({ 
        ""
      })
    }
    
    if (year_range[1] == year_range[2]) {
      return ()
    }
    
    pop <- data.frame(population)
    class(pop$year) <- "character"
    
    if (type == 1) {
      pop <- pop %>% group_by(year, origin) %>% rename("country" = origin)
    } else {
      pop <- pop %>% group_by(year, asylum) %>% rename("country" = asylum)
    }
    
    if (length(refugee_type) == 0) {
      refugee_type <- "refugees"
    }
    
    pop <- pop %>% filter(year >= year_range[1], year <= year_range[2]) %>% 
      summarise(across(c(refugee_type), sum), .groups = 'drop') %>%
      mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE))
    
    country <- pop %>% filter(country == countries[1])
    fig <- plot_ly(x = country$year, y = country$total, type = 'scatter', name = countries[1], mode = 'lines+markers',
                   line = list(color = randomColor[[1]], width = 3), marker = list(color = randomColor[[1]], size = 10))
    
    if (length(countries) > 1) {
      for (i in 2:length(countries)) {
        country <- pop %>% filter(country == countries[i]) 
        fig <- fig %>% add_trace(y = country$total, name = countries[i], line = list(color = randomColor[[i]], width = 3),
                                 marker = list(color = randomColor[[i]], size = 10))
      }
    }

    fig %>% layout(paper_bgcolor='transparent',
                   plot_bgcolor  = "rgba(0, 0, 0, 0)")
  })

  observeEvent(input$pop_country_year,{
    if(input$pop_country_year[1] == input$pop_country_year[2]){
      if (input$pop_country_year[1] != 2010) {
        updateSliderTextInput(session,"pop_country_year",selected = c((input$pop_country_year[1]-1),input$pop_country_year[2]))
      } else {
        updateSliderTextInput(session,"pop_country_year",selected = c((input$pop_country_year[1]),input$pop_country_year[2]+1))
      }
    }
  })
  
  output$treemap_comparison <- renderUI({
    
    countries <- input$treemap_countries
    refugee_type <- input$treemap_refugee_type
    year_range <- as.integer(input$treemap_year)
    type <- as.integer(input$treemap_pop_type)
    
    if (!is.null(countries)) {
      if (type == 1) {
        countries_to_include <- countries %in% origin_countries
      } else {
        countries_to_include <- countries %in% asylum_countries
      }
      
      typeChecker <- ifelse(type == 1, "origin", "asylum")
      
      for (num in 1:length(countries[countries_to_include])) {
        if (num == 1) {
          pop2 <- population[population[typeChecker] == countries[countries_to_include][num],]
        } else {
          pop2 <- rbind(pop2, population[population[typeChecker] == countries[countries_to_include][num],])
        }
      }
      
      if (!is.null(pop2$origin[1])) {
        population <- pop2
      }
    }
    
    pop <- population %>% filter(year >= year_range[1], year <= year_range[2])
    
    if (type == 1) {
      if (year_range[1] == year_range[2]) {
        title = paste0("Country of Origin in ", year_range[1])
      } else {
        title = paste0("Country of Origin from ", year_range[1], " to ", year_range[2])
      }
      pop <- pop %>% group_by(origin) %>% rename("country" = origin)
    } else {
      if (year_range[1] == year_range[2]) {
        title = paste0("Country of Asylum in ", year_range[1])
      } else {
        title = paste0("Country of Asylum from ", year_range[1], " to ", year_range[2])
      }
      pop <- pop %>% group_by(asylum) %>% rename("country" = asylum)
    }
    
    if (length(refugee_type) == 0) {
      refugee_type <- "refugees"
    }
    
    pop <- pop %>% select(country, refugee_type) %>%
      summarise(across(c(refugee_type), sum), .groups = 'drop') %>%
      mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
      arrange(desc(total))
    pop <- pop[1:25,]
    
    hchart(pop, type="treemap",hcaes(x = country, value = total, color = total)) %>%
      hc_title(text = title) %>%
      hc_colorAxis(stops = dshmstops) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_credits(enabled = TRUE, text = "Data Source: UNHCR Refugee Data", style = list(fontSize = "12px")) %>%
      hc_legend(enabled = FALSE) %>%
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
  
  output$sankey_comparison <- renderUI({
    year_range <- as.integer(input$sankey_year)
    refugee_type <- input$sankey_refugee_type
    
    if (length(refugee_type) == 0) {
      refugee_type <- "refugees"
    }
    
    df <- data.frame(population)
    
    df <- df %>% filter(year >= year_range[1], year <= year_range[2])
    
    df <- df %>% group_by(origin, asylum) %>%
      select(origin, asylum, refugee_type) %>%
      summarise(across(c(refugee_type), sum), .groups = 'drop') %>%
      mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE)) %>%
      arrange(desc(total))
    
    df <- df[1:20, ]
    df <- df %>% group_by(asylum) %>% mutate(IDtarget=cur_group_id())
    df <- df %>% group_by(origin) %>% mutate(IDsource=cur_group_id())
    df_nodes <- data.frame(name=c(as.character(df$origin), as.character(df$asylum)) %>% unique())
    
    df$IDsource=match(df$origin, df_nodes$name)-1 
    df$IDtarget=match(df$asylum, df_nodes$name)-1
    
    ColourScal ='d3.scaleOrdinal() .range(["#FDE725FF","#B4DE2CFF","#6DCD59FF","#35B779FF", "#c402d9FF", "#02d9c4FF", "#fda233FF", "#bd3552FF",
"#1F9E89FF","#26828EFF","#31688EFF","#3E4A89FF","#482878FF","#440154FF", "#800813FF", "#084f80FF", "#088039FF", "#081380FF"])'
    
    sankeyNetwork(Links = data.frame(df), Nodes = df_nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "total", NodeID = "name", 
                  sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, nodePadding=20, fontFamily = "Verdana")
  })
  
})
