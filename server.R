library(shiny)
library(shinyWidgets)
library(dplyr)
library(googlesheets)
library(ggplot2)
library(tidyr)
library(readr)
library(forcats)
library(stringr)
library(purrr)
library(leaflet)

function(input, output, session) {
  
  bag_2017 <- read_csv("./bag_2017.csv")
  
  fit_dry_fresh <- lm(drywt ~ freshwt, data = bag_2017)
  fit_n_fresh <-   lm(nwt ~ log(freshwt), data = bag_2017)
  
  
  # bags ----
  bags <- reactive({
    
    withProgress(
      message = "Downloading data",
      
      expr = {
        bm_gs_2018 <- 
          list(
            # GA = "secret_key",
            # NC = "secret_key",
            # MD = "secret_key"
          ) %>% 
          map(~{incProgress(1/12); gs_key(.x, lookup = F)}) %>% 
          map_dfr(~{
            
            incProgress(1/12)
            
            gs_read(.x, ws = "Biomass Export") %>% 
              filter(Biomass_Sample != "Bexample") %>% 
              select(
                `Site year`, 
                lastname = `Farmer Last Name`, 
                Year, 
                subplot,
                freshwt = matches("fresh")
              ) %>% 
              mutate_at(vars(matches("wt")), funs(as.numeric))
            
          }, .id = "state")
        
        
        bm_gs_2017 <- 
          list(
            # GA = "secret_key",
            # NC = "secret_key",
            # MD = "secret_key"
          ) %>% 
          map(~{incProgress(1/12); gs_key(.x, lookup = F)}) %>% 
          map_dfr(~{
            
            incProgress(1/12)
            
            gs_read(.x, ws = "Biomass")  %>% 
              filter(Set %in% c("A", "B", "A0", "B0")) %>% 
              select(
                `Site year`,
                lastname = `Farm name long`,
                Year,
                subplot,
                freshwt = matches("BiomassFreshWt")
              ) %>% 
              mutate_at(vars(matches("wt")), funs(as.numeric))
            
          }, .id = "state") 
        
        bind_rows(bm_gs_2017, bm_gs_2018)
      }
    )
    
  })
  
  
  # bags_pred ----
  bags_pred <- reactive({
    bags() %>% 
      mutate(state = fct_collapse(state, "GA/NC/SC" = c("GA", "NC"), "MD/PA" = "MD")) %>% 
      filter(!is.na(freshwt)) %>% 
      mutate(
        drywt = map(log(freshwt), 
                    ~predict(fit_dry_fresh, 
                             newdata = data_frame(freshwt = .x), 
                             interval = "confidence") %>% 
                      as_data_frame() %>%
                      rename_all(function(nms) {paste0("drywt_", nms)})
                    ),
        nwt = map(log(freshwt), 
                  ~predict(fit_n_fresh, 
                           newdata = data_frame(freshwt = .x), 
                           interval = "confidence") %>% 
                    as_data_frame() %>%
                    rename_all(function(nms) {paste0("nwt_", nms)})
                  )
        ) %>%
      unnest() %>% 
      group_by(state, `Site year`, lastname, Year) %>% 
      summarise_if(is.numeric, mean) %>% 
      mutate_at(vars(matches("wt_")), funs(exp)) %>% 
      mutate_at(vars(matches("wt")), funs(signif(4046.86*./454, 3))) %>% 
      group_by(state) %>% 
      mutate(rnk = row_number(drywt_fit)) 
  })
  
  # sites ----
  sites <- reactive({
    if (is.null(bags_pred()) | input$lastname == "") return(NULL)
    
    bags_pred() %>% 
      filter(str_detect(str_to_lower(lastname), str_to_lower(input$lastname)))
    
  })
  
  # errorreporter ----
  errorreporter <- reactive({
    if (is.null(bags()) | is.null(sites())) {
      msg <- paste("What's going wrong?\n\n---\n", Sys.time())
    } else {
      msg <- 
        paste0("What's going wrong?\n\n---\n",
               "\nEntered name: ", input$lastname,
               "\nMatched name(s): ", sites() %>% 
                 pull(lastname) %>% unique() %>% 
                 paste(collapse = ","),
               "\nSelected field: ", input$fieldinfo,
               "\n", Sys.time()
        )
      
    }
    
    
    a(href = paste0("mailto:brianwdavis@gmail.com?subject=Biomass%20web%20app%20error%20report&body=",  
                    URLencode(msg)), 
      "Submit details via email here.")
    
  })
  
  output$errorreport <- renderUI(errorreporter())
  
  # output$missingerror ----
  output$missingerror <- renderUI({
    if (is.null(sites())) return(NULL)
    
    if (input$lastname != "" & nrow(sites()) == 0) {
      h4(div("We're missing some of your data for this field right now. Check back soon!", 
             errorreporter(),
             class = "alert alert-info"))
    } else {
      return(NULL)
    }
  })
  
# output$fieldinfo ----
  output$fieldinfo <- renderUI({
    if (is.null(sites())) return(NULL)
    
    checkboxGroupButtons(
      "fieldinfo", "Field ID:",
      choices = unique(sites()$`Site year`),
      selected = unique(sites()$`Site year`),
      individual = T, status = "outline-primary"
    )
  })
  
  # latlongs ----
  latlongs_gs <- reactive({
    withProgress(message = "Loading map", {
      
      # ll_gs <- 
      #   "secret_key" %>% 
      #   {incProgress(1/3); gs_key(., lookup = F)} 
      
      ll_2017 <- 
        ll_gs %>% 
        {incProgress(1/3); gs_read(., ws = "2017", skip = 1)} %>% 
        select(
          Code = `Farm code`, 
          lastname = `Last name`, 
          `Latitude, Longitude`)
      
      ll_2018 <- 
        ll_gs %>% 
        {incProgress(1/3); gs_read(., ws = "2018", skip = 1)} %>% 
        select(
          Code, 
          lastname = `Farmer Last Name`, 
          `Latitude, Longitude`
          )
      
      bind_rows(
        "2017" = ll_2017,
        "2018" = ll_2018,
        .id = "Year"
        ) %>%  
        separate(`Latitude, Longitude`, c("lat", "long"), sep = ",") %>% 
        mutate_at(vars(lat, long), funs(as.numeric)) 
      }
    )
  })
  
  latlongs <- reactive({
    if (is.null(latlongs_gs())) return(NULL)
    
    latlongs_gs() %>% 
      filter(str_detect(str_to_lower(lastname), 
                        str_to_lower(input$lastname))) %>% 
      filter(!is.na(lat))  
  })
  
  # output$ll_error ----
  output$ll_error <- renderUI({
    if (is.null(latlongs()) | is.null(sites())) {
      return(
        tags$small("Loading field locations...", class = "text-muted")
        )
      }
    
    if (nrow(latlongs()) < length(unique(sites()$`Site year`))) {
      return(
        tags$small(
          "Some field GPS coordinates aren't available yet.",  
          class = "text-muted", style = "float: right;"
          )
        )
    } else {
      return(NULL)
    }
  })
  
  # basemap ----
  basemap <- 
    leaflet(options = leafletOptions(attributionControl = F)) %>% 
      addProviderTiles(providers$Esri.WorldImagery) %>% 
      addProviderTiles(providers$Stamen.TonerLabels, 
                       options = tileOptions(opacity = 0.5)) %>% 
      fitBounds(-74.17, 39.96, -84.9,  32.15)
  
  # output$map ----
  output$map <- renderLeaflet({
    trash <- input$reset
    
    if (is.null(latlongs()) | input$lastname == "") {
      return(basemap)
    } 
    
    if (nrow(latlongs()) > 0 & input$lastname != "") {
      basemap %>% clearBounds() %>% 
      addCircleMarkers(data = latlongs(),
        ~long, ~lat, label = ~Code,
        labelOptions = labelOptions(
          noHide = T, 
          offset = c(0,15),
          opacity = 0.65, 
          style = list("color" = "black", "font-size" = "18px", "font-weight" = "900")
          ),
        clusterOptions = markerClusterOptions(
          spiderLegPolylineOptions = list(color = "#ffffff", weight = 2.5),
          style = list("font-weight" = "900")
          ),
        color = "white", opacity = 0.8
        )
      } else {basemap}
  
  })
  
  # output$freshwt_text ----
  output$freshwt_text <- renderUI({
    if (is.null(sites())) return(NULL)
    
    if (is.null(input$fieldinfo)) {
      div("Select one or more fields to get a summary of your cover crop data.")
    } else {
      df <- sites() %>% filter(`Site year` %in% input$fieldinfo) %>% ungroup() %>%  
        select(Year, `Site year`, freshwt) %>% 
        arrange(Year, `Site year`)
      
      pmap(df, function(...) {
        tags$li(
          HTML(paste0(
            ..1, ..2, ": ", 
            tags$b(signif(..3/4356.0, 2)), 
            " lbs/10ft", tags$sup("2")
            ))
          )
      }) %>% 
        div("We collected fresh plant tissues of your cover crop:", ., tags$hr())
    }
  })
  
  # output$drywt_text ----
  output$drywt_text <- renderUI({
    if (is.null(sites())) return(NULL)
    
    if (is.null(input$fieldinfo)) {
      div(" ")
    } else {
      df <- sites() %>% filter(`Site year` %in% input$fieldinfo) %>% ungroup() %>%  
        select(Year, `Site year`, drywt_lwr, drywt_upr) %>% 
        arrange(Year, `Site year`)
      
      pmap(df, function(...) {
        tags$p(
          "In field ", tags$b(..2), "(", ..1, "), your cover crop had between:", tags$br(),
          tags$b(..3), " and ", tags$b(..4), " lbs/acre of dry matter."
          )
      }) %>% div(., tags$hr(), tags$br())
    }
  })
  
  # output$nwt_text ----
  output$nwt_text <- renderUI({
    if (is.null(sites())) return(NULL)
    
    if (is.null(input$fieldinfo)) {
      div(" ")
    } else {
      df <- sites() %>% filter(`Site year` %in% input$fieldinfo) %>% ungroup() %>%  
        select(Year, `Site year`, nwt_fit, nwt_lwr, nwt_upr) %>% 
        mutate_if(is.numeric, funs(round(.,1)))
      
      pmap(df, function(...) {
        tags$p(
          "In field ", tags$b(..2), "(", ..1, "), your cover crop contained between:", tags$br(),
          tags$b(..4), " and ", tags$b(..5), " lbs /acre of N.", tags$br(),
          "Available to the corn at sidedress:", tags$b(round(0.4*..3, 1)), "lbs."
          )
      }) %>% div(., tags$hr(), tags$br())
    }
  })
  
  
  # output$dry_plot ----
  output$dry_plot <- renderPlot({
    if (is.null(bags_pred())) return(NULL)
    
    df <- bags_pred() %>% 
      mutate(flag = `Site year` %in% input$fieldinfo)
    
    regions <- df %>% filter(flag) %>% pull(state) %>% unique()
    
    if (length(regions) == 1) {
      df <- df %>% filter(state %in% regions)
    } 
    
    ggplot(df, aes(rnk, drywt_fit, color = flag)) +
      geom_linerange(aes(ymin = drywt_lwr, ymax = drywt_upr),
                     show.legend = F, color = "grey65") +
      geom_point(show.legend = F, size = 3.5) +
      facet_grid(state ~ ., scales = "free_y", space = "free_y") +
      geom_text(data = function(d) filter(d, flag),
                aes(x = rnk, y = drywt_lwr, label = `Site year`),
                nudge_y = -200, hjust = 1, fontface = "bold",
                show.legend = F) +
      coord_flip() +
      scale_y_continuous(
        limits = c(-100,NA),
        labels = scales::comma,
        sec.axis = dup_axis(name = NULL)) +
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
      labs(x = NULL, y = expression("Dry matter: "*frac(lbs,acre)),
           title = "Your farm compared with others in your region") +
      theme_minimal() +
      theme(axis.text.y = element_blank(),
            axis.text.x = element_text(size = rel(1.75)),
            strip.text = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            title = element_text(face = "bold"))

  }
  )
  

  

}