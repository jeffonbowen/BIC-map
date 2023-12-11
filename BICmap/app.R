### Bowen Island Conservancy Web Mapping Application - BICmap ###

source("global.R")

# User Interface ----------------------------------------------------------

ui <- page_navbar(
  
  theme = bs_theme(bootswatch = "cerulean"),
#                   fg = "#E7E8E8"),
                     
  # Alternates: yeti, superhero, sketchy, sandstone cyborg, cosmo, cerulean
  
  title = ("BIC Map "),
  
  nav_panel(
    title = "Layer Explorer",
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 4fr"),
      
      card(
        card_header(
          tags$b("Bowen Island Conservancy's Biodiversity Data Explorer")
          ),
        card_body(p("This website is currently in development. Updates and revisions are ongoing."),
                  p("Select base layers and overlays in the map window controls."),
                  p("Try the GrayCanvas base for a clean background."),
                  p("Note that the BIM streams layer needs fixing.")
        ),
        card_footer(
          p("Parks and greens spaces, lakes, ponds, wetlands, and streams are from BIM",
            style = "font-size:80%")
        )
        ),
      
      card(
        card_body(
          leafletOutput("main", width = "100%", height = "100%")
          )
        )
      )
    ),
  
  nav_panel(
    title = "Sensitive Ecosystem Explorer",
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 3fr 1fr"),
      
      card(
        card_header("Explore Locations of Sensitive Ecosystems"),
        card_body(
          radioButtons("class",
                      label = "Select a sensitive ecosystem:",
                      choices = sei_lgnd$complgnd,
                      selected = character(0)),
          p("The sensitive ecosystem may occur as the dominant ecosystem in a polygon or as a secondary or tertiary ecocosystem.")
          )
        ),
      
      card(
        leafletOutput("sei", width = "100%", height = "100%")
        ),

      card(
        card_header("SEI Overview"),
        card_body(
          div(
            style = "font-size:85%",
            fill = TRUE,
            tableOutput("sei_overview")
            )
          ),
        card_header("Ecosystem Detail"),
        card_body(
          div(
            style = "font-size:85%",
            textOutput("listclass"),
            tableOutput("sei_table")
          )
        )
        )
        )
  ),
  
  nav_panel(
    title = "Species Explorer",
    layout_column_wrap(
      width = NULL,
      style = css(grid_template_columns = "1fr 2fr 2fr"),
      card(
        card_body(
          "Explore data from iNaturalist. ",
          "Only records classified as research grade are included.", 
          p("Select a point to show the species name."),
          p("Use the filters to restrict displayed records."),
          br(),
          actionButton(
            "clear_rows_button",
            "Clear Selected"
            ),
          textOutput("sp_sel")
          )
        ),
      
      card(
        div(
          DTOutput("species_table", height = "auto", fill = TRUE),
          style = "font-size:75%"
          )
      ),
      
      card(
        leafletOutput("inat", width = "100%", height = "100%")
      )
    )
    ),

  
  nav_spacer(),  

  
  nav_menu(
    title = "About",
    align = "right",
    value = "Nothing here yet"
    )
  )


# Wrap your UI with secure_app
# ui <- secure_app(ui)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # # call the server part
  # # check_credentials returns a function to authenticate users
  # res_auth <- secure_server(
  #   check_credentials = check_credentials(credentials)
  # )
  # 
  # output$auth_output <- renderPrint({
  #   reactiveValuesToList(res_auth)
  # }
  # )
  
  
# Main map ----------------------------------------------------------------
  
#  bs_themer()
  
  output$main <- renderLeaflet({
    main
  })
  

# SEI Explorer ------------------------------------------------------------

  overlays.sei <-  c(overlays.base, "SEI")
  
  output$sei <- renderLeaflet({
    base |>
      addPolygons(data = sei, 
                  group = "SEI",
                  color = "green", 
                  fill = TRUE, fillOpacity = 0, stroke = TRUE,
                  weight = 0.5, popup = pop.sei) |> 
      addLayersControl(
        baseGroups = basegroups,
        overlayGroups = overlays.sei,
        options = layersControlOptions(collapsed = FALSE),
        position = "topleft") |> 
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |> 
      hideGroup(c("Parcels", "Parks and Green Spaces"))
  })
  
  observeEvent(
    input$class, {
      leafletProxy("sei") %>%
        clearGroup("select") |>
        clearControls() |>
        addPolygons(data = filter(sei, comp3lgnd == input$class),
                    color = "yellow", fill = TRUE,
                    fillOpacity = 0.8, stroke = FALSE,
                    group = "select") |>
        addPolygons(data = filter(sei, comp2lgnd == input$class),
                    color = "orange", fill = TRUE,
                    fillOpacity = 0.8, stroke = FALSE,
                    group = "select") |>
        addPolygons(data = filter(sei, comp1lgnd == input$class),
                    color = "red", fill = TRUE,
                    fillOpacity = 0.8, stroke = FALSE,
                    group = "select") |>
        addPolygons(data = sei, 
                    group = "SEI",
                    color = "green", 
                    fill = TRUE, fillOpacity = 0, stroke = TRUE,
                    weight = 1, popup = pop.sei) |> 
        addLegend(colors = c("red", "orange", "yellow"),
                  labels = c("Primary", "Secondary", "Tertiary"),
                  title = "Ecosystem Dominance")
    }
  )
  
  output$sei_overview <- renderTable(
        data.frame(m = c("Area of Bowen Island: ", "Sensitive Ecosystems: "),
                   a = c("5071.2 ha", "3624.8 ha")
                          ),
        colnames = FALSE, bordered = TRUE
        )
  
  observeEvent(input$class, {
    output$listclass <- renderText({
      paste(input$class,"Areas (in hectares)")
      })
    output$sei_table <- renderTable(
        sei_summary |> select(Dominance, input$class),
        rownames = FALSE, colnames = FALSE,
        digits = 1,
        bordered = TRUE
        )
    }
    )


# iNaturalist --------------------------------------------------------------

  overlay.inat  <- c("Parcels", "Parks and Green Spaces", "iNat Obs")
  pal.inat <- colorFactor(topo.colors(6), inat$taxon_kingdom_name)
  
  output$inat <- renderLeaflet({
inatm <-     base |> 
      addCircles(dat = inat, lng = ~longitude, lat = ~latitude,
                       radius = 1,
                       popup = paste(inat$common_name, "<br>",
                                     inat$scientific_name, "<br>",
                                     inat$observed_on),
                       color = ~pal.inat(taxon_kingdom_name),
                       group = "iNat Obs") |> 
    addLayersControl(
        baseGroups = basegroups,
        overlayGroups = overlay.inat,
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") |>
      addLegend(pal = pal.inat, values = inat$taxon_kingdom_name,
                title = "Kingdom", group = "iNat Obs") |>
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |>
      hideGroup(c("Parcels", "Parks and Green Spaces"))
  })

  output$species_table <- renderDT(
    datatable(sp_list, filter = 'top', 
              options = list(pageLength = -1),
              selection = "single"),
    server = TRUE
  )
  
  # Message in console for debugging.
  observeEvent(input$species_table_rows_selected, {
    message(sp_list[input$species_table_rows_selected, "Scientific Name"])
  }
  )
    
#  output$sp_sel <- renderPrint( {
#    sp_list[input$species_table_rows_selected, "Scientific Name"]
#    }
#    )
    
  observeEvent(input$species_table_rows_selected, {
    sp_sel <- sp_list[input$species_table_rows_selected, "Scientific Name"]
  }
  )

  inat_filter <- reactive(filter(inat, scientific_name == sp_sel))
  
  
  observeEvent(input$species_table_rows_selected, {
    leafletProxy("inat")|> 
      clearGroup("iNat Obs") |>
      clearControls() |> 
      addCircles(dat = inat_filter, lng = ~longitude, lat = ~latitude,
                 radius = 1,
                 popup = paste(inat_filter$common_name, "<br>",
                               inat_filter$scientific_name, "<br>",
                               inat_filter$observed_on),
                 color = ~pal.inat(taxon_kingdom_name),
                 group = "iNat Obs") |>
      addLayersControl(
        baseGroups = basegroups,
        overlayGroups = overlay.inat,
        options = layersControlOptions(collapsed = TRUE),
        position = "topleft") |>
      addLegend(pal = pal.inat, values = inat$taxon_kingdom_name,
                title = "Kingdom", group = "iNat Obs") |>
      addMeasure(primaryLengthUnit = "metres",
                 primaryAreaUnit = "hectares") |>
      hideGroup(c("Parcels", "Protected Areas"))
      
    })
  
  # create a proxy to modify datatable without recreating it completely
  species_table_proxy <- dataTableProxy("species_table")
  
  # clear row selections when clear_rows_button is clicked
  observeEvent(input$clear_rows_button, {
    selectRows(species_table_proxy, NULL)
    
    leafletProxy("inat") |> 
      clearGroup("iNat Obs") |>
      clearControls() |> 
      addCircles(dat = inat, lng = ~longitude, lat = ~latitude,
                   radius = 1,
                   popup = paste(inat$common_name, "<br>",
                                 inat$scientific_name, "<br>",
                                 inat$observed_on),
                   color = ~pal.inat(taxon_kingdom_name),
                   group = "iNat Obs") |> 
      addLayersControl(
          baseGroups = basegroups,
          overlayGroups = overlay.inat,
          options = layersControlOptions(collapsed = TRUE),
          position = "topleft") |>
      addLegend(pal = pal.inat, values = inat$taxon_kingdom_name,
                  title = "Kingdom", group = "iNat Obs") |>
      addMeasure(primaryLengthUnit = "metres",
                   primaryAreaUnit = "hectares") |>
      hideGroup(c("Parcels", "Parks and Green Spaces"))

  })
  
}

shinyApp(ui, server)
