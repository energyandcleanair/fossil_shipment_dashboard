


# Event Observers --------------------------------------



# Download Handlers ----------------------------------


# Output Elements --------------------------------------


# output$selectUnit <- renderUI({
#   # req(flows())
#   # req(input$commodity)
#   # available_units <- unique(flows() %>% filter(commodity==input$commodity) %>% pull(unit))
#   available_units <- c("MWh/day"="MWh/day", "m3/day"="m3")
#   selectInput("unit", "Unit:", choices=available_units, selected=available_units[1])
# })


# # Reactive Elements --------------------------------------
plot_data <- reactive({

  voyages <- voyages()
  req(input$arrival_or_departure)
  req(voyages)
  add_ongoing <- input$add_ongoing

  commodities <- c("lng"="LNG",
                   "crude_oil"="Crude Oil",
                   "oil_products"="Oil Products",
                   "oil_or_chemical"="Oil or Chemical",
                   "bulk"="Bulk (ex. coal)",
                   "bulk_notcoal"="Bulk (ex. coal)",
                   "coal"="Coal")

  d <- voyages

  if(add_ongoing){
    d <- d %>% filter(status %in% c("ongoing", "completed")) %>%
      mutate(arrival_iso2=ifelse(status=="ongoing", destination_iso2, arrival_iso2))
  }else{
    d <- d %>% filter(status %in% c("completed"))
  }

  d <- d %>%
    # mutate(quantity = ifelse(unit=="tonne", quantity/1000, quantity),
    # unit = ifelse(unit=="tonne", "thousand tonne", unit)) %>%
    mutate(quantity=value_tonne/1000, unit="thousand tonne") %>%
    mutate(commodity = recode(commodity, !!!commodities)) %>%
    filter(!grepl("bulk|lpg|oil_or_ore", commodity, ignore.case = T))


  d$date <- d[,input$arrival_or_departure]
  d$destination_region = countrycode::countrycode(d$arrival_iso2, "iso2c", "country.name")
  d[d$arrival_iso2 %in% setdiff(codelist$iso2c[codelist$eu28=="EU"], NA), "destination_region"] <- "EU28"

  identified_regions <- c("EU28",  "China", "United States", "Japan", "India", "Turkey", "South Korea", "Taiwan", "Malaysia", "Egypt", "Unknown")
  d[is.na(d$destination_region), "destination_region"] <- "Unknown"
  d[!d$destination_region %in% identified_regions, "destination_region"] <- "Other"

  # d %>% group_by(arrival_iso2, destination_region) %>% summarise_at("quantity", sum, na.rm=T) %>% arrange(desc(quantity)) %>% View()
  top_importers <- d %>% group_by(destination_region) %>% summarise(dwt=sum(dwt, na.rm=T)) %>% arrange(desc(dwt)) %>% pull(destination_region)
  d$destination_region <- factor(d$destination_region, levels=c(setdiff(top_importers, "Other"),"Other"))

  d <- d %>%
    group_by(destination_region, date, commodity, unit) %>%
    summarise_at(c("dwt","quantity"), sum, na.rm=T) %>%
    ungroup()

  filler <- tibble(date=seq.Date(min(d$date), max(d$date), by="day")) %>%
    tidyr::crossing(d %>% ungroup() %>% distinct(destination_region, commodity, unit))

  # Add 0s
  d <- d %>% full_join(filler) %>%
    mutate(quantity = tidyr::replace_na(quantity, 0),
           dwt = tidyr::replace_na(dwt, 0))

  return(d)
})


output$plot_voyages_lines <- renderPlotly({

  d <- plot_data()
  req(d)

  # Add 0s and rolling average
  d <- d %>%
    rcrea::utils.running_average(7, vars_to_avg = c("quantity","dwt"))

  colourCount = length(unique(d$commodity))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(d) +
    geom_line(aes(date, dwt, col=commodity),
             stat="identity") +
    facet_wrap(~destination_region, nrow=3) +
    rcrea::theme_crea() +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    scale_x_date(date_labels = "%b %Y", limits=c(as.Date("2022-01-01"), NA)) +
    scale_color_manual(values = getPalette(colourCount), name=NULL) +
    labs(x=sprintf("%s date", tools::toTitleCase(input$arrival_or_departure)),
         y="tonnage")

  plt <- ggplotly(plt) %>%
    config(displayModeBar = F)
  return(plt)
})


output$plot_voyages_bars <- renderPlotly({

  d <- plot_data()
  req(d)

  d <- d %>%
    mutate(commodity = sprintf("%s - %s", commodity, unit))

  add_trailing <- function(x) paste0(x, "   ")  # Tweak to prevent cut legend...
  d$destination_region <- factor(add_trailing(d$destination_region), levels=rev(add_trailing(levels(d$destination_region))))


  d <- d %>%
    group_by(destination_region, date, commodity, unit) %>%
    summarise(quantity=sum(quantity, na.rm=T))

  colourCount = length(unique(d$destination_region))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(d) +
    geom_bar(aes(date, quantity, fill=destination_region),
             stat="identity",
             show.legend = F) +
    facet_wrap(~commodity, scales="free_y", ncol = 1) +
    rcrea::theme_crea() +
    scale_fill_manual(values = getPalette(colourCount),
                      name=NULL) +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    scale_x_date(date_labels = "%b %Y", limits=c(as.Date("2022-01-01"), NA)) +
    # theme(legend.position='none') +
    labs(x=sprintf("%s date", tools::toTitleCase(input$arrival_or_departure)), , y=NULL)

  plt <- ggplotly(plt) %>%
    config(displayModeBar = F)
  return(plt)
})
