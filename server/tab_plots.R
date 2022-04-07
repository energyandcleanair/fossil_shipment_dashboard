


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


output$plot_voyages_lines <- renderPlotly({

  voyages <- voyages()
  req(input$arrival_or_departure)
  req(voyages)

  commodities <- c("lng"="LNG",
                   "crude_oil"="Crude Oil",
                   "oil_products"="Oil Products",
                   "oil_or_chemical"="Oil or Chemical",
                   "bulk"="Bulk (ex. coal)",
                   "bulk_notcoal"="Bulk (ex. coal)",
                   "coal"="Coal")

  d <- voyages %>%
    mutate(quantity=dwt) %>%
    # mutate(quantity = ifelse(unit=="tonne", quantity/1000, quantity),
           # unit = ifelse(unit=="tonne", "thousand tonne", unit)) %>%
    mutate(commodity = recode(commodity, !!!commodities)) %>%
    # mutate(commodity = sprintf("%s - %s", commodity, unit)) %>%
    filter(status=="completed") %>%
    filter(!grepl("bulk", commodity, ignore.case = T))

  d$date <- d[,input$arrival_or_departure]
  d["destination_region"] = countrycode::countrycode(d$arrival_iso2, "iso2c", "country.name")
  d[d$arrival_iso2 %in% codelist$iso2c[codelist$eu28=="EU"], "destination_region"] <- "EU28"


  identified_regions <- c("EU28",  "China", "United States", "Japan", "India", "Turkey", "South Korea", "Taiwan", "Malaysia", "Egypt", "Morocco")
  d[!d$destination_region %in% identified_regions, "destination_region"] <- "Other"

  # d %>% group_by(arrival_iso2, destination_region) %>% summarise_at("quantity", sum, na.rm=T) %>% arrange(desc(quantity)) %>% View()
  top_importers <- d %>% group_by(destination_region) %>% summarise(quantity=sum(quantity, na.rm=T)) %>% arrange(desc(quantity)) %>% pull(destination_region)

  add_trailing <- function(x) paste0(x, "   ")  # Tweak to prevent cut legend...
  d$destination_region <- factor(add_trailing(d$destination_region), levels=add_trailing(c(setdiff(top_importers, "Other"),"Other")))

  d <- d %>%
    group_by(destination_region, date, commodity) %>%
    summarise(quantity=sum(quantity, na.rm=T)) %>%
    ungroup()

  filler <- tibble(date=seq.Date(min(d$date), max(d$date), by="day")) %>%
    tidyr::crossing(d %>% ungroup() %>% distinct(destination_region, commodity))


  # Add 0s and rolling average
  d <- d %>% full_join(filler) %>%
    mutate(quantity = tidyr::replace_na(quantity, 0)) %>%
    rcrea::utils.running_average(7, vars_to_avg = "quantity")

  colourCount = length(unique(d$commodity))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(d) +
    geom_line(aes(date, quantity, col=commodity),
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


output$plot_voyages <- renderPlotly({

  voyages <- voyages()
  req(input$arrival_or_departure)
  req(voyages)

  commodities <- c("lng"="LNG",
                   "crude_oil"="Crude Oil",
                   "oil_products"="Oil Products",
                   "oil_or_chemical"="Oil or Chemical",
                   "bulk"="Bulk (ex. coal)",
                   "bulk_notcoal"="Bulk (ex. coal)",
                   "coal"="Coal")

  d <- voyages %>%
    mutate(quantity = ifelse(unit=="tonne", quantity/1000, quantity),
      unit = ifelse(unit=="tonne", "thousand tonne", unit)) %>%
    mutate(commodity = recode(commodity, !!!commodities)) %>%
    mutate(commodity = sprintf("%s - %s", commodity, unit),
           quantity = quantity)

  d$date <- d[,input$arrival_or_departure]
  d["destination_region"] = d$arrival_iso2
  d[d$arrival_iso2 %in% codelist$iso2c[codelist$eu28=="EU"], "destination_region"] <- "EU28"

  identified_regions <- c("EU28", "Japan", "China", "United States", "India", "Korea")
  d[!d$destination_region %in% identified_regions, "destination_region"] <- "Other"

  add_trailing <- function(x) paste0(x, "   ")  # Tweak to prevent cut legend...
  d$destination_region <- factor(add_trailing(d$destination_region), levels=rev(add_trailing(c(identified_regions,"Other"))))
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
