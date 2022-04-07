tabPanel("Plots",
         value="plots",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             # shinyjs::useShinyjs(),

             # uiOutput("selectSource"),
             # uiOutput("selectCommodity"),
             # uiOutput("selectUnit"),
             div(HTML("<br>")),
             selectInput("arrival_or_departure", "Date:", multiple=F, choices = c("arrival","departure"), selected="departure"),
             checkboxInput("add_ongoing", "Include ongoing voyages", value = T)
             # selectInput("group_by", "Group by:", multiple=F, choices = group_bys, selected=group_bys[1]),
             # selectInput("chart_type", "Chart type:", multiple=F, choices = chart_types, selected=chart_types[1]),
             # uiOutput("selectUnit"),
             # downloadButton(outputId="downloadCsv","Download (.csv)",class="btn-secondary")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             width=10,
             div(class="container_trade",
                 h1("Shipments from Russian ports"),
                 h3(class="subtitle", "Shipments tonnage (7-day rolling average)"),
                 plotlyOutput("plot_voyages_lines", width='100%', height='650px') %>% withSpinner(color="#8cc9D0"),
                 h3(class="subtitle", "Shipments daily quantity"),
                 plotlyOutput("plot_voyages_bars", width='100%', height='800px') %>% withSpinner(color="#8cc9D0"),
                 div(class="row",
                     div(class="col-xs-12 caption", span("Source: CREA analysis based on AIS data.")
                     )
                 )
             )
           )
         )
)
