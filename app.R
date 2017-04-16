library(shiny)
library(ggvis)
library(dplyr)

ui <- fluidPage(
  headerPanel('Life Expectancy vs. Fertility Rate by Country'),
  sidebarPanel(
    sliderInput("yr", "Year", 1960, 2014, 1960, sep="", animate=animationOptions(interval=300, loop=TRUE)),
    radioButtons("reg", "Region",
      choices = list("East Asia & Pacific" = "East Asia & Pacific",
                     "Europe & Central Asia" = "Europe & Central Asia",
                     "Latin America & Caribbean" = "Latin America & Caribbean",
                     "Middle East & North Africa" = "Middle East & North Africa",
                     "North America" = "North America",
                     "South Asia" = "South Asia",
                     "Sub-Saharan Africa" = "Sub-Saharan Africa",
                     "All" = "all"),
      selected = "all"
    )
  ),
  mainPanel(
    uiOutput("ggvis_ui"),
    ggvisOutput("ggvis")
  )
)

server <- function(input, output) {
  plot_year <- reactive(input$yr)
  plot_reg <- reactive(input$reg)
  
  df <- read.csv("data.csv")
  #df$id <- 1:nrow(df)
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- df[(df$country == x$country) & (df$yr == plot_year()), ]
    paste0("Country: ", row$country, "</br>", 
           "Population (M): ", format(row$pop/1000000, digits=2, nsmall=2), "</br>",
           "Life Expectancy: ", format(row$life, digits=2, nsmall=1), "</br>",
           "Fertility Rate: ", format(row$fert, digits=2, nsmall=1))
  }
  
  df$country <- as.factor(df$country)
  df$radius <- df$pop / 200000
  df_sub <- reactive({df %>% 
                        filter(yr==plot_year()) %>%
                        arrange(desc(pop)) %>%
                        mutate(opacity = 0.8) %>%
                        mutate(opacity = ifelse(Region==plot_reg() | plot_reg()=="all", 0.8, 0.2))})

  df_sub %>% 
    ggvis(~life, ~fert, fill = ~Region, key := ~country, stroke := 'black', strokeWidth := 0.5) %>%
    add_tooltip(all_values, "hover") %>%
    layer_points(size := ~radius, fillOpacity := ~opacity, strokeOpacity := ~opacity, size.hover := ~radius+100, 
                 strokeWidth.hover := 1.0, fillOpacity.hover := 1.0, strokeOpacity.hover := 1.0) %>%
    add_axis("x", title="Life Expectancy") %>%
    add_axis("y", title="Fertility Rate") %>%
    scale_numeric("x", domain = c(20, 85), nice = FALSE) %>%
    scale_numeric("y", domain = c(0, 9), nice = FALSE) %>%
    bind_shiny("ggvis", "ggvis_ui")
}

shinyApp(ui = ui, server = server)