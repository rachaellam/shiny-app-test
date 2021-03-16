#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
### Workspace Set-Up ###
# install.packages("opendatatoronto")
# install.packages("tidyverse")
# install.packages("devtools")
# install.packages("dplyr")
# install.packages("reshape2") #to reshape data later down
# install.packages("ggplot2")
# install.packages("here")

library(opendatatoronto)
library(tidyverse)
library(devtools)
library(dplyr)
library(reshape2)
library(ggplot2)
library(here)


### Finding the dataset from Toronto Open Data ###
all_data <- 
    opendatatoronto::search_packages("crime") %>%
    filter(title == "Neighbourhood Crime Rates") %>%
    select(id) %>%
    opendatatoronto::list_package_resources() %>%
    get_resource()

### Saving the dataset ###
write_csv(all_data, "raw_data.csv")
raw_data <- read.csv("raw_data.csv")

### Grabbing specific columns ###
toronto_crime_all <- raw_data %>%
    as_tibble() %>%
    select(Neighbourhood,
           Assault_2014, Assault_2015, Assault_2016, Assault_2017, Assault_2018, Assault_2019,
           AutoTheft_2014, AutoTheft_2015, AutoTheft_2016, AutoTheft_2017, AutoTheft_2018, AutoTheft_2019,
           BreakandEnter_2014, BreakandEnter_2015, BreakandEnter_2016, BreakandEnter_2017, BreakandEnter_2018, BreakandEnter_2019,
           Homicide_2014, Homicide_2015, Homicide_2016, Homicide_2017, Homicide_2018, Homicide_2019,
           TheftOver_2014, TheftOver_2015, TheftOver_2016, TheftOver_2017, TheftOver_2018, TheftOver_2019,
           Robbery_2014, Robbery_2015, Robbery_2016, Robbery_2017, Robbery_2018, Robbery_2019)

### Finding the top 10 neighbourhoods (out of 140) with the most crimes ###
toronto_crime_top <- toronto_crime_all %>%
    melt(id = c("Neighbourhood"), # using `reshape2` to change dataframe to long-format data for plotting. kept Neighbourhood as the ID
         variable.name = "Year",
         value.name = "Number_of_Crimes") %>%
    group_by(Neighbourhood) %>%
    summarise(Number_of_Crimes = sum(Number_of_Crimes)) %>% # summing crime by neighbourhood 
    slice_max(Number_of_Crimes, n = 10) # selecting top 10 neighbourhoods with the most crime

### Compare all sum crimes across neighbourhoods ###
toronto_crime_yearly <- toronto_crime_all %>%
    melt(id = c("Neighbourhood"), # using `reshape2` to change dataframe to long-format data for plotting. kept Neighbourhood as the ID
         variable.name = "Year",
         value.name = "Number_of_Crimes")

toronto_crime_yearly$Year <- as.character(toronto_crime_yearly$Year)

toronto_crime_yearly <- toronto_crime_yearly %>% # this code was produced with help from https://community.rstudio.com/t/replace-entire-string-by-one-specific-word/17302/6
    mutate(Year = case_when(
        str_detect(Year, "Assault_2014") ~ "2014",
        str_detect(Year, "Assault_2015") ~ "2015",
        str_detect(Year, "Assault_2016") ~ "2016",
        str_detect(Year, "Assault_2017") ~ "2017",
        str_detect(Year, "Assault_2018") ~ "2018",
        str_detect(Year, "Assault_2019") ~ "2019",
        str_detect(Year, "AutoTheft_2014") ~ "2014",
        str_detect(Year, "AutoTheft_2015") ~ "2015",
        str_detect(Year, "AutoTheft_2016") ~ "2016",
        str_detect(Year, "AutoTheft_2017") ~ "2017",
        str_detect(Year, "AutoTheft_2018") ~ "2018",
        str_detect(Year, "AutoTheft_2019") ~ "2019",
        str_detect(Year, "BreakandEnter_2014") ~ "2014",
        str_detect(Year, "BreakandEnter_2015") ~ "2015",
        str_detect(Year, "BreakandEnter_2016") ~ "2016",
        str_detect(Year, "BreakandEnter_2017") ~ "2017",
        str_detect(Year, "BreakandEnter_2018") ~ "2018",
        str_detect(Year, "BreakandEnter_2019") ~ "2019",
        str_detect(Year, "Homicide_2014") ~ "2014",
        str_detect(Year, "Homicide_2015") ~ "2015",
        str_detect(Year, "Homicide_2016") ~ "2016",
        str_detect(Year, "Homicide_2017") ~ "2017",
        str_detect(Year, "Homicide_2018") ~ "2018",
        str_detect(Year, "Homicide_2019") ~ "2019",
        str_detect(Year, "TheftOver_2014") ~ "2014",
        str_detect(Year, "TheftOver_2015") ~ "2015",
        str_detect(Year, "TheftOver_2016") ~ "2016",
        str_detect(Year, "TheftOver_2017") ~ "2017",
        str_detect(Year, "TheftOver_2018") ~ "2018",
        str_detect(Year, "TheftOver_2019") ~ "2019",
        str_detect(Year, "Robbery_2014") ~ "2014",
        str_detect(Year, "Robbery_2015") ~ "2015",
        str_detect(Year, "Robbery_2016") ~ "2016",
        str_detect(Year, "Robbery_2017") ~ "2017",
        str_detect(Year, "Robbery_2018") ~ "2018",
        str_detect(Year, "Robbery_2019") ~ "2019",
        TRUE ~ Year
    )) %>%
    group_by(Neighbourhood, Year) %>%
    summarise(Number_of_Crimes = sum(Number_of_Crimes)) %>%
    filter(Neighbourhood %in% c("Waterfront Communities-The Island", "Bay Street Corridor", "Church-Yonge Corridor", 
                                "West Humber-Clairville", "Moss Park", "York University Heights",
                                "Downsview-Roding-CFB", "Kensington-Chinatown", "Woburn", "West Hill")) # grabbing the population of the top 10 neighbourhoods with the most crime

toronto_crime_yearly <- toronto_crime_yearly %>%
    group_by(Year)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Toronto Crime Yearly Data"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Year",
                        "Starting year:",
                        min = 2013,
                        max = 2019,
                        value = 2013)
        ),
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$distPlot <- renderPlot({
        toronto_crime_yearly %>%
            filter(Year > input$Year) %>%
            ggplot(aes(x = Year, y = Number_of_Crimes, color = Neighbourhood)) +
            geom_smooth(aes(group = Neighbourhood), se = FALSE) +
            scale_y_continuous(limits = c(0, NA)) +
            labs(color = "Neighbourhoods",
                 x = "Year",
                 y = "Number of Robberies",
                 title = "Toronto Crime Rate",
                 subtitle = "Number of robberies per year") +
            theme_minimal()
        
    })
}
# Run the application 
shinyApp(ui = ui, server = server)