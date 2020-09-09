
library(shiny)
library(tidyverse)
library(scales)
library(janitor)

key_crop_yields  = readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')


yields <- key_crop_yields %>%
  clean_names() %>%
  rename_all(str_remove, "_tonnes.*")

yields_tidy <- yields %>%
  pivot_longer(wheat:bananas, names_to = "crop", values_to = "yield") %>%
  filter(!is.na(yield))%>%
  mutate(crop = str_replace_all(crop, "_", " "),
         crop = str_to_title(crop)) 



# Define UI for application of a webpage
#User Interface
ui <- fluidPage(
   
   # Application title
   titlePanel("Top 10 Countries of Crop Yield Across Years"),
   
   # Sidebar with a slider input for number of bins 
         selectInput('crop', 'Select Crop',
                      choices = c('Wheat','Rice','Maize', 'Potatoes','Barley','Bananas','Beans','Peas','Soybeans')
                         ),
         sliderInput('year','Select Year', value = 1961, min = 1961, max = 2018, sep = ""),
             
      # Show a plot of the generated distribution
     plotOutput('plot_top_10_country')
 
   )


# Define server logic required to draw a histogram
server <- function(input, output) {
   
    output$plot_top_10_country <- renderPlot({
      # subset data to get top 10 country for a select crop
      top_10 <- yields_tidy %>%
                 filter(crop == input$crop) %>%
                 filter(year == input$year) %>% 
                 top_n(10, yield) 
      
      # draw the plot
      ggplot(data = top_10, aes(entity, yield)) +
        geom_col(fill = "#0000FF")
         })
}

# Run the application 
shinyApp(ui = ui, server = server)

