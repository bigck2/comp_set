
library(shiny)
library(tidyverse)
library(readxl)
library(stringr)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Analyze nearby apartments"),
   
   # Sidebar with some user inputs 
   sidebarLayout(
      sidebarPanel(
         fileInput(inputId = "my_file_input", 
                   label = "Select Axio CPS report")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         dataTableOutput("my_clean_data")
      )
      
   )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
 
  
  axio <- reactive({
    
    # Note this first part required some research
    # https://stackoverflow.com/questions/30624201/read-excel-in-a-shiny-app
    inFile <- input$my_file_input
    if(is.null(inFile)){return(NULL)}
    
    file.rename(inFile$datapath,
                paste0(inFile$datapath, ".xlsx"))
    
    # Note it required some reseach here too,
    # Had to manually enter in 10 times for "text" col_type
    # in non-Shiny script it recycled the argument just fine
    axio <- read_excel(paste0(inFile$datapath, ".xlsx"), 
               sheet = "Map - Comps", col_names = FALSE,
               col_types = rep("text", 10))
    
    
    # Find the first row of the actual data and subset
    start_row <- which(axio[,1] == "#")
    axio <- axio[start_row:nrow(axio),]
    rm(start_row)
    
    # The subject property has a NA for number, we will 
    # change this to "0"
    axio[2, 1] <- "0"
    
    # The subject property also has a NA distance value,
    # weill change this to 0 for now also
    axio[2, 10] <- "0"
    
    # We will use row 1 for column names, but first 
    # we want to replace the # symbol with 'id'
    axio[1, 1] <- "id"
    
    # Let's get the column names, and subset
    my_names <- str_to_lower(axio[1,])
    my_names <- str_replace_all(my_names, pattern = " ", 
                                replacement = "_")
    names(axio) <- my_names
    axio <- axio[-1,]
    
    
    # The distance column is a mix of numeric and string
    # Let's clean this up
    axio$distance <- str_replace_all(axio$distance, 
                                     pattern = " miles", 
                                     replacement = "")
    
    # let's convert the numeric columns to the correct type
    # and we'll also drop some precision decimal places
    axio <- axio %>%
      mutate(year = as.numeric(year),
             units = as.numeric(units),
             aus = round(as.numeric(aus), 0),
             erpu = round(as.numeric(erpu), 0),
             erpsf = round(as.numeric(erpsf), 2),
             occ = round(as.numeric(occ), 4),
             distance = as.numeric(distance))
    
    # These names are easier for me to remember:
    axio <- axio %>%
      rename(avg_sf = aus,
             eff_rent = erpu,
             eff_rent_sf = erpsf)
    
    
    
    # Manipulate and Analyze --------------------------------------------------
    
    # Let's start by getting subject prop info
    sub_year <- axio[[1, "year"]]
    sub_units <- axio[[1, "units"]]
    sub_avg_sf <- axio[[1, "avg_sf"]]
    sub_eff_rent <- axio[[1, "eff_rent"]]
    sub_eff_rent_sf <- axio[[1, "eff_rent_sf"]]
    sub_occ <- axio[[1, "occ"]]
    
    # Note: I was going to remove these subject variables, BUT
    # they may be useful in making plots
    
    
    
    # Now we can add these columns to the axio tble
    # and calculate the variance for each property
    # relative to the subject
    
    # We may also want the absolute variance
    
    axio <- axio %>%
      mutate(sub_year = sub_year,
             sub_units = sub_units,
             sub_avg_sf = sub_avg_sf,
             sub_eff_rent = sub_eff_rent,
             sub_eff_rent_sf = sub_eff_rent_sf, 
             sub_occ = sub_occ,
             var_year = year - sub_year,
             var_units = units - sub_units,
             var_avg_sf = avg_sf - sub_avg_sf,
             var_eff_rent = eff_rent - sub_eff_rent,
             var_eff_rent_sf = eff_rent_sf - sub_eff_rent_sf,
             var_occ = occ - sub_occ,
             abs_var_year = abs(var_year),
             abs_var_units = abs(var_units),
             abs_var_avg_sf = abs(var_avg_sf),
             abs_var_eff_rent = abs(var_eff_rent),
             abs_var_eff_rent_sf = abs(var_eff_rent_sf),
             abs_var_occ = abs(var_occ)
      )
    
    
    # We now have lot's of variables, for graphing 
    # it may be helpful to have a factor variable
    axio$subject <- factor(if_else(axio$id == 0, 
                                   "Subject Property", 
                                   "Comp"))
    
    axio
    
    
    })
  
  
  output$my_clean_data <- renderDataTable({
    datatable(axio(),  
    options = list(paging = TRUE, 
                   lengthMenu = list(c(2, 5, 10), c('2', '5', '10')),
                   pageLength = 5))  
    })
  
  
  
  
}




# Run the application 
shinyApp(ui = ui, server = server)

