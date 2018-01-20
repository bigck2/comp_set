library(tidyverse)
library(readxl)


# Read in data, select the proper "Map - All" worksheet,
# and read all data in as text
axio <- read_excel(choose.files(), 
                   sheet = "Map - All", 
                   col_types = "text")

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





