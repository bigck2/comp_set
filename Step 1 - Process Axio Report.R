# Load Packages -----------------------------------------------------------
library(tidyverse)
library(readxl)



# Import and Clean --------------------------------------------------------

# Read in data, select the proper "Map - Comps" worksheet,
# and read all data in as text
axio <- read_excel(choose.files(), 
                   sheet = "Map - Comps", 
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



# Manipulate and Analyze --------------------------------------------------

# Let's start by getting subject prop info
sub_year <- axio[[1, "year"]]
sub_units <- axio[[1, "units"]]
sub_avg_sf <- axio[[1, "avg_sf"]]
sub_eff_rent <- axio[[1, "eff_rent"]]
sub_eff_rent_sf <- axio[[1, "eff_rent_sf"]]
sub_occ <- axio[[1, "occ"]]


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





# Plots -------------------------------------------------------------------

# This one looks kind of busy with so many properties
ggplot(axio, aes(x = reorder(property_name, eff_rent),
                 y = eff_rent,
                 fill = subject)) +
  geom_bar(stat = 'identity') +
  coord_flip()


ggplot(axio, aes(x = eff_rent, fill = subject)) + 
  geom_histogram() +
  scale_x_continuous(labels = scales::dollar) +
  labs(x = "Effective Rent", 
       y = "Property Counts", 
       title = "Axiometrics: Effective Rent Distribution") +
  guides(fill = FALSE)


# This really isn't that informative 
# because almost all properties are 90-100% Occupied
ggplot(axio, aes(x = occ, fill = subject)) + 
  geom_histogram() +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "Occupancy", 
       y = "Property Counts", 
       title = "Axiometrics: Occupancy Distribution") +
  guides(fill = FALSE)













