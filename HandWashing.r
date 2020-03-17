
# Load in the tidyverse package
# .... YOUR CODE FOR TASK 1 ....
library (tidyverse)
# Read datasets/yearly_deaths_by_clinic.csv into yearly
yearly <- read_csv("datasets/yearly_deaths_by_clinic.csv")

# Print out yearly
yearly
# .... YOUR CODE FOR TASK 1 ....

# Adding a new column to yearly with proportion of deaths per no. births
# .... YOUR CODE FOR TASK 1 ....
yearly <- yearly %>%
mutate(proportion_deaths=(deaths/births))
# Print out yearly
yearly

# Setting the size of plots in this notebook
options(repr.plot.width=7, repr.plot.height=4)

# Plot yearly proportion of deaths at the two clinics
ggplot(yearly, aes(x = year, y = proportion_deaths , colour = clinic)) +
  geom_line()
# .... YOUR CODE FOR TASK 3 ....

# Read datasets/monthly_deaths.csv into monthly
monthly <- read_csv("datasets/monthly_deaths.csv")

# Adding a new column with proportion of deaths per no. births
# .... YOUR CODE FOR TASK 4 ....
monthly <- monthly %>% 
mutate( proportion_deaths = deaths/births)
# Print out the first rows in monthly
head(monthly)
# .... YOUR CODE FOR TASK 4 ....

# Plot monthly proportion of deaths
ggplot(monthly,aes(x=date,y=proportion_deaths))+
geom_line()

# ... YOUR CODE FOR TASK 5 ...

# From this date handwashing was made mandatory
handwashing_start = as.Date('1847-06-01')

# Add a TRUE/FALSE column to monthly called handwashing_started
# .... YOUR CODE FOR TASK 6 ....
monthly <- monthly %>%
    mutate(handwashing_started = date >= handwashing_start )
# Plot monthly proportion of deaths before and after handwashing
ggplot(monthly , aes(x=date , y=proportion_deaths , color = handwashing_started ))+
    geom_line()
# .... YOUR CODE FOR TASK 6 ....

# Calculating the mean proportion of deaths 
# before and after handwashing.

monthly_summary <- monthly %>% 
  group_by(handwashing_started) %>%
  summarise(mean_proportion_deaths = mean(proportion_deaths))

# Printing out the summary.
monthly_summary

# Calculating a 95% Confidence intrerval using t.test 
test_result <- t.test( proportion_deaths ~ handwashing_started, data = monthly)
test_result

# The data Semmelweis collected points to that:
doctors_should_wash_their_hands <- FALSE
