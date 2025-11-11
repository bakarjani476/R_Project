library(dplyr)
data("mtcars")

# Convert the dataframe into tibble and assign it to variable df. Here <- is the assignment operator
df <- as_tibble(mtcars)

# Now let's do data analysis on our dataset.

df %>%
  filter(mpg>20)%>%
  select(mpg,cyl,hp,wt)%>%
  group_by(cyl)%>%
  summarize(avg_hp=mean(hp))

df %>%
  filter(hp>100)%>%
  select(carb,hp,cyl)%>%
  group_by(carb)%>%
  summarize(avg_hp=mean(hp))
# It tells the horsepower increases with number of carborators

# Now we will create some new columns using mutate
df%>%
  mutate(power_to_wt = hp/wt)

# Reassign to df to permanently store the new added column in the dataframe
df <- df%>%
  mutate(power_to_wt = hp/wt)

head(df)
# To see the newly created column:
df %>% 
  select(hp, wt, power_to_wt)

# Create a performance_level column:

# For this purpose we will use case_when() function which is used to create categorical variables
# based on numeric or logical conditions. It acts like a multiple If-else statement.
# We typically use it inside a mutate function to create a column.

df %>%
  mutate(performance_level = case_when(
    hp < 100 ~ "Low",
    hp < 200 ~ "Medium",
    TRUE ~ "High"
  ))

# Here TRUE works as else statement.

df <- df %>%
  mutate(performance_level = case_when(
    hp < 100 ~ "Low",
    hp < 200 ~ "Medium",
    TRUE ~ "High"
  ))
head(df)

df%>%
  select(hp, performance_level)

# Summarization

# Find average mpg and hp by number of cylinders (cyl).

# Find car with max mpg per transmission type (am).
df%>%
  select(cyl,hp,mpg)%>%
  group_by(cyl)%>%
  summarise(
    avg_mpg=mean(mpg),
    avg_hp=mean(hp)
    )

df%>%
  select(mpg,am)%>%
  group_by(am)%>%
  summarise(max_mpg_per_am = round(max(mpg)))%>%
  arrange(desc(max_mpg_per_am))

# Now let's combine all the above codes in a single pipline
df%>%
  filter(mpg>20 & hp>100)%>%
  select(mpg, hp, am,cyl, carb,wt)%>%
  mutate(power_to_wt = hp/wt,
         performance_level = case_when(
           hp < 100 ~ "Low",
           hp < 200 ~ "Medium",
           TRUE ~ "High" ))%>%
  group_by(cyl,am, carb)%>%
  summarise(avg_mpg=mean(mpg),
            avg_hp=mean(hp),
            max_mpg_per_am = round(max(mpg)))%>%
  arrange(desc(max_mpg_per_am))

head(df)

# Now let's create a new small datafram car_categories where we will mention
# which category do each car belong to(i.e sports car)

car_category <- data.frame(
  car = c("Mazda RX4", "Datsun 710", "Hornet 4 Drive", "Valiant", "Ferrari Dino"),
  category = c("Sports", "Sports", "Sedan", "Sedan", "Sports")
)

# mtcars does not have a car column by default in which it would have the car
# names.The rownames are car names. So we need to turn them into a column
# for joining.

head(rownames(mtcars)) # shows the car names

df2 <- mtcars%>%
  tibble:: rownames_to_column(var = "car")  # means use the rownames_to_column() function from the tibble package — even if other packages also have a function with the same name.

# now df2 has a column "car" we can join on

df2_joined <- df2%>%
  left_join(car_category, by = "car")

# here df2 that is coming from the coming would be the left table and car
# category would be the right table. so it will select all the rows/cars
# of first table(df2) and only the matching rows/cars from the right
# table (car category), and if there is not any match in the right table 
# with the left table, there will be NA values.
  
df2_joined%>%
  select(car,category)

# Use across() function to find summary stats (mean, sd) for multiple numeric columns by cyl.

df2_joined%>%
  group_by(cyl)%>%
  summarise(
    across(c(mpg,hp, wt),   # columns we want to summarize
           list(mean=mean, sd=sd),  # functions to apply
           .names = "{.col}_{.fn}")   # naming pattern for new columns
    
           )

# So far, we are done with dplyr.From now onward, we will work on data.table library
library(data.table)

DT <- as.data.table(mtcars, keep.rownames = "car")

# `as.data.table(mtcars)`

# This converts your mtcars data frame into a data.table — a high-performance data structure in R (from the data.table package).

# It behaves similarly to a data frame but allows faster filtering, grouping, and joining using a different syntax.

# `keep.rownames = "car" `

# By default, rownames (like "Mazda RX4", "Datsun 710", etc.) are not kept when converting to a data.table.

# keep.rownames = "car"

# tells R to save the rownames as a regular column called "car" in the resulting data.table.


DT[mpg>25, cyl==4]

# Compute avg mpg by cylinder:
DT[, .(avg_mpg = mean(mpg)), by=cyl]

# Add new columns(in place)
DT[, power_to_wt := hp/wt]

# multiple summaries
DT[, .(avg_mpg=mean(mpg), avg_hp = mean(hp)), by = .(am, cyl)]

# sorting
DT[order(-mpg)]


# Visualization using ggplot2

library(ggplot2)

# Scatter plot
ggplot(mtcars, aes(wt, mpg, color=factor(cyl))) + geom_point(size = 3)+ theme_minimal()

# Now to add the regression line on it, we will have to add another layer
ggplot(mtcars, aes(wt, mpg, color=factor(cyl))) + geom_point(size = 3) + geom_smooth(method ="lm", se = FALSE)



# compare mpg by transmission type(am)
# We’ll compare the distribution of miles per gallon (mpg) between manual (1) and automatic (0) transmissions.

ggplot(mtcars, aes(x = factor(am), y = mpg, fill = factor(am))) + geom_boxplot()+
  labs(
    title = "mpg by Transmission type",
    x = "Transmission (0 = Automatic, 1 = Manual)",
    y = "Miles per Gallon"
  ) + theme_minimal()

# here factor(am) converts the numeric variable am into a factor (categorical) variable.

# Bar chart for avg_hp by cylinder count

avg_hp <- mtcars%>%
  group_by(cyl)%>%
  summarise(avg_hp = mean(hp))

ggplot(avg_hp, aes(x = factor(cyl), y = avg_hp, fill = factor(cyl) )) + 
  geom_col() + labs(
    title = "Avg horsepower by Cylinder count",
    x = "Number of Cylinders",
    y = "Avg Horsepower"
  ) + theme_minimal()

# Histogram (Distribution of car wts)

ggplot(mtcars, aes(x = wt)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Car Weights",
    x = "Weight (1000 lbs)",
    y = "Count"
  ) +
  theme_minimal()

# Line plot(Ranked fuel efficiency)

mtcars_ranked <- mtcars %>%
  arrange(desc(mpg)) %>%
  mutate(rank = row_number())

ggplot(mtcars_ranked, aes(x = rank, y = mpg)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 2, color = "darkgreen") +
  labs(
    title = "Fuel Efficiency Ranked by MPG",
    x = "Rank (1 = Highest MPG)",
    y = "Miles per Gallon"
  ) +
  theme_minimal()

# We can combine all these multiple plots with patchwork or cowplot libraries

# Now we will save all the plots in output folder


p1 <- ggplot(mtcars, aes(wt, mpg, color=factor(cyl))) + geom_point(size = 3) + geom_smooth(method ="lm", se = FALSE)

ggsave("output/wt by mpg.png", plot = p1, width = 7, height = 5)

p2 <- ggplot(mtcars, aes(x = factor(am), y = mpg, fill = factor(am))) + geom_boxplot()+
  labs(
    title = "mpg by Transmission type",
    x = "Transmission (0 = Automatic, 1 = Manual)",
    y = "Miles per Gallon"
  ) + theme_minimal()

ggsave("output/mpg by transmission.png", plot = p2, width = 7, height = 5)

p3 <- ggplot(avg_hp, aes(x = factor(cyl), y = avg_hp, fill = factor(cyl) )) + 
  geom_col() + labs(
    title = "Avg horsepower by Cylinder count",
    x = "Number of Cylinders",
    y = "Avg Horsepower"
  ) + theme_minimal()

ggsave("output/avg hp by cyl count.png", plot = p3, width = 7, height = 5)

p4 <- ggplot(mtcars, aes(x = wt)) +
  geom_histogram(binwidth = 0.5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Car Weights",
    x = "Weight (1000 lbs)",
    y = "Count"
  ) +
  theme_minimal()

ggsave("output/distribution of car wt.png", plot = p4, width = 7, height = 5)

p5 <- ggplot(mtcars_ranked, aes(x = rank, y = mpg)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(size = 2, color = "darkgreen") +
  labs(
    title = "Fuel Efficiency Ranked by MPG",
    x = "Rank (1 = Highest MPG)",
    y = "Miles per Gallon"
  ) +
  theme_minimal()


ggsave("output/fuel efficiency ranked by mpg.png", plot = p5, width = 7, height=5)

