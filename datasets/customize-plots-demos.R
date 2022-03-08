# before you start running this make sure:
# - your current working directory is set to the datasets/ folder
#   in the D2M2022 repository
# - you have installed the package "jtools"

library(tidyverse)

# read in the M&M data and make it long
mmdata <- read_csv("MM Data.csv")
mmdata.long <- read_csv("MM Data.csv") %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange",
                        "Yellow", "Brown"),
               names_to = "Color", values_to = "Number")

## each geom has some options for aesthetic mappings, e.g.,
ggplot(mmdata.long, aes(Color, Number)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, color = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, alpha = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, shape = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, size = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, fill = Color)) +
  geom_jitter()
ggplot(mmdata.long, aes(Color, Number, fill = Color)) +
  geom_jitter(shape = 24)
ggplot(mmdata.long, aes(Color, Number, fill = Color, stroke = Number)) +
  geom_jitter(shape = 24)

## what is happening with inheritance?
# let's see with the mpg dataset (free with tidyverse)
ggplot(mpg, aes(x = cyl, y = hwy,
                fill = class, color = class)) +
  geom_jitter(shape = 24) +
  geom_smooth(method = "lm")

ggplot(mpg, aes(x = cyl, y = hwy,
                fill = class)) +
  geom_jitter(shape = 24) +
  geom_smooth(aes(color = class), method = "lm")

## be careful when you're setting the same aes twice or
## using more than one dataset
ggplot(mpg, aes(x = cyl, y = hwy)) +
  geom_jitter(aes(color = manufacturer), shape = 24) +
  geom_smooth(aes(color = class), method = "lm")

ggplot(mpg, aes(x = cyl, y = hwy, fill = class, color = class)) +
  geom_jitter(shape = 24) +
  geom_smooth(data = filter(mpg, class != "2seater"),
              aes(x = cyl, y = hwy), method = "lm")

## let's dive into those more complex figs from last time
adultdata <- read_csv("adult.data.csv")
adultdata <- adultdata %>%
  mutate(
    age.bins = case_when(
      age <= 35 ~ "<= 35",
      age > 35 & age <= 50 ~ "36-50",
      age > 50 & age <= 65 ~ "51-65",
      age > 65 ~ "66+",
      TRUE ~ "uh oh"
    ),
    education.bins = case_when(
      education %in% c("Preschool", "1st-4th",
                       "5th-6th", "7th-8th", "9th", "10th", "11th") ~ 'pre-HS',
      education %in% c("12th", "HS-grad", "Some-college") ~ 'HS',
      education %in% c("Assoc-acdm", "Assoc-voc", "Prof-school") ~ 'Assoc',
      education %in% c("Bachelors", "Masters", "Doctorate") ~ 'BA+',
      TRUE ~ "uh oh"
    ),
    education.bins = fct_relevel(
      education.bins, c('pre-HS', 'HS', 'Assoc', 'BA+')),
    income = case_when(
      grepl("<", income) ~ "<=50K",
      grepl(">", income) ~ ">50K",
      TRUE ~ "uh oh"
    ),
    race = as_factor(race),
    race = factor(race, labels = c("White", "Black", "AsiPacIsl",
                                   "AmerIndFirsNat", "Other")))

## ex. income category across demographic groups
ggplot(adultdata, aes(education.bins, fill = income)) +
  geom_bar(position = "fill")
ggplot(adultdata, aes(age.bins, fill = income)) +
  geom_bar(position = "fill")
ggplot(adultdata, aes(sex, fill = income)) +
  geom_bar(position = "fill")
ggplot(adultdata, aes(race, fill = income)) +
  geom_bar(position = "fill")

# by using faceting, we can break out the dataset into a matrix
ggplot(adultdata, aes(race, fill = income)) +
  geom_bar(position = "fill") +
  facet_grid(education.bins ~ sex)

# by using labs() we can customize the axis labels
ggplot(adultdata, aes(race, fill = income)) +
  geom_bar(position = "fill") +
  facet_grid(education.bins ~ sex) +
  labs(x = "Race", y = "Proportion",
       title = "Income across demographic groups",
       subtitle = "(Sex, education group, and race)")

# by using guides() we can customize the legend titles
ggplot(adultdata, aes(race, fill = income)) +
  geom_bar(position = "fill") +
  facet_grid(education.bins ~ sex) +
  labs(x = "Race", y = "Proportion",
       title = "Income across demographic groups",
       subtitle = "(Sex, education group, and race)") +
  guides(fill = guide_legend("Income group"))

# by using theme() we can adjust the position of the axis text
ggplot(adultdata, aes(race, fill = income)) +
  geom_bar(position = "fill") +
  facet_grid(education.bins ~ sex) +
  labs(x = "Race", y = "Proportion",
       title = "Income across demographic groups",
       subtitle = "(Sex, education group, and race)") +
  guides(fill = guide_legend("Income group")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# by using scale_fill_manual() we can specify different colors for
# the fill (note that if you want to change color—not fill—you need
# scale_color_manual(), and you can think the same for other aes
# mappings)
ggplot(adultdata, aes(race, fill = income)) +
  geom_bar(position = "fill") +
  facet_grid(education.bins ~ sex) +
  labs(x = "Race", y = "Proportion",
       title = "Income across demographic groups",
       subtitle = "(Sex, education group, and race)") +
  guides(fill = guide_legend("Income group")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("goldenrod", "lightblue"))


## ex. age sampling across age and edu bins
ggplot(adultdata, aes(education.bins, age)) +
  geom_jitter() +
  geom_boxplot()

# facet by race
ggplot(adultdata, aes(education.bins, age)) +
  geom_jitter() +
  geom_boxplot() +
  facet_grid(. ~ race)

# make boxplots prettier with jitter (no outliers, transparent)
# make jitter less distracting by making it more transparent
ggplot(adultdata, aes(education.bins, age)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, ) +
  facet_grid(. ~ race)

## add color by education bin in each facet (redundant but useful)
ggplot(adultdata, aes(education.bins, age, color = education.bins)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0) +
  facet_grid(. ~ race)

## change boxplot color to black across all plots for better
# visibility
ggplot(adultdata, aes(education.bins, age, color = education.bins)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0, color = "black") +
  facet_grid(. ~ race)

## as before, update the axis labels
ggplot(adultdata, aes(education.bins, age, color = education.bins)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0, color = "black") +
  facet_grid(. ~ race) +
  labs(x = "Education type", y = "Age (years)")

## since color is redundant, we should remove the legend
ggplot(adultdata, aes(education.bins, age, color = education.bins)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0, color = "black") +
  facet_grid(. ~ race) +
  labs(x = "Education type", y = "Age (years)") +
  guides(color = "none")

## one more change? use the apa theme!
library(jtools)
ggplot(adultdata, aes(education.bins, age, color = education.bins)) +
  geom_jitter(alpha = 0.1) +
  geom_boxplot(outlier.shape = NA, alpha = 0.0, color = "black") +
  facet_grid(. ~ race) +
  labs(x = "Education type", y = "Age (years)") +
  guides(color = "none") +
  theme_apa()

## M & M data
mmdata <- read_csv("MM Data.csv")
mmdata.long <- read_csv("MM Data.csv") %>%
  pivot_longer(cols = c("Red", "Green", "Blue", "Orange",
                        "Yellow", "Brown"),
               names_to = "Color", values_to = "Number")
mmdata.long.by.bag <- mmdata.long %>%
  group_by(Bag, Weight) %>%
  summarize(
    total.mms = sum(Number)
  )

## adding text-based datapoints and annotations
ggplot(mmdata.long.by.bag, aes(Weight, total.mms)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Weight (oz)", y = "# M&M candies") +
  theme_apa()

ggplot(mmdata.long.by.bag, aes(Weight, total.mms, label = Bag)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Weight (oz)", y = "# M&M candies") +
  geom_text() +
  theme_apa()

ggplot(mmdata.long.by.bag, aes(Weight, total.mms, label = Bag)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Weight (oz)", y = "# M&M candies") +
  geom_text(nudge_y = 0.5) +
  theme_apa()

ggplot(mmdata.long.by.bag, aes(Weight, total.mms, label = Bag)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Weight (oz)", y = "# M&M candies") +
  geom_text(nudge_y = 0.5) +
  annotate("text", label = "OOPS...",
           x = 53, y = 45, size = 8, colour = "red") +
  theme_apa()

# let's rescale the axes so we can see things in a more zoomed out way
ggplot(mmdata.long.by.bag, aes(Weight, total.mms, label = Bag)) +
  geom_smooth(method = "lm") +
  geom_point() +
  labs(x = "Weight (oz)", y = "# M&M candies") +
  geom_text(vjust = 0, nudge_y = 0.5) +
  annotate(
    "text", label = "HELLO!!",
    x = 53, y = 45, size = 8, colour = "red"
  ) +
  scale_x_continuous(limits = c(45,55), breaks = 45:55) +
  scale_y_continuous(limits = c(40,70), breaks = seq(40,70,10)) +
  theme_apa()
