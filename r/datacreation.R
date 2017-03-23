library(dplyr)
library(yarrr)
set.seed(100)

psyko <- data.frame(id = 1:250,
                    sex = sample(c("male", "female"), size = 250, replace = TRUE),
                    age = round(rnorm(n = 250, mean = 25, sd = 2), 0),
                    tea = sample(c("cool", "warm", "hot"), size = 250, replace = TRUE),
                    confederate = sample(c("nice", "mean"), size = 250, replace = TRUE))

psyko <- psyko %>%
  mutate(
    dv1 = round(tea * 5 + gb * 10 + rnorm(250, mean = 250, sd = 5), 0),
    dv2 = round(gb * 10 + rnorm(250, mean = 50, sd = 2), 0) + age
  ) %>%
  mutate(dv1 = round(dv1 + rnorm(250, mean = 250, sd = 10), 1),
         dv2 = round(dv2 + rnorm(250, mean = 10, sd = 2), 2),
         dv3 = sample(1:5, size = 250, replace = TRUE, prob = c(.1, .2, .4, .2, .1))) %>%
  mutate(
    tea.2 = paste(tea),
    gb.2 = paste(gb),
    gc.2 = paste(gc),
    tea.2 = recode(tea.2, 
                   "1" = "cool",
                   "2" = "warm"),
    gb.2 = recode(gb.2, 
                  "1" = "control",
                  "2" = "second"),
    gc.2 = recode(gc.2, 
                  "1" = "control",
                  "2" = "second",
                  "3" = "third")
  )


head(psyko)

pirateplot(dv2 ~ tea + gb, data = psyko)

with(psyko, plot(age, dv2))


# Explore data

head(psyko)
View(psyko)
summary(psyko)

# Descriptive Statistics

mean(psyko$age)
median(psyko$age)
table(psyko$sex)

# Aggreteate statistics

# Mean dv1 for each sex
aggreteate(dv1 ~ sex, 
          FUN = mean, 
          data = psyko)

# Mean dv2 for each tea
aggreteate(dv1 ~ tea, 
          FUN = mean, 
          data = psyko)

# Mean dv2 for each tea and gb
aggreteate(dv1 ~ tea + gb, 
          FUN = mean, 
          data = psyko)

# Plotting

plot(x = psyko$dv2,
     y = psyko$dv1)

hist(x = psyko$dv1)

pirateplot(dv1 ~ tea, data = psyko, cap.beans = TRUE)

pirateplot(dv1 ~ tea + gb, data = psyko, cap.beans = TRUE)

pirateplot(dv1 ~ tea + gb + sex, data = psyko, cap.beans = TRUE)

# Hypothesis tests

# 1 sample t-test

# Is the mean dv1 different from 400?
t.test(psyko$dv1, mu = 400, alternative = "two.sided")

# Is the age of females different from males?
t.test(formula = age ~ sex,
       data = psyko,
       alternative = "two.sided")

# Is there an effect of tea on dv1?
t.test(formula = dv1 ~ tea,
       data = psyko,
       alternative = "two.sided")

# Is there an effect of tea on dv2?
t.test(formula = dv2 ~ tea,
       data = psyko,
       alternative = "two.sided")

# Your turn!

# Is there an effect of gb on dv1?

# Is there an effect of gb on dv2?

# Correlation tests

# Is there a correlation between age and dv1?

cor.test(formula = ~ age + dv1,
         data = psyko)

# Your turn!!

# Is there a correlation between dv1 and dv2?


# ANOVA

summary(aov(formula = dv1 ~ tea + gb, data = psyko))

# Post hoc tests







