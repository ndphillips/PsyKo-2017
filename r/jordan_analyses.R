# Jordan et al.
# Study 1

# -------------
# Load libraries
# --------------

library(yarrr)          # For R piratery
library(BayesFactor)    # Bayesian analyses
library(tidyverse)      # Data wrangling
library(tm)             # Natural language processing
library(wordcloud)      # Creating wordclouds
library(stringr)        # Text management functions


# -------------
# Load Data
# --------------

# Participant raw response data
s1 <- read.table("PsyKo_slidify/data/jordan_s1.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
s2 <- read.table("PsyKo_slidify/data/jordan_s2.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
s3 <- read.table("PsyKo_slidify/data/jordan_s3.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
s4 <- read.table("PsyKo_slidify/data/jordan_s4.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
s5 <- read.table("PsyKo_slidify/data/jordan_s5.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Open ended question data
s1.open <- read.table("PsyKo_slidify/data/jordan_s1_open.txt", sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE)
s2.open <- read.table("PsyKo_slidify/data/jordan_s2_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
s3.open <- read.table("PsyKo_slidify/data/jordan_s3_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
s4.open <- read.table("PsyKo_slidify/data/jordan_s4_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
s5.open <- read.table("PsyKo_slidify/data/jordan_s5_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)


# -------------
# Data cleaning
# --------------

s1 <- s1 %>% mutate(
  likeall = (likework + likeromantic + likeacademic + likedrugs) / 4,
  gender = ifelse(gender == 1, "male", "female"),
  goodinfo = ifelse(goodinfo == 1, "Good", "None"),
  condemn = ifelse(condemn == 1, "Target", "Other")) %>%
    filter(percCorrect == 1 & 
             is.finite(age))    # As recommended by the main study author in an email

s2 <- s2 %>% mutate(
  likeall = (likework + likeromantic + likeacademic + likedrugs) / 4,
  gender = ifelse(gender == 1, "male", "female"),
  direct = ifelse(direct == 1, "Direct", "Moral"),
  signal = ifelse(signal == 1, "Target", "Other")) %>%
  filter(percCorrect == 1)

s3 <- s3 %>% mutate(
  gender = ifelse(gender == 1, "male", "female"))

s4 <- s4 %>% mutate(
  gender = ifelse(gender == 1, "male", "female"))

s5 <- s5 %>% mutate(
  gender = ifelse(gender == 1, "male", "female"))
 

# -------------
# STUDY 1
# --------------

# Explore study 1

# Show me the first few rows of s1
head(s1)

# Show me the structure of s1
str(s1)

# Show me the entire dataset in a new window
View(s1)


# ----------------------
# Descriptive Statistics
# ----------------------

# What was the mean age? (According to the article, it's 31)
mean(s1$age, na.rm = TRUE)

# How many males and females?
table(s1$gender)

# What percent were male? (According to the article, it's 59%)
mean(s1$gender == "male")

# What percent of comprehension questions did the median participant get correct?
median(s1$percCorrect)

# How old was the oldest participant?
# Your turn!

# What was the mean overall likabilty rating (likeall?)
# Your turn!


# ----------------------
# Grouped Descriptive Statistics
# ----------------------

# What was the mean likeall rating for each group?

s1.likeall.summary <- s1 %>% 
              group_by(goodinfo, condemn) %>%
              summarise(
                        likeall.mean = mean(likeall, na.rm = TRUE),
                        likeall.sd = sd(likeall, na.rm = TRUE)
                        )

s1.likeall.summary

# ----------------------
# Plot
# ----------------------

plot(x = s1$age,
     y = s1$likeall,
     pch = 16, 
     col = gray(.5, .5),
     xlab = "Age",
     ylab = "Liking")

abline(lm(likeall ~ age, data = s1), 
       lty = 2, col = "steelblue", lwd = 2)


papaja::apa_barplot(data = s1, 
                    dv = "likeall", 
                    factors = c("goodinfo", "condemn"), 
                    id = "id", args_legend = list(x = "topleft"), ylim = c(1, 7))


pirateplot(likeall ~ condemn + goodinfo, 
           data = s1, 
           theme = 2, 
           ylim = c(1, 7), 
           cap.beans = TRUE)


# ----------------------
# Hypothesis tests
# ----------------------

# Do men give different liking ratings than women?
t.test(likeall ~ gender, data = s1)

# Is there a correlation between age and liking
cor.test(~ likeall + age, data = s1)

# ----------------------
# ANOVA
# ----------------------

# Conduct an ANOVA on likeall as a function of goodinfo and condemn
# According to the paper
#  Information main effect, F(1, 165) = 137.93, p < .001
#  Condemn main effect, F(1, 165) = 13.20, p < .001
#  Interaction: F(1, 165) = 8.51, p = .004

s1.aov <- aov(likeall ~ goodinfo * condemn, 
              data = s1)

summary(s1.aov)


# ----------------------
# Mixed effects modelling
# ----------------------

# Create long form of s1
s1.long <- s1 %>% select(id, goodinfo, condemn, likework, likeromantic, likeacademic, likedrugs) %>%
  gather(condition, like, 4:7)

s1.lmer <- lme4::lmer(formula = like ~ goodinfo * condemn + (1 | condition) + (1 | id), 
                      data = s1.long)

summary(s1.lmer)

# ----------------------
# Bayesian
# ----------------------

s1.bf <- BayesFactor::generalTestBF(formula = likeall ~ goodinfo * condemn,
                                       data = subset(s1, is.finite(likeall)))


plot(s1.bf)

s1.bf[1] / s1.bf[4]

s1.post <- posterior(s1.bf, iterations = 1e5, index = 4)
plot(s1.post)

# ----------------------
# Language Processing
# ----------------------

s1.corpus <- VCorpus(VectorSource(unlist(s1.open)))
s1.corpus <- tm_map(s1.corpus, content_transformer(tolower))
words_to_remove <- c("said","from","what","was","game", "you", "your", "the", "and", "but", "told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
s1.corpus <- tm_map(s1.corpus, removeWords, words_to_remove)

wordcloud(s1.corpus)
wordcloud(s1.corpus, min.freq = 10)







# -------------------
# STUDY 2
# -------------------

# Conduct an ANOVA on likeall as a function of goodinfo and condemn
papaja::apa_barplot(data = s2, 
                    dv = "likeall", 
                    factors = c("signal", "direct"), 
                    id = "id", args_legend = list(x = "topleft"), ylim = c(1, 7))


# Conduct an ANOVA on likeall as a function of goodinfo and condemn
pirateplot(likeall ~ direct + signal, 
           data = s2, 
           theme = 2, 
           ylim = c(1, 7), 
           cap.beans = TRUE)


# ----------------------
# ANOVA
# ----------------------

# Conduct an ANOVA on likeall as a function of goodinfo and condemn
s2.aov <- aov(likeall ~ direct * signal, 
              data = s2)

summary(s2.aov)


# Linear mi

# Create long form of s2
s2.long <- s2 %>% select(id, direct, signal, likework, likeromantic, likeacademic, likedrugs) %>%
  gather(condition, like, 4:7)

s2.lmer <- lme4::lmer(formula = like ~ direct * signal + (1 | condition) + (1 | id), 
                      data = s2.long)

summary(s2.lmer)


# ----------------------
# Bayesian
# ----------------------

s2.bf <- BayesFactor::generalTestBF(formula = likeall ~ direct * signal,
                                    data = subset(s2, is.finite(likeall)))


s2.post <- posterior(s2.bf, iterations = 1e5, index = 4)
plot(s2.post)


# ----------------------
# Language Processing
# ----------------------

s2.corpus <- VCorpus(VectorSource(iconv(str_trim(unlist(s2.open)), "latin1", "ASCII", sub = "")))
s2.corpus <- tm_map(s2.corpus, content_transformer(tolower))
words_to_remove <- c("said","from","what","was","game", "you", "your", "the", "and", "but", "told","over","more","other","have","last","with","this","that","such","when","been","says","will","also","where","why","would","today")
s2.corpus <- tm_map(s2.corpus, removeWords, words_to_remove)
wordcloud(s2.corpus)
wordcloud(s2.corpus, min.freq = 100)



# ------------
# STUDY 3
# ------------

# Conduct an ANOVA on likeall as a function of goodinfo and condemn
papaja::apa_barplot(data = s3, 
                    dv = "overall", 
                    factors = c("condition"), 
                    id = "id", args_legend = list(x = "topleft"), ylim = c(0, 50))


# Conduct an ANOVA on likeall as a function of goodinfo and condemn
pirateplot(overall ~ condition, 
           data = s3, 
           theme = 2, 
           cap.beans = TRUE)


# ----------------------
# ANOVA
# ----------------------

# Conduct an ANOVA on likeall as a function of goodinfo and condemn
s3.aov <- aov(overall ~ condition,
              data = s3)

summary(s3.aov)


# ----------------------
# Bayesian
# ----------------------

s3.bf <- BayesFactor::generalTestBF(formula = overall ~ condition,
                                    data = subset(s3, is.finite(overall)))


s3.post <- posterior(s3.bf, iterations = 1e5, index = 1)
plot(s3.post)




# -------------
# Simulation
# ------------



