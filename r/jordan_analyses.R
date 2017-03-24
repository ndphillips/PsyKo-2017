# Replicating the analysis from Jordan et al.
# Jordan, J. J., Sommers, R., Bloom, P., & Rand, D. G. (2017). Why Do We Hate Hypocrites? Evidence for a Theory of False Signaling.
# http://journals.sagepub.com/doi/full/10.1177/0956797616685771
#
# Original data obtained from: https://osf.io/pszjz/
#
# Abstract
# Why do people judge hypocrites, who condemn immoral behaviors that they in fact engage in, so negatively? 
# We propose that hypocrites are disliked because their condemnation sends a false signal about their personal conduct, 
# deceptively suggesting that they behave morally. We show that verbal condemnation signals moral goodness (Study 1) and 
# does so even more convincingly than directly stating that one behaves morally (Study 2). We then demonstrate that 
# people judge hypocrites negatively — even more negatively than people who directly make false statements about their 
# morality (Study 3). Finally, we show that “honest” hypocrites — who avoid false signaling by admitting to committing 
# the condemned transgression — are not perceived negatively even though their actions contradict their stated values (Study 4). 
# Critically, the same is not true of hypocrites who engage in false signaling but admit to unrelated transgressions (Study 5). 
# Together, our results support a false-signaling theory of hypocrisy.

# -------------------------------------------------
# Load libraries
#  If they aren't installed on your computer, you must install them with install.packages().
#  E.g.; install.packages("yarrr")
# --------------------------------------------------

# install.packages(yarrr)          # For R piratery
# install.packages(BayesFactor)    # Bayesian analyses
# install.packages(tidyverse)      # Data wrangling
# install.packages(tm)             # Natural language processing
# install.packages(wordcloud)      # Creating wordclouds
# install.packages(stringr)        # Text management functions
# install.packages(rprojroot)      # For working directory management
# install.packages(papaja)         # For apa analyses

library(yarrr)          # For R piratery
library(BayesFactor)    # Bayesian analyses
library(tidyverse)      # Data wrangling
library(tm)             # Natural language processing
library(wordcloud)      # Creating wordclouds
library(stringr)        # Text management functions
library(rprojroot)      # For working directory management
library(papaja)         # For apa analyses

# -------------------------------------------------
# Set working directory
# This will set the working directory to the directory containing an .Rproj file
#  (If you're not working in a project, just delete this)
# --------------------------------------------------

setwd(rprojroot::is_rstudio_project$find_file())

# -------------------------------------------------
# Load Data
# --------------------------------------------------

online <- TRUE

# If online, get the data from github
if(online) {
  
  # Participant raw response data
  s1 <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s1.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  s2 <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s2.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  s3 <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s3.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  s4 <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s4.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  s5 <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s5.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
  
  # Open ended question data
  s1.open <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s1_open.txt", sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE)
  s2.open <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s2_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
  s3.open <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s3_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
  s4.open <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s4_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
  s5.open <- read.table("https://raw.githubusercontent.com/ndphillips/PsyKo-2017/master/data/jordan_s5_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
  
}

# If not online, read the data from a folder called data
if(online == FALSE) {

# Participant raw response data
s1 <- read.table("data/jordan_s1.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
s2 <- read.table("data/jordan_s2.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
s3 <- read.table("data/jordan_s3.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
s4 <- read.table("data/jordan_s4.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
s5 <- read.table("data/jordan_s5.txt", sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Open ended question data
s1.open <- read.table("data/jordan_s1_open.txt", sep = "\t", header = TRUE, quote = "", stringsAsFactors = FALSE)
s2.open <- read.table("data/jordan_s2_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
s3.open <- read.table("data/jordan_s3_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
s4.open <- read.table("data/jordan_s4_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)
s5.open <- read.table("data/jordan_s5_open.txt", sep = "\t", header = TRUE,  quote = "",stringsAsFactors = FALSE)

}

# -------------------------------------------------
# Data cleaning
# --------------------------------------------------

# Change gender from 1 and 2 to "male" and "female"
# Change goodinfo and condemn to character

s1 <- s1 %>% mutate(
    gender = ifelse(gender == 1, "male", "female"),
    goodinfo = ifelse(goodinfo == 1, "Good", "None"),
    condemn = ifelse(condemn == 1, "Target", "Other")) %>%
    filter(percCorrect == 1 & 
           is.finite(age))    # As recommended by the main study author in an email

s2 <- s2 %>% mutate(
  gender = ifelse(gender == 1, "male", "female"),
  direct = ifelse(direct == 1, "Direct", "Moral"),
  signal = ifelse(signal == 1, "Target", "Other")) %>%
  filter(percCorrect == 1)

# -------------------------------------------------
# STUDY 1
# --------------------------------------------------

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

# How did participants do on the comprehension questions?
table(s1$percCorrect)

# How old was the oldest participant?
# Your turn!

# What was the mean sumtotal rating
# Your turn!


# ----------------------
# Grouped Descriptive Statistics
# ----------------------

# What was the mean and sd sumtotal rating for each group?

s1.sumtotal.summary <- s1 %>%                               # From the study 1 data...
    group_by(goodinfo, condemn) %>%                         # Group results by goodinfo and condemn
    summarise(
              sumtotal.mean = mean(sumtotal, na.rm = TRUE), #  mean sumtotal
              sumtotal.sd = sd(sumtotal, na.rm = TRUE),     #  sd of sumtotal
              N = n()                                       #  N = number of observations
              )                                      

# Print the results
s1.sumtotal.summary


# YOUR TURN!
# What was the mean and sd of age for each group?





# ----------------------
# Plotting
# ----------------------


# Histogram of sumtotal scores

hist(x = s1$sumtotal, 
     main = "sumtotal", 
     yaxt = "n", 
     ylab = "")

# Scatterplot of age and sumtotal
plot(x = s1$age,
     y = s1$sumtotal,
     pch = 16, 
     col = gray(.5, .5),
     xlab = "Age",
     ylab = "Liking")

abline(lm(sumtotal ~ age, data = s1), 
       lty = 2, col = "steelblue", lwd = 2)

# Barplot of sumtotal by goodinfo and condemn

papaja::apa_barplot(data = s1, 
                    dv = "sumtotal",                       # DV is sumtotal
                    factors = c("goodinfo", "condemn"),    # group by goodinfo and condemn
                    id = "id",                             # subjects are defined by id
                    args_legend = list(x = "topleft"),     # Put legend on top left
                    ylim = c(1, 7))


# YOUR TURN!!!
# Create a barplot showing the distribution of sumdo by goodinfo and condemn







# Pirateplot of sumtotal by condemn and goodinfo
pirateplot(sumdo ~ condemn + goodinfo, 
           data = s1, 
           theme = 3, 
           ylim = c(1, 7), 
           cap.beans = TRUE)





# ----------------------
# Hypothesis tests
# ----------------------

# Do men give different liking ratings than women?
t.test(sumtotal ~ gender, 
       data = s1)

# Is there a correlation between age and liking
cor.test(~ sumtotal + age, 
         data = s1)

# ----------------------
# ANOVA
# ----------------------

# Conduct an ANOVA on sumtotal as a function of goodinfo and condemn
# According to the paper
#  Information main effect, F(1, 165) = 137.93, p < .001
#  Condemn main effect, F(1, 165) = 13.20, p < .001
#  Interaction: F(1, 165) = 8.51, p = .004

s1.aov <- aov(sumtotal ~ goodinfo * condemn, 
              data = s1)

summary(s1.aov)

# ----------------------
# Mixed effects modelling
# ----------------------

# Create long form of s1
s1.long <- s1 %>% select(id, goodinfo, condemn, 
                         dowork, doromantic, doacademic, dodrugs,
                         trustswork, trustsromantic, trustsacademic, trustsdrugs,
                         trustgwork, trustgromantic, trustgacademic, trustgdrugs,
                         likework, likeromantic, likeacademic, likedrugs
                         ) %>%
gather(condition, rating, 4:19)

s1.long <- s1.long %>%
  mutate(
    
    # Extract vignette and domain from condition
    
    vignette = ifelse(grepl("work", condition), "work",
               ifelse(grepl("romantic", condition), "romantic",
               ifelse(grepl("academic", condition), "academic", "drugs"))),
    domain =   ifelse(grepl("do", condition), "nottransgress",
               ifelse(grepl("trusts", condition), "domainspecifictrust",
               ifelse(grepl("trustg", condition), "domaingentrust", "likeability"))),
    
    # Convert IVs to factors
    
    goodinfo = factor(goodinfo),
    condemn = factor(condemn),
    condition = factor(condition),
    vignette = factor(vignette),
    domain = factor(domain),
    id = factor(id)
  )


# Linear mixed effects model with random intercents for vignette, domain, and id
s1.lmer <- lme4::lmer(formula = rating ~ goodinfo * condemn + (1 | vignette) + (1 | domain) + (1 | id), 
                      data = s1.long)


summary(s1.lmer)


# ----------------------
# Bayesian
# ----------------------

# Bayesian ANOVA on sumtotal
s1.bf <- BayesFactor::generalTestBF(formula = sumtotal ~ goodinfo * condemn,
                                       data = subset(s1, is.finite(sumtotal)))


plot(s1.bf)


# Bayesian model with random intercepts for condition and vignette
s1.longbf <- BayesFactor::generalTestBF(formula = rating ~ goodinfo * condemn + condition + vignette,
                                    data = subset(s1.long, is.finite(rating)),
                                    whichRandom = c("condition", "vignette"))


s1.longbf


# ----------------------
# Language Processing
# ----------------------

s1.corpus <- VCorpus(VectorSource(unlist(s1.open)))
s1.corpus <- tm_map(s1.corpus, content_transformer(tolower))
words_to_remove <- c("said","from","what","was","game", "you", "your", "the", "and", "but", "told","over","more","other","have",
                     "last","with","this","that","such","when","been","says","will","also","where","why","would","today")
s1.corpus <- tm_map(s1.corpus, removeWords, words_to_remove)

wordcloud(s1.corpus)
wordcloud(s1.corpus, min.freq = 10)



# ----------------------
# Simulation
# If there were only 25 participants per condition, would we still find an interaction?
# ----------------------

nsim <- 1000
n.per.cond <- 25
s1.int.values <- rep(NA, nsim)

for(i in 1:nsim) {
  
  s1.sample <- s1[sample(1:nrow(s1), size = n.per.cond * 4),]
  
  s1.sample.aov <- aov(sumtotal ~ goodinfo * condemn, 
                       data = s1.sample)
  
  s1.sample.int.b <- s1.sample.aov$coefficients[4]
  
  s1.int.values[i] <- s1.sample.int.b
}

par(mfrow = c(1, 2))

hist(s1.int.values, col = gray(.7), border = "white")
abline(v = aov(sumtotal ~ goodinfo * condemn, 
               data = s1)$coefficients[4],
       lwd = 3, lty = 2, col = "green")

plot(x = seq(-.3, 1, .1),
     y = sapply(seq(-.3, 1, .1), FUN = function(x) {mean(s1.int.values > x)}),
     type = "b", 
     ylab = "Cumulative Probability", 
     xlab = "p(b.int > x)",
     ylim = c(0, 1),
     main = paste("p(b.int > 0) =",  mean(s1.int.values > 0)))

grid()
segments(0, 0, 0, mean(s1.int.values > 0), lty = 2)
points(0, mean(s1.int.values > 0), pch = 16, col = "green")


# -------------------
# STUDY 2
# -------------------

# Conduct an ANOVA on sumtotal as a function of goodinfo and condemn
papaja::apa_barplot(data = s2, 
                    dv = "sumtotal", 
                    factors = c("signal", "direct"), 
                    id = "id", args_legend = list(x = "topleft"), ylim = c(1, 7))


# Conduct an ANOVA on sumtotal as a function of goodinfo and condemn
pirateplot(sumtotal ~ direct + signal, 
           data = s2, 
           theme = 2, 
           ylim = c(1, 7), 
           cap.beans = TRUE)


# ----------------------
# ANOVA
# ----------------------

# Conduct an ANOVA on sumtotal as a function of goodinfo and condemn
s2.aov <- aov(sumtotal ~ direct * signal, 
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

s2.bf <- BayesFactor::generalTestBF(formula = sumtotal ~ direct * signal,
                                    data = subset(s2, is.finite(sumtotal)))


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


# ----------------------
# Simulation
# If there were only 25 participants per condition, would we still find an interaction?
# ----------------------

nsim <- 1000
n.per.cond <- 25
s2.int.values <- rep(NA, nsim)

for(i in 1:nsim) {
  
  s2.sample <- s2[sample(1:nrow(s2), size = n.participants * 4),]
  
  s2.sample.aov <- aov(sumtotal ~ direct * signal, 
                       data = s2.sample)
  
  s2.sample.int.b <- s2.sample.aov$coefficients[4]
  
  s2.int.values[i] <- s2.sample.int.b
  
}

par(mfrow = c(1, 2))

hist(interaction.values, col = gray(.7), border = "white")
abline(v = aov(sumtotal ~ direct * signal, 
               data = s2)$coefficients[4],
       lwd = 3, lty = 2, col = "green")

plot(x = seq(-.3, 1, .1),
     y = sapply(seq(-.3, 1, .1), FUN = function(x) {mean(s2.int.values > x)}),
     type = "b", ylab = "Cumulative Probability", xlab = "p(b.int > 0)")

grid()
segments(0, 0, 0, mean(s2.int.values > 0), lty = 2)
points(0, mean(s2.int.values > 0), pch = 16, col = "green")


