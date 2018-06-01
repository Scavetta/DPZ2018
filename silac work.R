# SILAC Analysis (Stable Isotope Labelling in Active Cells)
# Rick Scavetta
# 31 May 2018
# Case study for workshop

# clear workspace
rm(list = ls())

# Load packages
# Includes dplyr, ggplot2, purrr and other packages
library(tidyverse)

# Read in the data:
protein.df <- read.delim("Protein.txt")

# Examine the data:
summary(protein.df)
str(protein.df)
glimpse(protein.df)

# print to screen -- make a tibble
class(protein.df) # Before tibble
protein.df <- as_tibble(protein.df)
class(protein.df) # After 
protein.df # better print to screen

# Transformations
# log10 of intensities:
protein.df$Intensity.H <-log10(protein.df$Intensity.H)
protein.df$Intensity.M <-log10(protein.df$Intensity.M)
protein.df$Intensity.L <-log10(protein.df$Intensity.L)

# Add log10 intensities:
protein.df$Intensity.H.M <- protein.df$Intensity.H + protein.df$Intensity.M
protein.df$Intensity.M.L <- protein.df$Intensity.M + protein.df$Intensity.L

# log2 of ratios: HM & ML
protein.df$Ratio.H.M <- log2(protein.df$Ratio.H.M)
protein.df$Ratio.M.L <- log2(protein.df$Ratio.M.L)

# Exercises, p 58
nrow(subset(protein.df, Contaminant == "+"))
sum(protein.df$Contaminant == "+")

##### For logical vectors, 
# TRUE == T == 1
# FALSE == F == 0
##### We can do math on logical vectors

# What proportion?
sum(protein.df$Contaminant == "+")/length(protein.df$Contaminant)
sum(protein.df$Contaminant == "+")/nrow(protein.df)

table(protein.df$Contaminant)/nrow(protein.df)

# Remove contaminants:
protein.df <- subset(protein.df, Contaminant != "+")

# Exercise 2, p59
subset(protein.df, Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE"))
# Exercise 1, p64: just rows
protein.df[protein.df$Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE") , ]
# also, get just columns of interest: Uniprot, Ratio.M.L, Ratio.H.M
protein.df[  ,  c("Uniprot", "Ratio.M.L", "Ratio.H.M")  ]
# Do both rows and columns:
protein.df[protein.df$Uniprot %in% paste0(c("GOGA7", "PSA6", "S10AB"), "_MOUSE") , 
           c("Uniprot", "Ratio.M.L", "Ratio.H.M") ]

# Exercise 3, p59
subset(protein.df, Ratio.H.M.Sig < 0.05)
# Exercise 2, p64: use []
protein.df[ protein.df$Ratio.H.M.Sig < 0.05 , ]

# Exercise 4, p59: HM ratio beyond [-2,2] - NAs are removed
subset(protein.df, Ratio.H.M > 2 | Ratio.H.M < -2)
# Exercise 3, p64: In this case the NAs are included
protein.df[protein.df$Ratio.H.M > 2 | protein.df$Ratio.H.M < -2 , ]

# Introduction to dplyr
# Start from the beginning:
rm(list = ls())
protein.df <- read.delim("Protein.txt", stringsAsFactors = FALSE)

# Make a tibble:
protein.df <- as_tibble(protein.df)

# 3 Main Components to dplyr
# 1 - %>% pipe operator (shift-ctrl-m)
1:10 %>% 
  mean()
# the same as
mean(1:10)

# 2 - The five verbs (The "grammar" of data analysis)
# 2a - filter (simiar to subset(), or [])
# 2b - arrange (by default lowest to highest "ascending")
# 2c - select (specific columns)
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(Ratio.H.M) %>% 
  select(Uniprot, Ratio.H.M, Ratio.H.M.Sig)

# 2c+ - Helper functions
# proteins with the 20 higest HM ratios (and other ratios)
protein.df %>% 
  filter(Contaminant != "+") %>% 
  arrange(desc(Ratio.H.M)) %>% 
  select(Uniprot, starts_with("Rat"), -ends_with("Sig")) %>% 
  .[1:20,]

# 2d - mutate, for transformations
# All ratios at once, in situ:
protein.df %>% 
  filter(Contaminant != "+") %>% 
  mutate_at(vars(starts_with("Rat"), -ends_with("Sig")), log2)

# Individual columns, add to data frame:
protein.df %>% 
  filter(Contaminant != "+") %>% 
  mutate(Ratio.H.M.log2 = log2(Ratio.H.M),
         Ratio.M.L.log2 = log2(Ratio.M.L))

# Go back to the main tutorial
# 2e - summarise, for aggregrations
# 3 - group_by, an adverb for splitting
