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

# Exercise 3
subset(protein.df, Ratio.H.M.Sig < 0.05)

# Exercise 4: HM ratio beyond [-2,2]
subset(protein.df, Ratio.H.M > 2 | Ratio.H.M < -2)



