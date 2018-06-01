# Intro to R
# Rick Scavetta
# 30 May 2018
# DPZ data analysis workshop

# Clear the workspace
rm(list = ls())

# Load packages:
library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)

# R Syntax:
n <- log2(8) # the log2 of 8
n # shortcut for print(n)

# A Simple Workflow: The PlantGrowth Dataset
PlantGrowth

# How many groups do we have?
# groups are called levels
nlevels(PlantGrowth$group)

# What are they?
levels(PlantGrowth$group)

# Descriptive statistics:
mean(PlantGrowth$weight) # global mean

# Group-wise desc stats:
# The apply family:
tapply(PlantGrowth$weight, PlantGrowth$group, mean)

# The dplyr way:
PlantGrowth %>% # shift-ctrl-m, "pipe operator", say "then..."
  group_by(group) %>%
  summarise(avg = mean(weight),
            stdev = sd(weight))

# Make plots:
# Add aesthetics: MAP data to visual scales
# (e.g. axes and colour)
# Add geometry: How the data will look

# base layer: Data and Aes
g <- ggplot(PlantGrowth, aes(x = group, y = weight))

# Box plot:
g +
  geom_boxplot()

# Dot plot:
g +
  geom_point(shape = 1, position = position_jitter(0.2))

# Group differences:
# Define a linear model:
plant.lm <- lm(weight ~ group, data = PlantGrowth)

# t-tests:
plant.lm
summary(plant.lm)

# 1-way ANOVA
anova(plant.lm)

# Aside: All pair-wise t-test with correction:
# TukeyHSD(aov(weight ~ group, data = PlantGrowth))

# Make a markdown file with
chickwts

# Element 2: Functions
# Everything that happens,
# is because of a function

# e.g. arithmetic operators
34 + 6
`+`(34, 6) # it's a function!

# BEDMAS - order of operations
# brackets, exp, div, mult, add, sub
2 - 3/4 # 1.25
(2 -3)/4 # -0.25

# Make some objects:
n <- 34
p <- 6

n + p # As above

# Calculations for four values:
# 3, 8, 9, 23
m <- 1.12
b <- -0.4

m * 3 + b
m * 8 + b
m * 9 + b
m * 23 + b

# Generic form of functions
# fun_name(fun_args)

# fun_args can be named or unnamed
# Call fun_args using names or position
# functions may have zero to many args

# e.g.
log2(8) # position
log2(x = 8) # named

# The same as:
log(8, base = 2) # position and name
log(8, 2) # both using position

# Terrible :/
log(2, x = 8)

# Some basic and common functions
# Combine/Concatenate unnamed arguments
xx <- c(3, 8, 9, 23)
myNames <- c("healthy", "tissue", "quantity")

# no args:
ls()

# Sequential numbers
seq(from = 1, to = 100, by = 7)
foo1 <- seq(1, 100, 7)

# use objects in functions
foo2 <- seq(1, n, p)

# The : operator for seq(x,y,1)
1:10
seq(1, 10, 1)

# Types of math functions:
# 1 - Transformation functions: mutate data
# N output == N input
scale(foo1) # z-scores
log2(foo1)
sqrt(foo1)

# 2 - Aggregration functions: summarise data
# Typically a single value as output
mean(foo1) # x-bar
sd(foo1) # s
length(foo1) # n
sum(foo1)
prod(foo1)

# Exercise 2, p30:
foo2 + 100 # Trans
foo2 + foo2 # Trans
sum(foo2) + foo2 # Trans (with an aggr)
1:3 + foo2 # Trans

#####################################
#### Fundamental Concept ############
#### Vector Recycling ###############
#####################################

1:4 + foo2

# Exercise 3, p 30:
m
b
xx
m * xx + b

# exercise 4, p 30:
m2 <- c(0, 1.12)
m2 * xx + b

0 * xx + b
1.12 * xx + b


# cheap way:
rep(m2, each = 4) * xx + b

# better way: reiterate over m2
# i.e. take first value, then the second, and so on.
equation <- function(x, m = 1.12) {
  m * x - 0.4
}

equation(xx)

# old ways:
lapply(m2, function(slope) xx * slope + b)
lapply(m2, function(slope) equation(xx, slope))

# new way: purrr package
# Map all elements of an object to a function
map(m2, ~ equation(xx, .))

# the same as:
equation(xx, 0)
equation(xx, 1.12)

# get function definitions:
methods(t.test)
getAnywhere(t.test.default)

# Element 3: Objects
# Anything that exists is an object

# Vectors - 1 dimensional, homogenous
# e.g.
foo1
foo2

# User-defined atomic vector types:
# Logical: 0/1, F/T, FALSE/TRUE (aka binary, boolean)
# Integer: 0, 1, 2, ...
# Double: 0.004, 1.4, 3.14, ...
# Character: names, id, ...

# Numeric - either integer or double

typeof(foo1)
typeof(myNames)

foo3 <- c("Liver", "Brian", "Testes",
          "Muscle", "Intestine","Heart")

foo4 <- c(T, F, F, T, T, F)
typeof(foo4)

# 1 Dimension, each number is an "element"
length(foo1) # 15 elements

# Homogenous: only 1 data type allowed!
test <- c(1:10, "bob")
test

# corece types:
as.integer(test)

# Major problem # 1: Wrong type!
# Solution: Coercion

# List - 1 Dimension, heterogenous
# Data frame (a specialised class of type list)
# where 2 dimensions, heterogenous and
# each element is a vector of the same length
# Rows = observations (individuals)
# Columns = variables (measurements)

# examples of lists:
typeof(g) # our plot is a list
typeof(plant.lm) # our lm is a list

# the class determines how other
# functions deal with an object:
class(g)
class(plant.lm)

# making a data frame from scratch:
foo.df <- data.frame(foo4, foo3, foo2)
typeof(foo.df)
class(foo.df)

# attributes, see p38
attributes(foo.df)
names(foo.df)
row.names(foo.df)

# The data
foo.df

# Replace names with another vector:
names(foo.df) <- myNames
foo.df

# Any named element can be called with $ notation:
foo.df$healthy

# Explore data frames:
summary(foo.df)
str(foo.df)
glimpse(foo.df) # from dplyr

dim(foo.df) # nrow, ncol
nrow(foo.df)
ncol(foo.df)

# Don't use length() on 2D objects! (i.e. data frames)
length(foo1) # the number of elements
length(foo.df) # the number of elements (i.e. columns)

# Element 4: Logical Expressions
# Relational operators: Ask YES/NO questions
# output is ALWAYS!!!: TRUE or FALSE - logical vector
# Questions:
# Equivalency ==
# Non-equivalency !=
# >, <, >=,<=
# !x, Negation, where x is logical

# don't confuse:
# <- assign,
# <=, == relational
# = arguments in a function

n
p
n > p
n < p
foo4
!foo4

# Logical operators: Combine T/F questions
# AND - &
# OR - | "pipe operator",
# don't confuse | with %>% used in dplyr for combining commands
# WITHIN - %in%

# Apply to logical data: use subset()
# All healthy
subset(foo.df, foo.df$healthy == TRUE)
subset(foo.df, healthy == TRUE)
subset(foo.df, healthy)

# All unhealthy
subset(foo.df, healthy != TRUE)
subset(foo.df, healthy == FALSE)
subset(foo.df, !healthy)

# Subsetting numeric data (double or integer):
# below quantity of 10
subset(foo.df, quantity < 10)

# Ranges: Between 10-20
subset(foo.df, quantity > 10 & quantity < 20)
foo.df$quantity > 10
foo.df$quantity < 20

# Meaningless
subset(foo.df, quantity > 10 | quantity < 20)

# Extremes: Outside 10-20
subset(foo.df, quantity < 10 | quantity > 20)

# Impossible:
subset(foo.df, quantity < 10 & quantity > 20)

# Subsetting characters:
# NO PATTERN MATCHING!
# Heart samples:
subset(foo.df, tissue == "Heart")

# 2 or more: Liver and Heart
# Cheap and easy:
subset(foo.df, tissue == "Heart" | tissue == "Liver")

# Easier way: TERRIBLE - NEVER do this!
subset(foo.df, tissue == c("Heart", "Liver"))
subset(foo.df, tissue == c("Liver", "Heart"))
# vector recycling is always used!
foo.df$tissue

# The proper way: %in% WITHIN operator
subset(foo.df, tissue %in% c("Heart", "Liver"))
subset(foo.df, tissue %in% c("Liver", "Heart"))

# Element 5: Indexing
# So far: find by equations (e.g. ==, &)
# Here: use equations as positions
# []

# Vectors:
foo1
foo1[6] # 6th position
foo1[p] # the pth position (i.e. 6th)
foo1[3:p] # 3rd to pth
foo1[p:length(foo1)] # pth (6th) to the last value (15th)

# use logical vectors!
# i.e. output from a relational operator
foo1[foo1 < 50]

# This is subset! but more flexible :)
subset(foo1, foo1 < 50)
# because subset is just a short cut to []

# Data frames with []:
# 2 dimensions: use [ <row> , <col> ]
foo.df[3,] # 3rd row, ALL columns
foo.df[ 3:p, "quantity"] # 3rd to pth row, only quantity column
foo.df[ 3:p, 3] # 3rd to pth row, only quantity column
foo.df[ 3:p , -3 ] # 3rd to pth row, exclude quantity

# Tissue of low quantity (less than 10) observations:
foo.df[ foo.df$quantity < 10 , "tissue" ]

# like subset:
subset(foo.df, foo.df$quantity < 10, select =  tissue)

# Combining values:
foo.df[foo.df$tissue == "Heart",] # only Heart samples

foo.df[foo.df$tissue == "Heart",][,3] # 3rd col, only Heart samples
foo.df[foo.df$tissue == "Heart", 3] # 3rd col, only Heart samples

# Not possible
foo.df[foo.df$tissue == "Heart"] # ERROR, no comma!
# no comma is a short cut for columns!
foo.df[3] # 3rd column, as DF
foo.df[,3] # 3rd column, as vector

# Major problem # 2: Wrong structure!
# Solution: rearrange or coerce structure


index <- foo.df$tissue == "Heart"
foo.df[index,]

# Get a specific column

foo.df[,2] # column 2
foo.df[,"tissue"] # by name

# Element 8: Factor Variables (with "levels")
# AKA: Categorical, discrete, qualitative
# with "groups" - small and known number

# e.g.
PlantGrowth$group

typeof(PlantGrowth$group) # integer, with associated levels
str(PlantGrowth)
class(PlantGrowth$group)

# some issues:
foo3 # character
foo.df$tissue # factor

# Converting:
test.df <- data.frame(test)
test.df$test # converted to a factor
# convert to integers:
as.integer(test.df$test) # from a factor
as.integer(test) # from the original character

# Element 9: Tidy Data
# Some play data

# work on a new data set:
PlayData <- data.frame(type = rep(c("A", "B"), each = 2),
                       time = 1:2,
                       height = seq(10, 40, 10),
                       width = seq(50, 80, 10))

# To rearrange our data to Tidy, use tidyr:
# four arguments:
# 1 - data,
# 2 - name of the output key,
# 3 - name of the output value
# 4 - the ID or the MEASURE variables
PlayData.t <- gather(PlayData, key, value, -c(type, time)) # using ID
gather(PlayData, key, value, c(height, width)) # using MEASURE

# Element 10: Using Split-Apply-Combine
# Group in different ways: first see SILAC example for dplyr

# Scenario 1, group by type and key:
PlayData.t %>%
  group_by(type, key) %>%
  summarise(Average = mean(value))

# Scenario 2, group by time and key:
PlayData.t %>%
  group_by(time, key) %>%
  summarise(Average = mean(value))

# Scenario 3, group by type and time:
PlayData.t %>%
  group_by(type, time) %>%
  summarise(Average = mean(value))


