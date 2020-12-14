# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : Boss Effect at MIDAS shop


# R-SCRIPT OF CODE IN CLIENT DELIVAREABLE
# PROJECT DESCRIPTION

# USEFUL PACKAGES
library(tidyverse)
library(causact)
library(greta)

# LOAD DATA
carsDF = readr::read_csv("carsFixed.csv")
# VIEW QUICK SUMMARY OF DATA
carsDF %>%
  group_by(shopID) %>%
  summarize(numberOfObservations = n(),
            numberOfBossVisits = sum(boss))

# CREATE GRAPHICAL MODEL
graph = dag_create() %>%
  dag_node("Cars Fixed","K",
           data = carsDF$carsFixed,
           rhs = poisson(lambda)) %>%
  dag_node("Exp Cars Fixed - Shop Level","lambda",
           rhs = exp(alpha_shop + beta_shop * x),
           child = "K") %>%
  dag_node("Intercept - Shop Level","alpha_shop",
           rhs = normal(alpha,sigma_alpha),
           child = "lambda") %>%
  dag_node("Boss Effect - Shop Level","beta_shop",
           rhs = normal(beta,sigma_beta),
           child = "lambda") %>%
  dag_node("Intercept - Midas Level","alpha",
           rhs = normal(3,2),
           child = "alpha_shop") %>%
  dag_node("Std Dev - Midas Level","sigma_alpha",
           rhs = uniform(0,2),
           child = "alpha_shop") %>%
  dag_node("Exp Boss Effect - Midas Level","beta",
           rhs = normal(0,1),
           child = "beta_shop") %>%
  dag_node("Std Dev Boss Effect","sigma_beta",
           rhs = uniform(0,2),
           child = "beta_shop") %>%
  dag_node("Boss Present","x",
           data = carsDF$boss,
           child = "lambda") %>%
  dag_plate("Observation","i",
            nodeLabels = c("K","lambda","x")) %>%
  dag_plate("Shop","j",
            nodeLabels = c("beta_shop","alpha_shop"),
            data = carsDF$shopID,
            addDataNode = TRUE)

# DISPLAY GRAPHICAL MODEL
graph %>% dag_render()

# GET POSTERIOR DISTRIBUTION
drawsDF = graph %>% dag_greta()
