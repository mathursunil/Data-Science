# Author : Sunil Mathur
# This code is part of the course curriculum for MS - Data Science at University of Delaware
# BUAD621 - Decision Analytics and Visualization

# Program Objective : get the course files from the server

install.packages("usethis")
# choose location for files by setting your working directory
# to a folder you create with your OS ... like c:\analytics
# use menus of RStudio:
# Session --> Set Working Directory --> Choose Directory(i.e. analytics)
courseURL = "https://github.com/flyaflya/buad621-2020/archive/master.zip"

usethis::use_course(url = courseURL, destdir = getwd())