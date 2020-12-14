# Pie Chart of July 2013 Car Sales in the United States
slices <- c(296539, 264488, 96195, 182, 184462, 283130,66749,67284,24155) 
lbls <- c("Midsize", "Small Car", "Luxury Car", "Large Car", "Pickup", "Cross-over", "Minivan", "Midsize SUV", "Large SUV")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie(slices,labels = lbls, col=rainbow(length(lbls)),
    main="July 2013 Car Sales by Segment (% of total units sold)") 