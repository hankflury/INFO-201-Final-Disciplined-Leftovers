library(ggplot2)
library(dplyr)

#Read in the earthquake dataset
new <- read.csv("pnsn_event_export_20190502.csv", header=T)

# Remove the last 3 characters of the Time.Local for the new eqks - these are either PDT/PST
new$Time.Local <- as.character(new$Time.Local)
numchar <- unique(nchar(new$Time.Local))
last.three.chars <- substr(new$Time.Local, numchar-3, numchar)
new$Time.Local <- substr(new$Time.Local, 1, numchar-4)

# Now change to Date string
new$Date.Clean <- as.Date(new$Time.Local, format = "%Y/%m/%d %X")

# Make a date+time variable that's in Date format. Split out the time characters
# from Time.Local and add them to Date.Clean
times <- substr(new$Time.Local, numchar-11, numchar)
new$Date.Time <- paste(new$Date.Clean, times, sep=" ")

# Reorder and rename the columns and sort it by time. Then write out this new catalog
cat.names.new <- c("Evid", "Date.Clean", "Date.Time", "Lat", "Lon", "Depth.Km", "Magnitude", "Distance.From")
cat.new <- new[,cat.names.new]
names(cat.new) <- c("id", "date", "time", "lat", "lon", "depth", "mag", "distance")

cat.new.sorted <- cat.new[order(cat.new$time, decreasing=F),]

cat.net.sorted <- cat.new.sorted[cat.new.sorted$date >= "1970-01-01", ]

#Export the cleaned earthquake dataset
write.csv(cat.new.sorted, "pnw-cat-recent-processed.csv", row.names=F)