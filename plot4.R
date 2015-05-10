
# plot4
# a function to construct the fourth (and final) plot of the assignment

plot4 <- function() {
  
  # Get the data file (if it's not already present)
  if (file.exists("household_power_consumption.txt") == FALSE) {
    print ("Data not found.  Downloading...");
    download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",
                  dest = "household_power_consumption.zip");
    unzip("household_power_consumption.zip");
  }
  
  # We'll store the data as a data.table so make sure the library is loaded up
  require(data.table);
  
  # Read the text file as a data.table with all values in character fields
  consumption_full <- fread("household_power_consumption.txt", sep=";",
                           colClasses=rep("character", 9), header=TRUE);
  
  # Keep only the readings for February 1 and 2, 2007
  consumption <- consumption_full[Date %like% "^[12]/2/2007$"];
  
  # free up memory and get rid of the full data set
  remove(consumption_full);
  
  # Convert columns 3 - 9 to numbers
  numColNames <- names(consumption)[3:9];
  suppressWarnings(consumption[, (numColNames) := lapply(.SD, as.numeric), .SDcols = numColNames]);
  
  # Convert the first two columns to date and time (respectively)
  consumption[, Date:=as.IDate(Date, format="%d/%m/%Y")];
  consumption[, Time:=as.ITime(Time, format="%H:%M:%S")];
  
  # ---------------------------------------------------------------------------
  # We have the data we need!  Now let's get to plotting!

  png("plot4.png", width=480, height=480);

  # Set up to make 4 plots in one (with 2 rows and 2 columns)
  par(mfrow = c(2, 2), mar = c(6, 5, 2, 1), oma = c(0, 0, 2, 0))
  
  # Make sub-plot #1
  plot(x=as.POSIXct(consumption$Time, consumption$Date),
       y=consumption$Global_active_power,
       type="l",
       xlab="",
       ylab="Global Active Power");

  # Make sub-plot #2
  plot(x=as.POSIXct(consumption$Time, consumption$Date),
       y=consumption$Voltage,
       type="l",
       xlab="datetime",
       ylab="Voltage");

  # Make sub-plot #3
  plot(x=as.POSIXct(consumption$Time, consumption$Date),
       y=consumption$Sub_metering_1,
       type="l",
       xlab="",
       ylab="Energy sub metering");
  
  lines(x=as.POSIXct(consumption$Time, consumption$Date),
        y=consumption$Sub_metering_2,
        col="red");
  
  lines(x=as.POSIXct(consumption$Time, consumption$Date),
        y=consumption$Sub_metering_3,
        col="blue");

  legend(x = "topright",
         legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
         col=c("black", "red", "blue"),
         lty=1,
         bty="n");

  
  # Make sub-plot #4
  plot(x=as.POSIXct(consumption$Time, consumption$Date),
       y=consumption$Global_reactive_power,
       type="l",
       xlab="datetime",
       ylab="Global_reactive_power");

  dev.off();
}