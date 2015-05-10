
# plot2
# a function to construct the second plot of the assignment

plot2 <- function() {
  
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

  png("plot2.png", width=480, height=480);
  
  plot(x=as.POSIXct(consumption$Time, consumption$Date),
       y=consumption$Global_active_power,
       type="l",
       xlab="",
       ylab="Global Active Power (kilowatts)");
  
  dev.off();
}