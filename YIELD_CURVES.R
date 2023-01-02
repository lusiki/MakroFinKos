require("xml2");require("dplyr");require("pbapply");require("quantmod")


# WEB: https://home.treasury.gov/resource-center/data-chart-center/interest-rates/TextView?type=daily_treasury_yield_curve&field_tdr_date_value=2022

YEAR <- 2022
RATES <- read.csv2(paste0("https://home.treasury.gov/resource-center/data-chart-center/interest-rates/",
                          "daily-treasury-rates.csv/",YEAR,"/all?type=daily_treasury_yield_curve&",
                          "field_tdr_date_value=",YEAR,"&page&_format=csv"),header = TRUE,sep = ",")

RATES <- as.data.frame(RATES)
# convert to xts
RATES <- xts(x = RATES[,2:ncol(RATES)],order.by =  as.Date(RATES$Date,format ="%m/%d/%Y"))
# rename header names
colnames(RATES) <- c("1M","2M","3M","4M","6M","1Y","2Y","3Y","5Y",
                     "7Y","10Y","20Y","30Y")

storage.mode(RATES) <- "numeric"

RATES <- na.omit(RATES)

# Plot SPREAD (ten year - three month rates)
# see FRED: https://fred.stlouisfed.org/series/T10Y3M
TEN.THREE <- RATES$`10Y` - RATES$`3M`
# plot(TEN.THREE,main="Ten Year - Three Month")
chartSeries(TEN.THREE, name="Ten Year - Three Month")

# Plotting yield curve
plot(x=c(1:13),y=RATES["20220901"], type="b", ylim=c(0,5), xaxt='n',xlab="Maturity", ylab="Rate")
lines(x=c(1:13),y=RATES["20220930"],col="blue", type="b",xaxt='n')
axis(1, at=1:13, labels=names(RATES), lwd=0.35)
legend("topleft", legend=c("Sep 1", "Sep 30"),col=c("black", "blue"),
       lty=1:2, cex=.5, pt.cex = .5, bty="n")

# Start of the month - JUNE 2022
START = which(index(RATES) == "2022-06-01")
# plot yield curve for selected date
plot(x=c(1:13),y=coredata(RATES[START,]), type="b", ylim=c(0,5), xaxt='n',xlab="Maturity", ylab="Rate",col="blue")
axis(1, at=1:13, labels=names(RATES), lwd=0.35)
# plot yiel curve for the rest of the month
for(ii in START:nrow(RATES)){
  lines(x=c(1:13),y=coredata(RATES[ii,]),col="grey", type="b",xaxt='n')
}
# highlight START & END Dates / make them more visible
lines(x=c(1:13),y=coredata(RATES[START,]),col="black", type="b",xaxt='n')
lines(x=c(1:13),y=coredata(RATES[nrow(RATES),]),col="red", type="b",xaxt='n')
legend("topleft", legend=c("June 1", "Sep 30"),col=c("black", "red"),
       lty=1:2, cex=.5, pt.cex = .5, bty="n")













