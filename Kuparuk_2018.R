library(readxl)

#Set a start time to convert time stamp into minutes since start of the year#
start.time = as.POSIXct("2018-01-01", format="%Y-%m-%d", tz="UTC")

#Read 2018 Kuparuk discharge data#
Q.data.2018 = as.data.frame(read_excel("Kuparuk_Q_2018.xlsx"))
#Convert timestamp from text to time R recognize#
Q.data.2018$Date = as.POSIXct(Q.data.2018$Date, format="%Y-%m-%d %H:%M:%S")
#Convert timestamp to minutes since start of the year#
Q.data.2018$min.since = as.numeric(difftime(Q.data.2018$Date, start.time, units="mins"))

#Interpolation function for Q~time#
Q.fun.2018 = approxfun(x=Q.data.2018$min.since, y=Q.data.2018$Discharge_m3s)

#Read water chemistry data and processing time stamp#
chem.data = read.csv("Kuparuk_Chem_2017_2019.csv")
chem.data$DateTime = as.POSIXct(chem.data$DateTime, format="%Y-%m-%d %H:%M", tz="UTC")
chem.data$min.since = as.numeric(difftime(chem.data$DateTime, start.time, units="mins"))

#Selection 2018 data#
chem.2018 = with(chem.data, chem.data[min.since>0 & min.since<365*24*60 & DOC_mgL>0,])
#Calculate discharge at the time of water chemistry measurement#
chem.2018$Q = Q.fun.2018(chem.2018$min.since)
chem.2018 = with(chem.2018, chem.2018[!is.na(Q),])


#Change quartz() to windows() if run on Windows and x11() if run on Linux#
quartz(w=9.5,h=3.2)
par(mfrow=c(1,3), mar=c(4,4,1.5,1), cex=0.85)

#Size of the moving window in days#
window.size = c(10,20,30)

for(m in 1:length(window.size)){
	#number of windows#
	no.window = floor(with(chem.2018, (max(min.since)-min(min.since))/60/24)-window.size[m]+1)
	#Array to store C-Q slope and confidence interval#
	b = array()
	b.up = array()
	b.low = array()
	for(i in 1:no.window){
		#Select data within each window#
		data.temp = with(chem.2018, chem.2018[min.since>min(min.since)+(i-1)*24*60 & min.since<min(min.since)+(i+29)*24*60,])
		#Log(C)~Log(Q) regression#
		#Replace DOC_mgL with Nitrate_mgL for nitrate analysis#
		mod = lm(log(DOC_mgL)~log(Q), data=data.temp)
		b[i] = coef(mod)[2]
		b.up[i] = confint(mod)[2,1]
		b.low[i] = confint(mod)[2,2]
	}

	julian.day = seq(from=floor(min(chem.2018$min.since)/60/24), by=1, length.out=no.window)
	
	#Plot the slope of the C-Q regression#
	title = paste0("2018, ",as.character(window.size[m])," days")
	plot(b~julian.day, type="l", ylim=range(b.low, b.up), pch=19, main=title)
	#Using polyon to plot the confidence interval of the slope#
	polygon(x=c(julian.day,rev(julian.day)), y=c(b.up, rev(b.low)), col=rgb(0,0,0,0.2), border=NA)
}
