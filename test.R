library(readxl)
setwd("C:/Users/nagal/OneDrive/GitHub/coursework2023/Данные")

data<-read_excel("Въездные турпоездки.xlsx")
#data2<-data[,2:10]
colors <- rainbow(length(data$Страна))
#par(mfrow=c(1,9))


#пироговые диаграммы по число въездных туристских поездок\nиностранных граждан в Россию в N году
pie(data$'2014',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2014 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(data$'2015',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2015 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(data$'2016',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2016 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(data$'2017',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2017 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(data$'2018',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2018 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(data$'2019',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2019 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(data$'2020',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2020 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(data$'2021',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2021 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(data$'2022',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2022 году")
legend("topleft", legend = data$Страна[1:(nrow(data)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = data$Страна[(nrow(data)/2 + 1):nrow(data)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


#Общее количество приезжих по страннам за 9 лет
all_data<-data.frame(Страна=data$Страна, ОбщееКоличество=rowSums(data[,2:10]))

plot(all_data$ОбщееКоличество, type="b", pch=19, col="navyblue", xaxt="n",xlim=c(0,66), ylim=c(0,25648.07),xlab='', ylab="Количество человек", main="Общее количество приезжих по страннам за 9 лет")

axis(1,at = 1:nrow(all_data), labels=all_data$Страна,las=2)



#