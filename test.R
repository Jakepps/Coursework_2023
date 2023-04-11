library(readxl)
#для компа
setwd("C:/Users/nagal/OneDrive/GitHub/coursework2023/Данные")
#для ноута 
#setwd("C:/Users/jakep/GitHub/coursework2023/Данные")

inbound_tours<-read_excel("Въездные турпоездки.xlsx")
#inbound_tours2<-inbound_tours[,2:10]
colors <- rainbow(length(inbound_tours$Страна))
#par(mfrow=c(1,9))


#пироговые диаграммы по число въездных туристских поездок\nиностранных граждан в Россию в N году
pie(inbound_tours$'2014',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2014 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(inbound_tours$'2015',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2015 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(inbound_tours$'2016',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2016 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(inbound_tours$'2017',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2017 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(inbound_tours$'2018',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2018 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(inbound_tours$'2019',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2019 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(inbound_tours$'2020',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2020 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(inbound_tours$'2021',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2021 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


pie(inbound_tours$'2022',  labels=NA, radius = 1, col = colors, clockwise = TRUE, main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2022 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], fill = colors[1:length(colors)/2], cex = 0.5)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.5)


#Общее количество приезжих по страннам за 9 лет
all_inbound_tours<-inbound_tours.frame(Страна=inbound_tours$Страна, ОбщееКоличество=rowSums(inbound_tours[,2:10]))

plot(all_inbound_tours$ОбщееКоличество, type="b", pch=19, col="navyblue", xaxt="n",xlim=c(0,66), ylim=c(0,25648.07),xlab='', ylab="Количество человек", main="Общее количество приезжих по страннам за 9 лет")

axis(1, at = 1:nrow(all_inbound_tours), labels=all_inbound_tours$Страна,las=2)



#
data<-read_excel("Кэшбэк от Ростуризма.xlsx")

colors2 <- rainbow(length(data$Округа))
pie(data$'2022',  labels=NA, radius = 1, col = colors2, clockwise = TRUE, main = "")
legend("topleft", legend = data$Округа[1:(nrow(data)/2)], fill = colors2[1:length(colors2)/2], cex = 0.4)
legend("topright", legend = data$Округа[(nrow(data)/2 + 1):nrow(data)], fill = colors2[(length(colors2)/2 + 1):length(colors2)], cex = 0.2)

all_data2<-data.frame(Округ=data$Округа, ОбщееКоличество=data$'2022')
#all_data2[all_data2$ОбщееКоличество/1000 > 5000, "ОбщееКоличество"] <- NA
#plot(all_data2$ОбщееКоличество/1000, type="b", pch=19, col="navyblue", xaxt="n", xlim=c(0,87), ylim=c(0,20122), xlab='', ylab="Количество человек (тыс.)", main="Общее количество приезжих по страннам за 9 лет")
#axis(1, at = 1:nrow(all_data2), labels = all_data2$Округ, las = 2)

plot(all_data2$ОбщееКоличество/1000, type="n", xaxt="n", xlim=c(0,87), ylim=c(0,20122), xlab='', ylab="Количество человек (тыс.)", main="Общее количесво человек, путешевствующих по областям в 2022 году")
points(1:nrow(all_data2), all_data2$ОбщееКоличество/1000, type="b", pch=19, col=colors2)

legend("topright", legend = data$Округа[1:(nrow(data)/2)], fill = colors2[1:length(colors2)/2], cex = 0.35)
legend(x = 55, y = 20900, legend = data$Округа[(nrow(data)/2 + 1):nrow(data)], fill = colors2[(length(colors2)/2 + 1):length(colors2)], cex = 0.3)



data2<-read_excel("Кэшбэк от Ростуризма общее.xlsx")
col2<-rainbow(length(data2$Округа))

plot(data2$'2022'/1000, type="n", xaxt="n", xlim=c(0,9), ylim=c(0, 42692), xlab='', ylab="Количество человек (тыс.)", main="Общее количесво человек, путешевствующих по общим областям в 2022 году")
points(1:nrow(data2), data2$'2022'/1000, type="b", pch=19, col=col2)

legend("topright", legend = data2$Округа, fill = col, cex = 0.5)

