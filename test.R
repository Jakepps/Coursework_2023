library(readxl)
setwd("C:/Users/nagal/OneDrive/GitHub/coursework2023/Данные")

data<-read_excel("test.xlsx")
data2<-data[,2:10]

par(mfrow=c(1,2))
pie(data$'2014',  labels=data$'2014', radius = 1, col=rainbow(length(data$'2014')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2014 году")
pie(data$'2015',  labels=data$'2015', radius = 1, col=rainbow(length(data$'2015')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2015 году")
pie(data$'2016',  labels=data$'2016', radius = 1, col=rainbow(length(data$'2016')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2016 году")
pie(data$'2017',  labels=data$'2017', radius = 1, col=rainbow(length(data$'2017')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2017 году")
pie(data$'2018',  labels=data$'2018', radius = 1, col=rainbow(length(data$'2018')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2018 году")
pie(data$'2019',  labels=data$'2019', radius = 1, col=rainbow(length(data$'2019')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2019 году")
pie(data$'2020',  labels=data$'2020', radius = 1, col=rainbow(length(data$'2020')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2020 году")
pie(data$'2021',  labels=data$'2021', radius = 1, col=rainbow(length(data$'2021')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2021 году")
pie(data$'2022',  labels=data$'2022', radius = 1, col=rainbow(length(data$'2022')), main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2022 году")

legend(-1.1, 1.1, data$Страна, cex = 0.5, fill=rainbow(length(data$Страна)))

all_data<-data.frame(Страна=data$Страна, ОбщееКоличество=rowSums(data[,2:10]))

plot(all_data, type="b", pch=19, col="navyblue", xaxt="n",xlim=c(0,66), ylim=c(0,25648.07), main="Призовые места России по вольной борьбе за 30 лет")

lines(prize_f, type="o", pch=19, col="hotpink")

legend(min(df_m$Год), 7, c("Мужчины", "Женщины"), fill=c("navyblue", "hotpink"))
axis(side=1, at=all_data$Страна)

