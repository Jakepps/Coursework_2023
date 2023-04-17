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
pie(inbound_tours$'2014',  labels=NA, radius = 1, col = colors, clockwise = TRUE,
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2014 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], 
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)],
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


pie(inbound_tours$'2015',  labels=NA, radius = 1, col = colors, clockwise = TRUE,
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2015 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)],
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)],
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


pie(inbound_tours$'2016',  labels=NA, radius = 1, col = colors, clockwise = TRUE,
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2016 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], 
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)], 
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


pie(inbound_tours$'2017',  labels=NA, radius = 1, col = colors, clockwise = TRUE,
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2017 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)],
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)],
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


pie(inbound_tours$'2018',  labels=NA, radius = 1, col = colors, clockwise = TRUE, 
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2018 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], 
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)],
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


pie(inbound_tours$'2019',  labels=NA, radius = 1, col = colors, clockwise = TRUE,
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2019 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], 
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)],
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


pie(inbound_tours$'2020',  labels=NA, radius = 1, col = colors, clockwise = TRUE,
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2020 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)],
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)],
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


pie(inbound_tours$'2021',  labels=NA, radius = 1, col = colors, clockwise = TRUE,
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2021 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)], 
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)],
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


pie(inbound_tours$'2022',  labels=NA, radius = 1, col = colors, clockwise = TRUE,
    main = "Число въездных туристских поездок\nиностранных граждан в Россию в 2022 году")
legend("topleft", legend = inbound_tours$Страна[1:(nrow(inbound_tours)/2)],
       fill = colors[1:length(colors)/2], cex = 0.47)
legend("topright", legend = inbound_tours$Страна[(nrow(inbound_tours)/2 + 1):nrow(inbound_tours)],
       fill = colors[(length(colors)/2 + 1):length(colors)], cex = 0.47)


#Общее количество приезжих по страннам за 9 лет
all_inbound_tours<-data.frame(Страна=inbound_tours$Страна, ОбщееКоличество=rowSums(inbound_tours[,2:10]))

plot(all_inbound_tours$ОбщееКоличество, type="b", pch=19, col="navyblue",
     xaxt="n",xlim=c(0,66), ylim=c(0,25648.07),xlab='', ylab="Количество человек",
     main="Общее количество приезжих по страннам за 9 лет")

axis(1, at = 1:nrow(all_inbound_tours), labels=all_inbound_tours$Страна,las=2)



#Путешествие в России
In_Russian<-read_excel("Кэшбэк от Ростуризма.xlsx")

Colors_in_russian <- rainbow(length(In_Russian$Округа))

#пироговая - кринж
pie(In_Russian$'2022',  labels=NA, radius = 1, col = Colors_in_russian, clockwise = TRUE, main = "")
legend("topleft", legend = In_Russian$Округа[1:(nrow(In_Russian)/2)],
       fill = Colors_in_russian[1:length(Colors_in_russian)/2], cex = 0.35)
legend("topright", legend = In_Russian$Округа[(nrow(In_Russian)/2 + 1):nrow(In_Russian)],
       fill = Colors_in_russian[(length(Colors_in_russian)/2 + 1):length(Colors_in_russian)], cex = 0.35)
#точечный график за 2022
In_Russian_2022<-data.frame(Округ=In_Russian$Округа, ОбщееКоличество=In_Russian$'2022')

#если убрать выбросы жесткие,  1000
#In_Russian_2022<- In_Russian_2022[In_Russian_2022$ОбщееКоличество/1000<1000,]

plot(In_Russian_2022$ОбщееКоличество/1000, type="n", xaxt="n", xlim=c(0,87), 
     ylim=c(0,20122), xlab='', ylab="Количество человек (тыс.)",
     main="Общее количесво человек, путешевствующих по областям в 2022 году")
points(1:nrow(In_Russian_2022), In_Russian_2022$ОбщееКоличество/1000, type="b", pch=19, col=Colors_in_russian)

legend("topright", legend = In_Russian$Округа[1:(nrow(In_Russian)/2)],
       fill = Colors_in_russian[1:length(Colors_in_russian)/2], cex = 0.25)
legend(x = 68, y = 20900, legend = In_Russian$Округа[(nrow(In_Russian)/2 + 1):nrow(In_Russian)],
       fill = Colors_in_russian[(length(Colors_in_russian)/2 + 1):length(Colors_in_russian)], cex = 0.25)

#Путешествия по россии за 2022 общее по округам
In_Russian_all<-read_excel("Кэшбэк от Ростуризма общее.xlsx")

Color_in_Russian_all<-rainbow(length(In_Russian_all$Округа))

plot(In_Russian_all$'2022'/1000, type="n", xaxt="n", xlim=c(0,9), ylim=c(0, 42692), 
     xlab='', ylab="Количество человек (тыс.)",
     main="Общее количесво человек, путешевствующих по общим областям в 2022 году")
points(1:nrow(In_Russian_all), In_Russian_all$'2022'/1000, type="b", pch=19, col=Color_in_Russian_all)
axis(side = 1, at = 1:nrow(In_Russian_all), tcl = 0.2, labels = FALSE)

legend("topright", legend = In_Russian_all$Округа, fill = Color_in_Russian_all, cex = 0.7)


