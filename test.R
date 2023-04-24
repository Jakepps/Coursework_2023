library(readxl)
library(tidyr)
library(ggplot2)

#для компа
setwd("C:/Users/nagal/OneDrive/GitHub/coursework2023/Данные")
#для ноута 
#setwd("C:/Users/jakep/GitHub/coursework2023/Данные")

inbound_tours<-read_excel("Въездные турпоездки.xlsx")
#inbound_tours2<-inbound_tours[,2:10]
colors <- rainbow(length(inbound_tours$Страна))
#par(mfrow=c(1,9))

#ВЪЕЗДНЫЕ
#пироговые диаграммы по число въездных туристских поездок иностранных граждан в Россию в N году
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
inbound_tours<-read_excel("Въездные турпоездки.xlsx")

all_inbound_tours<-data.frame(Страна=inbound_tours$Страна, ОбщееКоличество=rowSums(inbound_tours[,2:10]))
#all_inbound_tours<-all_inbound_tours[all_inbound_tours$ОбщееКоличество < 15000,]


plot(all_inbound_tours$ОбщееКоличество, type="b", pch=19, col="navyblue",
     xaxt="n",xlim=c(0,length(all_inbound_tours$Страна)), ylim=c(0,max(all_inbound_tours$ОбщееКоличество)),xlab='', ylab="Количество человек",
     main="Общее количество приезжих по страннам за 9 лет")

axis(1, at = 1:nrow(all_inbound_tours), labels=all_inbound_tours$Страна,las=2)

#ВЫЕЗДНЫЕ

field_tours<-read_excel("Выездные турпоездки.xlsx")
#inbound_tours2<-inbound_tours[,2:10]
field_tours_colors <- rainbow(length(field_tours$Страна))

#пироговые диаграммы по число выездных туристских поездок иностранных граждан в Россию в N году
pie(field_tours$'2014',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2014 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2015',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2015 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)],
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2016',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2016 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)], 
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2017',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2017 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)],
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2018',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE, 
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2018 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2019',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2019 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2020',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2020 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)],
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2021',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2021 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2022.5',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nиностранных граждан в Россию в 2022 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)],
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)

#Общее количество уезжих по страннам за 9 лет
all_field_tours<-data.frame(Страна=field_tours$Страна, ОбщееКоличество=rowSums(field_tours[,2:10]))

plot(all_field_tours$ОбщееКоличество, type="b", pch=19, col="navyblue",
     xaxt="n",xlim=c(0,66), ylim=c(0,34690),xlab='', ylab="Количество человек",
     main="Общее количество приезжих по страннам за 9 лет")

axis(1, at = 1:nrow(all_field_tours), labels=all_field_tours$Страна,las=2)


#Путешествие в России
In_Russian<-read_excel("Внутри России.xlsx")

Colors_in_russian <- rainbow(length(In_Russian$Округа))
Colors_in_russian <- c("black", Colors_in_russian[-1])

#пироговая - кринж
#pie(In_Russian$'2022',  labels=NA, radius = 1, col = Colors_in_russian, clockwise = TRUE, main = "")
#legend("topleft", legend = In_Russian$Округа[1:(nrow(In_Russian)/2)],
#       fill = Colors_in_russian[1:length(Colors_in_russian)/2], cex = 0.35)
#legend("topright", legend = In_Russian$Округа[(nrow(In_Russian)/2 + 1):nrow(In_Russian)],
#       fill = Colors_in_russian[(length(Colors_in_russian)/2 + 1):length(Colors_in_russian)], cex = 0.35)
#точечный график за 2022
In_Russian_2022<-data.frame(Округ=In_Russian$Округа, ОбщееКоличество=In_Russian$'2022')

#если убрать выбросы жесткие,  1000
In_Russian_2022<- In_Russian_2022[In_Russian_2022$ОбщееКоличество/1000<1000,]

plot(In_Russian_2022$ОбщееКоличество/1000, type="n", xaxt="n", xlim=c(0,length(In_Russian_2022$Округ)+20), 
     ylim=c(0,max(In_Russian_2022$ОбщееКоличество/1000)), xlab='', ylab="Количество человек (тыс.)",
     main="Общее количесво человек, путешевствующих по областям в 2022 году")
points(1:nrow(In_Russian_2022), In_Russian_2022$ОбщееКоличество/1000, type="b", pch=19, col=Colors_in_russian)
axis(side = 1, at = 1:nrow(In_Russian_2022), tcl = 0.2, labels = FALSE)

legend("topright", legend = In_Russian$Округа[1:(nrow(In_Russian)/2)],
       fill = Colors_in_russian[1:length(Colors_in_russian)/2], cex = 0.25)
legend(x=60, y = max(In_Russian_2022$ОбщееКоличество/1000), legend = In_Russian$Округа[(nrow(In_Russian)/2 + 1):nrow(In_Russian)],
       fill = Colors_in_russian[(length(Colors_in_russian)/2 + 1):length(Colors_in_russian)], cex = 0.25)
#x = 68, y = 20900


#Путешествия по россии за 2022 общее по округам
In_Russian_all<-read_excel("Внутри России общее.xlsx")

Color_in_Russian_all<-rainbow(length(In_Russian_all$Округа))
Color_in_Russian_all <- c("black", Color_in_Russian_all[-1])

plot(In_Russian_all$'2022'/1000, type="n", xaxt="n", xlim=c(0,9), ylim=c(0, 42692), 
     xlab='', ylab="Количество человек (тыс.)",
     main="Общее количесво человек, путешевствующих по общим областям в 2022 году")
points(1:nrow(In_Russian_all), In_Russian_all$'2022'/1000, type="b", pch=19, col=Color_in_Russian_all)
axis(side = 1, at = 1:nrow(In_Russian_all), tcl = 0.2, labels = FALSE)

legend("topright", legend = In_Russian_all$Округа, fill = Color_in_Russian_all, cex = 0.8)

#Туриндустрия

data<-read_excel("Туриндустрия.xlsx")

#Все организации
number_organ<- rbind(data[1:1, ])

number_organ_long<-gather(number_organ,key="Год",value="Количество", -Наименование)

ggplot(number_organ_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#Прибыльные
number_organ_plus<- rbind(data[2:2, ])

number_organ_plus_long<-gather(number_organ_plus,key="Год",value="Количество", -Наименование)

ggplot(number_organ_plus_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#Убыточные
number_organ_minus<- rbind(data[3:3, ])

number_organ_minus_long<-gather(number_organ_minus,key="Год",value="Количество", -Наименование)

ggplot(number_organ_minus_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

