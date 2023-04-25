library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)

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

ggplot(all_inbound_tours, aes(x = Страна, y = ОбщееКоличество)) +
  geom_bar(stat = "identity", fill = "navyblue", color = "black") +
  labs(x = "", y = "Количество человек", 
       title = "Общее количество приезжих по страннам за 9 лет") +
  scale_x_discrete(labels = all_inbound_tours$Страна, 
                   limits = all_inbound_tours$Страна) +
  coord_flip() +
  theme_bw() 

#ВЫЕЗДНЫЕ

field_tours<-read_excel("Выездные турпоездки.xlsx")
#inbound_tours2<-inbound_tours[,2:10]
field_tours_colors <- rainbow(length(field_tours$Страна))

#пироговые диаграммы числа выездных туристских поездок граждан России в зарубежные страны
pie(field_tours$'2014',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2014 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2015',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2015 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)],
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2016',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2016 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)], 
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2017',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2017 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)],
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2018',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE, 
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2018 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2019',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2019 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2020',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2020 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)],
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2021',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2021 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)], 
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)


pie(field_tours$'2022.5',  labels=NA, radius = 1, col = field_tours_colors, clockwise = TRUE,
    main = "Число выездных туристских поездок\nграждан России в зарубежные страны в 2022 году")
legend("topleft", legend = field_tours$Страна[1:(nrow(field_tours)/2)],
       fill = field_tours_colors[1:length(field_tours_colors)/2], cex = 0.47)
legend("topright", legend = field_tours$Страна[(nrow(field_tours)/2 + 1):nrow(field_tours)],
       fill = field_tours_colors[(length(field_tours_colors)/2 + 1):length(field_tours_colors)], cex = 0.47)

#Общее количество уезжих по страннам за 9 лет
all_field_tours <- data.frame(Страна = field_tours$Страна, 
                              ОбщееКоличество = rowSums(field_tours[, 2:10]))

ggplot(all_field_tours, aes(x = Страна, y = ОбщееКоличество)) +
  geom_bar(stat = "identity", fill = "navyblue", color = "black") +
  labs(x = "", y = "Количество человек", 
       title = "Общее количество приезжих по страннам за 9 лет") +
  scale_x_discrete(labels = all_field_tours$Страна, 
                   limits = all_field_tours$Страна) +
  coord_flip() +
  theme_bw()

#Путешествие в России
In_Russian<-read_excel("Внутри России.xlsx")

Colors_in_russian <- rainbow(length(In_Russian$Округа))
Colors_in_russian <- c("black", Colors_in_russian[-1])

In_Russian_2022<-data.frame(Округ=In_Russian$Округа, ОбщееКоличество=In_Russian$'2022')

#без ggplot
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

#с ggplot
In_Russian_2022 <- In_Russian_2022[In_Russian_2022$ОбщееКоличество/1000 < 1000,]

ggplot(In_Russian_2022, aes(x = Округ, y = ОбщееКоличество / 1000, fill = Округ)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Количество человек (тыс.)", limits = c(0, max(In_Russian_2022$ОбщееКоличество/1000))) +
  labs(title = "Общее количесво человек, путешевствующих по областям в 2022 году") +
  theme_bw() +
  theme(legend.position = "bottom",axis.text.x = element_blank())

#Путешествия по россии за 2022 общее по округам
In_Russian_all <- read_excel("Внутри России общее.xlsx")
In_Russian_all <- In_Russian_all %>% mutate(row = row_number())

Color_in_Russian_all <- rainbow(length(In_Russian_all$Округа))

ggplot(In_Russian_all, aes(x = row, y = `2022` / 1000, color = Округа, group = 1)) +
  geom_line(color = "black", size = 0.3) +
  geom_point(shape = 19) +
  scale_x_continuous(name = "", breaks = 1:nrow(In_Russian_all)) +
  scale_y_continuous(name = "Количество человек (тыс.)", limits = c(0, max(In_Russian_all$'2022'/1000))) +
  labs(title = "Общее количесво человек, путешевствующих по общим областям в 2022 году") +
  theme_bw() +
  guides(color=guide_legend(title="Округа")) + 
  scale_color_manual(name = "Округа", values = Color_in_Russian_all)


legend <- In_Russian_all %>% select(Округа) %>% unique()
ggplot2::guides(color=guide_legend(title="Округа")) + 
  scale_color_manual(name = "Округа", values = Color_in_Russian_all) 


#Туриндустрия

travel_industry<-read_excel("Туриндустрия.xlsx")

#Все организации
number_organ<- rbind(travel_industry[1:1, ])

number_organ_long<-gather(number_organ,key="Год",value="Количество", -Наименование)

ggplot(number_organ_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#Прибыльные
number_organ_plus<- rbind(travel_industry[2:2, ])

number_organ_plus_long<-gather(number_organ_plus,key="Год",value="Количество", -Наименование)

ggplot(number_organ_plus_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#Убыточные
number_organ_minus<- rbind(travel_industry[3:3, ])

number_organ_minus_long<-gather(number_organ_minus,key="Год",value="Количество", -Наименование)

ggplot(number_organ_minus_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#Выручка
revenue<- rbind(travel_industry[4:4, ])

revenue_long<-gather(revenue,key="Год",value="Количество", -Наименование)

ggplot(revenue_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#Прибыль
revenue_plus<- rbind(travel_industry[5:5, ])

revenue_plus_long<-gather(revenue_plus,key="Год",value="Количество", -Наименование)

ggplot(revenue_plus_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#Убыток
revenue_minus<- rbind(travel_industry[6:6, ])

revenue_minus_long<-gather(revenue_minus,key="Год",value="Количество", -Наименование)

ggplot(revenue_minus_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#NFR(прибыль минус убыток)
NFR<- rbind(travel_industry[7:7, ])

NFR_long<-gather(NFR,key="Год",value="Количество", -Наименование)

ggplot(NFR_long, aes(x = Год, y = Количество, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge")

#Ввод в действие объектов туризма с отелями

tourism_facilities<-travel_industry[9:14,]

tourism_facilities_long <- gather(tourism_facilities, key = "Год", value = "Значение", -Наименование)

ggplot(tourism_facilities_long, aes(x = Год, y = Значение, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ввод в действие объектов туризма за период с 2014 по 2021 год",
       x = "Год",
       y = "Значение") +
  theme_bw()

#Ввод в действие объектов туризма без отелей

tourism_facilities<-travel_industry[10:14,]

tourism_facilities_long <- gather(tourism_facilities, key = "Год", value = "Значение", -Наименование)

ggplot(tourism_facilities_long, aes(x = Год, y = Значение, fill = Наименование)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Ввод в действие объектов туризма за период с 2014 по 2021 год",
       x = "Год",
       y = "Значение") +
  theme_bw()

