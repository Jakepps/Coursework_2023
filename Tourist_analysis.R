library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)

#для компа
setwd("C:/Users/nagal/OneDrive/GitHub/Coursework_2023/Данные")
#для ноута 
#setwd("C:/Users/jakep/GitHub/Coursework_2023/Данные")

#ВЪЕЗДНЫЕ
inbound_tours<-read_excel("Въездные турпоездки.xlsx")
#colors <- rainbow(length(inbound_tours$Страна))

inbound_tours_long <- inbound_tours %>%
  gather(key = "Год", value = "number_of_travelers", -Страна) %>%
  group_by(Страна, Год) %>%
  summarise(Общее_количество = sum(number_of_travelers))

#inbound_tours_long <- inbound_tours_long[inbound_tours_long$Общее_количество < 2000,]

ggplot(inbound_tours_long, aes(x = Год, y = Общее_количество, fill = Страна)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Количество приезжих", limits = c(0, max(inbound_tours_long$Общее_количество))) +
  labs(title = "Количество приезжих из каждой страны за все года",
       x = "Год", y = "Количество приезжих") +
  theme_bw()


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

#field_tours_colors <- rainbow(length(field_tours$Страна))

field_tours_long <- field_tours %>%
  gather(key = "Год", value = "number_of_travelers", -Страна) %>%
  group_by(Страна, Год) %>%
  summarise(Общее_количество = sum(number_of_travelers))

#field_tours_long <- field_tours_long[field_tours_long$Общее_количество < 5000,]

ggplot(field_tours_long, aes(x = Год, y = Общее_количество, fill = Страна)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Количество граждан", limits = c(0, max(field_tours_long$Общее_количество))) +
  labs(title = "Количество выездных граждан России в страны за все года",
       x = "Год", y = "Количество граждан") +
  theme_bw()


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
#TODO: кластеризация
In_Russian<-read_excel("Внутри России.xlsx")

Colors_in_russian <- rainbow(length(In_Russian$Округа))
Colors_in_russian <- c("black", Colors_in_russian[-1])

In_Russian_2022<-data.frame(Округ=In_Russian$Округа, ОбщееКоличество=In_Russian$'2022')

In_Russian_2022 <- In_Russian_2022[In_Russian_2022$ОбщееКоличество/1000 < 1000,]

ggplot(In_Russian_2022, aes(x = Округ, y = ОбщееКоличество / 1000, fill = Округ)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Количество человек (тыс.)", limits = c(0, max(In_Russian_2022$ОбщееКоличество/1000))) +
  labs(title = "Общее количесво человек, путешевствующих по областям в 2022 году") +
  theme_bw() +
  theme(legend.position = "bottom",axis.text.x = element_blank())

#Путешествия по россии за 2022 общее по округам
In_Russian_all <- read_excel("Внутри России общее.xlsx")
colors <- brewer.pal(8, "Dark2")

ggplot(In_Russian_all, aes(x = Округ, y = Количество/1000, fill = Округ)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "Количество путешественников в округах России",
       x = "Округ", y = "Количество путешественников(в тыс.)") +
  guides(fill = FALSE)

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


#по кварталам, по сезонам

season<-read_excel("По сезонам.xlsx")

season_long <- season %>%
  gather(key = "Квартал", value = "Количество", -Округ) %>%
  mutate(Квартал = factor(Квартал, levels = c("1 квартал", "2 квартал", "3 квартал", "4 квартал","Холодные","Теплые")))

season_long <- season_long %>% 
  mutate(Количество = Количество / 1000)

# для кварталов 2 и 3
season_23 <- season_long %>%
  filter(Квартал %in% c("2 квартал", "3 квартал"))

ggplot(season_23, aes(x = Округ, y = Количество/1000, fill = Квартал)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Количество путешественников по округам во 2 и 3 кварталах",
       x = "Округ",
       y = "Количество путешественников (тыс.)") +
  scale_fill_manual(values =  c("#E69F00", "#56B4E9")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 9)) +
  scale_x_discrete(limits = unique(season_23$Округ), expand = c(0, 0.5))

# для кварталов 1 и 4
season_14 <- season_long %>%
  filter(Квартал %in% c("1 квартал", "4 квартал"))

ggplot(season_14, aes(x = Округ, y = Количество/1000, fill = Квартал)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Количество путешественников по округам во 1 и 4 кварталах (холодные)",
       x = "Округ",
       y = "Количество путешественников (тыс.)") +
  scale_fill_manual(values =  c("#FF8000", "#A6CEE3")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 9)) +
  scale_x_discrete(limits = unique(season_14$Округ), expand = c(0, 0.5))

# для кварталов холодных и теплых
season_cold_warm <- season_long %>%
  filter(Квартал %in% c("Холодные", "Теплые"))

ggplot(season_cold_warm, aes(x = Округ, y = Количество/1000, fill = Квартал)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Количество путешественников по округам холодные и теплые",
       x = "Округ",
       y = "Количество путешественников (тыс.)") +
  scale_fill_manual(values =  c("#A6CEE3", "#FF8000")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1, size = 9)) +
  scale_x_discrete(limits = unique(season_cold_warm$Округ), expand = c(0, 0.5))

#по кварталам, по сезонам(общее)

season_all<-read_excel("По сезонам общее.xlsx")

season_long_all <- season_all %>%
  gather(key = "Квартал", value = "Количество", -Округ) %>%
  mutate(Квартал = factor(Квартал, levels = c("1 квартал", "2 квартал", "3 квартал", "4 квартал","Холодные","Теплые")))

season_long_all <- season_long_all %>% 
  mutate(Количество = Количество / 1000)

# для кварталов 2 и 3
season_23_all <- season_long_all %>%
  filter(Квартал %in% c("Холодные", "Теплые"))

ggplot(season_23_all, aes(x = Округ, y = Количество/1000, fill = Квартал)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Количество путешественников по округам в холодные и теплые",
       x = "Округ",
       y = "Количество путешественников (тыс.)") +
  scale_fill_manual(values =  c("#56B4E9", "#E69F00")) +
  theme_bw()  +
  scale_x_discrete(limits = unique(season_23_all$Округ), expand = c(0, 0.5))
