library(readxl)
library(tidyr)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(factoextra)
library(dendextend)
library(randomcoloR)

#для компа
setwd("C:/Users/nagal/OneDrive/GitHub/Coursework_2023/Данные")
#для ноута 
#setwd("C:/Users/jakep/GitHub/Coursework_2023/Данные")

#ВЪЕЗДНЫЕ
inbound_tours<-read_excel("Въездные турпоездки.xlsx")

colors <- c("#FF0000", "#00FF00", "#0000FF", "#FFFF00", "#00FFFF", "#FF00FF", "#800000", 
            "#008000", "#000080", "#808000", "#008080", "#800080", "#FFA07A", "#20B2AA", 
            "#B0E0E6", "#BA55D3", "#BC8F8F", "#C71585", "#00BFFF", "#F4A460", "#FFFACD", 
            "#F08080", "#E6E6FA", "#D3D3D3", "#696969", "#800000", "#A9A9A9", "#000000", 
            "#00FF00", "#800080", "#FF4500", "#1E90FF", "#F0E68C", "#DAA520", "#808080", 
            "#00CED1", "#FFDAB9", "#FF1493", "#191970", "#BDB76B", "#7FFF00", "#CD853F", 
            "#FFC0CB", "#D2691E", "#4682B4", "#B0C4DE", "#DDA0DD", "#F5DEB3", "#DEB887", 
            "#9ACD32", "#FFF0F5", "#FF8C00", "#87CEFA", "#7B68EE", "#FF69B4", "#B22222", 
            "#F0FFFF", "#00FF7F", "#32CD32", "#FFD700", "#FDF5E6", "#FF00FF", "#ADD8E6", 
            "#FFFFE0", "#FFA500", "#FFFAFA")


inbound_tours_long <- inbound_tours %>%
  gather(key = "Год", value = "number_of_travelers", -Страна) %>%
  group_by(Страна, Год) %>%
  summarise(Общее_количество = sum(number_of_travelers))

#inbound_tours_long <- inbound_tours_long[inbound_tours_long$Общее_количество > 2500,]

ggplot(inbound_tours_long, aes(x = Год, y = Общее_количество, fill = Страна)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Количество въезжающих туристов", limits = c(0, max(inbound_tours_long$Общее_количество))) +
  labs(title = "Количество въезжающих туристов из каждой страны за все года",
       x = "Год", y = "Количество въезжающих туристов") +
  scale_fill_manual(values = colors) +
  #guides(fill=FALSE)+
  theme_bw()


#Общее количество приезжих по страннам за 9 лет
all_inbound_tours<-data.frame(Страна=inbound_tours$Страна, ОбщееКоличество=rowSums(inbound_tours[,2:10]))

ggplot(all_inbound_tours, aes(x = Страна, y = ОбщееКоличество)) +
  geom_bar(stat = "identity", fill = "navyblue", color = "black") +
  labs(x = "", y = "Количество человек", 
       title = "Общее количество въезжающих туристов по странам за 9 лет") +
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

#field_tours_long <- field_tours_long[field_tours_long$Общее_количество > 5000,]


ggplot(field_tours_long, aes(x = Год, y = Общее_количество, fill = Страна)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Количество граждан", limits = c(0, max(field_tours_long$Общее_количество))) +
  labs(title = "Количество выездных граждан России в страны за все года",
       x = "Год", y = "Количество человек") +
  scale_fill_manual(values = colors) +
  #guides(fill=FALSE)+
  theme_bw()

#Общее количество уезжих по страннам за 9 лет
all_field_tours <- data.frame(Страна = field_tours$Страна, 
                              ОбщееКоличество = rowSums(field_tours[, 2:10]))

ggplot(all_field_tours, aes(x = Страна, y = ОбщееКоличество)) +
  geom_bar(stat = "identity", fill = "navyblue", color = "black") +
  labs(x = "", y = "Количество человек", 
       title = "Общее количество выездных граждан по странам за 9 лет") +
  scale_x_discrete(labels = all_field_tours$Страна, 
                   limits = all_field_tours$Страна) +
  coord_flip() +
  theme_bw()

#Путешествие в России
In_Russian<-read_excel("Внутри России.xlsx")

set.seed(123)
colors <- randomColor(count = 87, luminosity = "bright")
#Colors_in_russian <- rainbow(length(In_Russian$Округ))
#In_Russian <- In_Russian[In_Russian$ОбщееКоличество/1000 < 1000,]

ggplot(In_Russian, aes(x = Округ, y = ОбщееКоличество / 1000, fill = Округ)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(name = "Количество человек (тыс.)", limits = c(0, max(In_Russian$ОбщееКоличество/1000))) +
  labs(title = "Количесво человек, путешествующих  по областям в 2022 году") +
  theme_bw() +
  theme(legend.position = "bottom",axis.text.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9)) +
  scale_x_discrete(limits = unique(In_Russian$Округ), expand = c(0, 0.5))+
  guides(fill = FALSE)

#кластеризация

In_Russian_clast <- select(In_Russian, -Округ)

In_Russian_clast_scaled <- scale(In_Russian_clast)

hc <- hclust(dist(In_Russian_clast_scaled), method = "ward.D2")

plot(hc, labels = In_Russian$Округ, main="Дендограмма",ylab="Сходство",xlab="Округа")

rect.hclust(hc, 3, border="red")
abline(h = 1.5, col = "blue", lwd='2') 

dend <- as.dendrogram(hc)
dend <- color_branches(dend, 3) 
plot(dend)

groups <- cutree(hc, 3) 
In_Russian[groups==1, 1]
In_Russian[groups==2, 1]
In_Russian[groups==3, 1]

#Путешествия по россии за 2022 общее по округам
In_Russian_all <- read_excel("Внутри России общее.xlsx")
colors <- brewer.pal(8, "Dark2")

ggplot(In_Russian_all, aes(x = Округ, y = Количество/1000, fill = Округ)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "Количесво человек, путешествующих  в округах России в 2022 году",
       x = "Округ", y = "Количество человек(в тыс.)") +
  guides(fill = FALSE)


#по кварталам, по сезонам

season<-read_excel("По сезонам.xlsx")

season_long <- season %>%
  gather(key = "Квартал", value = "Количество", -Округ) %>%
  mutate(Квартал = factor(Квартал, levels = c("1 квартал", "2 квартал", "3 квартал", "4 квартал","Холодные","Теплые")))

season_long <- season_long %>% 
  mutate(Количество = Количество / 1000)

# для кварталов холодных и теплых
season_cold_warm <- season_long %>%
  filter(Квартал %in% c("Холодные", "Теплые"))
season_cold_warm <- season_cold_warm %>% rename(Сезон = Квартал)

ggplot(season_cold_warm, aes(x = Округ, y = Количество/1000, fill = Сезон )) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Количество человек, путешествующих по округам в холодные и теплые сезоны в 2022 году",
       x = "Округ",
       y = "Количество путешественников (тыс.)") +
  scale_fill_manual(values =  c("#A6CEE3", "#FF8000")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 9)) +
  scale_x_discrete(limits = unique(season_cold_warm$Округ), expand = c(0, 0.5))

#по кварталам, по сезонам(общее)

season_all<-read_excel("По сезонам общее.xlsx")

season_long_all <- season_all %>%
  gather(key = "Квартал", value = "Количество", -Округ) %>%
  mutate(Квартал = factor(Квартал, levels = c("1 квартал", "2 квартал", "3 квартал", "4 квартал","Холодные","Теплые")))

season_long_all <- season_long_all %>% 
  mutate(Количество = Количество / 1000)

# для кварталов холодных и теплых
season_23_all <- season_long_all %>%
  filter(Квартал %in% c("Холодные", "Теплые"))
season_23_all <- season_23_all %>% rename(Сезон = Квартал)

ggplot(season_23_all, aes(x = Округ, y = Количество/1000, fill = Сезон)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Количество человек, путешествующих по округам в холодные и теплые сезоны в 2022 году",
       x = "Округ",
       y = "Количество путешественников (тыс.)") +
  scale_fill_manual(values =  c("#56B4E9", "#E69F00")) +
  theme_bw()  +
  scale_x_discrete(limits = unique(season_23_all$Округ), expand = c(0, 0.5))


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

