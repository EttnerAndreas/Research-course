library(dplyr)
library(ggplot2)
library(ggpubr)


data <- read_excel("analysis/data.xlsx")


getwd()
View(data)
str(data)
# 1: SEX as character
# 2: Mask as numeric (recommend changing to character)
# 3: Age as numeric
# 4: Group as numeric (recommend changing to character)
# 5: position argument as character
# location :
#' a= Neupfahrplatz,
#' b= Donau,
#' c=Steinerne Brücke,
#' d= Weg zu Stadt am Hof


# Tippfehler, d statt f und n statt m
data$Sex

for (i in 1:1000){
  if (data$Sex[i] == "d"){
    print("These are the d´s" )
    print( i)
  }
  if (data$Sex[i] == "n"){
    print("These are the n´s" )
    print(i)
  }

}

# d in 408
# n in 489

# now change it within R
data$Sex[408] = "f"
data$Sex[689] = "m"


data$Mask = as.character(data$Mask)
data$Group = as.character(data$Group)
str(data)
summary(data)

#'
#'
#'
#'
#'
#'
#'
df_free <- data %>%
  filter(Location %in% c("a","c")) %>% #' LOCATION FILTER'
  filter(Sex %in% c("m", "f")) %>%  # MANN FRAU FILTER
  group_by(Location,Sex, Mask) %>%
  summarise(counts = n())
df_free
colnames(df_free) = c("location_obligatory","Sex", "Mask", "counts")
df_free

df2_free <- data.frame(location_obligatory =rep(c("obligatory location B","obligytory location D"), each=4),
                       Sex=rep(c("female no mask","female mask","male no mask","male mask"),2),
                       counts=df_free$counts)
df2_free

ggplot(data=df2_free, aes(x=Sex, y=counts, fill=location_obligatory)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values = c("#1E646E", "#002C2F"))

#'
#'
#'
#'
#'
# FÜR LOCATION B UND D
df_free <- data %>%
  filter(Location %in% c("b","d")) %>% #' LOCATION FILTER'
  filter(Sex %in% c("m", "f")) %>%  # MANN FRAU FILTER
  group_by(Location,Sex, Mask) %>%
  summarise(counts = n())
df_free
colnames(df_free) = c("location_voluntary","Sex", "Mask", "counts")
df_free

df2_free <- data.frame(location_voluntary =rep(c("voluntary location B","voluntary location D"), each=4),
                       Sex=rep(c("female no mask","female mask","male no mask","male mask"),2),
                       counts=df_free$counts)
df2_free

ggplot(data=df2_free, aes(x=Sex, y=counts, fill=location_voluntary)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values = c("#1E646E", "#002C2F"))  # HEX; FARBCODE

#'
#'
#'
#'
#'
#' For all!!
#'
data1 <- data %>%
  filter(Sex %in% c("m", "f")) %>%  # MANN FRAU FILTER
  group_by(Location,Sex, Mask) %>%
  summarise(counts = n())
data1
colnames(data1) = c("location","Sex", "Mask", "counts")
data1

out1 <- data.frame(location =rep(c("obligatory","voluntary"), each=4),
                       Sex=rep(c("female no mask","female mask","male no mask","male mask"),2),
                       counts=data1$counts)
out1

ggplot(data=out1, aes(x=Sex, y=counts, fill=location)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#1E646E", "#002C2F"))  # HEX; FARBCODE

#'
#'
#'
#'
#'
#' LOCATION A UND C FEMALE; MALE VERGLEICH
data2 <- data %>%
  filter(Location %in% c("a", "c")) %>%
  filter(Sex %in% c("m", "f")) %>%  # MANN FRAU FILTER
  group_by(Location,Sex,Mask) %>%
  summarise(counts = n())
data2
for (i in 1:8){
  data2$counts[i]= (data2$counts[i])/500
}
data2
colnames(data1) = c("location","Sex", "Mask", "counts")
data2

out2 <- data.frame(Sex=rep(c("female","male"),each=2),
                   Mask =rep(c("NO","YES"), 1),
                   counts=data2$counts)
out2

ggplot(data=out2, aes(x=Sex, y=counts, fill=Mask)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#5D353e",  "#00142F"))  # HEX; FARBCODE


#' LOCATION B UND D FEMALE; MALE VERGLEICH
data2 <- data %>%
  filter(Location %in% c("b", "d")) %>%
  filter(Sex %in% c("m", "f")) %>%  # MANN FRAU FILTER
  group_by(Location,Sex,Mask) %>%
  summarise(counts = n())
for (i in 1:8){
  data2$counts[i]= (data2$counts[i])/500
}
data2
colnames(data1) = c("location","Sex", "Mask", "counts")
data2

out2 <- data.frame(Sex=rep(c("female","male"),each=2),
                   Mask =rep(c("NO","YES"), 1),
                   counts=data2$counts)
out2

ggplot(data=out2, aes(x=Sex, y=counts, fill=Mask)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#a88661", "#504E63"))  # HEX; FARBCODE

#'
#'
#'
#'
#'
#'
#'
data2 <- data %>%
  filter(Sex %in% c("m", "f")) %>%  # MANN FRAU FILTER
  group_by(Location,Sex,Mask) %>%
  summarise(counts = n())
data2
colnames(data1) = c("location","Sex", "Mask", "counts")
data2

out2 <- data.frame(Sex=rep(c("female","male","2xFEMALE","2xMALE"),each=4),
                   Mask =rep(c("NO","YES"), 1),
                   counts=data2$counts)
out2

ggplot(data=out2, aes(x=Sex, y=counts, fill=Mask)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values = c("#57233A" ,"#00142f"))  # HEX; FARBCODE


