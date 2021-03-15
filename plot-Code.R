library(dplyr)
library(ggplot2)
library(ggpubr)


data <- read_excel("C:/Users/andie/Downloads/Research skills/data.xlsx")
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




par(mfrow = c(2,2))  # split plot output to 2x2 plots

data$Mask = as.character(data$Mask)
data$Group = as.character(data$Group)
str(data)
summary(data)

fit <- lm(Mask ~ Sex + Age + Location , data = data)
fit

plot(fit)



df <- data %>%
  filter(Sex %in% c("m", "f")) %>%
  group_by(Location,Sex, Mask) %>%
  summarise(counts = n())
df

# Hex colour combo´s
#FDDCEF  #C2E3F4    - light colours
#"#A13E97", "#82BBB5"    - darker colours
# "#B78338", "#915C4C", "#40686A", "#2f1812"
ggplot(df, aes(x = Location, y = counts)) +
  geom_bar(
    aes(color = Sex, fill = Sex),
    stat = "identity", position = position_stack()
  ) +
  scale_color_manual(values = c("#A13E97", "#82BBB5" ))+
  scale_fill_manual(values = c("#A13E97", "#82BBB5" ))

# Use position = position_dodge()
p <- ggplot(df, aes(fill = Mask,x = Location, y = counts)) +
  geom_bar(
    aes(color = Mask, fill = Sex),
    stat = "identity", position = position_dodge(0.8),
    width = 0.8
  ) +
  scale_color_manual(values = c("#000000", "#000000","#000000", "#000000"))+
  scale_fill_manual(values = c("#57233A", "#0359AE","#57233A", "#0359AE"))
p



df
df$counts
df2 <- data.frame(Location=rep(c("A", "B","C","D"), each=4),
                  Sex=rep(c("Male","Male+Mask","Female","Female+Mask"),2),
                  counts=df$counts)
df2

ggplot(data=df2, aes(x=Location, y=counts, fill=Sex)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_text(aes(label=counts), vjust=1.6, color="white",
            position = position_dodge(0.8), size=4.5)+
  scale_fill_manual(values = c("#FEC804", "#F26322", "#7A2D59", "#181B46"))


