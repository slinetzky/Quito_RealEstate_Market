rm(list = ls())
library(ggplot2)
library(dplyr)
house_rent <- read.csv("quito_house_rent.csv")
house_sell <- read.csv("quito_house_sell2.csv")

### Filter observations and remove outliers
house_rent <- house_rent[house_rent$arriendo <=2000 & house_rent$area <=500 & house_rent$area > 10 & house_rent$banos <=4 &
                              house_rent$parqueadero <= 5 & house_rent$habitaciones <= 5 & 
                              house_rent$habitaciones >0 & house_rent$banos > 0, ]
house_sell<- house_sell[house_sell$precio <=500000 & house_sell$area <=500 & house_sell$banos <=4 & house_sell$parqueadero <= 5 & house_sell$habitaciones <= 5
                        & house_sell$habitaciones >0 & house_sell$banos > 0 & house_sell$precio > 5000 & house_sell$area >10, ]



### Data Exploration:


#### Sectors distribution of observations:
library(patchwork)
library(stringr)
distribution_rent <- ggplot(data=house_rent, aes(fill="rent", x = str_sub(sector, 1, 5))) +
  geom_bar(position = "identity") +
  labs(x=element_blank(), title="Sectors' Distribution") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold")) +
  scale_fill_manual(values = "red") +
  theme(axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5))

distribution_sell <- ggplot(data=house_sell, aes(str_sub(sector, 1, 20), fill="sell")) +
  geom_bar(position = "identity") +
  scale_fill_manual(values="blue") +
  labs(x="sector") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  guides(fill=guide_legend(title=element_blank()))

combined_dist <- distribution_rent / distribution_sell +
  plot_layout(ncol = 1, guides = "collect")

### Descrpitive statistics
summary(house_sell$precio)
sd(house_sell$precio)
summary(house_rent$arriendo)
sd(house_rent$arriendo)

### Top 5 sector with highest rent
top5_sector_rent <- house_rent %>%
  group_by(sector) %>%
  summarize(mediana=median(arriendo)) %>%
  arrange(desc(mediana)) %>%
  top_n(5)
data_top5_rent <- house_rent %>%
  filter(sector %in% top5_sector_rent$sector)

### Top 5 with lowest rent
low5_sector_rent <- house_rent %>%
  group_by(sector) %>%
  summarize(mediana=median(arriendo)) %>%
  arrange(desc(mediana)) %>%
  tail(5)
data_low5_rent <- house_rent %>%
  filter(sector %in% low5_sector_rent$sector)

### Plotting Both Top 5 High and Low Rents:
ggplot(data=data_top5_rent, aes(sector, arriendo)) +
  geom_boxplot(aes(fill="Highest")) +
  labs(x="sector", y="rent (USD)", title="Top 5 Highest and Lowest Rents by Sector") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold"))+
  geom_boxplot(data=data_low5_rent, aes(fill="Lowest"))

### Top 5 sectors with highest price
top5_sector_price <- house_sell %>%
  group_by(sector) %>%
  summarize(mediana=median(precio)) %>%
  arrange(desc(mediana)) %>%
  top_n(5)
data_top5_price <- house_sell %>%
  filter(sector %in% top5_sector_price$sector)

### Top 5 sectors with lowest price
low5_sector_price <- house_sell %>%
  group_by(sector) %>%
  summarize(mediana=median(precio)) %>%
  arrange(desc(mediana)) %>%
  tail(5)
data_low5_price <- house_sell %>%
  filter(sector %in% low5_sector_price$sector)

### Plotting both top 5 for price
ggplot(data=data_top5_price, aes(sector, precio)) +
  geom_boxplot(aes(fill="Highest")) +
  labs(x="sector", y="rent (USD)", title="Top 5 Highest and Lowest House Price by Sector") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold"))+
  geom_boxplot(data=data_low5_price, aes(fill="Lowest"))




### bedroom v/s price
promedio_por_habitaciones <- house_sell %>%
  group_by(habitaciones) %>%
  summarize(promedio_precio = mean(precio))

### bathroom v/S price
promedio_por_bano<- house_sell %>%
  group_by(banos) %>%
  summarize(promedio_precio = mean(precio))

### area v/s price
library(scales)
ggplot(data=house_sell, aes(area, precio)) +
  geom_point() +
  geom_smooth(method="lm") +
  labs(x="area (m²)", y="price (in 1000 USD)", title="House Price v/s Built Square Metres") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold")) +
  scale_y_continuous(labels = label_number(scale = 1e-3))
  
### correlation matrix
library(corrplot)
data_cor_rent=cor(house_rent[,3:7])
corrplot(data_cor_rent, method = "circle")

data_cor_sell <- cor(house_sell[,3:7])
colnames(data_cor_sell) <- c("price", "area", "bedrooms", "bathrooms", "parking")
rownames(data_cor_sell) <- c("price", "area", "bedrooms", "bathrooms", "parking")
corrplot(data_cor_sell, method = "circle")



### linear regression model
m_rent <- lm(arriendo ~  sector + area + habitaciones + banos + parqueadero + piscina, data= house_rent)
summary(m_rent)


### using model to predict rent prices 
rent_predict <- predict(m_rent, newdata= house_sell)
house_sell$arriendo <- rent_predict

### creating GRM indicator and filtering
house_sell$grm <- house_sell$precio/(house_sell$arriendo*12)
house_sell <- house_sell[house_sell$grm>=4 & house_sell$grm < 50,]
summary(house_sell$grm)

### GRM distribution
ggplot(data=house_sell, aes(grm)) +
  geom_histogram(fill="royalblue2", color="black") +
  theme_bw() +
  labs(x= "Gross Rent Multiplier", title="GRM Distribution") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold")) +
  xlim(4,50)
  

### GRM vs price
ggplot(data=house_sell, aes(precio, grm)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x= "price (1000 USD)", title="GRM v/s Price", y= "GRM") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold")) +
  scale_x_continuous(labels = label_number(scale = 1e-3)) 
  
### GRM vs rent
ggplot(data=house_sell, aes(arriendo, grm)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x= "rent (USD)", title="GRM v/s Rent", y= "GRM") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold"))

### GRM vs Bathrooms
ggplot(data=house_sell, aes(as.factor(banos), grm)) +
  geom_boxplot() +
  theme_bw() +
  labs(x= "n° of bathrooms", title="GRM per N° of Bathrooms", y= "GRM") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold"))

### GRM vs pool
ggplot(data=house_sell, aes(piscina, grm)) +
  geom_boxplot() +
  theme_bw() +
  labs(x= "pool (t= true)", title="GRM Pool Comparison", y= "GRM") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold"))

### GRM vs squared meters
ggplot(data=house_sell, aes(area, grm)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  labs(x= "area (m²)", title="GRM v/s Square Meters", y= "GRM") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold"))

####Top 5 highest GRM per sector:
top5_sector_grm <- house_sell %>%
  group_by(sector) %>%
  summarize(mediana=median(grm)) %>%
  arrange(desc(mediana)) %>%
  top_n(5)
data_top5_grm <- house_sell %>%
  filter(sector %in% top5_sector_grm$sector)

###Top 5 sectors with lowest GRM
low5_sector_grm <- house_sell %>%
  group_by(sector) %>%
  summarize(mediana=median(grm)) %>%
  arrange(desc(mediana)) %>%
  tail(5)
data_low5_grm <- house_sell %>%
  filter(sector %in% low5_sector_grm$sector)

###Plotting both top 5
ggplot(data=data_top5_grm, aes(sector, grm)) +
  geom_boxplot(aes(fill="Highest")) +
  labs(x="sector", y="GRM", title="Top 5 Highest and Lowest House GRMs by Sector") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  theme(plot.title=element_text(face="bold"))+
  geom_boxplot(data=data_low5_grm, aes(fill="Lowest"))

### GRM correlation heatmap
cor_columns <- c("grm", "precio", "arriendo", "habitaciones", "banos", "parqueadero", "area")
data_cor <- house_sell[cor_columns]
cor_matrix <- cor(data_cor)
colnames(cor_matrix) <- c("grm", "price", "rent", "bedrooms", "bathrooms", "parking", "area")
rownames(cor_matrix) <- c("grm", "price", "rent", "bedrooms", "bathrooms", "parking", "area")
correlations <- cor_matrix["grm",]
correlation_data <- data.frame(variable = names(correlations), correlation = correlations)
correlation_data <- correlation_data[order(-abs(correlation_data$correlation)), ]
ggplot(correlation_data, aes(x = reorder(variable, -abs(correlation)), y = "", fill = correlation)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "correlation with GRM", y = "", fill = "Correlation (abs)", title="GRM Correlation Heatmap") +
  theme(plot.title=element_text(face="bold", hjust=0.5))


### Top 20 list of lowest GRM listings
top20_grm <- house_sell %>%
  arrange(desc(grm)) %>%
  tail(20)
top20_grm <- top20_grm[rev(seq_len(nrow(top20_list))),]
rownames(top20_grm) <- seq_len(nrow(top20_grm))
top20_list <- top20_grm[,c("nombre","sector","precio","arriendo","area", "banos","grm")]
colnames(top20_list) <- c("description", "sector", "price", "predicted rent", "area","bathrooms", "grm")
