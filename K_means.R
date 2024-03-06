library(tidyverse)
library(dplyr)
library(factoextra)

df = read.csv("./college.csv", sep = ",")
class(df)
head(df)


rownames(df) = NULL


df2 = df %>%
  filter(state == "MD") %>% 
  column_to_rownames(var="name")
  
summary(df2["admission_rate"])
summary(df2["sat_avg"])


df3 = df2 %>%
  select(admission_rate, sat_avg) %>%
  mutate_at(c("admission_rate", "sat_avg"), funs(c(scale(.))))

summary(df3)

km = kmeans(df3, centers = 3, nstart = 25)

str(km)
km[["centers"]]
#3 klastry: 
#pierwszy o wielkości 2, środek [-1.7425275  1.7871932]
#drugi o wielkośći 9, środek [-0.2001854 -0.8322366]
#trzeci o wielkości 8, środek [0.6608405  0.4894679]

fviz_cluster(km, df3, geom = "point")
# 2 elementowy klaster to linia miedzy 2 punktami ostajacymi od reszty

df2$loan_default_rate = as.numeric(df2$loan_default_rate)

km_osiem = kmeans(df2[ ,c("loan_default_rate", "undergrads", "tuition", "median_debt","faculty_salary_avg")], centers = 3, nstart = 25)  
                       
df2$cluster = as.factor(km_osiem$cluster)        

cluster_srednia = aggregate(df2[ ,c("loan_default_rate", "undergrads", "tuition", "median_debt", "faculty_salary_avg")],
                            by = list(df$cluster),
                            FUN = mean)
#Error argumenty nie ta sama dlugosc, wcześniej działało, brało średnią klastra dla wybranych wierszy
print(cluster_srednia)

                      
fviz_nbclust(df3, kmeans, method = "wss")
#optymalna ilość klastrów to 4

km_optimal(df3, centers = 4, nstart = 25)

fviz_cluster(km_osiem, df3, geom = "point")