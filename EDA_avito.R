
# --------------------import packages------------------------------------


library(tidyverse)
library(translate)
library(tm)
library(stringr)
library(ggplot2)
library(ggthemes)


# load data

train <- read_csv("train.csv",locale = locale(encoding = 'utf-8'))

test <- read_csv("test.csv",locale = locale(encoding = 'utf-8'))





# --------------------------------EDA----------------------------------------


##compute missing data percentage per feature in train set

propmiss <- function(dataframe)
  lapply(dataframe,function(x)
    data.frame(nmiss=sum(is.na(x)),
               n=length(x), 
               perc_miss= sum(is.na(x))/length(x))
  )

missingperc<- bind_rows(propmiss(train))

missingperc['variable_name'] <- colnames(train)

missingperc <- missingperc %>% 
  arrange(desc(perc_miss)) %>% 
  select(variable_name,perc_miss,nmiss,n)

missingperc

ggplot(missingperc, aes(x= variable_name,y = perc_miss))+ 
  geom_col(fill = 'blue') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept= 0.05,linetype="dashed", color = "red")


# it seems that parameter 2 and 3 have over 50% data missing - might need to be removed if they
#  don't have any correlation high level of correlation with other features or the target variables.
#  parameter 1 is under 5% with description, image, image_top_1 and price just a little over 5%.






## EDA of all variables with target feature
range(train$activation_date) 


get_unique_count <- function(dataframe){
  unique_count<- data.frame()
  for (i in (1:ncol(dataframe))){
    unique_count[i,'variable_name'] <- colnames(dataframe[i])
    unique_count[i,'unique_records_count'] <- count(unique(dataframe[i]))
  }
  unique_count <- unique_count %>% 
    arrange(unique_records_count)
  
  unique_count
}

unique_count <- get_unique_count(train)

# for visualization purpose,it seems that the top 4 variables user type, parent_category_name,
# activation_date, dow_activation can be converted to categorical variables for visualization





ggplot(train, aes(x =user_type, y = deal_probability)) + 
  geom_boxplot()
#the deal probability does not seem to be drastically different among the different user type


ggplot(train, aes(x = parent_category_name, y = deal_probability)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# the deal probability seem to be quite different across different parent category name
ggplot(train) + 
  geom_boxplot(aes(x = activation_date, y = deal_probability, group = activation_date)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#plot activation_date as time series data per day of activation. it seems that certain days 
# such as March29th,april 1st, 3rd etc have much higher deal probabilities. 

ggplot(train, aes(x = region, y = deal_probability)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# doesn not seem to be anything significant different across regions in deals probabilities

ggplot(train, aes(x = category_name, y = deal_probability)) + 
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# it seems that the different category name has very different levels of deals probabilities
ggplot(train,aes(x = price, y = deal_probability)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  geom_jitter() 


nodealperc <- sum(train$deal_probability == 0)/nrow(train)
ggplot(train, aes(deal_probability)) + 
  geom_histogram(aes(y = ..density../30))+
  ylab("percentage distribution")
# it seems that over 64% of theh deal probabilities are 0


# To find the most popular region
```{r}
train %>% 
    filter(!is.na(region)) %>% 
    group_by(region) %>% 
    summarize(Count = n()/nrow(train)*100) %>%
    arrange(desc(Count)) %>% 
    mutate(region = reorder(region, Count)) %>% 
 ggplot(aes(x = region,y = Count) ) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = region, y = 1, label = paste0("(",round(Count)," %)",sep="")))+
    labs(x = 'region', 
       y = 'Percentage', 
       title = 'Most popular region') +
  coord_flip() 
```
# To find total number of regions
```{r}
train %>% 
    filter(!is.na(region)) %>% 
    group_by(region) %>% 
    summarize(Count = n()) %>%
    arrange(desc(Count))

## to visualize most popular category
train %>% 
    filter(!is.na(parent_category_name)) %>% 
    group_by(parent_category_name) %>% 
    summarize(Count = n()/nrow(train)*100) %>%
    arrange(desc(Count)) %>% 
    mutate(parent_category_name = reorder(parent_category_name, Count)) %>% 
 ggplot(aes(x = parent_category_name,y = Count) ) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = parent_category_name, y = 1, label = paste0("(",round(Count)," %)",sep="")))+
    labs(x = 'Parent Category Name', 
       y = 'Percentage', 
       title = 'Most Popular Parent Category') +
  coord_flip() 

 ## to visualize most popular Category
train %>% 
    filter(!is.na(parent_category_name)) %>% 
    group_by(parent_category_name) %>% 
    summarize(Count = n()) %>%
    arrange(desc(Count))

train %>% 
    filter(!is.na(category_name)) %>% 
    group_by(category_name) %>% 
    summarize(Count = n()/nrow(train)*100) %>%
    arrange(desc(Count)) %>% 
    mutate(category_name = reorder(category_name, Count)) %>% 
    head(10) %>% 
 ggplot(aes(x = category_name,y = Count) ) +
  geom_bar(stat='identity',colour="white") +
  geom_text(aes(x = category_name, y = 1, label = paste0("(",round(Count)," %)",sep="")))+
    labs(x = 'Category Name', 
       y = 'Percentage', 
       title = 'Most Popular Category') +
  coord_flip()








