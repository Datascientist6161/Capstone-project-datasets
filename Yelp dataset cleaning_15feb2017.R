library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggmap)
library(treemap)
require(xlsReadWrite)
library(readxl)
library(cluster)
xls.getshlib()
# load the csv file into R
tb <- tbl_df(read.csv("yelp_academic_dataset_business.csv"))
# Delete emplty rows in R
tb <- tb[!apply(is.na(tb) | tb == "", 1, all),]
# Filter categories column so that only restaurant business are selected and assigned 1. Data stored under 
#new column called categories3. grep function is applied on the categories column.
# other business like auto serive repair etc are saved as 0
tb<-mutate(tb,categories3=ifelse(grepl("Restaurants",tb$categories),1,0)) 
# Filter data to only show restaurant businesses
tb<-filter(tb,tb$categories3==1)
#Added additional columns for each cusine (American, Mexican, Chinese, Italian, Indian, Pakisthani,Other)
# Data in each column is saved as 1 or 0
tb<-mutate(tb,American_Restaurant=ifelse(grepl("American",tb$categories),1,0))
tb<-mutate(tb,Chinese_Restaurant=ifelse(grepl("Chinese",tb$categories),1,0))
tb<-mutate(tb,Italian_Restaurant=ifelse(grepl("Italian|Pizza",tb$categories),1,0))
tb<-mutate(tb,Mexican_Restaurant=ifelse(grepl("Mexican",tb$categories),1,0))
tb<-mutate(tb,Indian_Restaurant=ifelse(grepl("India",tb$categories),1,0))
tb<-mutate(tb,Pakisthani_Restaurant=ifelse(grepl("Pakisthani",tb$categories),1,0))
#Other categories restaurants########################################
tb<-mutate(tb,Other_Restaurant=ifelse(grepl("'Sandwiches'|'Delis|'Japnese'|'Fast Food'|'Caribbean'|'Korean'|'Vietnamese'|'Ethiopian|'Coffee & Tea'|'Hawaiian'|'French'|'Barbeque'",tb$categories),1,0))
#############################################################
#######Run the below code if you want the count of other restuarant types###
## Other_category<-tb_merged%>%select(state,categories,stars)
## Other_category<-mutate(Other_category,Other_Restaurant=ifelse(grepl("'Sandwiches'|'Delis|'Japnese'|'Fast Food|'Caribbean'|'Korean'|'Vietnamese'|'Ethiopian|'Coffee & Tea'|'Hawaiian'|'French'|'Barbeque'",Other_category$categories),1,0))
## h<-sum(Other_category$Other_Restaurant)
## h
###############################################################
### Removing quotes and cleaning column for state
tb %<>% mutate(state = str_replace_all(state, 'b',''))

####Filtering data to only show data for cities in United States. UK data is removed.
tb<- filter(tb,tb$state %in% c("'PA'","'NC'","'WI'","'IL'","'AZ'","'NV'"))
tb %<>% mutate(city = str_replace_all(city, 'b',''))

#####################################################################
Restaurant_Othercategory<-mutate(tb,Other_Restaurant=ifelse(grepl("'Sandwiches'|'Delis|'Japnese'|'Fast Food'|'Caribbean'|'Korean'|'Vietnamese'|'Ethiopian|'Coffee & Tea'|'Hawaiian'|'French'|'Barbeque'",tb$categories),1,0))

  
#Plotting the cities for which restaurant data is considered(plotting latitude & longitude colums data)
map <- get_map(location ='united states', zoom = 4)
mapPoints <-ggmap(map) +
  geom_point(aes(x = longitude, y =latitude),data = tb, alpha = .5)
plot(mapPoints)

# Creating additional column to group all the restaurants in the suburbs of a particular city as the city itself
tb<-mutate(tb,new_city= ifelse((state=="'PA'"),"Pittsburg",
                                ifelse((state=="'NV'"),"LasVegas",
                                        ifelse((state=="'NC'"),"Charlotte",
                                               ifelse((state=="'AZ'"),"Phoenix",
                                                  ifelse((state=="'IL'"),"Urbana_Champaign","Madison"))))))
tb3<-select(tb,new_city,state)#Viewing just the newly created city and checking if it is in the right state
#selected newly created columns and saved in a new data frame tb2 only for viewing
tb2<-select(tb,new_city,American_Restaurant,Italian_Restaurant,Mexican_Restaurant,Indian_Restaurant,Pakisthani_Restaurant,Chinese_Restaurant,Other_Restaurant)
##############################################################
#Tree map with star ratings, city, restaurant category, count######
#################################################
tb2_treemap<-select(tb,new_city,stars,American_Restaurant,Italian_Restaurant,Mexican_Restaurant,Indian_Restaurant,Pakisthani_Restaurant,Chinese_Restaurant)
tb3_new<-gather(tb2_treemap,"Restaurant_Category","n",3:8)
tb3_new%>%group_by(new_city,Restaurant_Category)%>%summarise(n=sum(n))
View(tb3_new)
treemap(tb3_new,index=c("new_city","Restaurant_Category","stars"),vSize = "n", type="index", palette="Set1", title="Number of Restaurants by City", fontsize.title = 14)
###### Using fun.aggregate="mean"####################
treemap(tb3_new,index=c("new_city","Restaurant_Category","stars"),vSize = "n", type="index", palette="Set1", fun.aggregate = "mean",title="Number of Restaurants by City-Mean", fontsize.title = 14)
########  TREE MAP ########################
treemap(tb3,index=c("new_city","Restaurant_Category"),vSize = "n", type="index", palette = "Set1", title="Number of Restaurants by City", fontsize.title = 14)

###################################
# Finding the total number of restaurants for each cuisine.
a<-sum(tb$American_Restaurant)
b<-sum(tb$Chinese_Restaurant)
r<-sum(tb$Italian_Restaurant)
d<-sum(tb$Mexican_Restaurant)
e<-sum(tb$Indian_Restaurant)
f<-sum(tb$Pakisthani_Restaurant)
g<-c(a,b,r,d,e,f)
print(g)
# Load the population demographics data excel file
tb_demographs<-read_excel("Population_Demographics_data.xlsx",sheet="Sheet1")
tb7<-select(tb_demographs,new_city,`      Black_or_African American`,`  Asian_Indian`,`    White`,` Chinese`,`Asian_All other`,Hispianic_Latino,`Non Hispianic`)
tb8<-gather(tb7,"Race","n",2:8)
tb8%>%group_by(new_city,Race)
View(tb8)
# PLOT POPULATION DEMOGRAPHICS DATA
ggplot(tb8,aes(x=new_city,y=n,fill=Race))+geom_bar(stat="identity",position="dodge") 
ggplot(tb8,aes(x=new_city,y=n,fill=Race))+geom_bar(stat="identity",position="stack")
#Merge Yelp data and population demographics table
tb_merged<-left_join(tb,tb_demographs,by="new_city")
#Other_category_restaurants<-tb_merged%>%select(state,categories,stars)
#table_newcity<-select(tb_merged,new_city)#no need to have this
#View(table_newcity)
#export(table_newcity,"newcitydata.csv")
# Remove the faulty rows that have data misaligned in the cells
#tb_merged<-mutate(tb_merged,baddata=ifelse((new_city==0),0,1))

#tb_merged<-mutate(tb_merged,baddata1=ifelse((baddata==1),1,0))

#tb_merged<-filter(tb_merged,tb_merged$baddata==1)
View(tb_merged)
#View(sample_n(tb,10)) # View small sample
#View(unique(tb$categories) %>% head(20))
tb3<-gather(tb2,"Restaurant_Category","n",2:8)
tb3%>%group_by(new_city,Restaurant_Category)%>%summarise(n=sum(n))
tb4<-tb3%>%group_by(new_city,Restaurant_Category)%>%summarise(n=sum(n))
# PLOT RESTAURANT TYPES IN EACH CITY
ggplot(tb4,aes(x=new_city,y=n,fill=Restaurant_Category))+geom_bar(stat="identity",position="dodge")
ggplot(tb4,aes(x=Restaurant_Category,y=n,fill=new_city))+geom_bar(stat="identity",position="dodge")
#####################################################################

#########Plots to get Restaurant % in each city#################################

tb4<-tb3%>%group_by(new_city,Restaurant_Category)%>%filter(new_city=='LasVegas')%>%summarise(n=sum(n))
LasVegas_percent<-mutate(tb4,Percent=n/sum(n)*100)
ggplot(LasVegas_percent,aes(x=Restaurant_Category,y=Percent,fill=Restaurant_Category))+geom_bar(stat='identity')+ggtitle("Restaurants in Las Vegas")+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
tb4<-tb3%>%group_by(new_city,Restaurant_Category)%>%filter(new_city=='Charlotte')%>%summarise(n=sum(n))
Charlotte_percent<-mutate(tb4,Percent=n/sum(n)*100)
ggplot(Charlotte_percent,aes(x=Restaurant_Category,y=Percent,fill=Restaurant_Category))+geom_bar(stat='identity')+ggtitle("Restaurants in Charlotte")+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
tb4<-tb3%>%group_by(new_city,Restaurant_Category)%>%filter(new_city=='Pittsburg')%>%summarise(n=sum(n))
Pittsburg_percent<-mutate(tb4,Percent=n/sum(n)*100)
ggplot(Pittsburg_percent,aes(x=Restaurant_Category,y=Percent,fill=Restaurant_Category))+geom_bar(stat='identity')+ggtitle("Restaurants in Pittsburg")+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
tb4<-tb3%>%group_by(new_city,Restaurant_Category)%>%filter(new_city=='Madison')%>%summarise(n=sum(n))
Madison_percent<-mutate(tb4,Percent=n/sum(n)*100)
ggplot(Madison_percent,aes(x=Restaurant_Category,y=Percent,fill=Restaurant_Category))+geom_bar(stat='identity')+ggtitle("Restaurants in Madison")+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
tb4<-tb3%>%group_by(new_city,Restaurant_Category)%>%filter(new_city=='Urbana_Champaign')%>%summarise(n=sum(n))
Urbana_Champaign_percent<-mutate(tb4,Percent=n/sum(n)*100)
ggplot(Urbana_Champaign_percent,aes(x=Restaurant_Category,y=Percent,fill=Restaurant_Category))+geom_bar(stat='identity')+ggtitle("Restaurants in Urbana_Champaign")+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
tb4<-tb3%>%group_by(new_city,Restaurant_Category)%>%filter(new_city=='Phoenix')%>%summarise(n=sum(n))
Phoenix_percent<-mutate(tb4,Percent=n/sum(n)*100)
ggplot(Phoenix_percent,aes(x=Restaurant_Category,y=Percent,fill=Restaurant_Category))+geom_bar(stat='identity')+ggtitle("Restaurants in Phoenix")+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))

##########################################################################################################
### FACET WRAP plots of popular restaurants in each city
#### IL= Urbana Champaign WI=Madison PA=Pittsburg NC=Charlotte NV=Vegas AZ=Phoenix
tb_popular_restaurant<-select(tb,review_count,state,stars,American_Restaurant,Italian_Restaurant,Mexican_Restaurant,Indian_Restaurant,Pakisthani_Restaurant,Chinese_Restaurant)
tb_popular_restaurant<-mutate(tb_popular_restaurant,Restaurant=ifelse((American_Restaurant==1),"AmericanRestaurant",
                       ifelse((Italian_Restaurant==1),"ItalianRestaurant",
                              ifelse((Mexican_Restaurant==1),"MexicanRestaurant",
                                     ifelse((Indian_Restaurant==1),"IndianRestaurant", 
                                            ifelse((Pakisthani_Restaurant==1),"PakisthaniRestaurant",
                                                    ifelse((Chinese_Restaurant==1),"ChineseRestaurant","OtherRestaurant")))))))
plot_popularrestaurant1<-select(tb_popular_restaurant,state,stars,review_count,Restaurant)
treemap(plot_popularrestaurant1,index=c("state","Restaurant"),vSize = "stars",type="index",fun.aggregate="mean",palette = "Set1", title="Restaurant Ratings & Types", fontsize.title = 14)
ggplot(plot_popularrestaurant1,aes(x=stars,fill=Restaurant))+geom_bar(stat="count")+facet_wrap(~Restaurant)+ggtitle('ALL STATES')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
Popularin_PA<-plot_popularrestaurant1%>%filter(state=="'PA'")
ggplot(Popularin_PA,aes(x=stars,fill=Restaurant))+geom_bar(stat="count")+facet_wrap(~Restaurant)+ggtitle('PA')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))  

Popularin_PA<-plot_popularrestaurant1%>%filter(state=="'PA'")
ggplot(Popularin_PA,aes(x=stars))+geom_bar(stat="count")+facet_wrap(~Restaurant)+ggtitle('PA')
Popularin_AZ<-plot_popularrestaurant1%>%filter(state=="'AZ'")
ggplot(Popularin_AZ,aes(x=stars))+geom_bar(stat="count")+facet_wrap(~Restaurant)+ggtitle('AZ')
Popularin_WI<-plot_popularrestaurant1%>%filter(state=="'WI'")
ggplot(Popularin_WI,aes(x=stars))+geom_bar(stat="count")+facet_wrap(~Restaurant)+ggtitle('WI')
Popularin_NC<-plot_popularrestaurant1%>%filter(state=="'NC'")
ggplot(Popularin_NC,aes(x=stars))+geom_bar(stat="count")+facet_wrap(~Restaurant)+ggtitle('NC')
Popularin_NV<-plot_popularrestaurant1%>%filter(state=="'NV'")
ggplot(Popularin_NV,aes(x=stars))+geom_bar(stat="count")+facet_wrap(~Restaurant)+ggtitle('NV')
Popularin_IL<-plot_popularrestaurant1%>%filter(state=="'IL'")
ggplot(Popularin_IL,aes(x=stars))+geom_bar(stat="count")+facet_wrap(~Restaurant)+ggtitle('IL')
############################plotting star ratings & the number of review counts
ggplot(plot_popularrestaurant1,aes(x=stars,y=review_count,fill=Restaurant))+facet_wrap(~Restaurant)+geom_bar(stat="identity")+ggtitle('ALL STATES')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
Popularin_PA<-plot_popularrestaurant1%>%filter(state=="'PA'")
ggplot(Popularin_PA,aes(x=stars,y=review_count))+facet_wrap(~Restaurant)+geom_bar(stat="identity")
ggplot(Popularin_PA,aes(x=stars,y=review_count,fill=Restaurant))+facet_wrap(~Restaurant)+geom_bar(stat="identity")+ggtitle('Pittsburg,PA')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
Popularin_AZ<-plot_popularrestaurant1%>%filter(state=="'AZ'")
ggplot(Popularin_AZ,aes(x=stars,y=review_count,fill=Restaurant))+facet_wrap(~Restaurant)+geom_bar(stat="identity")+ggtitle('Phoenix,AZ')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
Popularin_WI<-plot_popularrestaurant1%>%filter(state=="'WI'")
ggplot(Popularin_WI,aes(x=stars,y=review_count,fill=Restaurant))+facet_wrap(~Restaurant)+geom_bar(stat="identity")+ggtitle('Madison,WI')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
Popularin_NC<-plot_popularrestaurant1%>%filter(state=="'NC'")
ggplot(Popularin_NC,aes(x=stars,y=review_count,fill=Restaurant))+facet_wrap(~Restaurant)+geom_bar(stat="identity")+ggtitle('Charlotte,NC')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
Popularin_NV<-plot_popularrestaurant1%>%filter(state=="'NV'")
ggplot(Popularin_NV,aes(x=stars,y=review_count,fill=Restaurant))+facet_wrap(~Restaurant)+geom_bar(stat="identity")+ggtitle('Las Vegas,NV')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
Popularin_IL<-plot_popularrestaurant1%>%filter(state=="'IL'")
ggplot(Popularin_IL,aes(x=stars,y=review_count,fill=Restaurant))+facet_wrap(~Restaurant)+geom_bar(stat="identity")+ggtitle('Urbana Champaign,IL')+theme(legend.position = "none")+ theme(plot.title = element_text(hjust = 0.5))
#################################################
# Finding the total number of restaurants for each cuisine.
####################################################
a<-sum(tb$American_Restaurant)
b<-sum(tb$Chinese_Restaurant)
r<-sum(tb$Italian_Restaurant)
d<-sum(tb$Mexican_Restaurant)
e<-sum(tb$Indian_Restaurant)
f<-sum(tb$Pakisthani_Restaurant)
g<-c(a,b,r,d,e,f)
count_restaurant<-ggplot(tb_merged,aes(x=g,y=count))
count_restaurant+geom_bar(stat=count,position="dodge")

######################################################################

##########PLOTTING RELATIONSHIP BETWEEN RESTAURANT TYPE AND POPULATION DEMOGRAPHICS
A1<-bind_rows(LasVegas_percent,Charlotte_percent,Pittsburg_percent,Madison_percent,Urbana_Champaign_percent,Phoenix_percent)
A2<-select(A1,new_city,Restaurant_Category,Percent)
A2<-spread(A2,Restaurant_Category,Percent)
A3<-left_join(A2,tb_demographs,by="new_city")
A3<-A3%>% mutate(White_Percent=(`    White`/`Total Population`)*100)%>% mutate(Black_Percent=(`      Black_or_African American`/`Total Population`)*100) %>% mutate(AsianIndian_Percent=(`  Asian_Indian`/`Total Population`)*100) %>% mutate(Chinese_Percent=(` Chinese`/`Total Population`)*100)%>%mutate(Asianallother_Percent=(`Asian_All other`/`Total Population`)*100)%>%mutate(Non_Hispanic_Percent=(`Non Hispianic`/(`Non Hispianic`+`Hispianic_Latino`)*100))
A3<-A3%>%mutate(Hispanic_Percent=(100-Non_Hispanic_Percent))
ggplot(A3,aes(x=Hispanic_Percent,y=Mexican_Restaurant))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Mexican Rest% & Hispanic Population %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Hispanic_MexicanRestRelation<-lm(Mexican_Restaurant~Hispanic_Percent,data=A3)
summary(Hispanic_MexicanRestRelation)
ggplot(A3,aes(x=Hispanic_Percent,y=Mexican_Restaurant+Italian_Restaurant))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Mexican Rest%+Italian Rest% & Hispanic Population %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Hispanic_Mex_ItalianRestRelation<-lm((Mexican_Restaurant+Italian_Restaurant)~Hispanic_Percent,data=A3)
summary(Hispanic_Mex_ItalianRestRelation)
ggplot(A3,aes(x=White_Percent,y=American_Restaurant))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w American Rest% & White Population %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
White_AmericanRestRelation<-lm((American_Restaurant)~(White_Percent),data=A3)
summary(White_AmericanRestRelation)
ggplot(A3,aes(x=White_Percent+Black_Percent,y=American_Restaurant))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w American Rest% & White+Black Population %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
WhiteBlack_AmericanRestRelation<-lm((American_Restaurant)~(White_Percent+Black_Percent),data=A3)
summary(WhiteBlack_AmericanRestRelation)
ggplot(A3,aes(x=AsianIndian_Percent,y=Indian_Restaurant))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w American Rest% & White Population %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Indian_IndianRestRelation<-lm((Indian_Restaurant)~(AsianIndian_Percent),data=A3)
summary(Indian_IndianRestRelation)
ggplot(A3,aes(x=Chinese_Percent,y=(Chinese_Restaurant)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Chinese Rest% & Chinese Population %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Chinese_ChineseRestRelation<-lm((Chinese_Restaurant)~(Chinese_Percent),data=A3)
summary(Chinese_ChineseRestRelation)
ggplot(A3,aes((Chinese_Percent+AsianIndian_Percent+Asianallother_Percent),y=(Chinese_Restaurant+Indian_Restaurant)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Chinese+Indian Rest% & Asian Indian+Chinese+Asian_allother Population %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Asian_AsianRestRelation<-lm((Chinese_Restaurant+Indian_Restaurant)~(Chinese_Percent+AsianIndian_Percent+Asianallother_Percent),data=A3)
summary(Asian_AsianRestRelation)
ggplot(A3,aes(x=Non_Hispanic_Percent,y=American_Restaurant))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Non Hispanic Population & American Restaurant %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
NonHispanic_AmericanRestRelation<-lm((American_Restaurant)~(Non_Hispanic_Percent),data=A3)
summary(NonHispanic_AmericanRestRelation)
ggplot(A3,aes(x=Non_Hispanic_Percent,y=Other_Restaurant))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Non Hispanic Population & Other Restaurant %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
NonHispanic_OtherRestRelation<-lm((Other_Restaurant)~(Non_Hispanic_Percent),data=A3)
summary(NonHispanic_OtherRestRelation)
ggplot(A3,aes((x=White_Percent+Black_Percent),y=Other_Restaurant))+geom_point(aes(colour=new_city))+ stat_smooth(method = "lm", col = "red")
WhiteBalck_OtherRestRelation<-lm((Other_Restaurant)~(White_Percent+Black_Percent),data=A3)
summary(WhiteBalck_OtherRestRelation)
ggplot(A3,aes((x=AsianIndian_Percent+Chinese_Percent+Asianallother_Percent),y=Other_Restaurant))+geom_point(aes(colour=new_city))+ stat_smooth(method = "lm", col = "red")
AllAsian_OtherRestRelation<-lm((Other_Restaurant)~(AsianIndian_Percent+Chinese_Percent+Asianallother_Percent),data=A3)
summary(AllAsian_OtherRestRelation)

###################################################
########## What influences star ratings of each restaurant category##########

A10<-plot_popularrestaurant1
A11<-mutate(A10,star_rating=stars)
A11<-select(A11,state,Restaurant,star_rating)
A11<-A11%>%spread(Restaurant,star_rating)
A11<-A11%>%group_by(state,Restaurant)%>%summarise(avg_star_rating=mean(star_rating))
A12<-spread(A11,Restaurant,avg_star_rating)
A12<-mutate(A12,new_city=ifelse((state=="'AZ'"),"Phoenix",
                                ifelse((state=="'IL'"),"Urbana_Champaign",
                                       ifelse((state=="'NV'"),"LasVegas",
                                              ifelse((state=="'WI'"),"Madison", 
                                                     ifelse((state=="'PA'"),"Pittsburg","Charlotte"))))))

A14<-left_join(A3,A12,by="new_city")
ggplot(A14,aes(x=MexicanRestaurant,y=Hispanic_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Hispanic Population & Mexican Rest Star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Hispanic_MexicanRestRating<-lm(Hispanic_Percent~MexicanRestaurant,data=A14)
summary(Hispanic_MexicanRestRating)

ggplot(A14,aes(x=MexicanRestaurant,y=Non_Hispanic_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Non Hispanic Population & Mexican Rest Star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
NonHispanic_MexicanRestRating<-lm(Hispanic_Percent~MexicanRestaurant,data=A14)
summary(NonHispanic_MexicanRestRating)

ggplot(A14,aes(x=IndianRestaurant,y=AsianIndian_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w AsianIndian Population & IndianRestaurant star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))

AsianIndian_IndianRestRating<-lm(AsianIndian_Percent~IndianRestaurant,data=A14)
summary(AsianIndian_IndianRestRating)

ggplot(A14,aes(x=ChineseRestaurant,y=Chinese_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Chinese Population & Chinese Restaurant star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))

ChineseRestRating<-lm(Chinese_Percent~ChineseRestaurant,data=A14)
summary(ChineseRestRating)

ggplot(A14,aes(x=AmericanRestaurant,y=(White_Percent+Black_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w American Population & American Restaurant star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))

AmericanRestRating<-lm(White_Percent+Black_Percent~AmericanRestaurant,data=A14)
summary(AmericanRestRating)

ggplot(A14,aes(x=ItalianRestaurant,y=Hispanic_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Hispanic Population & Mexican Rest Star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Hispanic_ItalianRestRating<-lm(Hispanic_Percent~ItalianRestaurant,data=A14)
summary(Hispanic_ItalianRestRating)

ggplot(A14,aes(x=OtherRestaurant,y=(White_Percent+Black_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w WhiteBalck Population & Other Rest Star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
OtherRestRating_WB<-lm((White_Percent+Black_Percent)~OtherRestaurant,data=A14)
summary(OtherRestRating_WB)

ggplot(A14,aes(x=OtherRestaurant,y=(Hispanic_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Hispanic Population & Other Rest Star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
OtherRestRating_Hisp<-lm((Hispanic_Percent)~OtherRestaurant,data=A14)
summary(OtherRestRating_Hisp)


ggplot(A14,aes(x=OtherRestaurant,y=(Chinese_Percent+AsianIndian_Percent+Asianallother_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Asian Population & Other Rest Star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
OtherRestRating_Asian<-lm((Chinese_Percent+AsianIndian_Percent+Asianallother_Percent)~OtherRestaurant,data=A14)
summary(OtherRestRating_Asian)

ggplot(A3,aes((x=Non_Hispanic_Percent),y=Other_Restaurant))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Non Hispanic Population & Other Restaurant %")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
OtherRestRating_NonHispanicPop<-lm((Non_Hispanic_Percent)~OtherRestaurant,data=A14)
summary(OtherRestRating_NonHispanicPop)

ggplot(A14,aes(x=IndianRestaurant,y=(White_Percent+Black_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w White+Black Population & IndianRestaurant star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
AsianIndian_IndianRestRating_WB<-lm((White_Percent+Black_Percent)~IndianRestaurant,data=A14)
summary(AsianIndian_IndianRestRating_WB)

ggplot(A14,aes(x=IndianRestaurant,y=(AsianIndian_Percent+Chinese_Percent+Asianallother_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w All Asian Population(Indian,Chinese,Other Asian) & IndianRestaurant star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
AsianIndian_IndianRestRating_Allasian<-lm((AsianIndian_Percent+Chinese_Percent+Asianallother_Percent)~IndianRestaurant,data=A14)
summary(AsianIndian_IndianRestRating_Allasian)

ggplot(A14,aes(x=IndianRestaurant,y=(Chinese_Percent+AsianAllother_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Chinese,Other Asian Population & IndianRestaurant star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
AsianIndian_IndianRestRating_Chinese_Otherasian<-lm((Chinese_Percent+AsianAllother_Percent)~IndianRestaurant,data=A14)
summary(AsianIndian_IndianRestRating_Chinese_Otherasian)

ggplot(A14,aes(x=IndianRestaurant,y=(Hispanic_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w HIspanic Population & IndianRestaurant star Rating")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
AsianIndian_IndianRestRating_HispanicPop<-lm((Hispanic_Percent)~IndianRestaurant,data=A14)
summary(AsianIndian_IndianRestRating_HispanicPop)


############################################################################
###### This is to calculate weighted means for each restaurant category in each state

tb_popular_restaurant<-select(tb,review_count,state,stars,American_Restaurant,Italian_Restaurant,Mexican_Restaurant,Indian_Restaurant,Pakisthani_Restaurant,Chinese_Restaurant)
tb_popular_restaurant<-mutate(tb_popular_restaurant,Restaurant=ifelse((American_Restaurant==1),"AmericanRestaurant",
                                                                      ifelse((Italian_Restaurant==1),"ItalianRestaurant",
                                                                           ifelse((Mexican_Restaurant==1),"MexicanRestaurant",
                                                                                  ifelse((Indian_Restaurant==1),"IndianRestaurant", 
                                                                                         ifelse((Pakisthani_Restaurant==1),"PakisthaniRestaurant",
                                                                                               ifelse((Chinese_Restaurant==1),"ChineseRestaurant","OtherRestaurant")))))))
plot_popularrestaurant2<-select(tb_popular_restaurant,state,stars,review_count,Restaurant)

plot_popularrestaurant2<-mutate(plot_popularrestaurant2,star_review=weighted.mean(stars,review_count))
plot_popularrestaurant2<-select(plot_popularrestaurant2,-star_review)
plot_popularrestaurant2<-mutate(plot_popularrestaurant2,star_rating=stars)
plot_popularrestaurant2<-mutate(plot_popularrestaurant2,rating_count=review_count)
plot_popularrestaurant2<-select(plot_popularrestaurant2,state,Restaurant,star_rating,rating_count)
B1<-filter(plot_popularrestaurant2,state=="'PA'")
B1<-filter(B1,Restaurant=="OtherRestaurant")
B1%>%summarise(n=weighted.mean(star_rating,rating_count))
B1<-filter(plot_popularrestaurant2,state=="'PA'")
B1<-filter(B1,Restaurant=="AmericanRestaurant")
B1%>%summarise(n=weighted.mean(star_rating,rating_count))
B1<-filter(plot_popularrestaurant2,state=="'PA'")
B1<-filter(B1,Restaurant=="ChineseRestaurant")
B1%>%summarise(n=weighted.mean(star_rating,rating_count))
B1<-filter(plot_popularrestaurant2,state=="'PA'")
B1<-filter(B1,Restaurant=="ItalianRestaurant")
B1%>%summarise(n=weighted.mean(star_rating,rating_count))
B1<-filter(plot_popularrestaurant2,state=="'PA'")
B1<-filter(B1,Restaurant=="IndianRestaurant")
B1%>%summarise(n=weighted.mean(star_rating,rating_count))
B1<-filter(plot_popularrestaurant2,state=="'PA'")
B1<-filter(B1,Restaurant=="MexicanRestaurant")
B1%>%summarise(n=weighted.mean(star_rating,rating_count))
###################################################################

######### Demographics Vs Weighted Mean Star Ratings#########
A16<-select(A14,new_city,Non_Hispanic_Percent,Hispanic_Percent,White_Percent,Black_Percent,AsianIndian_Percent,Chinese_Percent,Asianallother_Percent,IndianRestaurant,ChineseRestaurant,ItalianRestaurant,MexicanRestaurant,OtherRestaurant)
A18<-select(A16,new_city,Non_Hispanic_Percent,Hispanic_Percent,White_Percent,Black_Percent,AsianIndian_Percent,Chinese_Percent,Asianallother_Percent)
A20<-read_excel("WeightedMean_StarRatings.xlsx",sheet="Sheet1")
View(A20)
A21<-left_join(A18,A20,by="new_city")
View(A21)

ggplot(A21,aes(x=Mean_MexRest_rating,y=Hispanic_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Hispanic Population % & Mean rating for Mexican Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Hispanic_MexRestRelation<-lm(Hispanic_Percent~Mean_MexRest_rating,data=A21)
summary(Hispanic_MexRestRelation)

ggplot(A21,aes(x=Mean_ItalRest_rating,y=Hispanic_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Hispanic Population % & Mean rating for Italian Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Hispanic_ItalRestRelation<-lm(Hispanic_Percent~Mean_ItalRest_rating,data=A21)
summary(Hispanic_ItalRestRelation)

ggplot(A21,aes(x=Mean_AmerRest_rating,y=White_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w White Population % & Mean rating for American Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
White_AmerRestRelation<-lm(White_Percent~Mean_AmerRest_rating,data=A21)
summary(White_AmerRestRelation)

ggplot(A21,aes(x=Mean_AmerRest_rating,y=(White_Percent+Black_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w White+Black Population % & Mean rating for American Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
White_Black_AmerRestRelation<-lm((White_Percent+Black_Percent)~Mean_AmerRest_rating,data=A21)
summary(White_Black_AmerRestRelation)


ggplot(A21,aes(x=Mean_AmerRest_rating,y=(Black_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Black Population % & Mean rating for American Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Black_AmerRestRelation<-lm((Black_Percent)~Mean_AmerRest_rating,data=A21)
summary(Black_AmerRestRelation)


ggplot(A21,aes(x=Mean_IndianRest_rating,y=(AsianIndian_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Indian Population % & Mean rating for Indian Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Indian_IndianRestRelation<-lm((AsianIndian_Percent)~Mean_IndianRest_rating,data=A21)
summary(Indian_IndianRestRelation)


ggplot(A21,aes(x=Mean_ChinRest_rating,y=(Chinese_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Chinese Population % & Mean rating for ChineseRestaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
Chinese_ChineseRestRelation<-lm((Chinese_Percent)~Mean_ChinRest_rating,data=A21)
summary(Chinese_ChineseRestRelation)


ggplot(A21,aes(x=Mean_AmerRest_rating,y=Non_Hispanic_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Non Hispanic Population % & Mean rating for American Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
NonHispanic_AmerRestRelation<-lm((Non_Hispanic_Percent)~Mean_AmerRest_rating,data=A21)
summary(NonHispanic_AmerRestRelation)


ggplot(A21,aes(x=Mean_OtherRest_rating,y=Non_Hispanic_Percent))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w Non Hispanic Population % & Mean rating for other Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
NonHispanic_OtherRestRelation<-lm((Non_Hispanic_Percent)~Mean_OtherRest_rating,data=A21)
summary(NonHispanic_OtherRestRelation)

ggplot(A21,aes(x=Mean_OtherRest_rating,y=(White_Percent+Black_Percent)))+geom_point(aes(colour=new_city),size=5)+ stat_smooth(method = "lm", col = "red")+ggtitle("Relationship b/w white+black Population % & Mean rating for other Restaurant")+theme(legend.position="top")+theme(plot.title = element_text(hjust = 0.5))
NonHispanic_OtherRestRelation<-lm((White_Percent+Black_Percent)~Mean_OtherRest_rating,data=A21)
summary(NonHispanic_OtherRestRelation)
########################################################################################################################################
########### How to Plot % of each Racial distribution##########

tb_demographs_percent<-tb_demographs

plot_tb_demographs_percent<-tb_demographs_percent %>% mutate(White_Percent=(`    White`/`Total Population`)*100)%>% mutate(Black_Percent=(`      Black_or_African American`/`Total Population`)*100) %>% mutate(AsianIndian_Percent=(`  Asian_Indian`/`Total Population`)*100) %>% mutate(Chinese_Percent=(` Chinese`/`Total Population`)*100)%>%mutate(Asianallother_Percent=(`Asian_All other`/`Total Population`)*100)%>%select(new_city,White_Percent,Black_Percent,AsianIndian_Percent,Chinese_Percent,Asianallother_Percent)

plot_tb_demographs_percent%>%mutate(Other_race_percent=100-(White_Percent+Black_Percent+AsianIndian_Percent+Chinese_Percent+Asianallother_Percent))

plot_tb_demographs_percent_modified<-gather(plot_tb_demographs_percent,"Race","Percent",2:6)

ggplot(plot_tb_demographs_percent_modified,aes(x=new_city,y=Percent))+geom_bar(stat="identity")+facet_wrap(~Race)

ggplot(plot_tb_demographs_percent_modified,aes(x=Race,y=Percent))+geom_bar(stat="identity")+facet_wrap(~new_city)


ggplot(plot_tb_demographs_percent,aes(x=new_city,y=AsianIndian_Percent))+geom_bar(stat="identity")
ggplot(plot_tb_demographs_percent,aes(x=new_city,y=Chinese_Percent))+geom_bar(stat="identity")

######################k-means clustering############
cluster_analysis<-select(tb_merged,attributes.Ambience.upscale,attributes.Parking.valet,attributes.Music.dj,attributes.Good.For.latenight,attributes.Wi.Fi,attributes.Good.for.Kids,attributes.Takes.Reservations,attributes.Noise.Level,attributes.Alcohol,attributes.Good.For.Groups,categories,stars)
View(cluster_analysis)

H1<-cluster_analysis%>%mutate(Ambience=ifelse((attributes.Ambience.upscale=="True"),1,0))
H1<-H1%>%mutate(ValetParking=ifelse((attributes.Parking.valet=="True"),1,0))
H1<-H1%>%mutate(Musicdj=ifelse((attributes.Music.dj=="True"),1,0))
H1<-H1%>%mutate(Latenight=ifelse((attributes.Good.For.latenight=="True"),1,0))
H1<-H1%>%mutate(Goodforkids=ifelse((attributes.Good.for.Kids=="True"),1,0))
H1<-H1%>%mutate(Reservations=ifelse((attributes.Takes.Reservations=="True"),1,0))
H1<-H1%>%mutate(GoodforGroups=ifelse((attributes.Good.For.Groups=="True"),1,0))
H1<-H1%>%mutate(attributes.Noise.Level=str_replace_all(attributes.Noise.Level,'b',''))
H1<-H1%>%mutate(attributes.Alcohol=str_replace_all(attributes.Alcohol,'b',''))
H1<-H1%>%mutate(FullBar=ifelse((attributes.Alcohol=="'full_ar'"),1,0))
H1<-H1%>%mutate(NoiseLevel=ifelse((attributes.Noise.Level!="'quiet'"),1,0))
View(H1)
H2<-select(H1,Ambience,ValetParking,Musicdj,Latenight,Goodforkids,Reservations,GoodforGroups,FullBar,NoiseLevel,stars)
results<-kmeans(H2,15)
##################PLOTTING SCREE PLOT OF WITHINSS VS NUMBER OF CLUSTERS TO DETERMINE OPTIMUM NUMBER OF CLUSTERS#####
H10<-select(H1,Ambience,ValetParking,Musicdj,Latenight,Goodforkids,Reservations,GoodforGroups,FullBar,NoiseLevel,stars)
results1<-rep(0,20)
for(k in 1:20)
{
  results1[k]<-kmeans(H10,nstart=10,centers=k)$tot.withinss
}
results1

plot(1:20, results1, xlab = "Number of clusters",ylab="Within Sum of Squares", col="blue", pch=10, main ="Scree Plot to determine optimal K")
### PLOTTING CLUSTER DENDOGRAM#####
## Computer hangs up with the below run.Clear memory before running##
clusplot(H2,results$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)
distance<-dist(H2,method = "euclidean")
hc.c<-hclust(distance,method="complete")
plot(hc.c,hang=-1)

#######PRINCIPLE COMPONENT ANALYSIS ##############
PCA<-prcomp(H2,center=TRUE,scale=TRUE)
print(PCA)
plot(PCA,type="l")
summary(PCA)
PCA.loadings<-PCA$rotation
pca_func<-function(attr,loadings){
  r<-loadings*attr
  r<-apply(r,2,sum)
  r
}
H5<- t(apply(H2, 1, pca_func, PCA.loadings ))
H6<-as.data.frame(H5)
H6<-cbind(H6,"Number"=1:nrow(H6))
H2<-cbind(H2,"Number"=1:nrow(H2))
H7<-left_join(H2,H6,by="Number")
View(H7)
##PLOTTING PRINCIPLE COMPONENTS#####
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=stars))+scale_colour_gradientn(colours = terrain.colors(10))
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=stars))+scale_colour_gradientn(colours = terrain.colors(10))+ggtitle("PC1 Vs PC2- Distribution by Star rating")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=stars),size=5)+scale_colour_gradientn(colours = terrain.colors(10))+ggtitle("PC1 Vs PC2- Distribution by Star rating")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=FullBar),size=5)+scale_colour_gradientn(colours = terrain.colors(10))+ggtitle("PC1 Vs PC2- Distribution by Fullbar")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=Reservations),size=5)+scale_colour_gradientn(colours = terrain.colors(10))+ggtitle("PC1 Vs PC2- Distribution by Fullbar")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=Reservations),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution by Fullbar")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=Reservations),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution of Reservations")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=FullBar),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution of FullBar")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=ValetParking),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution by ValetParking")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=Ambience),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution by Ambience")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=Musicdj),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution by Musicdj")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=Latenight),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution by Latenight")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=NoiseLevel),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution by NoiseLevel")
ggplot(H7,aes(x=PC1,y=PC2))+geom_point(aes(colour=Goodforkids),size=5)+scale_colour_gradient2()+ggtitle("PC1 Vs PC2- Distribution by Goodforkids")
#####################################################
# Out put data into csv
#install.packages("rio")
# library("rio")
#export(tb,"yelp_academic_dataset_business_clean_2017.csv")
#export(tb_merged,"yelp_academic_dataset_business_demographics_clean_2017.csv") #3/9/17
#export(H7,"final_capstone_data.csv")