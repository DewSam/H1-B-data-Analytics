#Somia Abdelrahman
#Final Poster presentaion

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(maps)
library(treemapify)
library(RColorBrewer)
library(webr)



############################################

# Reading multiple files and merge them into one dataset/ csv-File
#-Only Run once as the data comes from multiple files each file represents one year

csv_folder <- "C:/Users/Sumaya/Documents/Fall 2023/Information visualization/Project/h1-b/files"


# List all CSV files in the directory
file_list <- list.files(path = csv_folder, pattern = "*.csv", full.names = TRUE)


for (file in file_list){
  
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, header=TRUE, sep=",")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read.csv(file, header=TRUE, sep=",")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
  
}


# Save the DataFrame to a CSV file
write.csv(dataset, file = "C:/Users/Sumaya/Documents/Fall 2023/Information visualization/Project/h1-b/h1b_Visa_updated.csv", row.names = TRUE)

########################################################################################################



##################
#reading dataset
df_full <- read.csv("C:/Users/Sumaya/Documents/Fall 2023/Information visualization/Project/h1-b/h1b_Visa_updated.csv")

#################Data-Cleaning##########################


#Removing year 2009 as the analysis intended to be from 2010 to 2023
df_full <- df_full[df_full$Fiscal.Year != 2009, ]


# adding total application/ total-initials/total-continuing 

#1- total applications submitted per each company
df_full$total <- df_full$Initial.Approvals+ df_full$Initial.Denials + df_full$Continuing.Approvals+ df_full$Continuing.Denials

#2- total **initials** applications submitted per each company

df_full$total.initials <- df_full$Initial.Approvals+ df_full$Initial.Denials 

#3- total **continuing** applications submitted per each company
df_full$total.continuing <- df_full$Continuing.Approvals+ df_full$Continuing.Denials


#4- total **approved** applications

df_full$total.approved <- df_full$Initial.Approvals+ df_full$Continuing.Approvals

#5- total **denied** applications
df_full$total.denied <- df_full$Initial.Denials + df_full$Continuing.Denials




######################Distribution#############################


#Employers distribution over years in the dataset
ggplot(df_full, aes(x = reorder(Fiscal.Year, table(Fiscal.Year)[Fiscal.Year]))) +
  geom_bar() + theme_minimal() + ggtitle("Employers distribution over years in the dataset")+ xlab("year") + ylab("Frequency")


#Distribution - Continuous 
#1
ggplot(df_full) + aes(y= Initial.Approvals) + 
  geom_boxplot() + theme_minimal() + ggtitle("Initial Approvals Distribution")

#2
ggplot(df_full) + aes(y= Initial.Denials) + 
  geom_boxplot() + theme_minimal() + ggtitle("Initial Denials Distribution")



#############################Distribution-2#############################
#install.packages("webr")

df_stat <- df_full  %>% summarise_at(vars(Initial.Approvals, Initial.Denials,Continuing.Approvals, Continuing.Denials  ), sum, na.rm=TRUE)


df_stat$ID <- seq(nrow(df_stat))

data_long_2 <- pivot_longer(df_stat,col= -ID, names_to = "Status", values_to = "Count")


data_long_2$Type <- ifelse(data_long_2$Status %in% c("Initial.Approvals","Initial.Denials" ), "Initial" , "Continuing")

PieDonut(data_long_2, aes(Type, Status, count=Count), title = "H1-B Applications by Type and Status",
         r0 = 0)

#######################################################################


################################################################





#################H1-B-Trend-Analysis######################
df_year <- df_full %>% group_by(Fiscal.Year) %>% summarise_at(vars(total, Initial.Approvals, Initial.Denials,Continuing.Approvals, Continuing.Denials  ), sum, na.rm=TRUE)


summary(df_year)

data_long <- pivot_longer(df_year, cols = -Fiscal.Year, names_to = "Variable", values_to = "Value")

filtered_data_initials <- subset(data_long, (Variable %in% c("Initial.Approvals", "Initial.Denials")))

filtered_data_continuing <- subset(data_long, (Variable %in% c("Continuing.Approvals", "Continuing.Denials")))

#Distribution of initial/continung approval/denial - Not included in the poster just for analysis
ggplot(data_long) + aes(y= Value, color = Variable) + 
  geom_boxplot() + theme_minimal() + ggtitle("Initial Distribution by year")



#Trend
filtered_data_initials 

p1 <- ggplot(filtered_data_initials, aes(x =Fiscal.Year , y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", alpha =0.8) +
  geom_line(aes(group = Variable, color = Variable), position = position_dodge(width = 0.9))+
  geom_point(aes(color = Variable ), position = position_dodge(width = 0.9))+
  scale_fill_manual(values = c("Initial.Approvals" = "#17234a", "Initial.Denials" = "#d3313d"))+
  labs(title = "Inital Approvedd VS Denied Petitions", x = "year", y = "Count") +
  theme_minimal()+ ylim(0, 350000) + theme(legend.position = "none")




filtered_data_continuing

p2 <- ggplot(filtered_data_continuing, aes(x =Fiscal.Year , y = Value, fill = Variable)) +
  geom_bar(stat = "identity", position = "dodge", alpha =0.8) +
  geom_line(aes(group = Variable, color = Variable), position = position_dodge(width = 0.9))+
  geom_point(aes(color = Variable ), position = position_dodge(width = 0.9))+
  scale_fill_manual(values = c("Continuing.Approvals" = "#17234a", "Continuing.Denials" = "#d3313d"))+
  labs(title = "Continuing Approvedd VS Denied Petitions", x = "year", y = "Count") +
  theme_minimal() +   ylim(0, 350000) +theme(legend.position = "none")

grid.arrange(p1, p2, nrow = 1)  # 1 row, 2 columns





####################################MAPS-Geographical-Distribution########################################

map_data("state")

df_full$State_Full <- gsub("^\\s+|\\s+$", "",df_full$State_Full) #global substitute 
df_full$State_Full <- tolower(df_full$State_Full) #global substitute 



colnames(df_full)[colnames(df_full) == "State_Full"] <- "region"

#Total Initial Approved applications by states
agg.data.approval.state <- aggregate(df_full$Initial.Approvals, list(region = df_full$region), sum, na.rm=TRUE)

colnames(agg.data.approval.state)[2] <- "total_initial_approvals"

map.data <- left_join(map_data("state"), agg.data.approval.state, by="region")

ggplot(map.data, aes(long,lat, group=group)) + 
  geom_polygon(aes(fill=total_initial_approvals), color = "white") +
  scale_fill_gradientn(colours = brewer.pal(8,"Blues"),
                       na.value = "azure2") +xlab("") + ylab("")+
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank())+
  ggtitle("H1-B visa approvals by states")+
  coord_map("albers", at0=45.5, lat1=29.5)



#Total Initial Approved applications by states -yearly
agg.data.approval.state.year <- aggregate(df_full$Initial.Approvals, list(region = df_full$region, year = df_full$Fiscal.Year), sum, na.rm=TRUE)

colnames(agg.data.approval.state.year)[3] <- "total_initial_approvals"

map.data.year <- left_join(map_data("state"), agg.data.approval.state.year, by="region")


options(scipen =99 )

# Create individual maps for each category using a loop
maps_list <- list()
categories <- unique(map.data.year$year)  


#display.brewer.all()

for (i in seq_along(categories)) {
  current_category <- categories[i]
  
  maps_list[[i]] <- ggplot(map.data.year[map.data.year$year == current_category,], aes(long,lat, group=group)) + 
    geom_polygon(aes(fill=total_initial_approvals), color = "white") +
    scale_fill_gradientn(colours = brewer.pal(8,"Blues"),
                         na.value = "azure2") +xlab("") + ylab("")+
    theme(legend.position = "none",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())+
    ggtitle(paste0(current_category))+
    coord_map("albers", at0=45.5, lat1=29.5)
}


maps_list

# Arrange plots in a grid
grid.arrange( maps_list[[6]],maps_list[[7]],maps_list[[8]],maps_list[[9]],maps_list[[10]],maps_list[[11]],maps_list[[12]], maps_list[[13]],maps_list[[14]], nrow = 3, ncol = 3)  # 1 row, 2 columns




#################################Cities###############################################

#Top Cities
#10
agg.data.approval.city <- aggregate(df_full$Initial.Approvals, list(City = df_full$City, State =df_full$State ), sum, na.rm=TRUE)
colnames(agg.data.approval.city)[3] <- "total_initial_approvals"

agg.data.approval.state <- aggregate(df_full$Initial.Approvals, list( State =df_full$State ), sum, na.rm=TRUE)
colnames(agg.data.approval.state)[2] <- "total_initial_approvals"




agg.data.approval.city <- agg.data.approval.city[agg.data.approval.city$total_initial_approvals > 10000, ]

#City
ggplot(agg.data.approval.city)+  geom_bar( aes(x = reorder(City, -total_initial_approvals), y = total_initial_approvals),fill= "#d3313d",stat = "identity", show.legend = FALSE) +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

#State
agg.data.approval.state
agg.data.approval.state <- agg.data.approval.state[agg.data.approval.state$total_initial_approvals > 10000, ]


ggplot(agg.data.approval.state)+  geom_bar( aes(x = reorder(State, -total_initial_approvals), y = total_initial_approvals),fill= "#d3313d",stat = "identity", show.legend = FALSE) +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

summary(agg.data.approval.state)

#agg.data.approval.state[agg.data.approval.state$total_initial_approvals == 264486, ]

#sorted_df <- agg.data.approval.state[order(-agg.data.approval.state$total_initial_approvals), ]

# Show the sorted dataframe
#print(sorted_df)

##################################TOP-Companies########################################################


agg.data.approval.employer <- aggregate(df_full$Initial.Approvals, list( Employer = df_full$Employer), sum, na.rm=TRUE)
colnames(agg.data.approval.employer)[2] <- "total_initial_approvals"

agg.data.approval.employer <- agg.data.approval.employer[agg.data.approval.employer$total_initial_approvals > 3000, ]

ggplot(agg.data.approval.employer)+  geom_bar( aes(x = reorder(Employer, total_initial_approvals), y = total_initial_approvals),stat = "identity",  fill= "#17234a", show.legend = FALSE) +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip()


######################################################################################




################################TOP Industry##########################################
agg.data.approval.industry <- aggregate(df_full$Initial.Approvals, list(industry = df_full$Industry), sum, na.rm=TRUE)

colnames(agg.data.approval.industry)[2] <- "total_initial_approvals"

ggplot(agg.data.approval.industry) + aes(area = total_initial_approvals,label= industry)+
  geom_treemap() +
  #geom_treemap_text(aes(label= industry), color = "white") +
  #scale_fill_brewer(palette = "Set3") +  # Set the color palette
  labs(title = "Top Industries")+
  geom_treemap_text(grow = F, reflow = T, colour = "white", 
                    place = "centre", size = 14)
######################################################################################


