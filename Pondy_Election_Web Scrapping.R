library(rvest)
library(dplyr)
library(XML)
library(ggplot2)
library(RColorBrewer)
library(car)
library(grid)
#library(sqldf)
library(ggrepel)

# Creating vector to store URL's
URL <- vector(mode = "character",length = 30)

# Creating list to store all the values from tables
my.list <- list()

# Creating empty dataframe to store winner and runner
Winner <- data.frame()
Runner <- data.frame()

# Framing the URL and store it in vector
for (Assembly_Constituency_No in c(1:30)){
  URL[Assembly_Constituency_No] <- paste("http://eciresults.nic.in/ConstituencywiseU07",Assembly_Constituency_No,".htm?ac=",Assembly_Constituency_No,sep = "")
}

#Accessing each Constituency URl and store the results in list
for(i in 1:30) {
  
  # Accessing the URL one by one
  input <- read_html(URL[i])
  
  # Extracting the values from table in HTML page
  tables2 <- input %>% html_nodes("#div1 table:nth-child(1)") %>% html_table()
  
  #Converting the table to data.frame
  final_output <- data.frame(tables2[[1]])
  
  # Takingthe Constituency_Name for future need
  Constituency_Name <- final_output[1,1]
  
  # Removing the NA rows from dataframe
  final_output <- na.omit(final_output)
  
  # Making the first row as Column name & removed the first row
  colnames(final_output) <- final_output[1,]
  
  final_output <- final_output[-1,]
  
  # Adding the Constituency_Name to result data.frame
  final_output$Constituency <- Constituency_Name
  
  # Adding the result data.frame to list
  my.list[[i]] <- final_output
  
  # Storing the winner and runner in Sperate  dataframe
  Winner <-rbind(Winner,final_output[1,])
  Runner <- rbind(Runner,final_output[2,])
  
}

# binding all the result data.frames in list to single data.frame
big_data <- do.call(rbind,my.list) 

#str(big_data)

# change the Votes column data type to numeric
big_data$Votes <-as.numeric(big_data$Votes)
Winner$Votes <- as.numeric(Winner$Votes)
Runner$Votes <- as.numeric(Runner$Votes)

#shrink the row numbers
rownames(big_data) <- NULL
rownames(Winner) <- NULL
rownames(Runner) <- NULL

# Altering the value in Constituency cloumn of big_data data.frame
big_data$Constituency<- gsub("Puducherry - ","",big_data$Constituency)
Winner$Constituency <- gsub("Puducherry - ","",Winner$Constituency)
Runner$Constituency <- gsub("Puducherry - ","",Runner$Constituency)


# Adding partyShortName column to big_data data.frame
big_data$PartyShortName <- recode(big_data$Party,"'Indian National Congress'='INC';'All India N.R. Congress'='AINRC';'All India Anna Dravida Munnetra Kazhagam'='AIADMK';'Dravida Munnetra Kazhagam'='DMK';'Independent'='IND';'Bharatiya Janata Party'='BJP';'None of the Above'='NOTA';'Communist Party of India'='CPI';
                                  'Pattali Makkal Katchi'='PMK';'Viduthalai Chiruthaigal Katchi'='VCK';'Naam Tamilar Katchi'='NTK';'Communist Party of India  (Marxist)'='CPI(M)';'Desiya Murpokku Dravida Kazhagam'='DMDK';'SOCIAL DEMOCRATIC PARTY OF INDIA'='SDPI';'Marumalarchi Dravida Munnetra Kazhagam'='MDMK';'Indiya Jananayaka Katchi'='IJK';'Bahujan Samaj Party'='BSP';'Agila India Makkal Kazhagam'='AIMK';'Communist Party of India  (Marxist-Leninist)  (Liberation)'='CPI(ML)';'Tamizhaga Vaazhvurimai Katchi'='TVK';'Revolutionary Socialist Party'='RSP';'Janata Dal  (United)' ='JD(U)'")  

Winner$PartyShortName <- recode(Winner$Party,"'Indian National Congress'='INC';'All India N.R. Congress'='AINRC';'All India Anna Dravida Munnetra Kazhagam'='AIADMK';'Dravida Munnetra Kazhagam'='DMK';'Independent'='IND';'Bharatiya Janata Party'='BJP';'None of the Above'='NOTA';'Communist Party of India'='CPI';
                                'Pattali Makkal Katchi'='PMK';'Viduthalai Chiruthaigal Katchi'='VCK';'Naam Tamilar Katchi'='NTK';'Communist Party of India  (Marxist)'='CPI(M)';'Desiya Murpokku Dravida Kazhagam'='DMDK';'SOCIAL DEMOCRATIC PARTY OF INDIA'='SDPI';'Marumalarchi Dravida Munnetra Kazhagam'='MDMK';'Indiya Jananayaka Katchi'='IJK';'Bahujan Samaj Party'='BSP';'Agila India Makkal Kazhagam'='AIMK';'Communist Party of India  (Marxist-Leninist)  (Liberation)'='CPI(ML)';'Tamizhaga Vaazhvurimai Katchi'='TVK';'Revolutionary Socialist Party'='RSP';'Janata Dal  (United)' ='JD(U)'")  

Runner$PartyShortName <- recode(Runner$Party,"'Indian National Congress'='INC';'All India N.R. Congress'='AINRC';'All India Anna Dravida Munnetra Kazhagam'='AIADMK';'Dravida Munnetra Kazhagam'='DMK';'Independent'='IND';'Bharatiya Janata Party'='BJP';'None of the Above'='NOTA';'Communist Party of India'='CPI';
                                'Pattali Makkal Katchi'='PMK';'Viduthalai Chiruthaigal Katchi'='VCK';'Naam Tamilar Katchi'='NTK';'Communist Party of India  (Marxist)'='CPI(M)';'Desiya Murpokku Dravida Kazhagam'='DMDK';'SOCIAL DEMOCRATIC PARTY OF INDIA'='SDPI';'Marumalarchi Dravida Munnetra Kazhagam'='MDMK';'Indiya Jananayaka Katchi'='IJK';'Bahujan Samaj Party'='BSP';'Agila India Makkal Kazhagam'='AIMK';'Communist Party of India  (Marxist-Leninist)  (Liberation)'='CPI(ML)';'Tamizhaga Vaazhvurimai Katchi'='TVK';'Revolutionary Socialist Party'='RSP';'Janata Dal  (United)' ='JD(U)'")  

##############################################################################
# Getting Voter list from EC website

# Reading the HTML page
EC_input <- read_html("http://ceopuducherry.py.gov.in/rolls/ac.htm")

#Extracting the values from table in HTML page
Voter_List_table <- EC_input %>% html_table(fill = TRUE)

#Converting the table to data.frame
final_Voter_List <- data.frame(Voter_List_table[[1]])

# Adusting the 2nd to one cell right

# Initialize a vector
Second_row <- vector(mode = "character",length = 12)

# store the values from 2nd row to that vector
for(i in 1:12)  {
  Second_row[i] <- final_Voter_List[2,i]
}

# Now adjust the 2nd row
for (i in 2:length(Second_row)+1) {
  final_Voter_List[2,i] <- Second_row[i-1]
}

final_Voter_List[2,1] <- NA

# Subsetting the final_Voter_List data frame by column names
Sample_Voter_List <- final_Voter_List[2:33,which(names(final_Voter_List) %in% c("X1","X10","X11","X12","X13"))]

# Setting The column name
Sample_Voter_List[1,1] <- "Constituency"

# Setting the first row as colun name
colnames(Sample_Voter_List) <- Sample_Voter_List[1,]

Sample_Voter_List <- Sample_Voter_List[-1,]

# Changing the column type
Sample_Voter_List$Total <- as.numeric(Sample_Voter_List$Total)
Sample_Voter_List$Male  <- as.numeric(Sample_Voter_List$Male)
Sample_Voter_List$Female <- as.numeric(Sample_Voter_List$Female) 
Sample_Voter_List$`Third Gender` <- as.numeric(Sample_Voter_List$`Third Gender`)

# Removing the numeric character in first cloumn
Sample_Voter_List$Constituency <- gsub("[[:digit:][:punct:]]","",Sample_Voter_List$Constituency)

# Re-ordered the row numbers
row.names(Sample_Voter_List) <- NULL

# Removing the white space in first column of Sample_Voter_List data rame
trim.leading <- function (x)  sub("^\\s+", "", x)

Sample_Voter_List$Constituency <- trim.leading(Sample_Voter_List$Constituency)

# Changing the Misspelt Constituenc in Sample_Voter_list data frame
Sample_Voter_List$Constituency <- recode(Sample_Voter_List$Constituency,"'THIRUBUVANAI'='THIRUBHUVANAI' ; 'ORLEANPETH'='ORLEAMPETH' ; 'NERAVY TR PATTINAM'='NERAVY T.R. PATTINAM'")
