library(tidyverse)
library("readxl")

Data<-read_excel("C:/Users/khush/OneDrive/Desktop/Loan Prediction Dataset1.xlsx")
View(Data)

#Check the Structure of the Dataset
str(Data)

#Summary Statistics of the Dataset
print(summary(Data))
view(summary(Data))

#Check the Missing Values
colSums(is.na(Data))

#Remove rows with missing values using na.omit()
Data=na.omit(Data)

#Q.1) What is the distribution of Loan Status?
Loan_Status_Counts=table(Data$Loan_Status)
view(Loan_Status_Counts)    # Count the number of approvals and rejections

#Visualization (Bar Plot)
ggplot(Data,aes(x=Loan_Status)) +
  geom_bar(fill="skyblue",color="black") +
  labs(title="Distribution of Loan Status",x="Loan Status",y="Count") +
  theme_minimal()

#Q.2) What is the average loan amount across different property areas?
#Group by Property_Area and calculate mean loan amount
Avg_Loan_Amount=aggregate(Loan_Amount ~ Property_Area,data=Data,FUN=mean,na.rm=TRUE)

#Visualization (Bar Plot)
ggplot(Avg_Loan_Amount,aes(x=Property_Area,y=Loan_Amount,fill=Property_Area)) +
  geom_bar(stat="identity",color="black") +
  geom_text(aes(label=Loan_Amount),vjust=-0.5,color="black",size=3)+
  labs(title="Average Loan Amount by Property Area",x="Property Area",y="Average Loan Amount") +
  theme_minimal()

#Q.3) What is the relationship between applicant income and loan amount?
#Visualization (Scatter Plot)
ggplot(Data,aes(x=Applicant_Income,y=Loan_Amount)) +
  geom_point(color="blue",alpha=0.6) +
  labs(title="Applicant Income vs Loan Amount",x="Applicant Income",y="Loan Amount")+
  theme_minimal()

#Q.4) Does Credit History influence Loan Status?
#Group by Credit_History and Loan_Status
Credit_VS_Loan=table(Data$Credit_History,Data$Loan_Status)

#Visualization (Scatter Plot)
ggplot(Data,aes(x=factor(Credit_History),fill=Loan_Status))+
  geom_bar(position="fill")+
  labs(title="Credit History vs Loan Status",x="Credit History",y="Proportion") +
  scale_fill_brewer(palette="Set2")+
  theme_minimal()

#Q.5) What is the distribution of Loan Amount?
#Visualization (Histogram)
ggplot(Data,aes(x = Loan_Amount))+
  geom_histogram(binwidth=20,fill="lightgreen",color="black") +
  labs(title="Distribution of Loan Amount",x="Loan Amount",y="Frequency")+
  theme_minimal()

#Q.6) How does Loan Status vary by Education and Gender?
#Visualization (Bar Plot)
ggplot(Data,aes(x=Education,fill=Loan_Status))+
  geom_bar(position="dodge")+
  facet_wrap(~ Gender)+
  labs(title="Loan Status by Education and Gender",x="Education Level",y="Count")+
  scale_fill_brewer(palette="Set1")+
  theme_minimal()

#Q.7) What is the gender distribution of loan applicants?
#Visualization (Bar Plot)
ggplot(Data,aes(x=Gender)) +
  geom_bar(fill="pink") +
  labs(title="Gender Distribution",x="Gender",y="Count")

#Q.8) What is the relationship between education and loan approval?
#Visualization (Bar Plot)
ggplot(Data,aes(x=Education,fill=Loan_Status))+
  geom_bar(position="dodge")+
  labs(title="Education vs Loan Approval",x="Education",y="Count")

#Q.9) How does marital status affect loan approval?
#Visualization (Bar Plot)
ggplot(Data,aes(x=Married,fill=Loan_Status))+
  geom_bar(position="dodge")+
  labs(title="Marital Status vs Loan Approval",x="Marital Status",y= "Count")

#Q.10) What is the density distribution of loan amounts for approved and non-approved loans?
#Visualization (Density Plot)
ggplot(Data,aes(x=Loan_Amount,fill=Loan_Status)) +
  geom_density(alpha=0.6)+
  labs(title="Density Distribution of Loan Amount by Loan Status",
       x="Loan Amount",
       y="Density")+
  scale_fill_manual(values=c("N"="red","Y"="green"),labels=c("Not Approved","Approved"))

#Q.11) What is the correlation between numerical variables in the dataset?
library(reshape2)

#Select Numerical Variables
Numerical_Data=Data[,c("Applicant_Income", "Coapplicant_Income", "Loan_Amount", 
                       "Loan_Amount_Term", "Credit_History")]

#Compute Correlation Matrix
Cor_Matrix=cor(Numerical_Data,use ="complete.obs")

#Melt the Correlation Matrix for ggplot2
Cor_Melt=melt(Cor_Matrix)

#Visualization (Heatmap) 
ggplot(Cor_Melt,aes(x=Var1,y=Var2,fill=value))+
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Q.12) How do applicant income, Coapplicant income, loan amount, and loan term relate to each other and to loan approval status?
library(GGally)

#Selecting Relevant Columns
Selected_Columns=Data[,c("Applicant_Income", "Coapplicant_Income", "Loan_Amount", "Loan_Amount_Term", "Loan_Status")]

#Converting Loan_Status to factor for better visualization
Selected_Columns$Loan_Status <- as.factor(Selected_Columns$Loan_Status)

#Visualization (Pair Plot) 
ggpairs(
  Selected_Columns,
  aes(color=Loan_Status,alpha=0.6),
  upper=list(continuous="points"),
  lower=list(continuous="smooth")
)+
  labs(title="Pair Plot of Numeric Variables by Loan Status")

#Q.13) What is the proportion of applicants from different property areas (Urban, Semi-Urban, Rural)?
# Calculate proportions
Property_Area_Counts=table(Data$Property_Area)
Property_Area_Proportions=round(100 * prop.table(Property_Area_Counts), 1)

#Visualization (Pie Chart)
pie(Property_Area_Counts,
    labels=paste0(names(Property_Area_Counts),"(",Property_Area_Proportions,"%)"),
    main="Proportion of Applicants by Property Area",
    col=c("skyblue", "lightgreen", "coral"))

#Q.14)How does the average loan amount change with the loan term?
# Calculate average loan amount for each loan term
Avg_Loan_By_Term=Data %>%
  group_by(Loan_Amount_Term) %>%
  summarize(Average_Loan_Amount=mean(Loan_Amount, na.rm = TRUE))

#Visualization (Line Chart)
ggplot(Avg_Loan_By_Term,aes(x = Loan_Amount_Term,y=Average_Loan_Amount))+
  geom_line(color="blue",size=1)+
  geom_point(color="red",size=2)+
  labs(title="Average Loan Amount vs Loan Term", 
       x="Loan Term (Months)", 
       y="Average Loan Amount") +
  theme_minimal()
#Q.15) What is the distribution of property area types?
#Visualization (Bar Chart)
ggplot(Data, aes(x = Property_Area)) +
  geom_bar(fill = "coral") +
  labs(title = "Property Area Distribution", x = "Property Area", y = "Count")

#Q.16) Are there outliers in applicant income?
#Visualization (Box Plot)
ggplot(Data, aes(x = "", y = Applicant_Income)) +
  geom_boxplot(fill = "cyan") +
  labs(title = "Outliers in Applicant Income", x = "", y = "Applicant Income")

#Q.17) What is the relationship between dependents and loan amount?
#Visualization (Box Plot)
ggplot(Data, aes(x = Dependents, y = Loan_Amount, fill = Dependents)) +
  geom_boxplot() +
  labs(title = "Dependents vs Loan Amount", x = "Dependents", y = "Loan Amount")

#Q.18) How does loan term vary across credit history statuses?
#Visualization (Box Plot)
ggplot(Data, aes(x = as.factor(Credit_History), y = Loan_Amount_Term)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Loan Term by Credit History", x = "Credit History", y = "Loan Term")

#Q.19) What is the credit history distribution?
#Visualization (Bar Graph)
ggplot(Data, aes(x = as.factor(Credit_History))) +
  geom_bar(fill = "purple") +
  labs(title = "Credit History Distribution", x = "Credit History", y = "Count")

#Q.20) How does property area correlate with credit history?
#Visualization (Bar Graph)
ggplot(Data, aes(x = Property_Area, fill = as.factor(Credit_History))) +
  geom_bar(position = "dodge") +
  labs(title = "Property Area vs Credit History", x = "Property Area", y = "Count")


# Performing Boston Analysis
library(caret)       # For Predictive Modelling
library(MASS)        # Contain Boston Housing Dataset
library(pROC) 

# Encode categorical variables
Data$Gender=as.factor(Data$Gender)
Data$Married=as.factor(Data$Married)
Data$Dependents=as.factor(Data$Dependents)
Data$Education=as.factor(Data$Education)
Data$Self_Employed=as.factor(Data$Self_Employed)
Data$Property_Area=as.factor(Data$Property_Area)
Data$Loan_Status=as.factor(Data$Loan_Status)

#Split the dataset into training (80%) and testing (20%) sets
set.seed(42)
trainIndex=createDataPartition(Data$Loan_Status,p=0.8,list=FALSE)
trainData=Data[trainIndex,]
testData=Data[-trainIndex,]

#Train a logistic regression model
model=glm(Loan_Status ~ ., data = trainData, family = binomial)

#Make predictions on the test set
testData$predicted=predict(model,newdata=testData,type="response")
testData$predicted_class=ifelse(testData$predicted > 0.5, "1", "0")

#Evaluate the model
accuracy=mean(testData$predicted_class==testData$Loan_Status)
roc_curve=roc(as.numeric(testData$Loan_Status),testData$predicted)
auc=auc(roc_curve)

# Print results
print(paste("Accuracy:",round(accuracy, 4)))
print(paste("AUC-ROC:",round(auc, 4)))

# Calculate Accuracy
accuracy=mean(testData$predicted_class==testData$Loan_Status)

print(paste("Accuracy:",round(accuracy,4)))

# Generate a Confusion Matrix
Confusion_Matrix=ConfusionMatrix(as.factor(testData$predicted_class),testData$Loan_Status)
print(Confusion_Matrix)

# Optional: Plot the ROC Curve
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
