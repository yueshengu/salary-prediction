library(rjson)
json_data <- fromJSON(file="C:/Users/ygu/Desktop/columbia/train.json")
length(json_data)
cuisine<-factor(sapply(1:length(json_data),function(i) json_data[[i]]$cuisine))
summary(cuisine) #20 different cuisines (20 response values)
ingredients<-factor(unlist(lapply(1:length(json_data),function(i) json_data[[i]]$ingredients)))
length(levels(ingredients)) #6714 different features


library(data.table)
#salary<-fread("C:/Users/ygu/Desktop/columbia/Train_rev1/Train_rev1.csv")
salary<-data.frame(salary)
class(salary)
summary(salary)
head(salary)



#save(salary,file="C:/Users/ygu/Desktop/columbia/Train_rev1/salary.RData")

salary$Title<-factor(salary$Title)
salary$LocationRaw<-factor(salary$LocationRaw)
salary$LocationNormalized<-factor(salary$LocationNormalized)
salary$ContractType<-factor(salary$ContractType)
salary$ContractTime<-factor(salary$ContractTime)
salary$Company<-factor(salary$Company)
salary$Category<-factor(salary$Category)
salary$SalaryRaw<-factor(salary$SalaryRaw)
salary$SourceName<-factor(salary$SourceName)
summary(salary)
nrow(salary)


length(levels(salary$Title))   #135,386
length(levels(salary$Category)) # 29

# predictors:
# title (cleaned)
# full description (converted into multiple dummy variables)
# Location normalized
# contract type
# contract time
# category