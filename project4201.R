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
salary$ContractType[salary$ContractType=='']<-'Missing'
salary$ContractType<-factor(salary$ContractType)
salary$ContractTime[salary$ContractTime=='']<-'Missing'
salary$ContractTime<-factor(salary$ContractTime)
salary$Company<-factor(salary$Company)
salary$Category<-factor(salary$Category)
salary$SalaryRaw<-factor(salary$SalaryRaw)
salary$SourceName<-factor(salary$SourceName)


salary$TitleNurse<-salary$TitleTeacher<-salary$TitleChef<-salary$TitleOfficer<-salary$TitleAdvisor<-
  salary$TitlePartner<-salary$TitleReceptionist<-salary$TitleDeveloper<-salary$TitleClerk<-
  salary$TitleWorker<-salary$TitleSurveyor<-salary$TitleAnalyst<-salary$TitleConsultant<-
  salary$TitleAdministrator<-salary$TitleAssistant<-salary$TitleCleaner<-salary$TitleController<-
  salary$TitleEngineer<-salary$TitleExecutive<-salary$TitleAccountant<-salary$TitleManager<-
  salary$TitleOperative<-salary$TitleSecretary<-salary$TitleSenior<-0
salary$TitleNurse[grepl('nurse',tolower(salary$Title))]<-1
salary$TitleTeacher[grepl('teacher',tolower(salary$Title))]<-1
salary$TitleChef[grepl('chef',tolower(salary$Title))]<-1
salary$TitleOfficer[grepl('officer',tolower(salary$Title))]<-1
salary$TitleAdvisor[grepl('advisor',tolower(salary$Title))]<-1
salary$TitlePartner[grepl('partner',tolower(salary$Title))]<-1
salary$TitleReceptionist[grepl('receptionist',tolower(salary$Title))]<-1
salary$TitleDeveloper[grepl('developer',tolower(salary$Title))]<-1
salary$TitleClerk[grepl('clerk',tolower(salary$Title))]<-1
salary$TitleWorker[grepl('worker',tolower(salary$Title))]<-1
salary$TitleSurveyor[grepl('surveyor',tolower(salary$Title))]<-1
salary$TitleAnalyst[grepl('analyst',tolower(salary$Title))]<-1
salary$TitleConsultant[grepl('consultant',tolower(salary$Title))]<-1
salary$TitleAdministrator[grepl('administrator',tolower(salary$Title))]<-1
salary$TitleAssistant[grepl('assistant',tolower(salary$Title))]<-1
salary$TitleCleaner[grepl('cleaner',tolower(salary$Title))]<-1
salary$TitleController[grepl('controller',tolower(salary$Title))]<-1
salary$TitleEngineer[grepl('engineer',tolower(salary$Title))]<-1
salary$TitleExecutive[grepl('executive',tolower(salary$Title))]<-1
salary$TitleAccountant[grepl('accountant',tolower(salary$Title))]<-1
salary$TitleManager[grepl('manager',tolower(salary$Title))]<-1
salary$TitleOperative[grepl('operative',tolower(salary$Title))]<-1
salary$TitleSecretary[grepl('secretary',tolower(salary$Title))]<-1
salary$TitleSenior[grepl('senior',tolower(salary$Title))]<-1

summary(salary$LocationNormalized)[1:30]
summary(salary$Category)
summary(salary)
nrow(salary)


length(levels(salary$Title))   #135,386
length(levels(salary$LocationNormalized))   #2732
length(levels(salary$Category)) # 29




library(qdap)
docs = Corpus(VectorSource(salary$FullDescription))
# remove numbers
docs <- tm_map(docs, removeNumbers)
# remove stop words
docs <- tm_map(docs, removeWords, stopwords("SMART"))
# make all letters lower case
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeWords,c("the","then","this"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
dtm <- DocumentTermMatrix(docs)
dim(dtm) #244768 241298
a<-as.matrix(dtm)

save(dtm,file="C:/Users/ygu/Desktop/columbia/Train_rev1/dtm.RData")



dtms <- removeSparseTerms(dtm, 0.9)
save(dtms,file="C:/Users/ygu/Desktop/columbia/Train_rev1/dtms.RData")
dim(as.matrix(dtms)) #244768    209
colnames(as.matrix(dtms))

dtmsData.frame<-data.frame(as.matrix(dtms))

salaryClean<-data.frame(salary,dtmsData.frame)
save(salaryClean,file="C:/Users/ygu/Desktop/columbia/Train_rev1/salaryClean.RData")
set.seed(8)
trainId<-sample(1:nrow(salaryClean),round(.7*nrow(salaryClean)))
save(trainId,file="C:/Users/ygu/Desktop/columbia/Train_rev1/trainId.RData")
trainSalary<-salaryClean[trainId,]
testSalary<-salaryClean[-trainId,]
save(trainSalary,file="C:/Users/ygu/Desktop/columbia/Train_rev1/trainSalary.RData")
save(testSalary,file="C:/Users/ygu/Desktop/columbia/Train_rev1/testSalary.RData")



freq <- colSums(as.matrix(dtms))
ord <- order(freq)
# most frequent terms
freq[tail(ord,n=15)]


inspect(docs)
qdocs <-data.frame(text=unlist(sapply(docs, `[`, "content")), 
                   stringsAsFactors=F)
# remove misc anomalies and white spaces
qdocs$text <- clean(Trim(qdocs$text))
# replace symbols with words a.k.a. % with 'percent'
qdocs$text <- replace_symbol(qdocs$text)
# replace other abbreviations
qdocs$text <- replace_abbreviation(qdocs$text)
# remove all abbreviated middle names
qdocs$text <- mgsub(paste0(" ",letters,".")," ",qdocs$text)


qdocs$text
salary$FullDescription[1]













install.packages("openNLPmodels.en",
                 repos = "http://datacube.wu.ac.at/",
                 type = "source")

library(NLP)
library(openNLP)
library(magrittr)

bio <- as.String(salary$FullDescription[1])
word_ann <- Maxent_Word_Token_Annotator()
sent_ann <- Maxent_Sent_Token_Annotator()
bio_annotations <- annotate(bio, list(sent_ann, word_ann))
class(bio_annotations)
head(bio_annotations)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)
sents(bio_doc) %>% head(2)
words(bio_doc) %>% head(10)

person_ann <- Maxent_Entity_Annotator(kind = "person")
location_ann <- Maxent_Entity_Annotator(kind = "location")
organization_ann <- Maxent_Entity_Annotator(kind = "organization")
pipeline <- list(sent_ann,
                 word_ann,
                 person_ann,
                 location_ann,
                 organization_ann)
bio_annotations <- annotate(bio, pipeline)
bio_doc <- AnnotatedPlainTextDocument(bio, bio_annotations)

entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

entities(bio_doc, kind = "person")
entities(bio_doc, kind = "location")
entities(bio_doc, kind = "organization")
#clean title
# locationNormalized
#category


# predictors:
# title (cleaned)
# full description (converted into multiple dummy variables)
## Location normalized
# contract type
# contract time
# category

# clean location 


## cleaning the test set
temp.location <- read.csv("Location_Tree.csv")
location.list <-strsplit(as.character(temp.location[,1]),"~")
load("testSalary.RData")

loc.test <- unique(testSalary$LocationNormalized)
num.loc.test <- length(loc.test)
test.location2 <- vector(length=length(testSalary$LocationNormalized))
find <-vector()
ptm<- proc.time()
for(i in seq(1,num.loc.test,by=1)){
  temp.index <- grep(loc.test[i],testSalary$LocationNormalized)
  line.id <- grep(loc.test[i],location.list)[1]
  ifelse(!is.na(line.id),test.location2[temp.index] <-unlist(location.list[line.id])[2],test.location2[temp.index] <-NA) 
  print(i)
}
proc.time() - ptm

# the NA in the following correspond to "Highlands" in LocationNormalized.
# However, it should be "Highland", which belongs to "Scotland" 
test.location2[which(is.na(test.location2))] <- "Scotland"
# those LocationNormalized are "UK", in the above code, they are assigned as London 
# may be we should delete those "UK" data first?
test.location2[grep("UK",testSalary$LocationNormalized)] <- "UK"
testSalary$LocationNormalized <- factor(test.location2)
save(testSalary,file="cleanTestSalary.RData")
