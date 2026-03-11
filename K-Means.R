library(tidyverse)

df<-data.frame("ID"=c(1,2,3,4,5,6), "IncomeK"=c(20,20,30,25,25,30),
               "EducationY"=c(18,16,20,15, 12,22))
view(df)

summary(df$IncomeK)
income_encode_func<-function(x){if(x>median(df$IncomeK)) 
  "HighIncome" else "LowIncome"
}

df$IncomeSegment<-sapply(df$IncomeK, income_encode_func)
view(df)

summary(df$EducationY)
education_encode_func<-function(x){if(x>median(df$EducationY)) 
  "HighEdu" else "LowEdu"
}

df$EduSegment<-sapply(df$EducationY,education_encode_func )
view(df)

HighEduHighIncome<-filter(df, df$EduSegment=="HighEdu"&
                            df$IncomeSegment=="HighIncome")

HighEduLowIncome<-filter(df, df$EduSegment=="HighEdu"&
                           df$IncomeSegment=="LowIncome")

LowEduHighIncome<-filter(df, df$EduSegment=="LowEdu"&
                           df$IncomeSegment=="HighIncome")
LowEduLowIncome<-filter(df, df$EduSegment=="LowEdu"&
                          df$IncomeSegment=="LowIncome")

?ggplot()
ggplot(HighEduHighIncome, aes(x=IncomeK, y=EducationY))+
  geom_point(color="red")+
  geom_point(data=HighEduLowIncome, color="orange")+
  geom_point(data=LowEduHighIncome, color="green")+
  geom_point(data=LowEduLowIncome, color="blue")+
  ggtitle("Segments by Education and Income")+
  xlab("income")+ylab("education")
  



