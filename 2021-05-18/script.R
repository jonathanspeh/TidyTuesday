# Packages
library(tidyverse)

# data
survey <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-18/survey.csv')

# wrangling
survey%>%head()
survey[,7:18]

survey%>%select(-c(additional_context_on_income,additional_context_on_job_title,city,state,job_title))%>%
  group_by(country)%>%count()%>%arrange(desc(n))


survey%>%select(-c(additional_context_on_income,additional_context_on_job_title,city,state,job_title))%>%
  mutate(country=case_when(str_detect(country,c("Puerto|United Stat|stares|Stares|states|States|United Sat|Uniited Stat|USA|US|U.S.|Us|Usa|usa|america|America|United states|UNITED STATE|Unites States|U. S.|united States|united states|Califor"))~"USA",
                           str_detect(country,"Australi|australi")~"Australia",
                           str_detect(country,"United Ki|Isle of|Jersey| kingdom|united ki|England|UK|uk|Uk|Northern Ire|scot|Scot|Brit|Cay|Bermuda")~"UK",
                           str_detect(country,"Cana|Can|Csnad")~"Canada",
                           str_detect(country,"ARGEN|Argen")~"Argentina",
                           str_detect(country,"Cataloni")~"Spain",
                           str_detect(country,"The Nether")~"Netherlands",
                           str_detect(country, "South Africa|South afr")~"South_Africa",
                           str_detect(country, "Hong Kong")~"Hongkong",
                           str_detect(country, "Sri lanka")~"Srilanka",
                           str_detect(country, "Czech|czech")~"Czech_Republic",
                           str_detect(country, "New Zea|New zea|new zea")~"New_Zealand",
                           str_detect(country, "South Korea")~"South_Korea",
                           str_detect(country, "China")~"China",
                           str_detect(country, "Italy")~"Italy",
                           str_detect(country,".. ..|Africa|Contracts")~"NA",
                           TRUE ~ country))->countriesCleand
                            
                           
## Convert Currency into euro 
survey$currency%>%unique()

conversion<-tribble(~currency,~ convFactor,
                    "USD", 0.82,
                    "GBP", 1.16,
                    "CAD", 0.68,
                    "EUR", 1,
                    "AUD/NZD", 0.64,
                    "Other", NA,
                    "CHF", 0.91,
                    "ZAR", 0.058,
                    "SEK", 0.099,
                    "HKD", 0.11,
                    "JPY", 0.0075)
                   
countriesCleand%>%left_join(.,conversion,by="currency")%>%
  mutate(annualEuro=annual_salary*convFactor)%>%
  select(timestamp,age=how_old_are_you,country,industry,
         annualEuro, overallExp=overall_years_of_professional_experience,
         specificExp=years_of_experience_in_field, 
         education=highest_level_of_education_completed,
         gender)%>%
  mutate(age=factor(age, levels = c("under 18", "18-24","25-34",
                                    "35-44","45-54","55-64", 
                                    "65 or over")))%>%
  filter(annualEuro<83640000)%>%drop_na()->surveyParsed


surveyParsed%>%mutate(group=case_when(country=="USA"~"US",
                                      TRUE ~ "Non-US"))%>%
  filter(annualEuro>0)%>%
  group_by(age,group, gender)%>%
  summarise(meanInc=mean(annualEuro),
            N=n(),
            sdInc=sd(annualEuro),
            seInc=sdInc/sqrt(N),
            minInc=min(annualEuro),
            maxInc=max(annualEuro),
            .groups = "drop")%>%ungroup()%>%
  filter(str_detect(gender,"Man|Woman"),
         age!= "under 18")->surveyPlot


#lineplot comparing average annual income of us and non-use male and female

surveyPlot%>%
  mutate(lgroup=paste(group,gender))%>%
  ggplot(aes(x=age,y=meanInc,colour=lgroup,shape=gender,group=lgroup))+
  geom_point()+
  geom_line()+
  scale_color_brewer(palette="Paired")+
  scale_y_continuous(labels = scales::comma)
  


# Lineplot only for US including errorbars
surveyPlot%>%filter(group=="US")%>%
  ggplot(aes(x=age,y=meanInc,colour=gender,group=gender))+
  geom_point()+
  geom_line()+
  geom_errorbar(aes(ymin=meanInc-seInc,ymax=meanInc+seInc),width=.2)+
  ggtitle("Average Annual Income of an American citizen")+
  ylab("Annual Income (EUR)")+
  xlab("Age Group")+
  scale_y_continuous(labels = scales::comma)
 




geom_errorbar(aes(ymin=minInc,ymax=maxInc),width=.1)


# something with distribution / violin plots + lines for average?

surveyParsed%>%filter(country=="USA", 
                      age != "under 18",
                      annualEuro>0,
                      annualEuro<300000,
                      str_detect(gender,"Man|Woman"))%>%drop_na()%>%
  ggplot(aes(x=age,y=annualEuro,colour=gender))+
  geom_violin(aes(fill=gender))+
  geom_boxplot(outlier.shape = NA,outlier.stroke=NA,
               width=0.2, position = position_dodge(.9))+
  #stat_summary(fun = mean,geom="point")+
  #geom_line(aes(group=gengender))+
  scale_y_continuous(labels = scales::comma)

# Something with the timestamps?

surveyParsed%>%separate(timestamp, into = c("date","time"), sep = " ")%>%
  separate(time, into = c("h","m","s"), sep = ":")%>%mutate(h=factor(h, levels = 0:24))->timesParsed


# preferred time by gender
timesParsed%>%filter(country=="USA")%>%
  ggplot(aes(x=h,fill=gender))+
  geom_bar(position = "dodge")

## set answering time in relation to income
### Use secondary y-axis (coef. 30) to display histogram and income
### respondents that earn above the generale average tend to respond later at 
###night than those with lower income
overallMean<-mean(timesParsed$annualEuro)/30
overallMedian<-median(timesParsed$annualEuro)/30

timesParsed%>%group_by(h)%>%
  summarise(N=n(),
            meanInc=mean(annualEuro),
            annualK=meanInc/30,
            sdInc=sd(annualEuro),
            medInc=median(annualEuro),
            seInc=sdInc/sqrt(N),
            respK=N/1000)%>%
  mutate(h=factor(h, levels = c(8:23,0:7)),
         above=case_when(annualK > overallMean ~ "above average",
                         TRUE ~ "below average"))%>%
  ggplot(aes(x=h))+
  geom_bar(aes(y=N),stat="identity", fill="darkred")+
  geom_point(aes(y=annualK,colour=above),stat="identity")+
  geom_line(aes(y=annualK,group=NA))+
  geom_hline(yintercept=overallMean)+
  scale_y_continuous(
    name = "Number of respondents",
    sec.axis = sec_axis(~./30, name="Mean Annual Income")
  )
  
# something with colours of the histogram - that has quite a nice tone

overallK<-round(mean(timesParsed$annualEuro)/1000,1)
overallK

timesParsed%>%group_by(h)%>%
  summarise(N=n(),
            meanInc=mean(annualEuro),
            annualK=meanInc/30,
            sdInc=sd(annualEuro),
            medInc=median(annualEuro),
            seInc=sdInc/sqrt(N),
            respK=N/1000)%>%
  mutate(h=factor(h, levels = c(8:23,0:7)),
         above=case_when(meanInc > overallK ~ "above",
                         TRUE ~ "below"))%>%
  ggplot(aes(x=h))+
  geom_bar(aes(y=N,fill=annualK),stat="identity")+
  geom_text(aes(y=N+150, label=round(meanInc/1000,1)),
             size=3)+
  annotate("text",x=20,y=3000, label=
           paste("Average annual income:", overallK, "K"))



# violin plots - not so good RN
timesParsed%>%mutate(h=factor(h,levels = c(8:23,0:7)))%>%
  ggplot(aes(x=h))+
  geom_bar()+
  geom_violin(aes(y=annualEuro/30),outlier.shape = NA,fill="navyblue")+
  geom_hline(yintercept=overallMedian)+
  scale_y_continuous(
    name = "Number of respondents",
    sec.axis = sec_axis(~./30, name="Mean Annual Income"),
    limits = c(0,7500))







