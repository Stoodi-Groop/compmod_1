pacman::p_load(tidyverse, lme4, lmerTest)

#making a list of files 
files <- list.files(path = "Simon data/",  
                    pattern = ".csv",  
                    full.names = T)   

#reading the files into a dataframe 
d <- lapply(files, read_csv) %>%
  bind_rows()

#removing uneccesary coloumn
d <- d[,-14]

#making congruency column and removing training data
d <- d %>% 
  mutate(congruency = 
            ifelse(position == "middle", "neutral", 
                  ifelse(position == "left"& color == "green", "congruent", 
                      ifelse(position == "right" & color == "red", "congruent", "incongruent")))) %>% 
  filter(part == "test")

  

#Plot the difference in reaction times between congruency conditions (only using correct responses).
d %>% 
  filter(accuracy == "TRUE") %>% 
  ggplot(aes(congruency, rt, fill = congruency)) +
  geom_bar(stat = 'summary') +
  geom_errorbar(stat = 'summary', fun.data = mean_se) +
  coord_cartesian(ylim = c(400, 500)) +
  xlab("Condition") +
  ylab("Mean Reaction Time") +
  theme_minimal()


#removing wrong answers
d.true <- d %>% 
  filter(accuracy == "TRUE")

#Use a linear regression to assess whether there is a difference in reaction times 
#between congruency conditions. Only use correct responses

m1 <- lmerTest::lmer(rt ~ congruency + (1|subject), data = d.true)

summary(m1)

#for the lols we tested whether accuracy had an effect on reaction time
m2 <- lmerTest::lmer(rt ~ accuracy + (1|subject), data = d)

summary(m2)

