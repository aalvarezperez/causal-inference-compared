source("env.R")

data <- MatchIt::lalonde
# data <- data %>%
#   filter(re78 < 30000)

psych::describe(data)

data %>% 
  tibble::rownames_to_column("id") %>%
  select(id, starts_with("re")) %>%
  pivot_longer(cols = c(re74, re75, re78), names_to = "year_of_income") %>%
  ggplot(aes(value, fill = year_of_income)) + 
  geom_histogram(position = "dodge")

lalonde_lm <- lm(
  re78 ~ treat + age + educ + race + married + nodegree + re74 + re75,
   data = data
  )

summary(lalonde_lm)

data$resid <- resid(lalonde_lm) 
data$fitted <- predict(lalonde_lm)

data %>%
  ggplot(aes(treat, fitted, group = treat)) + 
  geom_boxplot()

data$re78 %>% hist(breaks = 100)
