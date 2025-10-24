set.seed(222)
library(tidyverse)
library(ggdag)
exampledag <- dagitty::randomDAG(9,0.6)
tidyobj <- tidy_dagitty(exampledag)
tidyobj
ggdag(tidyobj)+theme_dag_gray()
ggdag_adjustment_set(tidyobj,exposure="x7",outcome ="x9")
ggdag(tidyobj)+theme_dag_gray()
install.packages("gapminder")
head(gapminder)
data <- gapminder::gapminder
ggplot(data=data$lifeExp,aes(x))+geom_histogram(aes(x),data=data$lifeExp)
simpleline1var <- lm(data$lifeExp~log(data$gdpPercap))
summary(simpleline1var)
hist(simpleline1var$residuals)
library(lmtest)
wt <- 1 / lm(abs(simpleline1var$residuals) ~ simpleline1var$fitted.values)$fitted.values^2
wlsline1var <- lm(data$lifeExp~log(data$gdpPercap),weights = wt)
summary(wlsline1var)
bptest(wlsline1var)
quantline1var <- quantreg::rq(data$lifeExp~log(data$gdpPercap),tau=0.25)
sysimpleline1varlog <- lm(log(data$lifeExp)~log(data$gdpPercap))
summmary(simpleline1var)
wt <- 1 / lm(abs(simpleline1varlog$residuals) ~ simpleline1varlog$fitted.values)$fitted.values^2
wlsline1varlog <- lm(log(data$lifeExp)~log(data$gdpPercap),weights = wt)
summary(wlsline1var)
bptest(wlsline1var)
simpleline1varnonlog <- lm(data$lifeExp~data$gdpPercap)
wtnl <- 1 / lm(abs(simpleline1varnonlog$residuals) ~ simpleline1varnonlog$fitted.values)$fitted.values^2
wlsline1varnonlog <- lm(data$lifeExp~data$gdpPercap,weights = wtnl)
summary(wlsline1varnonlog)
bptest(wlsline1varnonlog)
ggplot(data, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(color = "blue", size = 3) + 
  geom_line(aes(y = wlspresd), color = "red", size = 1) +  # Regression line
wlspred <- wlsline1varnonlog$fitted.values
fedata <- data %>% mutate(lGdp = log(gdpPercap)) %>% mutate(llife = log(lifeExp)) %>% group_by(country) %>% mutate(wgLifex = lifeExp-mean(lifeExp)) %>% mutate(wglGDP = lGdp - mean(lGdp)) %>% mutate(wglLife = llife - mean(llife)) %>% ungroup()
femodel <- lm(wgLifex~wglGDP+0,data=fedata)
femodelLh <- lm(wglLife~wglGDP+0,data=fedata)






