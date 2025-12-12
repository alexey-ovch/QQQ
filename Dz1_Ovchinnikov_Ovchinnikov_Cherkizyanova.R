# install.packages('Ecdat')
# install.packages("sandwich")
# install.packages('AER')
# library(AER)
# library(sandwich)
# library('Ecdat')
# library(lmtest)
# library(stargazer)
# library(car)
# library(lmtest)
# library(dplyr)

###ЧАСТЬ 1###

data('Computers')

data <- Computers

mod <- lm(price ~ speed + hd + ram + screen + cd + multi + premium + ads + trend, 
          data = data)
summary(mod)

vif(mod)

mod1 <- lm(price ~ speed + ram + screen + cd + multi + premium + ads + trend, 
           data = data)

summary(mod1)
vif(mod1)

mod2 <- lm(price ~ speed + ram + screen + cd + multi + premium + trend, 
           data = data)

summary(mod2)
vif(mod2)

stargazer(mod, mod1, mod2, type = 'html', out = 'mods.html')

# тест Бокса-Кокса
boxCox(mod2)

plot(mod2, which = 2)

# преобразуем
mod3 <- update(mod2, log(price) ~ .)
summary(mod3)

#что будет, если посмотреть еще раз
boxCox(mod3)
plot(mod3, which = 2)

#пропущен ли квадрат?
#надо провести тест Рамсея или reset test
resettest(mod3)
resettest(mod3, power = 2)
#вообще квадраты какие-то пропущены!

crPlots(mod3)

#не хочу добавлять, потому что это не совсем понятно, как осмысливать
#данные распределены очень неравномерно

# мы взяли модель 3
h <- hatvalues(mod3)
dim(Computers)
# показывает число строк (6259) и столбцов (10)
# в нашей модели 8 регрессоров

treh <- 8/6259*2

plot(h, ylab = "Leverage (hat values)", pch = 19)
abline(h = treh, col = "red", lwd = 2, lty = 2)

#если больше порога, это точка высокой напряженности
# то есть мы сейчас хотим посмотреть на точки высокой напряженности
num <- which(h > treh)
# это не плохие наблюдения, просто возможно модель
# под них подстраивается

# выведем график диагностики
influenceIndexPlot(mod3)
influencePlot(mod3)
#я посмотрел, но решил это не удалять

num <- unique(c(which(h > treh), 80, 1701))

# проверим на устойчивость
# уберем наблюдения с этими номерами

data <- slice(Computers, -num)

# обновим модель для этих данных
mod4 <- update(mod3, .~., data = data)
stargazer(mod3, mod4, type = 'html', out = 'mods.html')
# значимость коэффициентов не поменялась, значит
# рез-ты устойчивы, точки высокой напряженности не влияют


# тест Вальда на сравнение короткой против длинной
linearHypothesis(mod3, c("speed = 0", "ram = 0", "screen = 0", "cdyes = 0", "multiyes = 0", "premiumyes = 0", "trend = 0"))


#тест Бреуша-Пагана
#H0: дисперсия всех ошибок равна
#H1: дисперсия зависит от x (sig^2 = a + b*x)
bptest(mod3)
#p-value < 0.05 - H0 отвергается (есть гетероскедастичность)


# Модель с робастными стандартными ошибками
V <- vcovHC(mod3, type = "HC0")

#выгрузка модели
cse <- function(mod) {
  V <- vcovHC(mod, type = "HC0")
  diag(V)^0.5
}
cse(mod3)

#выгрузка
stargazer(mod3, type = "html", out = "mods.html", 
          se = list(cse(mod3)))

###Часть 1.1 окончена###

###Часть 1.2###

data('DoctorVisits')
?DoctorVisits
View(DoctorVisits)
summary(DoctorVisits)


df<-DoctorVisits
m<-lm(visits~gender+age+income+illness+reduced+health+
        private+freepoor+freerepat+nchronic+lchronic, 
      data = df)
summary(m)
vif(m)

m1<-lm(visits~gender+age+income+illness+reduced+health+
         private+freepoor+lchronic, 
       data = df)
summary(m1)
vif(m1)


m2<-lm(visits~gender+age+income+illness+reduced+health+
         freepoor, 
       data = df)
summary(m2)
vif(m2)
m2_adj<-lm(visits+0.001~gender+age+income+illness+reduced+health+freepoor+lchronic, 
           data = df)

stargazer(m, m1, m2, type = 'html', out = 'mods.html')

boxCox(m2_adj)
# Лямбда близка к 0, значит стоит попробовать логарифмирование
plot(m2, which = 2)

# Модель с логарифмированной зависимой переменной
m2_log <- update(m2, log(visits + 0.001) ~ .)  # +1 чтобы избежать log(0)
summary(m2_log)

# Проверка QQ-plot для остатков
plot(m2_log, which = 2)

# Тест Рамсея на пропущенные переменные
resettest(m2_log)
resettest(m2_log, power = 2)

# Визуальная диагностика спецификации
crPlots(m2_log)

# Проверка на квадратичные эффекты
m_sq <- update(m2_log, . ~ . +I(health^2)+I(income^2))
summary(m_sq)
crPlots(m_sq)


# Проверка устойчивости к точкам высокой напряженности
h <- hatvalues(m2_log)
treh <- 8/5190 * 2  #порог для точек высокой напряженности

# Находим точки высокой напряженности
num <- which(h > treh)
length(num)  # количество таких точек

plot(h, ylab = "Leverage (hat values)", pch = 19)
abline(h = treh, col = "red", lwd = 2, lty = 2)

#если больше порога, это точка высокой напряженности
# проверим на устойчивость, уберем наблюдения с этими номерами

#data <- slice(df, -num)
# обновим модель для этих данных
#m3 <- update(m2_log, .~., data = data)

### Часть 1.2 окончена ###


###Часть 2###


install.packages(c("mlmRev", 'lm4', 'lmerTest', 'performance'))
library('mlmRev')
library('lme4')
library('lmerTest')
library('performance')
install.packages("stargazer")
library('stargazer')

# Загрузка данных Exam
data(Exam)
head(Exam)

# Фиксируем случайное число
set.seed(111)
# Получаем уникальные идентификаторы школ
unique_schools <- unique(Exam$school)
# Отбираем половину школ случайным образом
half_schools <- sample(unique_schools, size = length(unique_schools) / 2)
# Создаем подвыборку с отобранными школами
Exam_half <- Exam[Exam$school %in% half_schools, ]


# 1. МОДЕЛЬ СО СЛУЧАЙНЫМИ КОНСТАНТАМИ
model1 <- lmer(normexam ~ standLRT + sex + (1 | school), data = Exam_half)
#(1| school) - случайные "константы" (БЕТТА 1) для каждой school
summary(model1)
install.packages('sjPlot')
library('sjPlot')
tab_model(model1)
#дисперсия случайного эффекта 0.09, небольшая (общая дисперсия 0.67)
#проверяем стало ли лучше
ranova(model1)
#Смотрим на AIC. Со случ эфф 4986, без них 5165. 
#Стало хуже (это видно и по функции правдоподобия). Значит случ эфф нужны
#или так можно: p-value для LRT < 0.05 - H0 не принимается, эффекты нужны
#Достаем эффекты
ranef(model1)
#Получили для свои const каждого наблюдения (для каждой school)
#визуализация
library(lattice)
a <- ranef(model1)
dotplot(a)

# 2. Модель со случ наклонами без корреляции
#нужно ли добавлять случайные наклоны?
#тяжелые хвосты в распределении остатков 
#могут быть признаком пропуска случайных наклонов
e <- resid(model1)
qqnorm(e)
qqline(e)
#Вообще тут все хорошо, нет тяжелых хвостов...

#есть ли для каждого индивида тренды в остатках?
plot(model1, 
     resid(., scaled = TRUE) ~ standLRT | school, abline = TRUE)
#Вообще говоря, трендов нет и тут тоже все хорошо

#Но попробуем сделать случайные наклоны, сначала без корреляции
model2 = lmer(normexam ~ standLRT + sex + (1 | school)
              + (0 + standLRT| school), data = Exam_half)
summary(model2)
tab_model(model2)
ranova(model2)

#Если по p-value смотреть, то оба эффекта нужны. Значит наклон все-таки добавим
#что там с хвостами?
e <- resid(model2)
qqnorm(e)
qqline(e)
#не стало ни лучше, ни хуже. Но ок, оставим.
#есть ли для каждого индивида тренды в остатках?
plot(model2, 
     resid(., scaled = TRUE) ~ standLRT | school, abline = TRUE)
#аналогично, все осталось так же. но трендов нет, это хорошо.

#что с эффектами?
ranef(model2)
dotplot(ranef(model2))

# 3. Модель со случ наклонами с корреляцией
model3 = lmer(normexam ~ standLRT + sex +
                + (1 + standLRT| school), data = Exam_half)
summary(model3)
tab_model(model3)
#corr = 0.54

#H0: cor(эффекты) = 0
#H1: cor(эффекты) != 0
anova(model2, model3)
#p-value LRT = 0.019 < 0.05 - H0 отвергается, есть корреляция. принимаем model3
install.packages('equatiomatic')
library('equatiomatic')
extract_eq(model3)

ranef(model3)
dotplot(ranef(model3))

#уравнение
summary(model3)
#normexam = beta_1i + beta_2i*standLRT + beta_3*sex + eps, eps ~ N(0, 0.559)
#beta_1i = gamma_i, gamma_i ~ N(0, 0.095). fixed intercept незначима
#beta_2i = 0.55 + teta_i, teta_i ~ N(0, 0.021)
#beta_3 = -0.2 (если sexM)
#cor(beta_1i, beta_2i) = 0.54
