# Data - S.in.C.clean.2 = only students who are controls or students in crisis
# Prior transformations - log of Credits = CREDITS_log and changed GPA to categorical = GPA_cat
# Outcomes: completed term, retention, graduated

############# Retention model ##############
#categorical GPA and log credits.
table(S.in.C.clean.2$retention.num, S.in.C.clean.2$group)
retention.model <- glm(retention.num ~group + relevel(factor(RACE), ref =7) + GENDER +
                           HISPANIC_IND + FIRST_GEN_STATUS + PELL_GRANT_ELIGIBLE +
                           GPA_cat + TERM_AGE + CREDITS_log,
                       data = S.in.C.clean.2, family="binomial")

summary(retention.model)
# change to Odds ratio
retention.OR <- exp(cbind(OR= coef(retention.model), confint(retention.model)))
retention.OR

# there was a statistically significant difference between control group and students in crisis
# students who received bruins emergency funds were more likely to retain than students who only
# received the other grant


################ graduation model############
table(S.in.C.clean.2$grad.num, S.in.C.clean.2$group)
grad.model <- glm(grad.num ~group + relevel(factor(RACE), ref =7) + GENDER + HISPANIC_IND +
                      FIRST_GEN_STATUS +
                      PELL_GRANT_ELIGIBLE + GPA_cat + TERM_AGE + CREDITS_log ,
                  data = S.in.C.clean.2, family="binomial")
summary(grad.model)
grad.OR <- exp(cbind(OR= coef(grad.model), confint(grad.model)))
grad.OR

# no statistically significant difference between control group and students in crisis


################### term completion model ###################
comp.term.model <- glm(complete.term.num ~group + relevel(factor(RACE), ref =7) + GENDER + HISPANIC_IND +
                           FIRST_GEN_STATUS + PELL_GRANT_ELIGIBLE + TERM_AGE,
                       data = S.in.C.clean.2, family="binomial")

summary(comp.term.model)
comp.term.OR <- exp(cbind(OR= coef(comp.term.model), confint(comp.term.model)))
comp.term.OR


################################
# check model assumptions
################################
# Assumption 1: binary outcome variable
# All outcomes are binary: retention, graduation, and completed term

# Assumption 2: Independence
# No reason to believe that they are dependent

################################
# Assumption 3: No Mulitcollinearity
car::vif(retention.model)

car::vif(grad.model)

car::vif(comp.term.model)

##############################
# Assumption 4: Extreme outliers
# retention model
plot(retention.model, which = 4, id.n=3)
model.data <- augment (retention.model) %>%
    mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) +
    geom_point(aes(color = group), alpha = .5) +
    theme_bw()
model.data %>%
    filter(abs(.std.resid) > 3)

# graduation model
plot(grad.model, which = 4, id.n=3)
model.data <- augment (grad.model) %>%
    mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) +
    geom_point(aes(color = group), alpha = .5) +
    theme_bw()
model.data %>%
    filter(abs(.std.resid) > 3) # 2 crisis students that have very low GPA but graduated

# completed term model
plot(comp.term.model, which = 4, id.n=3)
model.data <- augment (comp.term.model) %>%
    mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) +
    geom_point(aes(color = group), alpha = .5) +
    theme_bw()
model.data %>%
    filter(abs(.std.resid) > 3)


##############################
# Assumption 5: Linear relationship between explanatory variables and the logit of the response

mydata <- S.in.C.clean.2%>%
    dplyr::select(c(TERM_AGE, TERM_GPA,
                    TOTAL_TERM_CREDITS, CREDITS_log))
predictors <- colnames(mydata)

# Retention model
probabilities.ret <- predict(retention.model, type = "response")
predicted.classes.ret <- ifelse(probabilities.ret > 0.5, "pos", "neg")
head(predicted.classes.ret)

# Bind the logit and tidying the data for plot
# I realized that this will also include all the variables that were changed to 1,0 for matching...
mydata <- mydata %>%
    mutate(logit = log(probabilities.ret/(1-probabilities.ret))) %>%
    gather(key = "predictors", value = "predictor.value", -logit)
retention.model.linear <- ggplot(mydata, aes(predictor.value, logit))+
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess") +
    theme_bw() +
    facet_wrap(~predictors, scales = "free_x")
retention.model.linear
# only concerned about CREDITS_log and TERM_AGE being linear the others aren't used


#graduation model
mydata <- S.in.C.clean.2%>%
    dplyr::select(c(TERM_AGE, TERM_GPA,
                    TOTAL_TERM_CREDITS, CREDITS_log))
predictors <- colnames(mydata)

probabilities.grad <- predict(grad.model, type = "response")
predicted.classes.grad <- ifelse(probabilities.grad > 0.5, "pos", "neg")
head(predicted.classes.grad)
# Bind the logit and tidying the data for plot
# I realized that this will also include all the variables that were changed to 1,0 for matching...
mydata <- mydata %>%
    mutate(logit = log(probabilities.grad/(1-probabilities.grad))) %>%
    gather(key = "predictors", value = "predictor.value", -logit)
grad.mod.linear.assump <- ggplot(mydata, aes(predictor.value, logit))+
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess") +
    theme_bw() +
    facet_wrap(~predictors, scales = "free_x")
grad.mod.linear.assump


# completion term model
mydata <- S.in.C.clean.2 %>%
    dplyr::select(c(TERM_AGE))
predictors <- colnames(mydata)

probabilities.comp.term <- predict(comp.term.model, type = "response")
predicted.classes.comp.term <- ifelse(probabilities.comp.term > 0.5, "pos", "neg")
head(predicted.classes.comp.term)
# Bind the logit and tidying the data for plot
# I realized that this will also include all the variables that were changed to 1,0 for matching...
mydata <- mydata %>%
    mutate(logit = log(probabilities.comp.term/(1-probabilities.comp.term))) %>%
    gather(key = "predictors", value = "predictor.value", -logit)
comp.term.linear.assump <- ggplot(mydata, aes(logit, predictor.value))+
    geom_point(size = 0.5, alpha = 0.5) +
    geom_smooth(method = "loess") +
    theme_bw() +
    facet_wrap(~predictors, scales = "free_y")
comp.term.linear.assump

############################
# Assumption 6: Large sample size

