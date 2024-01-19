#estimate Return on Investment based on a logistic regression model 
#Salt Lake Community College data - background: Math department randomly assigned a learning assistant to courses
   #students could use the learning assistant for help. We don't know if they did or did not though.
   #results, not significant, but were looking promising. The math department was encouraged enough to use the data
   #to ask for additional funding in the 2024 Utah Legislative session. 

#####################
# VARIOUS STATISTICAL MODELS 
#model for success - mixed effect
success.model = glmer(success.ind.num ~ pell.eligible + gender + race +
                          hispanic + first.gen + la.ind.T.F  +
                          #repeat.sequence.ind + #Fixed effects
                          #model is struggling to converge removed
                          #campus + concurrent.section +
                          # + (1 | instructor.pidm) #got rid of a few errors
                          # (1 | pidm) +
                          (1 | crn), #random effects from nested or heirarchical structure
                      data = combined.df, family = "binomial")

summary(success.model)
confint(success.model)

# logistic regression
logit.success.model <- glm(success.ind.num~ pell.eligible + gender + race +
                               hispanic + first.gen + la.ind.T.F +
                               repeat.sequence.ind + term.code +
                               campus + course.num,
                           data = combined.df, family = "binomial")
summary(logit.success.model)



#PASSED
logit.passed.model <- glm(passed.ind.num~ pell.eligible + gender + race +
                              hispanic + first.gen + la.ind.T.F +
                              repeat.sequence.ind + term.code +
                              campus + course.num,
                          data = combined.df, family = "binomial")
summary(logit.passed.model)

#model for passing - mixed effect
passed.model = glmer(passed.ind.num ~ pell.eligible + gender + race +
                         hispanic + first.gen + la.ind.T.F +
                         #Fixed effects
                         #model is struggling to converge removed
                         #campus + concurrent.section +
                         # + (1 | instructor.pidm) #got rid of a few errors
                         # (1 | pidm) + # not enought repeat students
                         (1 | crn), #random effects from nested or hierarchical structure
                     data = combined.df, family = "binomial")

summary(passed.model)
get_model_data(passed.model, type = "est")

plot_model(passed.model)
plot_model(passed.model,
           width = .5,
           show.values = T) + ylim(0.1,2.1)

#graph the random effect of CRN
plot_model(passed.model, type ="re")

#due to limited repeat students and errors appearing in glmer models we opted for a logit model

# check model assumptions - logit model = logit.success.model
################################
# Assumption 1: binary outcome variable
# All outcomes are binary: retention, graduation, and completed term

# Assumption 2: Independence
# No reason to believe that they are dependent

################################
# Assumption 3: No Mulitcollinearity
car::vif(logit.success.model)

##############################
# Assumption 4: Extreme outliers
# retention model
plot(logit.success.model, which = 4, id.n=3) #3 observations with higher than normal cooks distance, but it's within reason
model.data <- augment (logit.success.model) %>%
    mutate(index = 1:n())
model.data %>% top_n(3, .cooksd)
ggplot(model.data, aes(index, .std.resid)) +
    geom_point(aes(color = la.ind.T.F), alpha = .5) +
    theme_bw()
model.data %>%
    filter(abs(.std.resid) > 3)


##############################
# Assumption 5: Linear relationship between explanatory variables and the logit of the response

#NA - none of the explanatory variables are continuous


############################
# Assumption 6: Large sample size





############################
# Estimate ROI

# Estimate potential increased headcount due to coaching.
preds.success.df = with(
    list(df = combined.df %>%
             filter(gender != "Unknown")),
    with(
        list(X = model.matrix(eval(parse(text = paste("~",
                                                      as.character(logit.success.model$call$formula)[3]))),
                              data = df),
             X.counter= model.matrix(eval(parse(text = paste("~",
                                                             as.character(logit.success.model$call$formula)[3]))),
                                     data = df %>%
                                         mutate(la.ind.T.F = !la.ind.T.F))),
        map_dfr(
            1:100,
            function(sample) {
                coefs = rnorm(length(coefficients(logit.success.model)),
                              mean = coefficients(logit.success.model),
                              sd = summary(logit.success.model)$coefficients[,"Std. Error"])
                combined.df %>%
                    dplyr::select(pidm, term.code, la.ind.T.F, success.ind, course.num) %>%
                    mutate(success.pred = (X %*% coefs)[,1],
                           success.counter = (X.counter %*% coefs)[,1])
            },
            .id = "sample"
        )
    )
)


########################
# Visualizations of predictions & summary tables
#BY TERM
################################
# How many extra students pass  (by term
# season)?

preds.success.df %>%
    filter(la.ind.T.F) %>%
    group_by(term.code, sample) %>%
    slice_sample(n = 400, replace = T) %>%
    mutate(rand = runif(n())) %>%
    summarise(n.success.la = sum(rand < invlogit(success.pred)),
              n.success.not.la = sum(rand < invlogit(success.counter)),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(n.extra = n.success.la - n.success.not.la) %>%
    group_by(term.code) %>%
    summarise(lower.95 = quantile(n.extra, 0.025),
              lower.50 = quantile(n.extra, 0.25),
              median = median(n.extra),
              upper.50 = quantile(n.extra, 0.75),
              upper.95 = quantile(n.extra, 0.975)) %>%
    ungroup() %>%
    ggplot(aes(y = fct_rev(term.code))) +
    geom_vline(xintercept = 0) +
    geom_linerange(aes(xmin = lower.95, xmax = upper.95), color = dsa.colors(1),
                   size = 1) +
    geom_linerange(aes(xmin = lower.50, xmax = upper.50), color = dsa.colors(1),
                   size = 2) +
    geom_point(aes(x = median), color = dsa.colors(1), size = 3)

# Same thing, but for all students, not just learning assistant students.
preds.success.df %>%
    group_by(term.code, sample) %>%
    slice_sample(n = 20) %>%
    mutate(rand = runif(n())) %>%
    summarise(n.success.la = sum(if_else(la.ind.T.F, invlogit(success.pred),
                                         invlogit(success.counter))),
              n.success.not.la = sum(if_else(la.ind.T.F,
                                             invlogit(success.counter),
                                             invlogit(success.pred))),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(n.extra = n.success.la - n.success.not.la) %>%
    group_by(term.code) %>%
    summarise(lower.95 = quantile(n.extra, 0.025),
              lower.50 = quantile(n.extra, 0.25),
              median = median(n.extra),
              upper.50 = quantile(n.extra, 0.75),
              upper.95 = quantile(n.extra, 0.975)) %>%
    ungroup() %>%
    ggplot(aes(y = fct_rev(term.code))) +
    geom_vline(xintercept = 0) +
    geom_linerange(aes(xmin = lower.95, xmax = upper.95), color = dsa.colors(1),
                   size = 1) +
    geom_linerange(aes(xmin = lower.50, xmax = upper.50), color = dsa.colors(1),
                   size = 2) +
    geom_point(aes(x = median), color = dsa.colors(1), size = 3) +
    labs(x = "Additional students success per course with learning assistant",
         y = "Term", title = "Estimated impact of learning assistant",
         subtitle = "Assuming a class size of 20 students per course")

# What's our predicted extra headcount (by term)?
preds.success.df %>%
    mutate(rand = runif(n())) %>%
    group_by(term.code, sample) %>%
    summarise(n = n(),
              n.success.not.la = sum(rand < if_else(la.ind.T.F,
                                                    invlogit(success.counter),
                                                    invlogit(success.pred))),
              prop.success.not.la = mean(rand < if_else(la.ind.T.F,
                                                        invlogit(success.counter),
                                                        invlogit(success.pred))),
              n.success.la = sum(rand < if_else(la.ind.T.F,
                                                invlogit(success.pred),
                                                invlogit(success.counter))),
              prop.success.la = mean(rand < if_else(la.ind.T.F,
                                                    invlogit(success.pred),
                                                    invlogit(success.counter))),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(n.boost = n.success.la - n.success.not.la,
           pct.boost = prop.success.la - prop.success.not.la) %>%
    group_by(term.code) %>%
    summarise(n.success.not.la = mean(n.success.not.la),
              prop.success.not.la = mean(prop.success.not.la),
              n.success.la = mean(n.success.la),
              prop.success.la = mean(prop.success.la),
              n.boost = mean(n.boost),
              pct.boost = mean(pct.boost))



#BY COURSE NUMBER
##############################
# Same thing, but for all students, not just learning assistant students. GROUP BY - COURSE NUM
preds.success.df %>%
    group_by(course.num, sample) %>%
    slice_sample(n = 25) %>%
    mutate(rand = runif(n())) %>%
    summarise(n.success.la = sum(if_else(la.ind.T.F, invlogit(success.pred),
                                         invlogit(success.counter))),
              n.success.not.la = sum(if_else(la.ind.T.F,
                                             invlogit(success.counter),
                                             invlogit(success.pred))),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(n.extra = n.success.la - n.success.not.la) %>%
    group_by(course.num) %>%
    summarise(lower.95 = quantile(n.extra, 0.025),
              lower.50 = quantile(n.extra, 0.25),
              median = median(n.extra),
              upper.50 = quantile(n.extra, 0.75),
              upper.95 = quantile(n.extra, 0.975)) %>%
    ungroup() %>%
    ggplot(aes(y = fct_rev(course.num))) +
    geom_vline(xintercept = 0) +
    geom_linerange(aes(xmin = lower.95, xmax = upper.95), color = dsa.colors(1),
                   size = 1) +
    geom_linerange(aes(xmin = lower.50, xmax = upper.50), color = dsa.colors(1),
                   size = 2) +
    geom_point(aes(x = median), color = dsa.colors(1), size = 3) +
    labs(x = "Additional students success per course with learning assistant",
         y = "Course", title = "Estimated impact of learning assistant",
         subtitle = "Assuming a class size of 25 students per course")



# What's our predicted extra headcount (by course number)?
preds.success.df %>%
    mutate(rand = runif(n())) %>%
    group_by(course.num, sample) %>%
    summarise(n = n(),
              n.success.not.la = sum(rand < if_else(la.ind.T.F,
                                                    invlogit(success.counter),
                                                    invlogit(success.pred))),
              prop.success.not.la = mean(rand < if_else(la.ind.T.F,
                                                        invlogit(success.counter),
                                                        invlogit(success.pred))),
              n.success.la = sum(rand < if_else(la.ind.T.F,
                                                invlogit(success.pred),
                                                invlogit(success.counter))),
              prop.success.la = mean(rand < if_else(la.ind.T.F,
                                                    invlogit(success.pred),
                                                    invlogit(success.counter))),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(n.boost = n.success.la - n.success.not.la,
           pct.boost = prop.success.la - prop.success.not.la) %>%
    group_by(course.num) %>%
    summarise(n.success.not.la = mean(n.success.not.la),
              prop.success.not.la = mean(prop.success.not.la),
              n.success.la = mean(n.success.la),
              prop.success.la = mean(prop.success.la),
              n.boost = mean(n.boost),
              pct.boost = mean(pct.boost))



# average estimate of head count of a class of 25 students
preds.success.df %>%
    group_by(sample) %>%
    slice_sample(n = 25) %>%
    mutate(rand = runif(n())) %>%
    summarise(n.success.la = sum(if_else(la.ind.T.F, invlogit(success.pred),
                                         invlogit(success.counter))),
              n.success.not.la = sum(if_else(la.ind.T.F,
                                             invlogit(success.counter),
                                             invlogit(success.pred))),
              .groups = "keep") %>%
    ungroup() %>%
    mutate(n.extra = n.success.la - n.success.not.la) %>%
    summarise(lower.95 = quantile(n.extra, 0.025),
              lower.50 = quantile(n.extra, 0.25),
              median = median(n.extra),
              upper.50 = quantile(n.extra, 0.75),
              upper.95 = quantile(n.extra, 0.975))
