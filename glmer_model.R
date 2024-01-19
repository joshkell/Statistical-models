# glmer = logistic mixed-effect models

#################
#retention model
retention.m = glmer(retention.num ~ prior.credits_centered + student.gender +
                      relevel(factor(student.race), ref=7) + hispanic.ind +
                      relevel(factor(term_gpa_category), ref = 6) +
                      relevel(factor(degree.seeking), ref = 3) +
                      relevel(factor(time_category), ref =2)+
                      instructor.gender + relevel(factor(instructor.race), ref=6) +
                      instructor.hispanic.ind + `INSTRUCTOR TYPE` + terms.taught_centered + #Fixed effects
                      (1 | CRN) + (1 | instructor.pidm) , #random effects
                  #   (1 | pidm) + (prior.credits | course) + (prior.credits | instructor.pidm), #random effects too many and takes too long
                  data = data_student_eval_df ,
                  family = binomial)

summary(retention.m)
print(retention.m, correlation=TRUE)

#there were some concerns with just running a retention model. GPA category showed that a 3.5-4.0 GPA had lower odds of retention
  #compared to other GPA categories, compared to the positive outcomes model which was either graduate or retain, this was not true.

#optimizer (Nelder_Mead) failure to converge 4/10,000
#degenerate Hessian failure to converge 1/10,000 evaluations
#warning message - explored, google searches appear to suggest that there may be
  #differences in standard error estiamtes, other people with similar warnings
  #tried different modificaiton of a glmer model and reported the same findings
  #with marginal differences in standard error.
#limitation with this model, we're not taking into account students who graduated


################
#graduation model
grad.m = glmer(graduated.num ~ prior.credits_centered + student.gender +
                        relevel(factor(student.race), ref=7) + hispanic.ind +
                        relevel(factor(term_gpa_category), ref = 6) +
                        relevel(factor(degree.seeking), ref = 3) +
                        relevel(factor(time_category), ref =2)+
                        instructor.gender + relevel(factor(instructor.race), ref=6) +
                        instructor.hispanic.ind + `INSTRUCTOR TYPE` + terms.taught_centered + #Fixed effects
                        (1 | CRN) + (1 | instructor.pidm) , #random effects
                    data = data_student_eval_df ,
                    family = binomial)

summary(grad.m)
print(grad.m, correlation=TRUE)



################
#positive outcomes model
positive.outcome.m = glmer(positive_outcome_num ~ prior.credits_centered + student.gender +
                   relevel(factor(student.race), ref=7) + hispanic.ind +
                   relevel(factor(term_gpa_category), ref = 6) +
                   relevel(factor(degree.seeking), ref = 3) +
                   relevel(factor(time_category), ref =2)+
                   instructor.gender + relevel(factor(instructor.race), ref=6) +
                   instructor.hispanic.ind + `INSTRUCTOR TYPE` + terms.taught_centered + #Fixed effects
                   (1 | CRN) + (1 | instructor.pidm) , #random effects
               data = data_student_eval_df ,
               family = binomial)

summary(positive.outcome.m)
print(positive.outcome.m, correlation=TRUE)

#removed time of course - time_category from the model
positive.outcome.m2 = glmer(positive_outcome_num ~ prior.credits_centered + student.gender +
                               relevel(factor(student.race), ref=7) + hispanic.ind +
                               relevel(factor(term_gpa_category), ref = 6) +
                               relevel(factor(degree.seeking), ref = 3) +
                               instructor.gender + relevel(factor(instructor.race), ref=6) +
                               instructor.hispanic.ind + `INSTRUCTOR TYPE` + terms.taught_centered + #Fixed effects
                               (1 | CRN) + (1 | instructor.pidm) , #random effects
                           data = data_student_eval_df ,
                           family = binomial)

summary(positive.outcome.m2)
print(positive.outcome.m2, correlation=TRUE)

