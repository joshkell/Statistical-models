# I realized I can't do a lmer model because the outcome is dichotomous. glmer = logistic mixed-effect models
# note - the course evaluation data which is one of the data sets to create the data_students_eval
# has a lot of missing (i.e. not all students submitted course evals)


# Create the new variable 'time_category'

data_student_eval_df <- data_student_eval %>%
    mutate(retention.num = ifelse(`RETAINED NEXT TERM IND` == "Y", 1, 0),
           graduated.num = ifelse(`GRADUATED THAT TERM IND` == "Y", 1,0),
           declared.major.ind = ifelse(`DECLARED MAJOR IND` == "Y", 1, 0)
    ) %>%
    mutate(cleaned_course_time = ifelse(grepl("a", `COURSE TIME`), gsub("a", "", `COURSE TIME`), `COURSE TIME`)) %>%
    mutate(cleaned_course_time = ifelse(cleaned_course_time == "000", "0000", cleaned_course_time)) %>%
    mutate(time_category = case_when(
        is.na(cleaned_course_time) ~ "TIME.MISSING",
        as.numeric(cleaned_course_time) <= 400 ~ "TIME.MISSING",
        as.numeric(cleaned_course_time) <= 900 ~ "Early Morning",
        as.numeric(cleaned_course_time) <= 1200 ~ "Mid-Morning",
        as.numeric(cleaned_course_time) <= 1500 ~ "Early Afternoon",
        as.numeric(cleaned_course_time) <= 1800 ~ "Late Afternoon",
        TRUE ~ "Evening"
    )) %>%
    select(-cleaned_course_time) %>%
    rename(pidm = PIDM,
           term.code = `TERM CODE`,
           term.age = `TERM AGE`,
           degree.seeking = `DEGREE SEEKING`,
           term.gpa = `TERM UG GPA`,
           overall.gpa = `OVERALL GPA`,
           prior.gpa = `PRIOR UG GPA`,
           final.grade = `FINAL GRADE`,
           instructor.pidm = `INSTRUCTOR PIDM`,
           years.employed = `YEARS EMPLOYED AT SLCC`,
           terms.taught = `TERMS TAUGHT UP TO SPRING 23`,
           student.gender = `STUDENT GENDER`,
           instructor.gender = `INSTRUCTOR GENDER`,
           hispanic.ind = `STUDENT HISPANIC IND`,
           instructor.hispanic.ind = `INSTRUCTOR HISPANIC IND`,
           student.race = `STUDENT RACE`,
           #instructor.race = `INSTRUCTOR RACE`,
           degree.seeking = `DEGREE SEEKING`,
           instructor.race = `INSTRUCTOR RACE`
    )

#double check
table(data_student_eval_df$time_category, data_student_eval_df$`COURSE TIME`, useNA = "ifany")



data_student_eval_df <- data_student_eval_df %>%
    mutate(num_instructors = str_count(INSTRUCTORUSERNAME, ',') +1,
           prior.credits = coalesce(`PRIOR UG CREDITS`, 0),
           course = gsub("-", "",
                         gsub("^([A-Z]+-[0-9]+)-.*$", "\\1",
                              COURSECODE)))


#transform and center a few categroical variables so hopefully the model will converge.
#Prior credits
skewness_before <- skewness(data_student_eval_df$prior.credits)
data_student_eval_df$prior.credits_transformed <- sqrt(data_student_eval_df$prior.credits) # Square root transformation to reduce skewness (did log(+1) but sqrt is better)
mean_prior_credits <- mean(data_student_eval_df$prior.credits_transformed) # Centering by subtracting the mean
data_student_eval_df$prior.credits_centered <- data_student_eval_df$prior.credits_transformed - mean_prior_credits
skewness_after <- skewness(data_student_eval_df$prior.credits_centered) # Check the skewness after transformation and centering

#TERMS TAUGHT
skewness_before_term <- skewness(data_student_eval_df$terms.taught)
data_student_eval_df$terms.taught_transformed <- sqrt(data_student_eval_df$terms.taught)
mean_terms_taught<- mean(data_student_eval_df$terms.taught_transformed)
data_student_eval_df$terms.taught_centered <- data_student_eval_df$terms.taught_transformed - mean_terms_taught
skewness_after_term <- skewness(data_student_eval_df$terms.taught_centered)
hist(data_student_eval_df$terms.taught_centered)

#GPA - some students have missing term gpa... changed to categorical
breaks_gpa <- c(0, 1.0, 2.0, 2.5, 3.0, 3.5, 4)
labels_gpa <- c("0-1.0", "1.01-2.0", "2.01-2.5", "2.51-3.0", "3.01-3.5", "3.51-4.0")
data_student_eval_df$term_gpa_category <- cut(data_student_eval_df$term.gpa, breaks = breaks_gpa, labels = labels_gpa, include.lowest = TRUE)
table(data_student_eval_df$term_gpa_category, useNA = "ifany")

# Add "missing_gpa" as a factor level
data_student_eval_df$term_gpa_category <- factor(data_student_eval_df$term_gpa_category, levels = c(labels_gpa, "missing_gpa"))
# Assign "missing_gpa" label for NA values
data_student_eval_df$term_gpa_category[is.na(data_student_eval_df$term.gpa)] <- "missing_gpa"
table(data_student_eval_df$term_gpa_category, useNA = "ifany")

#positive outcomes
data_student_eval_df$positive_outcome_num <- ifelse(data_student_eval_df$retention.num == 1
                                                    | data_student_eval_df$graduated.num == 1, 1, 0)

cache("data_student_eval_df")

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
cache("retention.m")
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
cache("grad.m")



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
cache("positive.outcome.m")

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
cache("positive.outcome.m2")
