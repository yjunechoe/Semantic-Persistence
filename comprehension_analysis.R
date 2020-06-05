###########
## SETUP ##
###########

library(tidyverse)
library(see)
library(stringr)
library(performance)
library(sjPlot)
library(lme4)
library(afex)

# Copied from the PennController Tutorial by Jeremy Zehr & Florian Schwarz (https://www.pcibex.net/wiki/r-script/)
read.pcibex <- function(filepath, auto.colnames=TRUE, fun.col=function(col,cols){cols[cols==col]<-paste(col,"Ibex",sep=".");return(cols)}) {
  n.cols <- max(count.fields(filepath,sep=",",quote=NULL),na.rm=TRUE)
  if (auto.colnames){
    cols <- c()
    con <- file(filepath, "r")
    while ( TRUE ) {
      line <- readLines(con, n = 1, warn=FALSE)
      if ( length(line) == 0) {
        break
      }
      m <- regmatches(line,regexec("^# (\\d+)\\. (.+)\\.$",line))[[1]]
      if (length(m) == 3) {
        index <- as.numeric(m[2])
        value <- m[3]
        if (index < length(cols)){
          cols <- c()
        }
        if (is.function(fun.col)){
          cols <- fun.col(value,cols)
        }
        cols[index] <- value
        if (index == n.cols){
          break
        }
      }
    }
    close(con)
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=cols))
  }
  else{
    return(read.csv(filepath, comment.char="#", header=FALSE, col.names=seq(1:n.cols)))
  }
}

# load data
result <- as_tibble(read.pcibex("data/result.txt"))




#############################
#                           #
#                           #
#      Pre-processing       #
#                           #
#                           #
#############################




##############
## Clean up ##
##############

# remove irrelevant columns
result <- result %>% 
  select(Type, PennElementType, Parameter, Value, EventTime, Item, Cond, Group, Participant_ID) %>% 
  rename(Subject = Participant_ID)

# isolate critical trials
data <- result %>% 
  filter(PennElementType %in% c('Selector', 'Timer'), Type == 'experiment') %>% 
  select(-c("Type"))

#####################################################
## Parse response times and add design information ##
#####################################################

# Parse RT

data_times <- data %>%
  arrange(Item) %>% 
  filter(EventTime != "Never") %>% 
  mutate(Parameter = ifelse(Parameter == "Start", "Start", "End")) %>% 
  pivot_wider(names_from = Parameter, values_from = EventTime)

data_times$Start <- data_times$Start %>% as.character() %>% as.double()
data_times$End <- data_times$End %>% as.character() %>% as.double()

start_times <- data_times$Start[!is.na(data_times$Start)]
end_times <- data_times$End[!is.na(data_times$End)]

times <- tibble(Start = start_times, End = end_times)

data_times <- data_times %>% 
  filter(Value != "Start") %>% 
  select(-Start) %>% 
  inner_join(times, by = "End") %>% 
  mutate(Time = End - Start)

# Add keys

data <- inner_join(data_times, read_csv("data/Key.csv"), by = "Item") %>% 
  select(-c("PennElementType", "End", "Start", "QuestionTarget", "Pitch")) %>% 
  mutate(Accuracy = ifelse(Value == "End", NA, as.numeric(Answer == Value)),
         Type = case_when(grepl("^f", Item) ~ "Filler",
                          grepl("^Catch", Item) ~ "Catch",
                          TRUE ~ "Critical"))

# Log response time

data <- data %>% 
  mutate(logRT = log(Time))

################################
## Exclude Trials an Subjects ##
################################

# Subjects with under 75% accuracy on fillers
exclude <- data %>% 
  filter(Type == "Filler") %>% 
  group_by(Subject) %>% 
  summarize(fAccuracy = mean(Accuracy, na.rm = TRUE)) %>% 
  filter(fAccuracy < .75) %>% 
  pull(Subject)

# Exclude subjects and trials
data <- data %>%
  filter(!Subject %in% exclude,
         !is.na(Accuracy),
         between(Time, 300, 5000),
         Type == "Critical")

# give number IDs to subjects
subj_ID <- tibble(Subject = unique(data$Subject),
                  Subject_ID = 1:60)

data <- inner_join(data, subj_ID, by = "Subject") %>% 
  rename(Subject_orig_ID = Subject,
         Subject = Subject_ID)

####################
## Add Item Norms ##
####################

data <- data %>%
  inner_join(read_csv("data/Item_norms.csv"), by = "Item") %>% 
  rename(Transitivity = trans_bias,
         SemanticFit = plaus_bias) %>% 
  # Change of language from plausibility -> semantic fit
  mutate(SemanticFit = -SemanticFit)



#############################
#                           #
#                           #
#     Descriptive Stats     #
#                           #
#                           #
#############################



###################
## Summary Stats ##
###################

data %>% 
  group_by(Cond) %>% 
  summarize_at(c('Accuracy', 'logRT'), c(mean, sd))

#######################
## Accuracy Data Viz ##
#######################

# Condition accuracy

Acc_plot <- data %>% 
  group_by(Subject, Cond) %>%
  summarize(Accuracy = mean(Accuracy)) %>%
  ggplot(aes(Cond, Accuracy)) +
  geom_violin(aes(fill = Cond), show.legend = FALSE) +
  geom_boxplot(width = 0.15) +
  scale_fill_flat() +
  theme_classic() +
  labs(fill = "", x = "Pitch Accent Condition", y = "Mean Subject Accuracy",
       title = "Mean Subject Accuracy by Condition") +
  theme(plot.title = element_text(hjust = 0.5))

### Condition accuracy by subject

Subj_acc_plot <- data %>% 
  group_by(Subject, Cond) %>% 
  summarize(Accuracy = mean(Accuracy), .groups = 'drop') %>% 
  ggplot(aes(Cond, Accuracy)) +
  geom_point() +
  facet_wrap(~Subject, ncol = 10) +
  geom_line(aes(group = 1)) +
  theme_modern()

### Condition accuracy by item

Item_acc_plot <- data %>% 
  group_by(Item, Cond) %>% 
  summarize(Accuracy = mean(Accuracy), .groups = 'drop') %>% 
  ggplot(aes(Cond, Accuracy)) +
  geom_point() +
  facet_wrap(~Item, ncol = 6) +
  geom_line(aes(group = 1)) +
  theme_modern()


#################
## RT Data Viz ##
#################

# overall RT

hist(data$logRT)

# Condition RT

RT_plot <- data %>% 
  ggplot(aes(logRT)) +
  geom_density(alpha = 0.5, color = 'black', aes(fill = Cond)) +
  labs(y = "Count", x = "Log Response Time", title = "Response Times by Condition") +
  scale_fill_flat() +
  theme_modern() +
  labs(fill = "", x = "Log Response Times", y = "",
       title = "Response Times by Condition") +
  geom_vline(aes(xintercept = log(5000))) +
  theme(plot.title = element_text(hjust=0.5),
        legend.position = c(0.85, 0.85),
        axis.line.y = element_blank(),
        axis.text.y = element_blank())

### Condition RT by subject

Subj_RT_plot <- data %>% 
  group_by(Subject, Cond) %>% 
  ggplot(aes(Cond, logRT)) + 
  geom_boxplot() +
  facet_wrap(~Subject, ncol = 10) +
  theme_modern()

### Condition RT by item

Item_RT_plot <- data %>% 
  group_by(Item, Cond) %>% 
  ggplot(aes(Cond, logRT)) + 
  geom_boxplot() +
  facet_wrap(~Item, ncol = 6) +
  theme_modern()




#############################
#                           #
#                           #
#     Accuracy Analysis     #
#                           #
#                           #
#############################


# Sum coding
data <- data %>% 
  mutate(Condition = ifelse(Cond == "Subject", -1, 1))


#####################
## Model Selection ##
#####################

### Model 1 (max)

# Find optimizer for maximal model (none)
all_fit(glmer(formula = Accuracy ~ Condition + SemanticFit + Transitivity + (1 + Condition | Subject) + (1 + Condition | Item),
              data = data, family = binomial()))

# Fit max model and evaluate random effects structure
acc_max_model <- glmer(formula = Accuracy ~ Condition + SemanticFit + Transitivity + (1 + Condition | Subject) + (1 + Condition | Item),
                       data = data, family = binomial())

# Item random effects correlation parameter is degenerate (-1)
VarCorr(acc_max_model)

# 2 components for Subject and 1 component for Item explains all variances
summary(rePCA(acc_max_model))


### Model 2 (ZCP)

# Drop correlation parameters and evaluate random effects structure
acc_ZCP_model <- glmer(formula = Accuracy ~ Condition + SemanticFit + Transitivity + (1 + Condition || Subject) + (1 + Condition || Item),
                   data = data, family = binomial())

# Item slope variance seems bit low
VarCorr(acc_ZCP_model)

# same PCA output
summary(rePCA(acc_ZCP_model))


### Model 3 (Item correlation also dropped)

# Drop condition slope in item random effects and evaluate random effects structure
acc_model_noItemSlope <- glmer(formula = Accuracy ~ Condition + SemanticFit + Transitivity + (1 + Condition || Subject) + (1 | Item),
                               data = data, family = binomial())

# Looks ok
VarCorr(acc_model_noItemSlope)
summary(rePCA(acc_model_noItemSlope))

# Check parsimony (no significant differences in goodness of fit)
anova(acc_model_noItemSlope, acc_max_model)



### Final model

acc_model <- acc_model_noItemSlope


## Evaluate model

# Some residual groups are outside the CI but they're near the tail of the distribution
binned_residuals(acc_model)

# Might be a pattern for Transitivity(?) but doesn't look very strong
binned_residuals(acc_model, term = "SemanticFit")
binned_residuals(acc_model, term = "Transitivity")


## model performance

model_performance(acc_model)


## Check output

summary(acc_model)

tab_model(acc_model,
          auto.label = FALSE, show.ci = FALSE,
          show.stat = TRUE,
          collapse.se = TRUE, linebreak = FALSE,
          show.intercept = FALSE,
          pred.labels = c("Pitch (Verb)", "SemanticFit", "Transitivity"),
          string.est = "Estimate (SE)", string.pred = " ", string.stat = "t", string.p = "p",
          show.re.var = FALSE, show.obs = FALSE, show.r2 = FALSE, show.icc = FALSE, show.ngroups = FALSE)



##########################
## Significance Testing ##
##########################

# Test significance of Condition
acc_model_noCondition <- glmer(formula = Accuracy ~ SemanticFit + Transitivity + (1 | Subject) + (1 | Item),
                               data = data, family = binomial())

anova(acc_model_noCondition, acc_model)

# Test significance of Semantic Fit
acc_model_noSemanticFit <- glmer(formula = Accuracy ~ Condition + Transitivity + (1 + Condition || Subject) + (1 | Item),
                                 data = data, family = binomial())

anova(acc_model_noSemanticFit, acc_model)

# Test significance of Transitivity
acc_model_noTransitivity <- glmer(formula = Accuracy ~ Condition + SemanticFit + (1 + Condition || Subject) + (1 | Item),
                                  data = data, family = binomial())

anova(acc_model_noTransitivity, acc_model)



#############################
#                           #
#                           #
#        RT Analysis        #
#                           #
#                           #
#############################

#####################
## Model Selection ##
#####################

### Model 1 (max)

# Find optimizer for maximal model (none)
all_fit(lmer(formula = logRT ~ Condition + SemanticFit + Transitivity + Accuracy + (1 + Condition | Subject) + (1 + Condition | Item),
             data = data, REML = FALSE))

# Fit max model and evaluate random effects structure
RT_max_model <- lmer(formula = logRT ~ Condition + SemanticFit + Transitivity + Accuracy + (1 + Condition | Subject) + (1 + Condition | Item),
                     data = data, REML = FALSE)

# Subject random effects correlation parameter is degenerate (1)
VarCorr(RT_max_model)

# 1 component for Subject and 2 components for Item explains all variances
summary(rePCA(RT_max_model))


### Model 2 (ZCP)

# Drop correlation parameters and evaluate random effects structure
RT_ZCP_model <- lmer(formula = logRT ~ Condition + SemanticFit + Transitivity + Accuracy + (1 + Condition || Subject) + (1 + Condition || Item),
                      data = data, REML = FALSE)

# Subject slope variance seems bit low
VarCorr(RT_ZCP_model)

# same PCA output
summary(rePCA(RT_ZCP_model))


### Model 3 (Subject correlation also dropped)

# Drop condition slope in item random effects and evaluate random effects structure
RT_model_noSubjSlope <- lmer(formula = logRT ~ Condition + SemanticFit + Transitivity + Accuracy + (1 | Subject) + (1 + Condition || Item),
                             data = data, REML = FALSE)

# Looks ok
VarCorr(RT_model_noSubjSlope)
summary(rePCA(RT_model_noSubjSlope))

# Check parsimony (no significant differences in goodness of fit)
anova(RT_model_noSubjSlope, RT_max_model)



### Final model

RT_model <- RT_model_noSubjSlope


## Evaluate model (looks ok)

check_model(RT_model)


## Model performance

model_performance(RT_model)


## Check output

summary(RT_model)

tab_model(RT_model,
          auto.label = FALSE, show.ci = FALSE,
          show.stat = TRUE,
          collapse.se = TRUE, linebreak = FALSE,
          show.intercept = FALSE,
          pred.labels = c("Pitch (Verb)", "SemanticFit", "Transitivity", "Accuracy"),
          string.est = "Estimate (SE)", string.pred = " ", string.stat = "t", string.p = "p",
          show.re.var = FALSE, show.obs = FALSE, show.r2 = FALSE, show.icc = FALSE, show.ngroups = FALSE)



##########################
## Significance Testing ##
##########################

# Test significance of Condition
RT_model_noCondition <-lmer(formula = logRT ~ SemanticFit + Transitivity + Accuracy + (1 | Subject) + (1 | Item),
                            data = data, REML = FALSE)

anova(RT_model_noCondition, RT_model)

# Test significance of Semantic Fit
RT_model_noSemanticFit <- lmer(formula = logRT ~ Condition + Transitivity + Accuracy + (1 | Subject) + (1 + Condition || Item),
                               data = data, REML = FALSE)

anova(RT_model_noSemanticFit, RT_model)

# Test significance of Transitivity
RT_model_noTransitivity <- lmer(formula = logRT ~ Condition + SemanticFit + Accuracy + (1 | Subject) + (1 + Condition || Item),
                                data = data, REML = FALSE)

anova(RT_model_noTransitivity, RT_model)

# Test significance of Accuracy
RT_model_noAccuracy <- lmer(formula = logRT ~ Condition + SemanticFit + (1 | Subject) + (1 + Condition || Item),
                                data = data, REML = FALSE)

anova(RT_model_noAccuracy, RT_model)