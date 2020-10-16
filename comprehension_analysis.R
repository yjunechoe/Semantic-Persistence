###########
## SETUP ##
###########

setwd("C:/Users/jchoe/OneDrive/Active research/Thesis/experiments")

library(tidyverse)
library(see)
library(stringr)
library(performance)
library(sjPlot)
library(lme4)
library(afex)

options(dplyr.summarise.inform = FALSE)

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
result <- as_tibble(read.pcibex("comprehension/result.txt"))




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

data <- inner_join(data_times, read_csv("comprehension/key.csv"), by = "Item") %>% 
  select(-c("PennElementType", "End", "Start", "QuestionTarget", "Pitch")) %>% 
  mutate(Accuracy = ifelse(Value == "End", NA, as.numeric(Answer == Value)),
         Type = case_when(grepl("^f", Item) ~ "Filler",
                          grepl("^Catch", Item) ~ "Catch",
                          TRUE ~ "Critical"))

# Log response time

data <- data %>% 
  mutate(logRT = log(Time))






#############################
#                           #
#                           #
#     Data Exploration      #
#                           #
#                           #
#############################


# reasponse times by trial types
trial_type_RT_plot <- all_data %>%
  filter(Time < 5000) %>% 
  ggplot(aes(Type, Time)) +
  geom_violin() +
  geom_boxplot(width = 0.2) +
  facet_wrap(~Cond)

# subject accuracy by trial types
trial_type_acc_plot <- all_data %>%
  group_by(Subject, Cond, Type) %>%
  summarize(Accuracy = mean(Accuracy)) %>%
  filter(Type == "Catch" && Accuracy != 0, !is.na(Accuracy)) %>% 
  ggplot(aes(Type, Accuracy)) +
  geom_boxplot() +
  geom_hline(aes(yintercept = 0.5), linetype = 2, color = "red") +
  facet_wrap(~Cond)





#############################
#                           #
#                           #
#     Data Preparation      #
#                           #
#                           #
#############################


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
         between(Time, 300, 5000))

# give number IDs to subjects
subj_ID <- tibble(Subject = unique(data$Subject),
                  Subject_ID = 1:60)

data <- inner_join(data, subj_ID, by = "Subject") %>% 
  rename(Subject_orig_ID = Subject,
         Subject = Subject_ID)

###############################
## Keep Only Critical Trials ##
###############################

all_data <- data

data <- data %>% 
  filter(Type == "Critical")

####################
## Add Item Norms ##
####################

data <- data %>%
  inner_join(read_csv("Item_norms.csv"), by = "Item") %>% 
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







# mean z-scored RT by condition rainplot

library(mdthemes)
library(ggtext)
library(ggrepel)
extrafont::loadfonts(device = 'win')
devtools::source_url("https://raw.githubusercontent.com/yjunechoe/geom_paired_raincloud/master/geom_paired_raincloud.R")

subject_RT_dist <- data %>% 
  group_by(Subject) %>% 
  summarize(mean = mean(logRT), sd = sd(logRT))

rainplot_data <- all_data %>% 
  right_join(subject_RT_dist, by = 'Subject') %>% 
  mutate(z_RT = (logRT - mean)/sd) %>% 
  filter(Type == "Critical" |
           str_detect(Item, ("(Tyler|Arthur|Harry|Tommy|Carlson|Parker|Thomas|Kelly|Nathan|Connor|Jennie|Bella)")) & Accuracy == 1) %>% 
  group_by(Item, Cond, Type) %>% 
  summarize(z_RT = mean(z_RT)) %>% 
  ungroup()

rainplot <- rainplot_data %>% 
  mutate(Cond = ifelse(str_detect(Item, "^f"), ifelse(Cond == "Verb", "Group B", "Group A"), Cond)) %>% 
  mutate(Cond = factor(Cond, levels = c("Group A", "Subject", "Group B", "Verb"))) %>% 
  ggplot(aes(Cond, z_RT, fill = Cond)) +
  geom_paired_raincloud(alpha = .5) +
  geom_point(aes(group = Item),
             position = position_nudge(c(.15, -.15)),
             alpha = .5, shape = 16) +
  geom_line(aes(group = Item),
            position = position_nudge(c(.18, -.18)),
            linetype = 3) +
  geom_boxplot(position = position_nudge(c(.07, -.07)),
               alpha = .5, width = .04, outlier.shape = " ") +
  facet_wrap(~Type, scales = "free_x",
             labeller = as_labeller(c('Critical' = "Critical (garden-path)",
                                      "Filler" = "Filler (non-garden-path)"))) +
  geom_curve(
    aes(x = 1.12, y = -.65, xend = 1.3, yend = -1.1),
    data = tibble(Type = "Critical"), inherit.aes = FALSE,
    arrow = arrow(ends = "first", length = unit(.1, "in"))
  ) +
  ggtext::geom_richtext(
    aes(x = 1.32, y = -1.1), family = "Adelle",
    data = tibble(Type = "Critical"), inherit.aes = FALSE,
    label = "Each dot represents z-scored<br>mean log-RT for an ITEM",
    hjust = 0, label.color = NA, fill = NA
  ) +
  ggpubr::geom_bracket(data = tibble(Type = "Filler"), inherit.aes = FALSE,
    xmin = .9, xmax = 2.1, label = "",
    y.position = -.2, tip.length = .07, hjust = 0, size = .5
  ) +
  ggtext::geom_richtext(
    aes(x = .8, y = -0.1), family = "Adelle",
    data = tibble(Type = "Filler"), inherit.aes = FALSE,
    label = "Same fillers used across experiment groups",
    hjust = 0, label.color = NA, fill = NA
  ) +
  scale_x_discrete(expand = expansion(.7)) +
  scale_y_continuous(expand = expansion(.1), breaks = scales::pretty_breaks(5)) +
  scale_fill_manual(values = rep(c("grey35", "#43b7c2", "grey35", "#ba5800"))) +
  theme_classic() +
  labs(title = "Response time distributions aggregated by item",
       y = NULL, x = NULL) + 
  theme(text = element_text(family = "Montserrat Medium", size = 12),
        panel.grid.major.y = element_blank(),
        strip.background = element_blank(),
        plot.title.position = "plot",
        plot.background = element_blank(),
        plot.margin = margin(1, 1, .7, .7, 'cm'),
        plot.title = element_markdown(size = 24,  margin = margin(0, 0, 1, 0, 'cm'), family = "Lora", face = "bold"),
        plot.subtitle = element_text(size = 14, margin = margin(0, 0, .5, 0, 'cm')),
        strip.text = element_markdown(size = 16, margin = margin(0, 0, .5, 0, 'cm')),
        axis.text = element_markdown(size = 12),
        axis.text.x.bottom = element_text(size = 16),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(margin = margin(0, .2, 0, 0, 'cm')),
        axis.text.x = element_text(margin = margin(t = .3, unit = 'cm')),
        axis.title = element_markdown(size = 16),
        axis.title.y = element_text(angle = 0, vjust = .5, lineheight = 1.2, hjust = 0,
                                    margin = margin(0, .5, 0, 0, 'cm')),
        axis.title.x = element_text(margin = margin(.5, 0, 0, 0, 'cm')),
        legend.position = 0)


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

acc_table <- tab_model(acc_model,
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
acc_model_noCondition <- glmer(formula = Accuracy ~ SemanticFit + Transitivity + (1 + Condition || Subject) + (1 | Item),
                               data = data, family = binomial())

acc_cond <- anova(acc_model_noCondition, acc_model)

# Test significance of Semantic Fit
acc_model_noSemanticFit <- glmer(formula = Accuracy ~ Condition + Transitivity + (1 + Condition || Subject) + (1 | Item),
                                 data = data, family = binomial())

acc_sem <- anova(acc_model_noSemanticFit, acc_model)

# Test significance of Transitivity
acc_model_noTransitivity <- glmer(formula = Accuracy ~ Condition + SemanticFit + (1 + Condition || Subject) + (1 | Item),
                                  data = data, family = binomial())

acc_trans <- anova(acc_model_noTransitivity, acc_model)





# Model subject random effects
acc_subj_ranef <- acc_model %>% 
  ranef() %>% 
  pluck("Subject") %>% 
  as_tibble(rownames = "ID") %>% 
  rename(Intercept = `(Intercept)`) %>% 
  mutate(Intercept_est = Intercept + tidy(acc_model)[[1, "estimate"]],
         Condition_est = Condition + tidy(acc_model)[[2, "estimate"]]) %>% 
  select(!Intercept:Condition) %>% 
  pivot_longer(-ID, names_pattern = "(.*)_(.*)", names_to = c("term", "measure")) %>% 
  mutate(conf_int = ifelse(term == "Intercept", tidy(acc_model)[[1, "std.error"]], tidy(acc_model)[[2, "std.error"]]),
         conf_int = conf_int * 1.96,
         ID = tidytext::reorder_within(ID, value, term),
         term = ifelse(term == "Condition", "Condition Slope", term),
         term = glue::glue("**{term}**"),
         term = fct_reorder(term, value, 'max', .desc=T))

acc_subj_ranef_plot <- acc_subj_ranef %>% 
  ggplot(aes(ID, value)) + 
  facet_wrap(~term, scales = "free") +
  tidytext::scale_x_reordered() +
  geom_errorbar(aes(ymin = value - conf_int, ymax = value + conf_int),
                width = 0, color = 'grey50') +
  geom_point(color = 'grey20') +
  geom_hline(aes(yintercept = 0), linetype = 2, color = 'red') +
  # geom_text(data = tibble(x = c(25, 20), y = c(.1, .02),
  #                         label = c("Above chance accuracy on average", "Reliable negative effect of verb accent"),
  #                         term = fct_inorder(levels(acc_subj_ranef$term))),
  #           aes(x, y, label = label), inherit.aes = FALSE, color = 'red') +
  md_theme_clean() +
  labs(title = "Accuracy Model Subject Random Effects",
       y = "Log Odds", x = "Subjects (_n_=60)") + 
  theme(text = element_text(family = "Verdana", size = 12),
        plot.title = element_markdown(hjust = 0.5, size = 18),
        strip.text = element_markdown(size = 14),
        axis.text = element_markdown(size = 10),
        axis.title = element_markdown(size = 14),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.caption = element_markdown(size = 9),
        legend.position = 0)

#ggsave("acc_subj_ranef.png", acc_subj_ranef_plot, width = 10, height = 5, units = 'in', dpi = 300)

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

RT_table <- tab_model(RT_model,
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
RT_model_noCondition <-lmer(formula = logRT ~ SemanticFit + Transitivity + Accuracy + (1 | Subject) + (1 + Condition || Item),
                            data = data, REML = FALSE)

RT_cond <- anova(RT_model_noCondition, RT_model)

# Test significance of Semantic Fit
RT_model_noSemanticFit <- lmer(formula = logRT ~ Condition + Transitivity + Accuracy + (1 | Subject) + (1 + Condition || Item),
                               data = data, REML = FALSE)

RT_sem <- anova(RT_model_noSemanticFit, RT_model)

# Test significance of Transitivity
RT_model_noTransitivity <- lmer(formula = logRT ~ Condition + SemanticFit + Accuracy + (1 | Subject) + (1 + Condition || Item),
                                data = data, REML = FALSE)

RT_trans <- anova(RT_model_noTransitivity, RT_model)

# Test significance of Accuracy
RT_model_noAccuracy <- lmer(formula = logRT ~ Condition + SemanticFit + (1 | Subject) + (1 + Condition || Item),
                                data = data, REML = FALSE)

RT_acc <- anova(RT_model_noAccuracy, RT_model)

# Summary Table
library(gt)
library(broom)
library(broom.mixed)

acc_tidy <- acc_model %>% 
  tidy() %>% 
  filter(effect == "fixed") %>% 
  select(term, estimate, std.error) %>% 
  slice(-1) %>% 
  mutate(LRT = map(list(acc_cond, acc_sem, acc_trans),
                   ~as_tibble(.x)),
         chisq = map_dbl(LRT, ~.x$Chisq[2]),
         p = map_dbl(LRT, ~.x$`Pr(>Chisq)`[2]),
         model = "Accuracy") %>% 
  select(-LRT) %>% 
  mutate(across(where(is.numeric), ~str_pad(round(.x, 2), 4, "right", "0"))) %>% 
  mutate(p = ifelse(str_detect(p, "^0+$"), "<.001", p),
         estimate = paste0(estimate, " (", std.error, ")")) %>% 
  select(-std.error)

RT_tidy <- RT_model %>% 
  tidy() %>% 
  filter(effect == "fixed") %>% 
  select(term, estimate, std.error) %>% 
  slice(-1) %>% 
  mutate(LRT = map(list(RT_cond, RT_sem, RT_trans, RT_acc),
                   ~as_tibble(.x)),
         chisq = map_dbl(LRT, ~.x$Chisq[2]),
         p = map_dbl(LRT, ~.x$`Pr(>Chisq)`[2]),
         model = "Response Time") %>% 
  select(-LRT) %>% 
  mutate(across(where(is.numeric), ~str_pad(round(.x, 2), 4, "right", "0"))) %>% 
  mutate(p = ifelse(str_detect(p, "^0+$"), "<.001", p),
         estimate = paste0(estimate, " (", std.error, ")")) %>% 
  select(-std.error)


acc_tidy %>% 
  select(-5) %>% 
  gt() %>% 
  opt_row_striping() %>%
  opt_all_caps() %>%
  opt_table_font("Roboto") %>% 
  opt_table_outline()

