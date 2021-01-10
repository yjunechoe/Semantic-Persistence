library(tidyverse)

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



## prep

keys <- vroom::vroom(
  "https://raw.githubusercontent.com/yjunechoe/Semantic-Persistence/master/data/Key.csv",
  col_types = 'cccc'
)

norms <- vroom::vroom(
  "https://raw.githubusercontent.com/yjunechoe/Semantic-Persistence/master/data/Item_norms.csv",
  col_types = 'cdd'
) %>% 
  transmute(Item, semantic_fit = -plaus_bias, trans_bias)

exp_df <- result %>% 
  select(Type, PennElementType, Parameter, Value, EventTime, Item, Cond, Group, Participant_ID) %>% 
  rename(Subject = Participant_ID) %>% 
  filter(PennElementType %in% c('Selector', 'Timer'), Type == 'experiment') %>% 
  select(-c("Type")) %>% 
  arrange(Item) %>% 
  filter(EventTime != "Never") %>% 
  mutate(
    Parameter = if_else(Parameter == "Start", "Start", "End"),
    EventTime = as.numeric(EventTime)
  ) %>% 
  pivot_wider(names_from = Parameter, values_from = EventTime) %>% 
  group_by(Subject, Item, Cond, Group) %>% 
  summarize(
    Value = Value[PennElementType == "Selector"],
    Time = End[2] - Start[1],
    .groups = 'drop'
  ) %>% 
  inner_join(keys, by = "Item") %>% 
  select(-c(QuestionTarget, Pitch)) %>% 
  mutate(
    Accuracy = as.numeric(Answer == Value),
    logRT = log(Time),
    Type = case_when(
      str_detect(Item, "^f") ~ "Filler",
      str_detect(Item, "^Catch") ~ "Catch",
      TRUE ~ "Critical"),
  ) %>% 
  group_by(Subject) %>% 
  filter(
    mean(Accuracy[Type == "Filler"], na.rm = TRUE) > .75,
    !is.na(Accuracy),
    between(Time, 300, 5000)
  ) %>% 
  ungroup() %>% 
  mutate(Subject = as.integer(as.factor(Subject))) %>% 
  left_join(norms, by = "Item")


#############
## Figures ##
#############
library(extrafont)
library(ggtext)

###################
# Norming scores ##
###################

norming_score_plot <- norms %>% 
  ggplot(aes(semantic_fit, trans_bias)) +
  geom_vline(aes(xintercept = 0), linetype = 2, alpha = .3) +
  geom_hline(aes(yintercept = 0), linetype = 2, alpha = .3) +
  ggrepel::geom_text_repel(
    aes(label = Item),
    size = 5,
    family = "Roboto"
  ) + 
  coord_equal(xlim = c(-2, 2), ylim = c(-2, 2)) +
  labs(
    title = "Normed local coherence variables",
    subtitle = "z-scored results from two off-line norming experiments",
    x = "Semantic Fit",
    y = "Transitivity Bias"
  ) +
  junebug::theme_junebug_clean() +
  theme(
    axis.title = element_text(face = "plain")
  )

ggsave('norming_score_plot.png', width = 18, height = 18, dpi = 600, scale = .4)


########################
# Recording durations ##
########################
library(rPraat)
library(glue)
library(here)
library(patchwork)


word.len <- function(tg, word) {
  i <- which(tg$Word$label == word)
  tg$Word$t2[i] - tg$Word$t1[i]
}

reg.len <- function(tg) {
  i <- which(tg$Region$label == "x")
  tg$Region$t2[i] - tg$Region$t1[i]
}

textgrids <- list.files(here("../recordings/final/annotated"), "\\.TextGrid$")
sounds <- list.files(here("../recordings/final/annotated"), "\\.wav$")

recordings_key <- vroom::vroom(here("../recordings/final/Key.csv"), col_types = 'ccccc') %>% 
  mutate(File = str_replace(File, ".wav", ".TextGrid")) %>% 
  mutate(FilePath = here("../recordings/final/annotated", File))

dur <- recordings_key %>%
  select(-contains("syl")) %>% 
  mutate(
    tg = map(FilePath, tg.read),
    V_dur = map2_dbl(tg, V, ~word.len(.x, .y)),
    NP2_dur = map2_dbl(tg, NP2, ~word.len(.x, .y)),
    region_dur = map2_dbl(tg, V, ~word.len(.x, .y)),
    syntax = rep(c("early", "late"), times = n()/2)
  )

dur_wider <- dur %>%
  select(File, contains("dur"), syntax) %>% 
  mutate(File = str_extract(File, ".*(?=_(early|late).TextGrid$)")) %>% 
  pivot_wider(names_from = syntax, values_from = contains("dur"),
              values_fill = list(V_dur = 0, NP2_dur = 0, region_dur = 0)) %>% 
  select(File, contains("early"), contains("late"))

dur_normed <- dur_wider %>% 
  mutate(ref_norm = region_dur_early/region_dur_late,
         V_dur_resynth = V_dur_late * ref_norm,
         NP2_dur_resynth = NP2_dur_late * ref_norm) %>% 
  select(-ref_norm)

dur_normed_long <- dur_normed %>% 
  pivot_longer(cols = -File, names_to = "Measure", values_to = "Duration") %>% 
  mutate(Duration = 1000* Duration)

dur_raw <- dur %>% 
  select(-Sentence, -NP1, -V, -NP2, -FilePath, -tg) %>% 
  pivot_longer(-c('File', 'syntax'), names_to = "Measure", values_to = "Duration") %>% 
  mutate(Measure = paste0(Measure, "_", syntax),
         Duration = Duration * 1000) %>% 
  select(-syntax) %>% 
  mutate(
    File = str_extract(File, ".*(?=_(early|late)\\.TextGrid)"),
    Type = paste0("raw_", str_extract(Measure, "(early|late)$")),
    Measure = str_extract(Measure, "^.*(?=_dur)")
  )
  
dur_target <- dur_normed %>%
  mutate(File = paste0(File, "_early.wav"),
         V_target = ((V_dur_late - V_dur_early)/2 + V_dur_early)/V_dur_early,
         NP2_target = ((NP2_dur_late - NP2_dur_early)/2 + NP2_dur_early)/NP2_dur_early
  ) %>% 
  select(File, contains("target")) %>% 
  mutate(
    File = str_extract(File, ".*(?=\\_early.wav)")
  ) %>% 
  pivot_longer(
    cols = -File,
    names_to = "Measure",
    names_pattern = "(.*)_target",
    values_to = "ratio"
  ) %>% 
  left_join(
    dur_raw %>% 
      filter(Type == "raw_early") %>% 
      select(-Type),
    by = c("File", "Measure")
  ) %>% 
  mutate(
    Type = "resynth",
    Duration = ratio * Duration
  )


dur_all <- bind_rows(
  dur_target %>% 
    select(-ratio),
  dur_raw
) %>% 
  extract(File, c("Item", "Pitch"), "^(.*)_(S|V)$") %>% 
  filter(Measure != "region") %>% 
  mutate(
    Pitch = c("S" = "Subject", "V" = "Verb")[Pitch],
    Type = factor(Type, levels = c("raw_early", "resynth", "raw_late")),
    Measure = factor(Measure, levels = c("V", "NP2"))
  )

dur_all_plot_df <- dur_all %>% 
  mutate(
    color = ifelse(str_detect(Type, "resynth"), "black", "grey40"),
    Measure = fct_recode(
      Measure,
      "Adjunct Verb" = "V",
      "Post-verbal Noun" = "NP2"
    ),
    Type = fct_recode(
      Type,
      "Early Closure<br>(raw recording)" = "raw_early",
      "Resynthesized<br>Stimuli" = "resynth",
      "Late Closure<br>(raw recording)" = "raw_late"
    ),
    Type = fct_relabel(
      Type, ~ {
        color <- ifelse(str_detect(.x, "Resynthesized"), "#202020", "#505050")
        glue("<span style='color:{color}'>{.x}</span>")
      }
    )
  )

dur_plot_fun <- function(Cond) {
  dur_all_plot_df %>% 
    filter(Pitch == Cond) %>% 
    ggplot(aes(Type, Duration, color = color)) +
    geom_boxplot(
      outlier.alpha = 0,
      width = .6
    ) + 
    ggbeeswarm::geom_quasirandom(
      alpha = .6,
      shape = 16,
      width = .3
    ) +
    scale_color_identity() +
    scale_y_continuous(
      labels = function(x) {
        reversed_labs <- rev(x)
        last_lab <- detect_index(reversed_labs, ~!is.na(.x))
        rev(replace(reversed_labs, last_lab, paste0(reversed_labs[last_lab], "ms")))
      }
    ) +
    facet_wrap(~ Measure) +
    labs(
      title = "Duration of words in the ambiguous region",
      x = glue("Recordings in the {toupper(Cond)} accent condition"),
      y = NULL
    ) +
    junebug::theme_junebug_clean() +
    theme(
      axis.text.x = element_markdown(
        lineheight = 1.2,
        margin = margin(t = 0.2, unit = "cm")
      ),
      axis.text.y = element_text(
        margin = margin(r = 0.1, unit = "cm")
      )
    )
}


dur_plot <- wrap_plots(
  dur_plot_fun("Subject"),
  dur_plot_fun("Verb") +
    labs(title = NULL) +
    theme(plot.margin = margin(t = 0, unit = "cm"))
  ,
  ncol = 1
)

ggsave('durations_plot.png', dur_plot, width = 24, height = 32, dpi = 600, scale = .4)


#################
# Recording F0 ##
#################
library(kableExtra)

pitch <- read_delim(here("../recordings/final/resynthesized", "F0_list.txt"), delim = "\t", col_names = FALSE) %>% 
  select(1, 2, 6, 8, 12)

names(pitch) <- c("File", "NP1", "NP1_f0_max", "V", "V_f0_max")

pitch_df <- pitch %>% 
  pivot_longer(contains("f0"), names_to = "Measure", values_to = "Maxf0", names_pattern = "^([A-Z])") %>% 
  mutate(
    Pitch = str_extract(File, "(S|V)$"),
    Pitch = c("S" = "Subject Accent", "V" = "Verb Accent")[Pitch],
    Measure = c("N" = "Adjunct Subject", "V" = "Adjunct Verb")[Measure]
  )

pitch_tbl <- pitch_df %>% 
  group_by(Pitch, Measure) %>% 
  summarize(
    across(Maxf0, list("Mean" = mean, "Std. Dev." = sd), .names = "{fn}"),
    .groups = 'drop'
  ) %>% 
  mutate(
    across(where(is.numeric), ~ round(.x, 1)),
    Pitch = replace(Pitch, c(2, 4), ""),
    Distribution = ""
  ) %>% 
  rename(
    "Condition" = 1,
    "Word" = 2
  ) %>% 
  kbl() %>% 
  kable_classic(full_width = FALSE) %>% 
  add_header_above(c(" " = 1, " " = 1, "Max F0" = 3), align = "center") %>% 
  kable_styling(
    bootstrap_options = "none", 
    font_size = 18,
    html_font = "Roboto Slab"
  ) %>% 
  column_spec(1, width_min = "4cm") %>% 
  column_spec(2, width_min = "4cm") %>% 
  column_spec(3, width_min = "2cm") %>% 
  column_spec(4, width_min = "2.5cm") %>%
  column_spec(
    5,
    extra_css = "border-left: 3px solid transparent;",
    width_min = "3cm",
    image = spec_boxplot(
      x = pitch_df %>%
        group_split(Pitch, Measure) %>%
        map(~.x$Maxf0),
      col = "grey",
      border = "black",
      res = 150
    )) %>% 
  row_spec(2, extra_css = "border-bottom: 1px solid black;")

save_kable(pitch_tbl, file = "pitch_tbl.png", zoom = 3)






