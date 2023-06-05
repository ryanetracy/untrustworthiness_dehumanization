##############################################
# facial untrustworthiness and dehumanization
# study 2
# primary and secondary pos/neg emotions
##############################################

url_source = 'https://github.com/ryanetracy/misc_functions/blob/main/misc_functions.R?raw=TRUE'

library(devtools)
devtools::source_url(url_source)

# packages
pckgs <- c('lme4',
           'lmerTest',
           'emmeans',
           'effsize',
           'ggcorrplot',
           'rstatix',
           'effectsize',
           'parameters',
           'interactions',
           'readxl',
           'performance',
           'tidyverse')

package_loader(pckgs)

# load the data
s2_df <- read_excel('data/experiment 2 data.xlsx')


# demographics
s2_demos <- s2_df[,c(338, 339, 341)]
names(s2_demos) <- c('age', 'sex', 'race')

mean(s2_demos$age, na.rm = T); sd(s2_demos$age, na.rm = T)

s2_demos %>%
  count(sex) %>%
  mutate(prop = 100 * (n/sum(n)))

s2_demos %>%
  count(race) %>%
  mutate(prop = 100 * (n/sum(n)))


# select only needed columns
s2_df <- s2_df[,c(1,10,13:332)]

# check participants who didn't finish and remove
s2_df <- s2_df %>%
  filter(Finished == 1) %>%
  select(-Finished)

# rename columns
new_names <- paste0(
  rep(c('untrustworthy',
        'trustworthy',
        'untrustworthy',
        'trustworthy',
        'untrustworthy',
        'trustworthy',
        'trustworthy',
        'trustworthy',
        'untrustworthy',
        'trustworthy',
        'untrustworthy',
        'trustworthy',
        'untrustworthy',
        'untrustworthy',
        'untrustworthy',
        'trustworthy',
        'untrustworthy',
        'trustworthy',
        'trustworthy',
        'untrustworthy'), each = 16),
  rep('_'),
  rep('stim'),
  rep(1:20, each = 16),
  rep('_'),
  rep(c('attraction',
        'pleasure',
        'surprise',
        'contentment',
        'compassion',
        'serenity',
        'anger',
        'disgust',
        'fear',
        'guilt',
        'shame',
        'remorse',
        'time1',
        'time2',
        'time3',
        'time4'))
)

names(s2_df)[2:321] <- new_names

# reshape
s2_long <- s2_df %>%
  pivot_longer(cols = untrustworthy_stim1_attraction:untrustworthy_stim20_time4,
               names_to = c('face_trust', 'stimID', 'trait'),
               values_to = 'rating',
               names_sep = '_') %>%
  pivot_wider(names_from = 'trait',
              values_from = 'rating') %>%
  rename('subj' = 'ResponseID') %>%
  mutate_at(.vars = c('subj', 'stimID'), .funs = as.factor)

# check response invariance
invar_check <- s2_long %>%
  group_by(subj, face_trust) %>%
  rstatix::get_summary_stats(
    attraction,
    pleasure,
    surprise,
    contentment,
    compassion,
    serenity,
    anger,
    disgust,
    fear,
    guilt,
    shame,
    remorse,
    type = 'mean_sd'
  ) %>%
  select(-c(n, mean)) %>%
  pivot_wider(names_from = 'variable',
              values_from = 'sd')

bad_ps <- invar_check %>%
  select(subj,
         face_trust,
         attraction,
         pleasure,
         surprise,
         contentment,
         compassion,
         serenity,
         anger,
         disgust,
         fear,
         guilt,
         shame,
         remorse) %>%
  filter(attraction == 0 &
           pleasure == 0 &
           surprise == 0) %>%
  filter(contentment == 0 &
           compassion == 0 &
           serenity == 0) %>%
  filter(anger == 0 &
           disgust == 0 &
           fear == 0) %>%
  filter(guilt == 0 &
           shame == 0 &
           remorse == 0)
bad_ps

s2_long <- s2_long %>%
  filter(!(subj %in% bad_ps$subj))


# check alphas for emotion ratings
psych::alpha(s2_long[,c('attraction', 'pleasure', 'surprise')])
psych::alpha(s2_long[,c('contentment', 'compassion', 'serenity')])
psych::alpha(s2_long[,c('anger', 'disgust', 'fear')])
psych::alpha(s2_long[,c('guilt', 'shame', 'remorse')])

# compute means for indices
s2_long$pos_primary <- rowMeans(s2_long[,c('attraction',
                                          'pleasure',
                                          'surprise')], na.rm = T)
s2_long$pos_secondary <- rowMeans(s2_long[,c('contentment',
                                            'compassion',
                                            'serenity')], na.rm = T)
s2_long$neg_primary <- rowMeans(s2_long[,c('anger',
                                          'disgust',
                                          'fear')], na.rm = T)
s2_long$neg_secondary <- rowMeans(s2_long[,c('guilt',
                                            'shame',
                                            'remorse')], na.rm = T)


# reshape for analyses with indices
s2_main <- s2_long %>%
  select(subj,
         stimID,
         face_trust,
         pos_primary,
         pos_secondary,
         neg_primary,
         neg_secondary) %>%
  pivot_longer(cols = pos_primary:neg_secondary,
               names_to = c('valence', 'emotion'),
               values_to = 'rating',
               names_sep = '_') %>%
  mutate(
    valence = if_else(valence == 'pos', 'positive', 'negative'),
    trust_c = if_else(face_trust == 'trustworthy', 1, -1),
    val_c = if_else(valence == 'positive', 1, -1),
    emo_c = if_else(emotion == 'primary', 1, -1)
  )



### LMM analyses ###
mod1 <- lmer(rating ~ trust_c * val_c * emo_c 
             + (trust_c|subj) + (1|stimID),
             data = s2_main)
model_summary_lmer(mod1)

# simple slopes
ss <- sim_slopes(mod1,
                 pred = trust_c,
                 modx = val_c,
                 mod2 = emo_c)$slopes |> as.data.frame()

# summarize and get effect size estimates (df based on full model)
# secondary emotions
ss_secondary_emo <- ss[, 1:7]
d_secondary <- t_to_d(t = ss_secondary_emo$t.val.,
                      df = 6447,
                      paired = T)

print(
  cbind(
    round(ss_secondary_emo, 3),
    round(d_secondary, 3)
  )
)

# primary emotions
ss_positive_emo <- ss[, 8:14]
d_primary <- t_to_d(t = ss_positive_emo$t.val..1,
                    df = 6447,
                    paired = T)

print(
  cbind(
    round(ss_positive_emo, 3),
    round(d_primary, 3)
  )
)



### plot ###
s2_summary <- Rmisc::summarySE(s2_main,
                               measurevar = 'rating',
                               groupvars = c('face_trust', 'valence', 'emotion'),
                               na.rm = T)
s2_summary

s2_participants <- Rmisc::summarySE(s2_main,
                                    measurevar = 'rating',
                                    groupvars = c('subj',
                                                  'face_trust',
                                                  'valence',
                                                  'emotion'),
                                    na.rm = T)

# change labels for facet_wrap
emo_labs <- c('Primary Emotions', 'Secondary Emotions')
names(emo_labs) <- c('primary', 'secondary')

plot_colors <- c('#003366', '#006666', '#ffd600')

s2_participants %>%
  ggplot(aes(valence, rating, fill = face_trust)) +
  geom_violin(color = 'black',
              alpha = .8,
              position = position_dodge(.9)) +
  geom_point(color = 'black',
             alpha = .2,
             position = position_jitterdodge(.15, .05, .9)) +
  geom_point(data = s2_summary,
             aes(valence, rating),
             color = plot_colors[3],
             alpha = .75,
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = s2_summary,
                aes(x = valence,
                    y = rating,
                    ymin = rating - ci,
                    ymax = rating + ci),
                width = .25,
                color = plot_colors[3],
                alpha = .75,
                position = position_dodge(.9)) +
  facet_wrap(~ emotion, labeller = labeller(emotion = emo_labs)) +
  theme_classic(base_size = 20) +
  scale_fill_manual(values = c(plot_colors[1], plot_colors[2]),
                    labels = c('Trustworthy\nTargets',
                               'Untrustworthy\nTargets')) +
  scale_x_discrete(labels = c('Negative Emotions', 'Positive Emotions')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  labs(x = '',
       y = 'Mean Rating',
       fill = '') +
  theme(legend.position = 'bottom')


# ggsave('experiment 2 means.jpg',
#        device = 'jpeg',
#        path = './plots',
#        units = 'cm')
