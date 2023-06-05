##############################################
# facial untrustworthiness and dehumanization
# study 3
# primary and secondary pos/neg emotions
# different secondary/neg emotions
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
s3_df <- read_excel("data/experiment 3 data.xlsx")


# demographics
mean(s3_df$age, na.rm = T); sd(s3_df$age, na.rm = T)

s3_df %>%
  count(gender) %>%
  mutate(prop = 100 * (n/sum(n)))

s3_df %>%
  count(ethnicity) %>%
  mutate(prop = 100 * (n/sum(n)))

# check participants on remaining metrics
s3_df <- s3_df %>%
  mutate(dupIP = duplicated(s3_df$IPAddress)) %>%
  filter(dupIP == F &
           Finished == 1 &
           Q1 == 1)

# attention check validation
s3_df %>%
  count(upsideDownTF)

s3_df %>%
  count(smilingTF)

s3_df %>%
  count(angryTF)

# check primary attention check method
s3_df <- s3_df %>%
  filter(upsideDownTF == 2)


# select only needed columns
s3_df <- s3_df %>%
  select(-contains(c('Q1',
                     'Date',
                     'Status',
                     'Progress',
                     'IPAdd',
                     'Duration', 
                     'Finished', 
                     'Recipient', 
                     'External',
                     'Location',
                     'Distribution',
                     'Language',
                     '_DO', 
                     'upsideDownTF', 
                     'seenBefore',
                     'deviceUsed',
                     'age', 
                     'gender',
                     '_TEXT',
                     'ethnicity',
                     'problems',
                     'dupIP',
                     'smilingTF',
                     'angryTF')))
colnames(s3_df)

# rename columns
new_names <- paste0(
  rep('stim'),
  rep(1:20, each = 12),
  rep('_'),
  rep(c('trustworthy',
        'untrustworthy',
        'untrustworthy',
        'trustworthy', 
        'trustworthy', 
        'trustworthy',
        'untrustworthy',
        'untrustworthy',
        'trustworthy',
        'untrustworthy',
        'untrustworthy',
        'untrustworthy',
        'trustworthy', 
        'untrustworthy',
        'trustworthy',
        'trustworthy',
        'untrustworthy',
        'trustworthy', 
        'untrustworthy', 
        'trustworthy'), each = 12),
  rep('_'),
  rep(c('attraction',
        'pleasure',
        'surprise',
        'contentment',
        'compassion',
        'serenity',
        'anger',
        'fear',
        'disgust',
        'indignation',
        'bitterness', 
        'contempt'))
)

names(s3_df)[2:241] <- new_names


# reshape
s3_long <- s3_df %>%
  pivot_longer(cols = stim1_trustworthy_attraction:stim20_trustworthy_contempt,
               names_to = c('stimID', 'face_trust', 'emo_cat'),
               values_to = 'rating',
               names_sep = '_') %>%
  pivot_wider(names_from = 'emo_cat',
              values_from = 'rating') %>%
  rename('subj' = 'ResponseId') %>%
  mutate_at(.vars = c('subj', 'stimID'), .funs = as.factor)

# check response invariance
invar_check <- s3_long %>%
  group_by(subj, face_trust) %>%
  rstatix::get_summary_stats(
    attraction,
    pleasure,
    surprise,
    contentment,
    compassion,
    serenity,
    anger,
    fear,
    disgust,
    indignation,
    bitterness, 
    contempt,
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
         fear,
         disgust,
         indignation,
         bitterness, 
         contempt) %>%
  filter(attraction == 0 &
           pleasure == 0 &
           surprise == 0) %>%
  filter(contentment == 0 &
           compassion == 0 &
           serenity == 0) %>%
  filter(anger == 0 &
           disgust == 0 &
           fear == 0) %>%
  filter(indignation == 0 &
           bitterness == 0 &
           contempt == 0)
bad_ps

s3_long <- s3_long %>%
  filter(!(subj %in% bad_ps$subj))

# check alphas for emotion ratings
psych::alpha(s3_long[,c('attraction', 'pleasure', 'surprise')])
psych::alpha(s3_long[,c('contentment', 'compassion', 'serenity')])
psych::alpha(s3_long[,c('anger', 'disgust', 'fear')])
psych::alpha(s3_long[,c('indignation', 'bitterness', 'contempt')])

# compute indices
s3_long$pos_primary <- rowMeans(s3_long[,c('attraction',
                                           'pleasure',
                                           'surprise')], na.rm = T)
s3_long$pos_secondary <- rowMeans(s3_long[,c('contentment',
                                             'compassion',
                                             'serenity')], na.rm = T)
s3_long$neg_primary <- rowMeans(s3_long[,c('anger',
                                           'disgust',
                                           'fear')], na.rm = T)
s3_long$neg_secondary <- rowMeans(s3_long[,c('indignation',
                                             'bitterness',
                                             'contempt')], na.rm = T)

# reshape with only indices
s3_main <- s3_long %>%
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
             + (trust_c|subj) + (0 + trust_c|stimID),
             data = s3_main)
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
s3_summary <- Rmisc::summarySE(s3_main,
                               measurevar = 'rating',
                               groupvars = c('face_trust', 'valence', 'emotion'),
                               na.rm = T)
s3_summary

s3_participants <- Rmisc::summarySE(s3_main,
                                    measurevar = 'rating',
                                    groupvars = c('subj',
                                                  'face_trust',
                                                  'valence',
                                                  'emotion'),
                                    na.rm = T)

# change labels for facet wrap
emo_labs <- c('Primary Emotions', 'Secondary Emotions')
names(emo_labs) <- c('primary', 'secondary')

plot_colors <- c('#003366', '#006666', '#ffd600')

s3_participants %>%
  ggplot(aes(valence, rating, fill = face_trust)) +
  geom_violin(color = 'black',
              alpha = .8,
              position = position_dodge(.9)) +
  geom_point(color = 'black',
             alpha = .2,
             position = position_jitterdodge(.15, .05, .9)) +
  geom_point(data = s3_summary,
             aes(valence, rating),
             color = plot_colors[3],
             alpha = .75,
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = s3_summary,
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


# ggsave('experiment 3 means.jpg',
#        device = 'jpg',
#        path = './plots',
#        units = 'cm')
