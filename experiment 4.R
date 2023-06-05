##############################################
# facial untrustworthiness and dehumanization
# study 4
# secondary interpersonal pos/neg emotions
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
s4_data <- read_excel('data/experiment 4 data.xlsx')


# demographics
s4_demos <- s4_data[,c(184,185,187)]
names(s4_demos) <- c('age', 'sex', 'race')

mean(s4_demos$age, na.rm = T); sd(s4_demos$age, na.rm = T)

s4_demos %>%
  count(sex) %>%
  mutate(prop = 100 * (n/sum(n)))

s4_demos %>%
  count(race) %>%
  mutate(prop = 100 * (n/sum(n)))



# drop unneeded columns
s4_data <- s4_data[,c(9,5,19:178)]

# drop participants with < 50% progress
s4_data <- s4_data %>%
  filter(Progress >= 50) %>%
  select(-Progress)

# rename columns
new_names <- paste0(
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
        'trustworthy'), each = 8),
  rep('_'),
  rep('stim'),
  rep(1:20, each = 8),
  rep('_'),
  rep(c('compassion',
        'gratitude',
        'pride',
        'selfSatisfaction',
        'guilt',
        'embarrassment',
        'contempt',
        'bitterness'))
)

names(s4_data)[2:161] <- new_names


# reshape
s4_long <- s4_data %>%
  pivot_longer(cols = trustworthy_stim1_compassion:trustworthy_stim20_contempt,
               names_to = c('face_trust', 'stimID', 'emo_cat'),
               values_to = 'rating',
               names_sep = '_') %>%
  pivot_wider(names_from = 'emo_cat',
              values_from = 'rating') %>%
  rename('subj' = 'Response ID') %>%
  mutate_at(.vars = c('subj', 'stimID'), .funs = as.factor)


# check response invariance
invar_check <- s4_long %>%
  group_by(subj, face_trust) %>%
  rstatix::get_summary_stats(
    compassion,
    gratitude,
    pride,
    selfSatisfaction,
    guilt,
    embarrassment,
    contempt,
    bitterness,
    type = 'mean_sd'
  ) %>%
  select(-c(n, mean)) %>%
  pivot_wider(names_from = 'variable',
              values_from = 'sd')

bad_ps <- invar_check %>%
  select(subj,
         face_trust,
         compassion,
         gratitude,
         pride,
         selfSatisfaction,
         guilt,
         embarrassment,
         contempt,
         bitterness) %>%
  filter(compassion == 0 &
           gratitude == 0) %>%
  filter(pride == 0 &
           selfSatisfaction == 0) %>%
  filter(guilt == 0 &
           embarrassment == 0) %>%
  filter(contempt == 0 &
           bitterness == 0)
bad_ps

s4_long <- s4_long %>%
  filter(!(subj %in% bad_ps$subj))


# check alphas of traits
psych::alpha(s4_long[,c('compassion', 'gratitude')])
psych::alpha(s4_long[,c('pride', 'selfSatisfaction')])
psych::alpha(s4_long[,c('guilt', 'embarrassment')])
psych::alpha(s4_long[,c('contempt', 'bitterness')])


# collapse into indices
s4_long$pos_other <- rowMeans(s4_long[,c('compassion', 'gratitude')],
                                     na.rm = T)
s4_long$pos_self <- rowMeans(s4_long[,c('pride', 'selfSatisfaction')],
                                    na.rm = T)
s4_long$neg_other <- rowMeans(s4_long[,c('contempt', 'bitterness')],
                                     na.rm = T)
s4_long$neg_self <- rowMeans(s4_long[,c('guilt', 'embarrassment')],
                                    na.rm = T)

# reshape with emotion indices
s4_main <- s4_long %>%
  select(subj,
         stimID,
         face_trust,
         pos_other,
         pos_self,
         neg_other,
         neg_self) %>%
  pivot_longer(cols = pos_other:neg_self,
               names_to = c('valence', 'orientation'),
               values_to = 'rating',
               names_sep = '_') %>%
  mutate(
    valence = if_else(valence == 'pos', 'positive', 'negative'),
    trust_c = if_else(face_trust == 'trustworthy', 1, -1),
    val_c = if_else(valence == 'positive', 1, -1),
    orien_c = if_else(orientation == 'self', 1, -1)
  )



### LMM analyses ###
mod1 <- lmer(rating ~ trust_c * val_c * orien_c 
             + (1|subj) + (1|stimID),
             data = s4_main)
model_summary_lmer(mod1)

# simple slopes
ss <- sim_slopes(mod1,
                 pred = trust_c,
                 modx = val_c,
                 mod2 = orien_c)$slopes |> as.data.frame()

# summarize and get effect size estimates (df based on full model)
# other-oriented emotions
ss_other_emo <- ss[, 1:7]
d_other <- t_to_d(t = ss_other_emo$t.val.,
                  df_error = 7689,
                  paired = T)

print(
  cbind(
    round(ss_other_emo, 3),
    round(d_other, 3)
  )
)

# self-oriented emotions
ss_self_emo <- ss[, 8:14]
d_self <- t_to_d(t = ss_self_emo$t.val..1,
                     df = 7689,
                     paired = T)

print(
  cbind(
    round(ss_self_emo, 3),
    round(d_self, 3)
  )
)



### plot ###
s4_summary <- Rmisc::summarySE(s4_main,
                               measurevar = 'rating',
                               groupvars = c('face_trust',
                                             'valence',
                                             'orientation'),
                               na.rm = T)
s4_summary

s4_participants <- Rmisc::summarySE(s4_main,
                                    measurevar = 'rating',
                                    groupvars = c('subj',
                                                  'face_trust',
                                                  'valence',
                                                  'orientation'),
                                    na.rm = T)


# change labels for facet wrap
emo_labs <- c('Other-Oriented Emotions', 'Self-Oriented Emotions')
names(emo_labs) <- c('other', 'self')

plot_colors <- c('#003366', '#006666', '#ffd600')

s4_participants %>%
  ggplot(aes(valence, rating, fill = face_trust)) +
  geom_violin(color = 'black',
              alpha = .8,
              position = position_dodge(.9)) +
  geom_point(color = 'black',
             alpha = .2,
             position = position_jitterdodge(.15, .05, .9)) +
  geom_point(data = s4_summary,
             aes(valence, rating),
             color = plot_colors[3],
             alpha = .75,
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = s4_summary,
                aes(x = valence,
                    y = rating,
                    ymin = rating - ci,
                    ymax = rating + ci),
                width = .25,
                color = plot_colors[3],
                alpha = .75,
                position = position_dodge(.9)) +
  facet_wrap(~ orientation, labeller = labeller(orientation = emo_labs)) +
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


# ggsave('experiment 4 means.jpg',
#        device = 'jpeg',
#        path = './plots',
#        units = 'cm')
