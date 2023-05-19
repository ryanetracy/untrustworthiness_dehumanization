##############################################
# facial untrustworthiness and dehumanization
# study 1
# agency and experience traits
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
s1_df <- read_excel('data/experiment 1 data.xlsx')

# demographics
s1_demos <- s1_df[,c(298,299,301)]
names(s1_demos) <- c('age', 'sex', 'race')

mean(s1_demos$age, na.rm = T); sd(s1_demos$age, na.rm = T)

s1_demos %>%
  count(sex) %>%
  mutate(prop = 100 * (n/sum(n)))

s1_demos %>%
  count(race) %>%
  mutate(prop = 100 * (n/sum(n)))


# select only needed columns
s1_df <- s1_df[,-c(2:9,11,12,293:307)]

# remove participants who didn't finish
s1_df <- s1_df %>%
  filter(Finished == 1) %>%
  select(-Finished)

# rename columns
names_cols <- paste0(
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
        'trustworthy'), each = 14),
  rep('_'),
  rep('stim'),
  rep(1:20, each = 14),
  rep('_'),
  rep(c('planning',
        'doThings',
        'haveIntentions',
        'feelPain',
        'haveEmotions',
        'sympathy',
        'competence',
        'warmth',
        'trustworthy',
        'dominant',
        'timing01',
        'timing02',
        'timing03',
        'timing04'))
  )

names(s1_df)[2:281] <- names_cols

# reshape
s1_long <- s1_df %>%
  pivot_longer(cols = trustworthy_stim1_planning:trustworthy_stim20_timing04,
               names_to = c('face_trust', 'stimID', 'trait'),
               values_to = 'rating',
               names_sep = '_') %>%
  pivot_wider(names_from = 'trait', values_from = 'rating') %>%
  rename('subj' = 'ResponseID')

# check response invariance
invar_check <- s1_long %>%
  group_by(subj, face_trust) %>%
  rstatix::get_summary_stats(
    planning,
    doThings,
    haveIntentions,
    feelPain,
    haveEmotions,
    sympathy,
    competence,
    warmth,
    trustworthy,
    dominant,
    type = 'mean_sd'
  ) %>%
  select(-c(n, mean)) %>%
  pivot_wider(names_from = 'variable',
              values_from = 'sd')

bad_ps <- invar_check %>%
  select(subj,
         face_trust,
         planning,
         doThings,
         haveIntentions,
         feelPain,
         haveEmotions,
         sympathy,
         competence,
         warmth,
         dominant,
         trustworthy) %>%
  filter(planning == 0 &
           doThings == 0 &
           haveIntentions == 0) %>%
  filter(feelPain == 0 &
           haveEmotions == 0 &
           sympathy == 0) %>%
  filter(competence == 0 |
           warmth == 0 |
           dominant == 0 |
           trustworthy == 0)
bad_ps

s1_long <- s1_long %>%
  filter(!(subj %in% bad_ps$subj))

# check trait reliability
psych::alpha(s1_long[,c('planning', 'doThings', 'haveIntentions')])
psych::alpha(s1_long[,c('feelPain', 'haveEmotions', 'sympathy')])

# create indices
s1_long$agency <- rowMeans(s1_long[,c('planning',
                                      'doThings',
                                      'haveIntentions')],
                          na.rm = T)
s1_long$experience <- rowMeans(s1_long[,c('feelPain',
                                          'haveEmotions',
                                          'sympathy')],
                              na.rm = T)


# reshape for agency/experience traits only
s1_main <- s1_long %>%
  select(subj,
         stimID,
         face_trust,
         agency,
         experience) %>%
  pivot_longer(cols = agency:experience,
               names_to = 'trait_cat',
               values_to = 'rating') %>%
  mutate(
    trust_c = if_else(face_trust == 'trustworthy', 1, -1),
    trait_c = if_else(trait_cat == 'agency', 1, -1)
  )




### LMM analyses ###
mod1 <- lmer(rating ~ trust_c * trait_c 
             + (trust_c|subj) + (0 + trust_c|stimID),
             data = s1_main)
model_summary_lmer(mod1)


# explore interaction
# agency traits
mod2.1 <- lmer(rating ~ trust_c 
               + (trust_c|subj) + (0 + trust_c|stimID),
               data = filter(s1_main, trait_c == 1))
model_summary_lmer(mod2.1)

# experience traits
mod2.2 <- lmer(rating ~ trust_c 
               + (trust_c|subj) + (0 + trust_c|stimID),
               data = filter(s1_main, trait_c == -1))
model_summary_lmer(mod2.2)




### additional traits ###
s1_long <- s1_long %>%
  mutate(trust_c = if_else(face_trust == 'trustworthy', 1, -1))

# trustworthiness
mod_trust <- lmer(trustworthy ~ trust_c
                  + (trust_c|subj) + (0 + trust_c|stimID),
                  data = s1_long)
model_summary_lmer(mod_trust)

# warmth
mod_warm <- lmer(warmth ~ trust_c
                 + (trust_c|subj) + (0 + trust_c|stimID),
                 data = s1_long)
model_summary_lmer(mod_warm)

# dominance
mod_dom <- lmer(dominant ~ trust_c
                + (trust_c|subj) + (0 + trust_c|stimID),
                data = s1_long)
model_summary_lmer(mod_dom)

# competence
mod_comp <- lmer(competence ~ trust_c
                 + (trust_c|subj) + (0 + trust_c|stimID),
                 data = s1_long)
model_summary_lmer(mod_comp)




### plot ###
s1_summary <- Rmisc::summarySE(s1_main,
                               measurevar = 'rating',
                               groupvars = c('face_trust', 'trait_cat'),
                               na.rm = T)
s1_summary

s1_participants <- Rmisc::summarySE(s1_main,
                                    measurevar = 'rating',
                                    groupvars = c('subj',
                                                  'face_trust',
                                                  'trait_cat'),
                                    na.rm = T)

plot_colors <- c('#003366', '#006666', '#ffd600')

s1_participants %>%
  ggplot(aes(trait_cat, rating, fill = face_trust)) +
  geom_violin(color = 'black',
              alpha = .8,
              position = position_dodge(.9)) +
  geom_point(color = 'black',
             alpha = .2,
             position = position_jitterdodge(.15, .05, .9)) +
  geom_point(data = s1_summary,
             aes(trait_cat, rating),
             color = plot_colors[3],
             alpha = .75,
             shape = 7,
             size = 3,
             position = position_dodge(.9)) +
  geom_errorbar(data = s1_summary,
                aes(x = trait_cat,
                    y = rating,
                    ymin = rating - ci,
                    ymax = rating + ci),
                width = .25,
                color = plot_colors[3],
                alpha = .75,
                position = position_dodge(.9)) +
  theme_classic() +
  scale_fill_manual(values = c(plot_colors[1], plot_colors[2]),
                    labels = c('Trustworthy\nTargets',
                               'Untrustworthy\nTargets')) +
  scale_x_discrete(labels = c('Agency Traits', 'Experience Traits')) +
  scale_y_continuous(breaks = seq(1, 7, 1)) +
  labs(x = '',
       y = 'Mean Rating',
       fill = '') +
  theme(legend.position = 'bottom')
  

# ggsave('experiment 1 means.jpg',
#        device = 'jpeg',
#        path = './plots',
#        units = 'cm')
