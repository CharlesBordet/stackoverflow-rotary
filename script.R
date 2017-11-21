
library(MASS)

library(data.table)

dt <- list()
for (y in 2008:2017) {
    dt <- append(dt, list(fread(paste0("./data/stackoverflow_", y, ".csv"))))
}

answers0 <- rbindlist(dt)
answers0[, ":="(Id = as.numeric(Id),
                Reputation = as.numeric(Reputation),
                Count = as.numeric(Count),
                Year = as.numeric(Year),
                Month = as.numeric(Month))]

answers1 <- answers0[!(Year == "2017" & Month == "11") &
                         !(Year == "2008" & Month == "7")]
answers1[, Date := as.Date(paste(Year, Month, "15", sep = "-"))]

answers2 <- answers1[CJ(Date = Date, Id = Id, unique = TRUE), 
                     on = c("Date", "Id")][, .(Id, Count, Date)]
answers2[is.na(Count), Count := 0]

answers3 <- merge(answers2, 
                  unique(answers1[, .(Id, Reputation, DisplayName)]), 
                  all.x = TRUE, by = c("Id"))
answers3[, ":="(Year = as.numeric(format(Date, "%Y")),
                Month = as.numeric(format(Date, "%m")))]

# Visualize the number of answers from high-reputation users over time

library(ggplot2)
answers.overall <- answers3[, .(nb_answers = sum(Count),
                                mean_answers = mean(Count)), by = Date]

ggplot(answers.overall, aes(x = Date, y = mean_answers)) +
    geom_line() +
    geom_smooth(method = "loess") +
    theme_minimal() +
    labs(x = "", y = "# réponses", 
         title = "Nombre de réponses des utilisateurs réputés",
         subtitle = "Depuis 2008") +
    theme(text = element_text(size = 16))
ggsave("stackoverflow nombre de réponses des utilisateurs réputés.png",
       width = 12, height = 8)


# For each user, fit a Quasi-Poisson model to test whether there is a trend over
# time or not.

answers3[, monthNumber := (Year - 2008) * 12 + Month - 7]

library(broom)
answers4 <- answers3[Year < 2012]
answers5 <- answers4[answers4[, .I[sum(Count) >= 1], by = "Id"]$V1]
models0 <- answers5[, cbind(tidy(glm(Count ~ monthNumber, 
                                     family = "quasipoisson")),
                            Answers = sum(Count)),
                    by = c("Id", "Reputation", "DisplayName")]
models0 <- models0[term == "monthNumber"]

ggplot(models0, aes(x = p.value)) +
    geom_histogram(binwidth = 0.05, center = 0.025) +
    theme_minimal() + 
    labs(title = "P-values des régressions de Quasi-Poisson",
         subtitle = "Avant 2012",
         x = "P-value",
         y = "Comptage") +
    theme(text = element_text(size = 16))
ggsave("stackoverflow pvalue des régressions de quasi-poisson avant 2012.png",
       width = 12, height = 8)

models0[, adjusted.p.value := p.adjust(p.value)]
models0[, sum(adjusted.p.value < 0.05)] # 902
models0[, sum(adjusted.p.value < 0.05) / .N] # 21%

models0[adjusted.p.value < 0.05, sum(exp(estimate) - 1 > 0)] # 782
models0[adjusted.p.value < 0.05, sum(exp(estimate) - 1 < 0)] # 120

models0[adjusted.p.value >= 0.05, early_category := "Autres"]
models0[adjusted.p.value < 0.05 & estimate < 0, early_category := "Lâcheurs Précoces"]
models0[adjusted.p.value < 0.05 & estimate > 0, early_category := "Adopteurs Précoces"]

answers6 <- merge(answers3, models0[, .(Id, early_category)], by = "Id", all.x = TRUE)
answers6[is.na(early_category), early_category := "Inexistants"]

answers.overall <- answers6[Year < 2012, .(mean_answers = mean(Count)), 
                            by = c("Date", "early_category")]
ggplot(answers.overall, aes(x = Date, y = mean_answers, col = early_category)) +
    geom_line() +
    theme_minimal() +
    labs(x = "", y = "# réponses", 
         title = "Nombre moyen de réponses des utilisateurs réputés",
         subtitle = "Avant 2012") +
    scale_colour_discrete(name = "Catégorie") +
    theme(text = element_text(size = 16))
ggsave("stackoverflow nombre moyen de réponses avant 2012.png",
       width = 12, height = 8)

##

library(broom)
answers7 <- answers3[Year >= 2012]
answers8 <- answers7[answers7[, .I[sum(Count) >= 1], by = "Id"]$V1]
models1 <- answers8[, cbind(tidy(glm(Count ~ monthNumber, 
                                     family = "quasipoisson")),
                            Answers = sum(Count)),
                    by = c("Id", "Reputation", "DisplayName")]
models1 <- models1[term == "monthNumber"]

ggplot(models1, aes(x = p.value)) +
    geom_histogram(binwidth = 0.05, center = 0.025) +
    theme_minimal() + 
    labs(title = "P-values des régressions de Quasi-Poisson",
         subtitle = "Après 2012",
         x = "P-value",
         y = "Comptage") +
    theme(text = element_text(size = 18))
ggsave("stackoverflow pvalue des régressions de quasi-poisson après 2012.png",
       width = 12, height = 8)

models1[, adjusted.p.value := p.adjust(p.value)]
models1[, sum(adjusted.p.value < 0.05)] # 1939
models1[, sum(adjusted.p.value < 0.05) / .N] # 36%

models1[adjusted.p.value < 0.05, sum(exp(estimate) - 1 > 0)] # 259
models1[adjusted.p.value < 0.05, sum(exp(estimate) - 1 < 0)] # 1680

models1[adjusted.p.value >= 0.05, late_category := "Autres"]
models1[adjusted.p.value < 0.05 & estimate < 0, late_category := "Lâcheurs Tardifs"]
models1[adjusted.p.value < 0.05 & estimate > 0, late_category := "Adopteurs Tardifs"]

answers9 <- merge(answers6, models1[, .(Id, late_category)], by = "Id", all.x = TRUE)
answers9[is.na(late_category), late_category := "Inexistants"]

t <- table(unique(answers9[, .(Id, early_category, late_category)])[, .(early_category, late_category)])
t <- data.frame(t)
colnames(t) <- c("Catégorie avant 2012", "Catégorie après 2012", "N")
DT::datatable(t, rownames = F, options = list("dom" = "t"))

answers9[, category := interaction(early_category, late_category, sep = " - ")]

answers.overall <- answers9[, .(mean_answers = mean(Count),
                                N = .N), 
                            by = c("Date", "category")][N >= 180]
ggplot(answers.overall, aes(x = Date, y = mean_answers, col = category)) +
    geom_line() +
    theme_minimal() +
    labs(x = "", y = "# réponses", 
         title = "Nombre moyen de réponses des utilisateurs réputés",
         subtitle = "Depuis 2008") +
    scale_colour_discrete(name = "Catégorie") +
    theme(text = element_text(size = 18))
ggsave("stackoverflow nombre moyen de réponses depuis 2008 inexistants adopteurs tardifs.png",
       width = 12, height = 8)

ggplot(answers.overall[category == "Inexistants - Adopteurs Tardifs"], aes(x = Date, y = mean_answers, col = category)) +
    geom_line() +
    theme_minimal() +
    labs(x = "", y = "# réponses", 
         title = "Nombre moyen de réponses des utilisateurs réputés",
         subtitle = "Depuis 2008") +
    scale_colour_discrete(name = "Catégorie") +
    theme(text = element_text(size = 18))
ggsave("stackoverflow nombre moyen de réponses depuis 2008 inexistants adopteurs tardifs.png",
       width = 12, height = 8)










# Isn't it normal that people become active, they gain reputation, and after
# some time they simply stop using the website because they have enough of it?

# Hypothesis: The normal behavior of a person is to become active, gain reputation,
# stay a couple years, and then stop being active.

# That would mean that if we look only at high-reputation users, we notice them
# when they start to decrease their activity.

# There are:
# - Joiners: Phase 1, the user becomes more active. We don't see many of them because
# for most they have passed this phase at 20000 of reputation.
# - Others: Phase 2, the user starts to stagnate his activity. He even may have started
# to decrease, but it's not significant yet.
# - Quitters: Phase 3, the user is getting tired and decreases his activity. He
# also has less reasons to keep grinding the reputation.

# Pour vérifier cette hypothèse, faire un GLM avec time^2


# JOINERS
set.seed(333)
joiners <- answers3[category == "Joiners"][Id %in% sample(Id, size = 9)]

ggplot(joiners, aes(x = Date, y = Count)) +
    geom_line() + 
    geom_smooth(method = "loess") +
    facet_wrap(~ DisplayName, scale = "free_y") +
    theme_minimal() +
    labs(x = "", y = "# answers",
         title = "Number of answers from \"Joiners\" over time",
         subtitle = "9 users chosen at random")

# OTHERS
set.seed(337)
others <- answers3[category == "Others"][Id %in% sample(Id, size = 9)]

ggplot(others, aes(x = Date, y = Count)) +
    geom_line() + 
    geom_smooth(method = "loess") +
    facet_wrap(~ DisplayName, scale = "free_y") +
    theme_minimal() +
    labs(x = "", y = "# answers",
         title = "Number of answers from \"Others\" over time",
         subtitle = "9 users chosen at random")

# QUITTERS
set.seed(337)
quitters <- answers3[category == "Quitters"][Id %in% sample(Id, size = 9)]

ggplot(quitters, aes(x = Date, y = Count)) +
    geom_line() + 
    geom_smooth(method = "loess") +
    facet_wrap(~ DisplayName, scale = "free_y") +
    theme_minimal() +
    labs(x = "", y = "# answers",
         title = "Number of answers from \"Quitters\" over time",
         subtitle = "9 users chosen at random")



# Interesting examples
# "I'd be curious to see what my profile looks like, btw. My experience with R 
# is exactly 0, though. – Martijn Pieters♦"
# https://imgur.com/C7SwS5s
qplot(x = Date, y = Count, data = answers1[Id == 100297], geom = "line")

# David Robinson
qplot(x = Date, y = Count, data = answers1[Id == 712603], geom = "line")















