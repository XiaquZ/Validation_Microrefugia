library(ggeffects)
library(ggplot2)
library(ggtext)
library(showtext)
library(gghighlight)
library(tidyverse)

####################################################################
#### Create panel plots for marginal effects for each predictor.####
####################################################################
# Second argument = path to .otf-file
font_add("fa-reg", "I:/SVG/otfs/Font Awesome 6 Free-Regular-400.otf")
font_add("fa-brands", "I:/SVG/otfs/Font Awesome 6 Brands-Regular-400.otf")
font_add("fa-solid", "I:/SVG/otfs/Font Awesome 6 Free-Solid-900.otf")
showtext_auto()

# Load the model.
load("I:/DATA/output/lm_rdata/lm_macro_fv.RData")
sum <- summary(lm_macro_fv)
sum
# FV
macrofv <- predict_response(
    lm_macro_fv, c("MI_FV", "macro_diff [0.4, 1.0, 1.6]"),
    mragin = "mean_mode"
)
macrofv
p_macrofv <- sum$coefficients[4, 4] # p-value of the interaction term.

annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " (a) ",
        paste("p = 0.059")
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) # adjust

svg("I:/SVG/MEB/lm_macro_fv.svg")
plot(macrofv) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high,
    fill = as.factor(group)), 
    alpha = 0.5, linetype = 0
  ) +
  geom_line(
    aes(x = x, y = predicted, color = as.factor(group)), size = 1.4) +
  labs(
    x = "MI of forward velocity",
    y = "Delta CIT",
    fill = "Legend",  # Label for the legend
    color = "Macro Diff",  # Label for the legend
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar,
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
   scale_fill_manual(
    values = c("#1b485e", "#568b87", "#b5d1ae")
    ) +  # Customize colors for ribbons
  scale_color_manual(
    values = c("#122740", "#326b77", "#80ae9a")
    ) + # Ensure line colors match ribbon colors
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()

# Linear model between macroclimate and warming magnitude.
load("I:/DATA/output/lm_rdata/lm_macro_wm.RData")
sum <- summary(lm_macro_wm)
sum

# Warming magnitude
macrowm <- predict_response(
    lm_macro_wm, c("MI_wm", "macro_diff [0.4, 1.0, 1.6]"),
    mragin = "mean_mode"
)
macrowm
p_macrowm <- sum$coefficients[4, 4] # P-value of the interaction term

annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " (b) ",
        paste("p = 0.206")
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) # adjust

svg("I:/SVG/MEB/lm_macro_wm.svg")
plot(macrowm) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high,
    fill = as.factor(group)), 
    alpha = 0.5, linetype = 0
  ) +
  geom_line(
    aes(x = x, y = predicted, color = as.factor(group)), size = 1.4) +
  labs(
    x = "MI of warming magnitude",
    y = NULL,
    fill = "Legend",  # Label for the legend
    color = "Macro Diff",  # Label for the legend
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar,
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
   scale_fill_manual(
    values = c("#1b485e", "#568b87", "#b5d1ae")
    ) +  # Customize colors for ribbons
  scale_color_manual(
    values = c("#122740", "#326b77", "#80ae9a")
    ) + # Ensure line colors match ribbon colors
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()

# Linear model for backward velocity
load("I:/DATA/output/lm_rdata/lm_macro_bv.RData")
sum <- summary(lm_macro_bv)
sum

# Backward velocity
macrobv <- predict_response(
    lm_macro_bv, c("MI_BV", "macro_diff [0.4, 1.0, 1.6]"),
    mragin = "mean_mode"
)
macrobv
p_macrobv <- sum$coefficients[4, 4] # P-value of the interaction term

annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " C ",
        paste("p = 0.977")
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) # adjust

svg("I:/SVG/MEB/lm_macro_bv.svg")
plot(macrobv) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high,
    fill = as.factor(group)), 
    alpha = 0.5, linetype = 0
  ) +
  geom_line(
    aes(x = x, y = predicted, color = as.factor(group)), size = 1.4) +
  labs(
    x = "MI of backward velocity",
    y = "Delta CIT",
    fill = "Legend",  # Label for the legend
    color = "Macro Diff",  # Label for the legend
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar,
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
   scale_fill_manual(
    values = c("#1b485e", "#568b87", "#b5d1ae")
    ) +  # Customize colors for ribbons
  scale_color_manual(
    values = c("#122740", "#326b77", "#80ae9a")
    ) + # Ensure line colors match ribbon colors
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()

#### Linear model for backward velocity. ####
load("I:/DATA/output/lm_rdata/lm_macro_diff_cit.RData")
sum <- summary(lm_macro)
sum

# Macroclimate difference
macrodiff <- predict_response(
    lm_macro,
    mragin = "mean_mode"
)
macrodiff
p_macro <- sum$coefficients[2, 4] # P-value of the interaction term

annotations <- data.frame(
    xpos = c(-Inf, Inf),
    ypos = c(Inf, Inf),
    annotateText = c(
        " C ",
        paste("p < 0.001")
    ),
    hjustvar = c(0, 1),
    vjustvar = c(1, 1)
) # adjust

svg("I:/SVG/MEB/lm_macrodiff.svg")
plot(macrodiff) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high,
    fill = as.factor(group)), 
    alpha = 0.5, linetype = 0
  ) +
  geom_line(
    aes(x = x, y = predicted, color = as.factor(group)), size = 1.4) +
  labs(
    x = "Delta macroclimate",
    y = "Delta CIT",
    fill = "Legend",  # Label for the legend
    color = "Macro Diff",  # Label for the legend
    title = NULL
  ) +
  geom_richtext(
    data = annotations, label.colour = NA, fill = NA, aes(
      x = xpos, y = ypos,
      hjust = hjustvar, vjust = vjustvar,
      label = annotateText
    ),
    size = 8, col = "black",
  ) +
  scale_y_continuous(labels = scales::label_number(accuracy = 0.01)) +
  scale_x_continuous(labels = scales::label_number(accuracy = 0.1)) +
   scale_fill_manual(
    values = "#b5d1ae"
    ) +  # Customize colors for ribbons
  scale_color_manual(
    values = "#80ae9a"
    ) + # Ensure line colors match ribbon colors
  theme_light() +
  theme(
    axis.line = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    text = element_text(size = 25)
  )
dev.off()
