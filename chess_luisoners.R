library(tidyverse)
library(readr)
library(ggrepel)
library(bigchess)
library(ggpmisc)
library(devtools)
library(ggpubr)
library(extrafont)
loadfonts(device = "win")
library(ggplot2)
library(hrbrthemes)
df <- read_csv("lichess_tournament_2021.07.16_NGYZktjj_v-torneo-luisoners (1).csv")
df<- df %>%
  mutate(Gain = (Performance - Rating))
df
df <- df %>%
  filter(df$Score >=16)
score_model <- lm(Score ~ Rating, data = df)
score_model
summary(score_model)
regression_points <- get_regression_points(score_model, ID = "Username")
regression_points
p1 <- ggplot(df, aes(x = Rating, y = Score)) +
  geom_smooth(method = "lm", se = F, color = "yellow") +
  geom_point(size = 2.3, alpha = 1, color = "green") +
  geom_text_repel(aes(label = Username), color = "white", size = 2.3) +
  labs(x = "Rating", y = "Score",
       title = "V Luisoners Arena. Scatterplot of relationship of Rating and Score") +
  stat_regline_equation(label.y = 73, aes(label = ..eq.label..), color = "red", size = 2.3) +
  stat_regline_equation(label.y = 70, aes(label = ..rr.label..), color = "red", size = 2.3) +
  labs(title = "V Luisoners Arena. Relationship between Rating and Score",
       subtitle = "",
       caption = "Lichess.org | @dataR_amateur") +
  theme( text = element_text(family = "mono", color = "grey20"),
         axis.text.x = element_text(face="bold", color="black",
                                    size=9),
         axis.text.y = element_text(face="bold", color="black",
                                    size=9),
         axis.title.x = element_text(color="blue", size=9, face="bold"),
         axis.title.y = element_text(color="blue", size=9, face="bold"),
         panel.background = element_rect(fill = "black", colour = "blue"),
         panel.grid = element_line(color = "black"),
         plot.caption = element_text(hjust = 1),
         plot.title = element_text(colour = "black", face = "bold",
                                   hjust = 0.5))
p1
p1 + ggsave("chess_relat.png", width = 13, height = 8.5, dpi = 500)






#-------- bigchess

library(bigchess)


data <- read.pgn("lichess_tournament_2021.07.16_NGYZktjj_v-torneo-luisoners.pgn")

data <- data %>% 
  filter(data$NMoves >=2)

ggplot(data, aes(x=W1, y = B1)) +
  geom_col(position = "dodge",fill = "grey50", colour = "black") +
  ggtitle("White's Opening Move") +
  theme_minimal()


ggplot(data, aes(x=W1, y = B1, Widht = Result, fill = factor(Result))) +
  geom_raster(position = "dodge", hjust = 0.5, vjust = 0.5, stat = "identity") +
  ggtitle("Blancas y Negras Opening Move") +
  theme_minimal()

p2 <- ggplot(data, aes(x= W1, fill = Result)) +
  geom_bar(position = "dodge", colour = "black") +
  ggtitle("White's Opening Move") +
  theme( text = element_text(family = "mono", color = "grey20"),
         axis.text.x = element_text(face="bold", color="black", 
                                    size=9),
         axis.text.y = element_text(face="bold", color="black", 
                                    size=9),
         axis.title.x = element_text(color="blue", size=9, face="bold"),
         axis.title.y = element_text(color="blue", size=9, face="bold"),
         panel.background = element_rect(fill = "white", colour = "blue"),
         panel.grid = element_blank(),
         plot.caption = element_text(hjust = 1),
         plot.title = element_text(colour = "black", face = "bold",
                                   hjust = 0.5)) +
  scale_fill_manual(values=c("green", "yellow", "red"))
p2

p3 <- ggplot(data, aes(x= B1, fill = Result)) +
  geom_bar(position = "dodge", colour = "black") +
  ggtitle("Black's Opening Move") +
  theme( text = element_text(family = "mono", color = "grey20"),
         axis.text.x = element_text(face="bold", color="black", 
                                    size=9),
         axis.text.y = element_text(face="bold", color="black", 
                                    size=9),
         axis.title.x = element_text(color="blue", size=9, face="bold"),
         axis.title.y = element_text(color="blue", size=9, face="bold"),
         panel.background = element_rect(fill = "white", colour = "blue"),
         panel.grid = element_blank(),
         plot.caption = element_text(hjust = 1),
         plot.title = element_text(colour = "black", face = "bold",
                                   hjust = 0.5)) +
  scale_fill_manual(values=c("green", "yellow", "red"))
p3

p4 <- ggplot(data, aes(x= W2, fill = Result)) +
  geom_bar(position = "dodge", colour = "black") +
  ggtitle("White's 2nd Move") +
  theme( text = element_text(family = "mono", color = "grey20"),
         axis.text.x = element_text(face="bold", color="black", 
                                    size=7, angle = 90),
         axis.text.y = element_text(face="bold", color="black", 
                                    size=9),
         axis.title.x = element_text(color="blue", size=9, face="bold"),
         axis.title.y = element_text(color="blue", size=9, face="bold"),
         panel.background = element_rect(fill = "white", colour = "blue"),
         panel.grid = element_blank(),
         plot.caption = element_text(hjust = 1),
         plot.title = element_text(colour = "black", face = "bold",
                                   hjust = 0.5)) +
  scale_fill_manual(values=c("green", "yellow", "red"))

p5 <- ggplot(data, aes(x= B2, fill = Result)) +
  geom_bar(position = "dodge", colour = "black") +
  ggtitle("Black's 2nd Move") +
  theme( text = element_text(family = "mono", color = "black"),
         axis.text.x = element_text(face="bold", color="black", 
                                    size=7, angle = 90),
         axis.text.y = element_text(face="bold", color="black", 
                                    size=9),
         axis.title.x = element_text(color="blue", size=9, face="bold"),
         axis.title.y = element_text(color="blue", size=9, face="bold"),
         panel.background = element_rect(fill = "white", colour = "blue"),
         panel.grid = element_blank(),
         plot.caption = element_text(hjust = 1),
         plot.title = element_text(colour = "black", face = "bold",
                                   hjust = 0.5)) +
  scale_fill_manual(values=c("green", "yellow", "red"))
p5



#GGARRANGE-------------
figure <- ggarrange(p2, p3, p4, p5, ncol = 2, nrow = 2, common.legend = TRUE, legend="top")
figure <- annotate_figure(figure,
                          top = text_grob("V Luisoners Arena",
                                          color = "black", face = "bold", size = 18),
                          bottom = text_grob("Lichess.org | @dataR_amateur",
                                             hjust = 1, x = 1, face = "italic", size = 8)) +
  theme(plot.background = element_rect(fill = "skyblue"))
figure
figure + ggsave("chess.png", width = 13, height = 8.5, dpi = 500)



