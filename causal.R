library(ggdag)
library(dagitty)
library(ggplot2)
library(dplyr)

dag <- dagify(x ~ u,
                  m ~ x,
                  y ~ u + m,
                  labels = c("x" = "Smoking",
                             "y" = "Cancer",
                             "m" = "Tar",
                             "u" = "Genotype"),
                  latent = "u",
                  exposure = "x",
                  outcome = "y")
tidy_dagitty(dag) +
ggdag(dag) +
  theme_dag() +
  geom_dag_label_repel()


tidy_dagitty(dag, layout = "nicely", seed = 2) %>%
  mutate(linetype = if_else(direction == "->", "solid", "dashed")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(end_cap = ggraph::circle(10, "mm"), edge_linetype = linetype)) +
  geom_dag_point() +  
  geom_dag_text(col = "white") +
  labs(title = "The causal effect of X is identifiable",
       subtitle = "There's no bi-directed path between X and its descendats")
