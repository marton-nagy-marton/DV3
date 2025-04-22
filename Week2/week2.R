library(data.table)
df <- fread('http://bit.ly/CEU-R-numbers-set')
str(df)

summary(df)

plot(df)

summary(lm(y ~ x, df))

library(ggplot2)
ggplot(df, aes(x = x, y = y)) + geom_boxplot()

for (i in 1:4) {
  print(summary(df[x == i, ]))
}

## DT approach
lapply(split(df, df$x), summary)

df[, as.list(summary(y)), by = x]

library(ggplot2)
ggplot(df, aes(factor(x), y)) + geom_boxplot() + theme_bw()

ggplot(df, aes(factor(x), y)) + geom_violin() + geom_jitter(alpha = 0.1) + theme_bw()

ggplot(df, aes(y, color = factor(x), fill = factor(x))) + geom_density(alpha = 0.1) + theme_bw()

ggplot(df, aes(y)) + geom_histogram() + theme_bw() + facet_wrap(~x)


df <- fread('http://bit.ly/CEU-R-numbers')
plot(df)

summary(df)

ggplot(df, aes(x, y)) + geom_point(alpha = 0.01) + theme_bw()

ggplot(df, aes(x, y)) + geom_tile()

ggplot(df, aes(x, y)) + geom_bin2d(binwidth = 50)

ggplot(df[, .N, by = list(round(x, -1.5), round(y, -1.5))], aes(round, round.1, fill = N)) + geom_tile()

ggplot(df, aes(x, y)) + geom_hex(binwidth = 35)

ggplot(df, aes(x, y)) + geom_point(alpha = 0.01) + theme_bw()

devtools::install_github('doehm/traceR')

library(traceR)

df2 <- trace_image(scale = FALSE)

?hclust
head(iris)

dm <- dist(iris[, 1:4])

str(dm)

# this is useful for clustering smaller datasets
hc <- hclust(dm)

str(hc)

hc$merge[1, ]

plot(hc)

rect.hclust(hc, k = 3)

for (i in 2:8) {
  plot(hc)
  rect.hclust(hc, k = i)
  Sys.sleep(1)
}

library(animation)
ani.options(interval = 1)
saveGIF({
  for (i in 2:8) {
    plot(hc)
    rect.hclust(hc, k = i)
  }
})

?animation

library(dendextend)

d <- as.dendrogram(hc)
d <- color_branches(d, k = 2)
ggplot(d, labels = FALSE) + scale_y_reverse(expand = c(0.2, 2)) + coord_polar(theta = 'x')


saveGIF({
  for (k in 2:8) {
    d <- as.dendrogram(hc)
    d <- color_branches(d, k = k)
    print(ggplot(d, labels = FALSE) + 
      scale_y_reverse(expand = c(0.2, 2)) + 
      coord_polar(theta = 'x'))
  }
})

?saveGIF

clusters <- cutree(hc, k = 3)

library(NbClust)

NbClust(iris[, 1:4], method = 'complete', index = 'all')

## TODO compare overlap on species and cluster membership

dt_iris = data.table(iris)
dt_iris[, cluster := clusters]

ggplot(dt_iris, aes(Sepal.Length, Sepal.Width, color = Species, shape = factor(cluster), linetype = factor(cluster))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

## ANIMATION BY SPECIES
saveGIF({
  for (s in unique(iris$Species)) {
    print(ggplot(dt_iris[Species == s], aes(Sepal.Length, Sepal.Width, color = factor(cluster), linetype = factor(cluster))) +
            geom_point() +
            geom_smooth(method = 'lm', se = FALSE) +
            ggtitle(s))
  }
})

unique(iris$Species)

library(gganimate)
df <- iris
df$cluster <- factor(clusters)

ggplot(df, aes(Sepal.Length, Sepal.Width, color = cluster)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(title = "{closest_state}", subtitle = '{nrow(subset(df, Species == closest_state))}') +
  transition_states(Species)

frame_vars()

library(cluster)
clusplot(iris, clusters, color = TRUE, shade = TRUE, labels = 2)

library(ggforce)

ggplot(df, aes(Sepal.Length, Sepal.Width, color = cluster)) +
  geom_point() +
  geom_mark_hull(aes(label = cluster, fill = cluster), concavity = 1)

library(datasauRus)

df <- copy(datasaurus_dozen_wide)

dt = rbindlist(lapply(seq(1,26,2), 
                      function(i) data.frame(x = df[, i, drop = TRUE],
                                             y = df[, i + 1, drop = TRUE],
                                             dataset_id = substr(colnames(df[, i]), 1, nchar(colnames(df[, i])) - 2))))

ggplot(dt, aes(x, y)) + 
  geom_point(color = 'orange') + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~dataset_id) +
  theme_minimal()

datasaurus_dozen

ggplot(datasaurus_dozen, aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  facet_wrap(~dataset) +
  theme_minimal()

ggplot(datasaurus_dozen, aes(x, y)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = FALSE) + 
  theme_minimal() +
  labs(title = 'Dataset: {closest_state}',
       subtitle = 'Mean(x): {round(mean(subset(datasaurus_dozen, dataset == closest_state)$x), 4)}') +
  transition_states(dataset, state_length = 2, wrap = TRUE)

?transition_states

round(2.43543645654, 4)

datasaurus_dozen[dataset == 'dino', x]

library(nycflights13)

f <- data.table(flights)

f_agg <- f[, .(avg_dep_delay = mean(dep_delay, na.rm = TRUE)), by = .(origin, month)]

ggplot(f_agg, aes(avg_dep_delay, reorder(origin, avg_dep_delay), fill = origin)) +
  geom_col() +
  theme_bw() +
  labs(title = 'No. of month: {closest_state}') +
  transition_states(month)

dt <- data.table(flights)[, .(avg_dep_delay = mean(dep_delay, na.rm = TRUE)), by = .(origin, month)][order(month, avg_dep_delay)][, rank := rank(avg_dep_delay), by = .(month)]

ggplot(dt, aes(x = avg_dep_delay, y = factor(rank), fill = origin)) +
  geom_col() +
  transition_states(month) +
  geom_text(aes(label = origin), hjust = 1.2, vjust = 0.5, size = 12, color = 'white') +
  theme_bw() +
  labs(title = 'Month: {month.name[as.numeric(closest_state)]} 2013',
       x = 'Avg. departure delay',
       y = 'Rank'
       ) +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
        )
  
?transition_states

dt <- data.table(flights)[, .(avg_arr_delay = mean(arr_delay, na.rm = TRUE)), by = .(dest, month)][order(month, avg_arr_delay)][, rank := order(-avg_arr_delay), by = .(month)][rank <= 10]

ggplot(dt, aes(x = avg_arr_delay, y = reorder(factor(rank), -rank), fill = dest)) +
  geom_col() +
  transition_states(month) +
  geom_text(aes(label = dest), hjust = 1.2, vjust = 0.5, size = 5, color = 'white') +
  theme_bw() +
  labs(title = 'Month: {month.name[as.numeric(closest_state)]} 2013',
       x = 'Avg. arrival delay',
       y = 'Rank'
  ) +
  theme(legend.position = 'none',
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank()
  )


ggplot(mtcars, aes(wt, qsec, color = factor(am))) + geom_point()

p <- last_plot()
p + theme_bw()

library(ggthemes)

p + theme_economist() + scale_color_economist()

theme_custom <- function() {
  theme(
    axis.text = element_text(family = 'Times New Roman', color = 'orange', size = 8),
    axis.title = element_text(family = 'Times New Roman', color = 'blue', size = 12, face = 'bold'),
    panel.background = element_rect(fill = 'white')
  )
}

p + theme_custom()

remotes::install_github('cttobin/ggthemr')

library(ggthemr)

ggthemr('pale', layout = 'scientific', spacing = 2, type = 'inner')

p

ggthemr_reset()

library(plotly)
ggplotly(p)

library(ggiraph)

girafe(ggobj = p)

p <- ggplot(mtcars, aes(wt, qsec, color = factor(am), tooltip = rownames(mtcars))) + geom_point_interactive()
girafe(ggobj = p)

