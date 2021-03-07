## Slopegraph

mytheme <- list(  
  # move the x axis labels up top
  scale_x_discrete(position = "top"),
  theme_bw(),
  # Format tweaks
  # Remove the legend
  theme(legend.position = "none"),
  # Remove the panel border
  theme(panel.border     = element_blank()),
  # Remove just about everything from the y axis
  theme(axis.title.y = element_blank()),
  theme(axis.text.y = element_blank()),
  theme(panel.grid.major.y = element_blank()),
  theme(panel.grid.minor.y = element_blank()),
  # Remove a few things from the x axis and increase font size
  theme(axis.title.x = element_blank()),
  theme(panel.grid.major.x = element_blank()),
  theme(axis.text.x.top = element_text(size=12)),
  # Remove x & y tick marks
  theme(axis.ticks = element_blank()),
  # Format title & subtitle
  theme(plot.title = element_text(size=14, face = "bold", hjust = 0.5)),
  theme(plot.subtitle = element_text(hjust = 0.5))
)

# https://acaird.github.io/computers/r/2013/11/27/slopegraphs-ggplot
# https://ibecav.github.io/slopegraph/
