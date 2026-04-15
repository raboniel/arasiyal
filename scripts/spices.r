# defining different styles

# 1 . the style for the general boxplots
spice_boxplot_voters <- function() {
  
  # base theme and font
  theme_linedraw(
    base_family = "Serif"
  )+ 
    
     # plot elements  
    theme(
      plot.background = element_rect(fill="#191a19",colour="#191a19"),
      plot.margin = margin(t=2,b=2,r=20,l=5),
      plot.title = element_text(size = 23, color = "#ededed",,face = "bold",margin = margin(t=10,b = 10)),
      plot.subtitle = element_text(size=19, colour = "#ededed" ),
      plot.caption = element_text(size = 13,colour="#ededed")
    )+
    
    # panel elements
    theme(
      panel.background = element_rect(fill="#727272"),
      panel.border = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour="#191a19",linewidth = 0.1),
      panel.grid.minor.y = element_line(colour="#191a19",linewidth = 0.1)
    )+
    
    # axis elements
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_text(size=17,colour = "#ededed",face = "bold"),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=17,colour = "#ededed",face="bold")
    )+
    
    # legend elements
    theme(
      legend.position = "none"
    )
  
}


## making some additions for the full analysis

spice_boxplot_voters_threekingdoms <- function() {
  
  spice_boxplot_voters()+
    
    theme(axis.text.x = element_blank())+
    
    theme(
      strip.background = element_rect(fill="darkred"),
      strip.text = element_text(color = "#ededed", size = 12,face = "bold")
    ) +
    
      theme(
    legend.background = element_rect(fill="#2b2b2b"),
    legend.title = element_text(size = 14,colour = "#EDEDED"),
    legend.text = element_text(size = 14,colour = "#EDEDED"),
    legend.position = "bottom",
    legend.justification = "centre",
    legend.key = element_rect(fill="#2b2b2b")
  )
  
  
}



# 2. the style for the urbanism

spice_boxplot_urbanism <- function() {
  # colours
  # e0b0ff - mauve
  
  # base theme and font
  theme_linedraw(
    base_family = "Liberation serif"
  )+ 
  
  # plot elements  
  theme(
    plot.background = element_rect(fill="#2b2b2b"),
    plot.margin = margin(t=2,b=2,r=20,l=5),
    plot.title = element_text(size = 23, color = "#ededed",,face = "bold",margin = margin(t=10,b = 10)),
    plot.subtitle = element_text(size=19, colour = "#ededed" ),
    plot.caption = element_text(size = 13,colour="#ededed")
   )+
  
  # panel elements
  theme(
    panel.background = element_rect(fill="#cecece"),
    panel.border = element_rect(colour="#2b2b2b"),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour="#727272",linewidth = 0.1),
    panel.grid.minor.y = element_line(colour="#727272",linewidth = 0.1)
  )+
    
  # axis elements
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size=17,colour = "#ededed")
  )+
    
  # faceting elements
  theme(
    strip.background = element_rect(fill="#e0b0ff"),
    strip.text = element_text(color = "#2b2b2b", size = 17,face = "bold")
  )+
  
  # legend elements
  theme(
    legend.background = element_rect(fill="#2b2b2b"),
    legend.title = element_text(size = 17,colour = "#EDEDED"),
    legend.text = element_text(size = 17,colour = "#EDEDED"),
    legend.position = "bottom",
    legend.justification = "left",
    legend.key = element_rect(fill="#2b2b2b")
  )
    
}

# changing it for regions
spice_boxplot_region <- function() {
  
  spice_boxplot_urbanism() +
    theme(strip.background = element_rect(fill="red"))
  
}



##### BASE
spice_boxplot <- function() {
  
  # colours
  # #2B2B2B - blackened chrome
  # #EDEDED - pale silver
  # CECECE - moonlight silver
  # 727272 - urban graphite
  # e0b0ff - mauve
  
  # base theme and font
  theme_dark(
    base_family = "Serif"
  )+ 
    
    # plot elements  
    theme(
      plot.background = element_rect(fill="#191a19"),
      plot.margin = margin(t=2,b=2,r=20,l=5),
      plot.title = element_text(size = 23, color = "#ededed",,face = "bold",margin = margin(t=10,b = 10)),
      plot.subtitle = element_text(size=19, colour = "#ededed" ),
      plot.caption = element_text(size = 13,colour="#ededed")
    )+
    
    # panel elements
    theme(
      panel.background = element_rect(fill="#343432"),
      panel.border = element_rect(colour="#191a19"),
      panel.grid.minor.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(colour="#727272",linewidth = 0.1),
      panel.grid.minor.y = element_line(colour="#727272",linewidth = 0.1)
    )+
    
    # axis elements
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_text(size=17,colour = "#ededed",face="bold")
    )+
    
    # faceting elements
    theme(
      strip.background = element_rect(fill="#e0b0ff"),
      strip.text = element_text(color = "#191a19", size = 16,face = "bold")
    )+
    
    # legend elements
    theme(
      legend.position = "none"
    )
  
}



