theme_benoit <- function (base_size = 10, base_family = "ArcherPro Book") {
	half_line <- base_size/2
    theme_gray(base_size = base_size, base_family = base_family) %+replace% 
        theme(
        	axis.text = element_text(size = rel(0.8), colour = rgb(68,67,59, max=255)),
            axis.title.x = element_text(size = base_size, colour = rgb(68,67,59, max=255), margin = margin(t = half_line), vjust = 1),
            axis.title.y = element_text(size = base_size, colour = rgb(68,67,59, max=255), angle=90, margin = margin(r = half_line), vjust = 1),
            plot.background = element_rect(fill="transparent", colour="transparent"),
            legend.background = element_rect(fill="transparent", colour="transparent")
        )   
}


# line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt"), 
# rect = element_rect(fill = "white", colour = "black", size = 0.5, linetype = 1),
# text = element_text(family = base_family, face = "plain", colour = "black", size = base_size, lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), debug = FALSE),
# axis.line = element_blank(),
# axis.line.x = NULL, 
# axis.line.y = NULL,
# axis.text.x = element_text(margin = margin(t = 0.8 * half_line/2), vjust = 1),
# axis.text.x.top = element_text(margin = margin(b = 0.8 * half_line/2), vjust = 0),
# axis.text.y = element_text(margin = margin(r = 0.8 * half_line/2), hjust = 1),
# axis.text.y.right = element_text(margin = margin(l = 0.8 * half_line/2), hjust = 0),
# axis.ticks = element_line(colour = "grey20"),
# axis.ticks.length = unit(half_line/2, "pt"), 
# axis.title.x = element_text(margin = margin(t = half_line), vjust = 1), 
# axis.title.x.top = element_text(margin = margin(b = half_line), vjust = 0), 
# axis.title.y = element_text(angle = 90, margin = margin(r = half_line), vjust = 1), 
# axis.title.y.right = element_text(angle = -90, margin = margin(l = half_line), vjust = 0),