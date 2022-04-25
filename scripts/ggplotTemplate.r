GGP_template = theme_bw() +
               theme(axis.ticks = element_line(color="red", size=1),
                     panel.grid.minor = element_line(color="grey75", linetype=4),
                     panel.background = element_rect(fill="lightyellow", color="red"),
                     panel.grid.major = element_line(color="grey75"),
                     axis.title.x=element_text(size=14, color="orangered2"),
                     axis.title.y=element_text(size=14, color="orangered4"),
                     plot.title=element_text(size=18, face="bold",
                                             color ="darkblue"),
                     plot.subtitle=element_text(size=10, face="bold.italic",
                                                color ="brown", family="serif"));