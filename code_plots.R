peer.exp$group <- 1:nrow(peer.exp)

apatheme=theme_bw()+
   theme(panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         panel.border=element_blank(),
         axis.line=element_line(),
         text=element_text(family="serif"),
         legend.title=element_blank(),
         axis.text.y=element_text(size = 12),
         axis.text.x=element_text(size = 12))

create_plot <- function(data, x_var, y_var, title, subtitle = "", y_label = NULL, y_scale_func = NULL) {
   plot <- data %>%
      ggplot(aes_string(x = x_var, y = y_var, color = "paper")) +
      geom_bar(stat = "identity", width = 0.4) +
      labs(x = "Process Specifications", y = y_label %||% y_var,
           title = title, subtitle = subtitle, caption = "") +
       apatheme + scale_color_viridis(discrete = TRUE, option = "viridis")
   
   if (!is.null(y_scale_func)) {
      plot <- plot + scale_y_continuous(labels = y_scale_func)
   }
   
   return(plot)
}

mc.plot <- create_plot(exp, "index", "input_ps", "Variable cost per kg of sludge", y_label = "EUR/Kg")
mr.plot <- create_plot(exp, "index", "mr", "Variable revenue per kg of sludge", y_label = "EUR/Kg")
mp.plot <- create_plot(exp, "index", "mp", "Variable profit per kg of sludge", y_label = "EUR/Kg")
rr.plot <- create_plot(exp, "index", "recovery_ratio", "Phosphorus recovery ratio", y_label = "in %")
npv.plot <- create_plot(exp, "index", "npv", "", y_label = "NPV in EUR", y_scale_func = scales::label_number_si())+ theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())
p_npv <- par$p_rock_P /(100)
lcop.plot <- create_plot(exp, "index", "LCOP", "", y_label = "price in EUR") + 
   geom_hline(yintercept = p_npv, linetype = "dashed", color = "red") +
   annotate("text", x = Inf, y = 23, vjust = 1, hjust = 1, size=5, label = "Price used for NPV estimation", color = "red")  + theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())

te.plot <- create_plot(exp, "index", "effcrs", "Technical Efficiency", y_label = "score")
npv.peers.plot <- create_plot(peer.exp, "group", "npv", "Net Present Value of Phosphorus Recovery", y_label = "NPV in EUR", y_scale_func = scales::label_number_si()) 
facet.exp$Scenario <- 0
facet.exp <- facet.exp %>%
   mutate(Scenario = recode(sce,
         "npv"  = "0 basis",
         "npv1" = "1 larger processor", 
         "npv2" = "2 higher sludge content",
         "npv3" = "3 higher discount rate",
         "npv4" = "4 disposal savings",
         "npv5" = "5 high phosphorus prices",
         "npv6" = "6 low phosphorus prices",
         "npv7" = "7 high energy price",
         "npv8" = "8 crisis scenario",
         )) 

plot <- ggplot(facet.exp, aes(x = group, y = npv_l, fill = paper)) +
   geom_bar(stat = "identity", position = "dodge") +
   facet_wrap(~Scenario, ncol = 3) +
   labs(x = "Technically Efficient Process Specifications", y = "Values",
        title = "",
        subtitle = "", 
        caption = "") + apatheme + theme(legend.position = "bottom") +
   scale_fill_viridis(discrete = TRUE, option = "viridis") +
   scale_y_continuous(labels = scales::label_number_si()) 
peer.exp$group <- 1:nrow(peer.exp)

# cost shares https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
input.exp$Input <- 0
input.exp <- input.exp %>%
   mutate(Input = recode(input,
                         "Yenergy"   = "Energy",
                         "Yexp_chem" = "Chemicals",
                         "Yinv"      = "Investment",

   )) 
input.plot <- ggplot(input.exp, aes(fill=Input, y=value, x=index)) + 
   geom_bar(position="fill", stat="identity") +
   labs(x = "Process Specifications", y = "Input share",
        title = "",
        subtitle = "", 
        caption = "") + apatheme + scale_fill_viridis(discrete = TRUE, option = "viridis")

shares <- input.exp %>%
   group_by(index) %>%
   mutate(total_value = sum(value),
          share = value / total_value) %>%
   ungroup() %>% filter(Input == "Energy")

mean(shares$share)

ggplot(input.exp, aes(fill=Input, y=value, x=index)) + 
   geom_bar(position="stack", stat="identity")


c.plot <- npv.plot +
         lcop.plot + plot_annotation(tag_levels = 'a') +plot_layout(guides='collect') &
   theme(legend.position='bottom')

app.plot <- mc.plot +
   rr.plot +
   mr.plot +
   mp.plot + plot_annotation(tag_levels = 'a') +plot_layout(guides='collect') &
   theme(legend.position='bottom')
