peer.exp$group <- 1:nrow(peer.exp)

#e41a1c "Numviyimana et al. (2020)"  
#377eb8 "Lavanya and Thanga (2021)"
#4daf4a "Numviyimana et al. (2022)"
#984ea3 "Numviyimana et al. (2021)"
#ff7f00 "Khalaf et al. (2022)"    

unique(exp$paper)
apatheme=theme_bw()+
   theme(panel.grid.major=element_blank(),
         panel.grid.minor=element_blank(),
         panel.border=element_blank(),
         axis.line=element_line(),
         text=element_text(family="serif"),
         legend.title=element_blank(),
         axis.text.y=element_text(size = 10),
         axis.text.x=element_text(size = 10),
         legend.text=element_text(size=9))

create_plot <- function(data, x_var, y_var, title, subtitle = "", y_label = NULL, y_scale_func = NULL) {
   plot <- data %>%
      ggplot(aes_string(x = x_var, y = y_var, fill = "paper")) +
      geom_bar(stat = "identity", width = 0.4) +
      labs(x = "Process Specifications", y = y_label %||% y_var,
           title = title, subtitle = subtitle, caption = "") +
       apatheme +   scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a",
                                                 "#984ea3", "#ff7f00"),
                                      limits = c("Numviyimana et al. (2020)",
                                                 "Lavanya and Thanga (2021)",
                                                 "Numviyimana et al. (2022)",
                                                 "Numviyimana et al. (2021)",
                                                 "Khalaf et al. (2022)"))
   
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
p_npv <-  0.46 * (P_prock / 100)
lcop.plot <- create_plot(exp, "index", "LCOP", "", y_label = "price in EUR") + 
   geom_hline(yintercept = p_npv, linetype = "dashed", color = "red") +
   annotate("text", x = Inf, y = 23, vjust = 1, hjust = 1, size=4, label = "Price used for NPV estimation", color = "red")  + theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())

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
   scale_y_continuous(labels = scales::label_number_si()) +
  scale_fill_manual(values = c("#377eb8", "#984ea3", "#ff7f00"),
  limits = c("Lavanya and Thanga (2021)",
             "Numviyimana et al. (2021)",
             "Khalaf et al. (2022)"))

peer.exp$group <- 1:nrow(peer.exp)

# cost shares https://r-graph-gallery.com/48-grouped-barplot-with-ggplot2
input.exp$Input <- 0
input.exp <- input.exp %>%
   mutate(Input = recode(input,
                         "Yenergy"   = "Energy",
                         "Yexp_chem" = "Chemicals",
                         "Yinv"      = "Investment",

   )) 
input.plot <- ggplot(input.exp, aes(color = Input, stroke =1 ,fill= paper, y=value, x=index)) + 
   geom_bar(position="fill", stat="identity") +
   labs(x = "Process Specifications", y = "Input share",
        title = "",
        subtitle = "", 
        caption = "") + apatheme +
   scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a",
                                  "#984ea3", "#ff7f00"),
                       limits = c("Numviyimana et al. (2020)",
                                  "Lavanya and Thanga (2021)",
                                  "Numviyimana et al. (2022)",
                                  "Numviyimana et al. (2021)",
                                  "Khalaf et al. (2022)")) +
   scale_color_manual(values = c("black", "black", "black"))

                   
sharesE <- input.exp %>%
   group_by(index) %>%
   mutate(total_value = sum(value),
          share = value / total_value) %>%
   ungroup() %>% filter(Input == "Energy")

sharesC <- input.exp %>%
   group_by(index) %>%
   mutate(total_value = sum(value),
          share = value / total_value) %>%
   ungroup() %>% filter(Input == "Chemicals")

sharesI <- input.exp %>%
   group_by(index) %>%
   mutate(total_value = sum(value),
          share = value / total_value) %>%
   ungroup() %>% filter(Input == "Investment")

input.facet <- rbind(sharesE, sharesC, sharesI)

input.plot2 <- ggplot(input.facet, aes(x = index, y = share, fill = paper)) +
   geom_bar(stat = "identity", position = "dodge") +
   facet_wrap(~Input, ncol = 1) +
   labs(x = "Process Specifications", y = "Input shares",
        title = "",
        subtitle = "", 
        caption = "") + apatheme + theme(legend.position = "bottom") +
   scale_y_continuous(labels = scales::label_number_si()) +
   scale_fill_manual(values = c("#e41a1c", "#377eb8", "#4daf4a",
                                "#984ea3", "#ff7f00"),
                     limits = c("Numviyimana et al. (2020)",
                                "Lavanya and Thanga (2021)",
                                "Numviyimana et al. (2022)",
                                "Numviyimana et al. (2021)",
                                "Khalaf et al. (2022)"))

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

