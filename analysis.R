library(tidyverse)
combined_results <- read_csv("final_sim_data_agg.csv") %>%
  select(-"...1")


round((sum(combined_results$true_var < combined_results$UB &
             combined_results$true_var > combined_results$LB) / nrow(combined_results))*100,2)

# Combined Plot 
reshape2::melt(combined_results[c("point_estimate_rub", "point_estimate", "point_estimate_boot")]) %>%
  ggplot(data = .,
         aes(x = value - 2, fill = variable, color = variable)) + 
  geom_density(aes(y = ..density..), alpha = 0.25) + 
  theme(axis.title.y = element_blank(), 
        panel.spacing=unit(1.5,"lines")) + 
  theme_bw() + 
  theme(axis.title.y = element_blank(), 
        panel.spacing=unit(1.5,"lines"), 
        strip.text = element_text(
          size = 9)) + 
  labs(
    x = latex2exp::TeX("$\\widehat{\\beta_1} - \\beta_1")
  ) + 
  scale_fill_discrete(name = "Method", labels = c("Rubin's Rules", "Jackknife", "Bootstrap")) + 
  scale_color_discrete(name = "Method", labels = c("Rubin's Rules", "Jackknife", "Bootstrap"))

# Faceted Plot 
reshape2::melt(combined_results[c("point_estimate_rub", "point_estimate", "point_estimate_boot")]) %>%
  ggplot(data = .,
         aes(x = value - 2, fill = variable, color = variable)) + 
  geom_density(aes(y = ..density..), alpha = 0.25) + 
  theme(axis.title.y = element_blank(), 
        panel.spacing=unit(1.5,"lines")) + 
  theme_bw() + 
  facet_wrap(~variable) + 
  theme(axis.title.y = element_blank(), 
        panel.spacing=unit(1.5,"lines"), 
        strip.text = element_text(
          size = 9)) + 
  labs(
    x = latex2exp::TeX("$\\widehat{\\beta_1} - \\beta_1")
  ) + 
  scale_fill_discrete(name = "Method", labels = c("Rubin's Rules", "Jackknife", "Bootstrap")) + 
  scale_color_discrete(name = "Method", labels = c("Rubin's Rules", "Jackknife", "Bootstrap")) + 
  theme(strip.background = element_blank(), strip.text = element_blank())


# Zipper plot for srs of n = 100, too dense otherwise
set.seed(234)
jackk <- combined_results %>%
  sample_n(1e2, replace = FALSE) %>%
  mutate(covers = ifelse(UB > true_var & true_var > LB, "Covers", "Does Not Cover")) %>%
  ggplot(., aes(x = 1:100)) + 
  geom_errorbar(aes(ymin = LB, ymax = UB, color = covers)) + 
  theme_bw() + 
  coord_flip() + 
  labs(
    x = latex2exp::TeX("$i^{th} iteration"), 
    y = "Confidence Intervals"
  ) + 
  geom_hline(yintercept = combined_results$true_var) + 
  scale_color_manual(name = NULL, values=c("#999999", "#FF0000")) + 
  ggtitle("Jackknife Estimator")

set.seed(234)
boot <- combined_results %>%
  sample_n(1e2, replace = FALSE) %>%
  mutate(covers = ifelse(UB_boot > true_var & true_var > LB_boot, "Covers", "Does Not Cover")) %>%
  ggplot(., aes(x = 1:100)) + 
  geom_errorbar(aes(ymin = LB_boot, ymax = UB_boot, color = covers)) + 
  theme_bw() + 
  coord_flip() + 
  labs(
    x = latex2exp::TeX("$i^{th} iteration"), 
    y = "Confidence Intervals"
  ) + 
  geom_hline(yintercept = combined_results$true_var) + 
  scale_color_manual(name = NULL, values=c("#999999", "#FF0000")) + 
  ggtitle("Bootstrap Estimator")

set.seed(234)
rubin <- combined_results %>%
  sample_n(1e2, replace = FALSE) %>%
  mutate(covers = ifelse(UB_rub > true_var & true_var > LB_rub, "Covers", "Does Not Cover")) %>%
  ggplot(., aes(x = 1:100)) + 
  geom_errorbar(aes(ymin = LB_rub, ymax = UB_rub, color = covers)) + 
  theme_bw() + 
  coord_flip() + 
  labs(
    x = latex2exp::TeX("$i^{th} iteration"), 
    y = "Confidence Intervals"
  ) + 
  geom_hline(yintercept = combined_results$true_var) + 
  scale_color_manual(name = NULL, values=c("#999999", "#FF0000")) + 
  ggtitle("Rubin's Rules")

benchmark_res <- read.csv("benchmark_detailed_res.csv") %>%
  select(-X)

benchmark_res$expr <- as.factor(benchmark_res$expr)

benchmark_res %>%
  ggplot(., aes(x = expr, y = time)) + 
  geom_violin() + 
  scale_y_log10() + 
  theme_bw() + 
  ggtitle("Benchmark for Methods Examined") + 
  coord_flip() + 
  labs(
    y = "Time (microseconds)"
  ) + 
  theme(axis.title.y = element_blank())


