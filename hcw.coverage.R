## COVERAGE SIMULATION 

set.seed(2018)

HCW_values <- hcw.data %>%
  pull(duration_hcw) %>%
  na.omit %>%
  MASS::fitdistr(.,
                 densfun = "gamma") 

HCW_samples <- mvtnorm::rmvnorm(n = 1000,
                                mean = HCW_values$estimate,
                                sigma = HCW_values$vcov) %>%
  as.data.frame

parms_mat <- expand.grid(acceptance = c(0.763, 0.964),
                         waning = c(0, 0.1),
                         reach = 1,
                         efficacy = c(1, 0.75),
                         time = seq(0,20, by = 0.25))

coverage <- function(samples, parms){
  
  bind_cols(samples, parms[rep(1, nrow(samples)),]) %>%
    dplyr::mutate(density_hcw = pgamma(q = time,
                                       shape = .$shape,
                                       rate = .$rate)) %>%
    dplyr::mutate(acceptance_sampled = rbeta(
      n = nrow(.),
      shape1 = .$acceptance*304,
      shape2 = (1 - .$acceptance)*304)) %>%
    dplyr::mutate(
      coverage = 
        acceptance_sampled*reach*efficacy*((1-waning)^time)*(1-density_hcw)) %>%
    return
}

HCW_simulation <- parms_mat %>%
  dplyr::mutate(row = 1:n()) %>%
  dplyr::mutate(label = 
                  case_when(
                    acceptance == 0.964 & efficacy == 1 ~ "A",
                    acceptance == 0.964 & efficacy == 0.75 ~ "B",
                    acceptance == 0.763 & efficacy == 1 ~ "C",
                    acceptance == 0.763 & efficacy == 0.75 ~ "D")) %>%
  split(.$row) %>%
  purrr::map_df(~coverage(HCW_samples, .x), id="row") %>%
  dplyr::mutate(efficacy = case_when(
    efficacy == 1 ~ "High efficacy (100%)",
    TRUE ~ "Low efficacy (75%)"),
    acceptance = case_when(
      acceptance == 0.964 ~ "High acceptance (96.4%)",
      TRUE ~ "Low acceptance (76.3%)"),
    Waning = case_when(
      waning == 0 ~ "Low (0%)",
      waning == 0.1 ~ "Medium (10%)",
      TRUE ~ "High (50%)"),
    Waning = fct_inorder(Waning))

p_simulation <- HCW_simulation %>% 
  dplyr::filter(time <= 10) %>%
  group_by(acceptance, Waning, reach, efficacy, time, label) %>%
  dplyr::summarise(lo = quantile(coverage, 0.025),
                   med = median(coverage),
                   hi = quantile(coverage, 0.975)) %>%
  ungroup %>%
  dplyr::mutate(Waning = fct_rev(Waning)) %>%
  ggplot(data=., aes(x=time)) +
  geom_ribbon(aes(ymin = lo, ymax = hi,
                  fill = Waning),
              alpha = 0.25) + 
  geom_line(aes(y = med,
                color = Waning)) +
  facet_wrap( ~ label) +
  theme_bw() +
  theme(legend.position="bottom", 
        strip.text = element_blank()) +
  ylab("Immunisation coverage") +
  scale_y_continuous(labels = scales::percent, 
                     limits = c(0, 1)) +
  guides(colour = guide_legend(override.aes = list(size=2))) +
  xlab("Time since vaccination campaign (years)") +
  geom_text(data = data.frame( x = 10, y=1, label=LETTERS[1:4]),
            aes(label = label,
                x = x,
                y = y))

list(`pdf` = "pdf",
     `png` = "png") %>%
  map(~ggsave(filename = paste("Figures\\Figure_4_Simulated_Coverage",.x, sep="."),
              width = 15, height = 15, units = "cm",
              dpi = 600,
              device = .x,
              plot = p_simulation))
