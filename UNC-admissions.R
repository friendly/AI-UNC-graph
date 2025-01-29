library(dplyr)
library(ggplot2)
library(scales)
library(ggrepel)

UNC <- read.csv(text =
"year,rate,applicants
2019,22.63,42466
2020,23.54,44382
2021,19.24,53776
2022,16.85,57221
2023,18.74,57902
2024,15.56,66535")

UNC <- UNC |>
  mutate(accepted = applicants * rate/100)

lm(rate ~ applicants, data=UNC)
lm(accepted ~ applicants, data=UNC)

ggplot(data=UNC,
       aes(x = applicants, y = rate)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1.7) +
  geom_smooth(method = "lm", fill="blue", alpha = 0.1) +
  labs(x="Number of applicants",
       y="Acceptance rate (%)",
       title = "UNC acceptance rates decline as applicants increase") +
  geom_label_repel(aes(label = year)) +
  theme_minimal(base_size = 14)

ggplot(data=UNC,
       aes(x = applicants, y = accepted)) +
  geom_point(size = 4) +
  geom_line(linewidth = 1.7) +
  geom_smooth(method = "lm", fill="blue", alpha = 0.1) +
  geom_hline(aes(yintercept = mean(accepted))) +
  labs(x="Number of applicants",
       y="Number accepted",
       title = "UNC admissions increase slowly but not steadily!") +
  geom_label_repel(aes(label = year)) +
  scale_x_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = "")) +
  scale_y_continuous(label = unit_format(unit = "K", scale = 1e-3, sep = "")) +
  theme_minimal(base_size = 14)


#John Fox graph
(acc <- UNC$accepted <- with(UNC, round(rate*applicants/100)))

plot(rate ~ I(applicants/1000), data=UNC, type="b", pch=21, bg="gray",
     cex=4*acc/max(acc),
     xlab="Number of Applicants (1000s)",
     ylab="Acceptance Rate (percent)")
with(UNC, text(applicants/1000, rate, labels=year, cex=0.9,
               pos=ifelse(applicants < 65000, 4, 2))) 
          text(58, 22, cex=0.9,
               labels="size of dots proportional to\nnumber of students admitted")
