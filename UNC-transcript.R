# Graph analysis with Claude

library(ggplot2)

UNC <- read.csv(text =
                  "year,rate,applicants
2019,22.63,42466
2020,23.54,44382
2021,19.24,53776
2022,16.85,57221
2023,18.74,57902
2024,15.56,66535")

ggplot(UNC, aes(x = year)) +
  geom_line(aes(y = applicants), color = "steelblue", size = 1.2) +
  geom_line(aes(y = rate), color = "darkgreen", size = 1.2) +
  scale_y_continuous(
    name = "Total Applicants",
    sec.axis = sec_axis(~ ., name = "Acceptance Rate (Percent)")
  ) +
  labs(
    title = "UNC sees drop in acceptance rate as the number of first-year applicants increase",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.y.right = element_text(color = "darkgreen"),
    axis.title.y.left = element_text(color = "steelblue")
  )


# MF: There is a problem here. You've used the same scale for the number of applicants as the rate of admission. And, rate is a percent of applicants. How would you correct this problem?

# Got it, let me rework the graph to properly display the applicant numbers and acceptance rate:

ggplot(UNC, aes(x = year)) +
  geom_line(aes(y = applicants), color = "steelblue", size = 1.2) +
  geom_line(aes(y = rate * 100), color = "darkgreen", size = 1.2) +
  scale_y_continuous(
    name = "Total Applicants",
    sec.axis = sec_axis(~ . / 100, name = "Acceptance Rate (Percent)")
  ) +
  labs(
    title = "UNC sees drop in acceptance rate as the number of first-year applicants increase",
    x = NULL, y = NULL
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.y.right = element_text(color = "darkgreen"),
    axis.title.y.left = element_text(color = "steelblue")
  )


# Can you think of a different kind of graph to display this information in an understandable way?
#  An alternative graph type that could effectively display this data is a combination chart with a bar chart for the total applicants and a line chart for the acceptance rate.

ggplot(UNC, aes(x = year)) +
  geom_bar(aes(y = applicants), stat = "identity", fill = "steelblue") +
  geom_line(aes(y = rate * 100), color = "darkgreen", size = 1.2) +
  scale_y_continuous(
    name = "Total Applicants",
    sec.axis = sec_axis(~ . / 100, name = "Acceptance Rate (Percent)")
  ) +
  labs(title = "UNC Applicants and Acceptance Rate", x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

# MF: Can you redo that graph putting text labels showing the numerical values of applicants and acceptance rate on the bars and lines?

ggplot(UNC, aes(x = year)) +
  geom_bar(aes(y = applicants), stat = "identity", fill = "steelblue") +
  geom_text(aes(y = applicants, label = format(applicants, big.mark = ",")), vjust = -0.5, size = 4) +
  geom_line(aes(y = rate * 100), color = "darkgreen", size = 1.2) +
  geom_text(aes(y = rate * 100, label = paste0(round(rate * 100, 2), "%")), vjust = 1.5, size = 4, color = "darkgreen") +
  scale_y_continuous(
    name = "Total Applicants",
    sec.axis = sec_axis(~ . / 100, name = "Acceptance Rate (Percent)")
  ) +
  labs(title = "UNC Applicants and Acceptance Rate", x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))


ggplot(UNC, aes(x = year)) +
  geom_bar(aes(y = applicants), stat = "identity", fill = "steelblue") +
  geom_text(aes(y = applicants, label = format(applicants, big.mark = ",")), vjust = -0.5, size = 4) +
  geom_line(aes(y = rate * 100), color = "darkgreen", size = 1.2) +
  geom_text(aes(y = rate * 100, label = paste0(round(rate, 2) * 100, "%")), vjust = 1.5, size = 4, color = "darkgreen") +
  scale_y_continuous(
    name = "Total Applicants",
    sec.axis = sec_axis(~ ., name = "Acceptance Rate (Percent)", breaks = seq(0, 25, 5))
  ) +
  labs(title = "UNC Applicants and Acceptance Rate", x = NULL, y = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

ggplot(UNC, aes(x = applicants, y = applicants * rate)) +
  geom_point(color = "steelblue", size = 3) +
  scale_x_continuous(name = "Total Applicants") +
  scale_y_continuous(name = "Students Accepted") +
  labs(title = "UNC Applicants vs Students Accepted") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, face = "bold"))

