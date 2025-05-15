library(ggplot2)

data = read.table("citi.txt")
View(data) # wird als Data Frame gespeichert

# Plot
ggplot(data, aes(x = time, y = trade)) +
  geom_point()

price_changes <- diff(data$trade)
#View(price_changes)

nonzero_price_changes <- price_changes[price_changes != 0]
#View(nonzero_price_changes)

acf(nonzero_price_changes)

