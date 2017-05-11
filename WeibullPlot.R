# Linearize y-axis on Weibull paper
WeibulLinTrans <- function (x) {log(log( 1/( 1 - x) )) }

# Generate some random Weibull data
x <- rweibull(100, shape = 1.8, scale = 60)

# Convert to data frame for convenient plotting
x.df <- data.frame(Lifetime = x)
x.df$rounded <- round(x.df$Lifetime, 2)

# Histogram of data
ggplot(x.df, aes(x = Lifetime)) + 
  geom_histogram(fill = "dodgerblue3", color = "black") + 
  ggtitle("Histogram of bearing lifetimes") + 
  xlab("Time (months)") + theme_tufte() +
  theme( axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12),
         axis.title=element_text(size=14,face="plain"), plot.title = element_text(size=18))

# Add x and y plot positions for Weibull plot
x.df <- x.df %>% arrange(Lifetime) %>% mutate(Rank = 1:nrow(x.df)) %>% mutate(MedRank = (Rank-0.3)/(nrow(x.df)+0.4)) %>%
  mutate(Logx = log(Lifetime), LogFTrans = WeibulLinTrans(MedRank) )

# Run median rank regression
Regression <- lm(data = x.df, formula = LogFTrans ~ Logx )
eta <- exp( -  (Regression$coefficients[1]/Regression$coefficients[2]  ))

# Make beta and eta labels
Betalab <- paste(expression(beta) ,"==", round(Regression$coefficients[2] , 2))
Betalab <- paste(expression(beta) ,"==", round(Regression$coefficients[2] , 2))
Etalab <- paste(expression(eta) ,"==", round(eta , 2))

Parlab <- paste(Betalab, "/n", Etalab)

# Find L10 life
L10y <- WeibulLinTrans(0.1) 
L10x <- (L10y - Regression$coefficients[1]) / Regression$coefficients[2]

y5yr <- Regression$coefficients[1] + Regression$coefficients[2] * log(60)
y10yr <- Regression$coefficients[1] + Regression$coefficients[2] * log(120) 

# Define the X tick positions
XMarks <- as.vector(log( sort(  c(min(2, min(x)), min(2, min(x))+1, 10, seq(from = 20, to = 100, by = 20) , 120, 150, exp(L10x))  ) ))

# Find failure rate at 60 and 120 months
y5yrprob <-pweibull(60, shape = Regression$coefficients[2], scale = eta)
y10yrprob <-pweibull(120, shape = Regression$coefficients[2], scale = eta)

# Define the Y tick positions
Yprobs <- round( c(0.01, 0.02, 0.05, seq(from = 0.1, to = 0.6, by = 0.1), 0.8, 0.99 , y5yrprob, y10yrprob), 2)
Yprobs <- Yprobs[!duplicated(Yprobs)]
YMarks <- WeibulLinTrans(Yprobs)

# Find position of "special" ticks corresponding to L10 life, 60 months and 120 months
XMarkSpec <- which(round(exp(XMarks),2) %in% round(c(exp(L10x), 60, 120 ),2))
YMarkSpec <- which(round(Yprobs,2) %in% round(c(0.1, y5yrprob, y10yrprob ),2))

# Set font and color for tick marks
facex <- rep("plain", length(XMarks))
facex[XMarkSpec] <- "bold"

colx <- rep("black", length(XMarks))
colx[XMarkSpec] <- c("dodgerblue4", "dodgerblue3", "dodgerblue2")

facey <- rep("plain", length(YMarks))
facey[YMarkSpec] <- "bold"

coly <- rep("black", length(XMarks))
coly[YMarkSpec] <- c("dodgerblue4", "dodgerblue3", "dodgerblue2")

# Make the Weibull plot on Weibull paper
ggplot(x.df, aes(x = Logx, y = LogFTrans)) + 
  geom_point(fill = "darkorchid4", alpha = 0.75, shape = 21, col = "black", size = 2) + 
  xlab("Lifetime (months)") +  ylab("Failure Rate") +
  scale_x_continuous(breaks = XMarks, minor_breaks = NULL, labels = round(exp(XMarks)), limits = c(0, log(max(x)+ 5) )) +
  scale_y_continuous(breaks = YMarks, minor_breaks = NULL, labels = Yprobs) +
  geom_smooth(method = "lm", col = "red", size = 0.5)	+
  annotate("text", x=log(4), y=WeibulLinTrans(0.3) , parse = T,
           label= Betalab , size = 4, hjust = 0) +
  annotate("text", x=log(4), y=WeibulLinTrans(0.2) , parse = T,
           label= Etalab , size = 4, hjust = 0) +
  geom_segment(aes(x = L10x, y = -Inf, xend = L10x, yend = L10y), col = "dodgerblue4", linetype = "dashed")  +
  geom_segment(aes(x = -Inf, y = L10y, xend = L10x, yend = L10y), col = "dodgerblue4", linetype = "dashed")  +
  geom_segment(aes(x = log(60), y = -Inf, xend = log(60), yend = WeibulLinTrans(y5yrprob)), col = "dodgerblue3", linetype = "dashed")  +
  geom_segment(aes(x = -Inf, y = WeibulLinTrans(y5yrprob), xend =  log(60), yend =  WeibulLinTrans(y5yrprob)), col = "dodgerblue3", linetype = "dashed")  +
  geom_segment(aes(x = log(120), y = -Inf, xend = log(120), yend = WeibulLinTrans(y10yrprob)), col = "dodgerblue2", linetype = "dashed")  +
  geom_segment(aes(x = -Inf, y = WeibulLinTrans(y10yrprob), xend =  log(120), yend =  WeibulLinTrans(y10yrprob)), col = "dodgerblue2", linetype = "dashed")  +
  theme_bw()+
  theme( axis.text.x = element_text(face=facex, color = colx, size = 10), axis.text.y = element_text(face=facey, color = coly, size = 10) )





