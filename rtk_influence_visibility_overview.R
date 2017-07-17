#influence and visibility visualization
#as-hoc report for OGAC slides 2017/07/07
library(graphics) #piechar  t()
library(ggplot2)
library(scales) #format labels in percent
library(plyr) #mutate
library(dplyr) #%>%
#setwd('USAID_Internship2017/dataset/')

rtk=read.csv(file = 'Global RTK Investment Profile_edit.csv', header = T, stringsAsFactors = F)

View(rtk)
names(rtk)

rtk_ = rtk[c(1:3,5:9,15:17)]
colnames(rtk_) = c("country", "rtk_usaid_2016", "rtk_gf_2016", "rtk_budget_2017", 
                   "non_rtk_com_2016", "non_rtk_com_budget_2017", "TA_2016", 
                   "TA_budget_2017", "G4", "G5", "G6")
rtk_ = rtk_[-1,] #exlude row 1, go back to excel formatting
#c("Angola", "Caribbean", "Central America", "Burma", "Guyana", "Ukraine", "Namibia")
rtk_ =rtk_[-c(32:33),] #exclude empty rows in the end
rtk_=rtk_[-c(1,3,7,8,14,20,28),] #exlude countries listed above
rtk_$country[10] = "Cote d'Ivoire" #fix country name

View(rtk_)

#Calculations and conversions

#convert money to numbers (data loaded in as string)
# rtk_[,c(2:8)] = as.numeric(gsub('\\$|,','',rtk_$total_investment))
# apply(rtk_[,c(2:8)], MARGIN = 2, function(x) as.numeric(gsub("\\$|,","", rtk_[,c(2:8)])))
rtk_[,2:8]=apply(rtk_[,2:8], MARGIN = 2, as.numeric)
totals = apply(rtk_[,2:8], MARGIN = 2, sum)
totalB=sum(totals[c(1,4,6)])
totalb=paste('$',formatC(totalB, big.mark=',', digits = 2, format = "fg")) #format

pie(totals[c(1,4,6)], labels = c("RTK", "commodities (non-RTK)", "TA"), 
    main = paste("Investment Distribution 2016 ( ",totalb, ")"))
totals[c(1,4,6)]/totalB
#rough idea on how money is divided

#GGPLOT2 method
dfp = data.frame(
  group = c("RTK", "Commodities (non-RTK)", "TA"),
  value = c(totals[c(1,4,6)])
)
dfp =dfp[order(dfp$value),] #has to be ordered to make labels make sense

bp = ggplot(dfp, aes(x ="", y = value, fill = group))+
  geom_bar(width=1, stat = "identity")
bp #barplot

pie = bp + coord_polar(theta = "y", start = 0)
pie

#create blank theme
blank_theme = theme_minimal() + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face = "bold")
  )

#label positioning
dfp = mutate(dfp, pos = cumsum(value) - value/3)

pie + scale_fill_brewer("Expense") + blank_theme +
  theme(axis.text.x = element_blank())+
  geom_text(aes(x = 1,y = dfp$pos,label = percent(value/totalB)),size = 5)
#  facet_grid(facets =.~group,labeller = label_value)


#BUBBLE plot

#Influence levels (numeric)
#(rtk_$rtk_gf_2016+rtk_$rtk_usaid_2016)  
rtk_$ghusaid = ifelse((rtk_[,2]+rtk_[,3]) != 0, rtk_[,2]/(rtk_[,2]+rtk_[,3]),0)
influence = rtk_$rtk_usaid_2016*rtk_$ghusaid +rtk_$TA_2016  + rtk_$non_rtk_com_2016
rtk_$influence = log(influence +1)
plot(influence)
plot(rtk_$)

#visiblity levels
level = c('Low','Low-medium', 'Medium','High' ) #visibility
levels_V =1:4
rtk_$visibility = level[1]
rtk_[rtk_$G4== 'yes','visibility'] = level[2]
rtk_[rtk_$G4== 'yes'& rtk_$G5 =='yes','visibility'] = level[3]
rtk_[rtk_$G4== 'yes'& rtk_$G5 =='yes'& rtk_$G6 =='yes','visibility'] = level[4]

rtk_$visibility  <- factor(rtk_$visibility  , levels=level,
                           ordered = is.ordered(levels_V)) 

#investment
rtk_$investment = rtk_$`2016_rtk_usaid`+rtk_$`2016_non-rtk_com` + rtk_$`2016_TA`


#SIZE
size <- sqrt(rtk_$investment/pi)
N = nrow(rtk_)
#COLOR
rtk_ = rtk_[order(rtk_$visibility),]
rtk_$op = unlist(sapply(table(rtk_$visibility), heat.colors))
# leg_rtk = rtk_$country; leg_op = rtk_$op #legend colors

#ORDER infleunce and visibility by increasing; investment by decreasing
rtk_= rtk_[order(-rtk_$influence,rtk_$visibility, rtk_$investment),]

# dev.new(width=6, height=5) #customize plot size
# plot(table(rtk_$influence, rtk_$visibility, dnn = c('Influence', 'Visibility')))

dev.off()
dev.new(width=10, height=5) #customize plot size
layout(cbind(1,2), widths = c(3,1)); layout.show(2)

#GRAPH
par(mar = c(4, 4, 2, 2), oma = c(0, 0, 0, 0))
with(rtk_, {
  palette(rtk_$op)
  symbols(rtk_$visibility, rtk_$influence, circles = size/16, 
          bg = 1:N, #main = "Influence and Visibility of USAID Investment by Country",
          ylab = "Influence", xlab = "Visibility")
})
plot.new()
par(mar = c(0,0, 0, 0))
legend("center",legend=rtk_$country, y= NULL, ,pch = 15, 
       col = palette(rtk_$op), bty="n")
#legend(bty="n") removes box around legend 
#why legend "right" doesn't give right alignment

#how to overlay country names
#how to show legends with break down of type

#op =heat.colors(n = N);palette(op)
# legend("bottomright",rtk_$country, y= NULL, ,pch = 15, col = palette(op))

#test to see how graph is ordered
table(rtk_$influence, rtk_$visibility)

#Investment by country, colored by influence
rtk_bar = rtk_[order(rtk_$investment),]
View(rtk_bar)
INF_N = length(levels(rtk_bar$influence))

  op =heat.colors(n = INF_N)
  palette(op)
  barplot(rtk_bar$investment, rtk_bar$influence,bg = 1:INF_N, xlab = "Countries")

~rtk_bar$influence
