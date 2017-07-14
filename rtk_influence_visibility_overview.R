#influence and visibility visualization
#as-hoc report for OGAC slides 2017/07/07
library(graphics) #piechart()
#setwd('USAID_Internship2017/dataset/')


rtk=read.csv(file = 'Global RTK Investment Profile_edit.csv', header = T, stringsAsFactors = F)

View(rtk)
names(rtk)

rtk_ = rtk[c(1:3,5:9,15:17)]
colnames(rtk_) = c("country", "2016_rtk_usaid", "2016_rtk_gf", "2017_rtk_budget", 
                   "2016_non-rtk_com", "2017_non-rtk_com_budget", "2016_TA", 
                   "2017_TA_budget", "G4", "G5", "G6")
rtk_ = rtk_[-1,] #exlude row 1, go back to excel formatting
#c("Angola", "Caribbean", "Central America", "Burma", "Guyana", "Ukraine", "Namibia")
rtk_ =rtk_[-c(32),] #exclude empty rows in the end
rtk_=rtk_[-c(1,3,7,8,14,20,28),] #exlude countries listed above
rtk_$country[10] = "Cote d'Ivoire" #fix country name

View(rtk_)

#Calculations and converstions
class(rtk_[1,3])
#convert money to numbers (data loaded in as string)
# rtk_[,c(2:8)] = as.numeric(gsub('\\$|,','',rtk_$total_investment))
# apply(rtk_[,c(2:8)], MARGIN = 2, function(x) as.numeric(gsub("\\$|,","", rtk_[,c(2:8)])))
rtk_[,2:8]=apply(rtk_[,2:8], MARGIN = 2, as.numeric)
totals = apply(rtk_[,2:8], MARGIN = 2, sum)
totalB=sum(totals[c(1,4,6)])
pie(totals[c(1,4,6)], labels = c("RTK", "commodities (non-RTK)", "TA"), 
    main = "Investment Distribution")

#Influence levels (numeric)

rtk_$influence = 
  
rtk_$influence  <- factor(rtk_$influence , levels=levels, 
                          ordered = is.ordered(levels_I)) 

#visiblity levels
level = c('Low','Low-medium', 'Medium','High' ) #visibility
levels_V =1:4
rtk_$visibility = level[1]
rtk_[rtk_$G4== 'yes','visibility'] = level[2]
rtk_[rtk_$G4== 'yes'& rtk_$G5 =='yes','visibility'] = level[3]
rtk_[rtk_$G4== 'yes'& rtk_$G5 =='yes'& rtk_$G6 =='yes','visibility'] = level[4]

rtk_$visibility  <- factor(rtk_$visibility  , levels=level,
                           ordered = is.ordered(levels_V)) 


#Order infleunce and visibility by increasing; investment by decreasing
rtk_= rtk_[order(rtk_$influence,rtk_$visibility, -rtk_$investment),]



# dev.new(width=6, height=5) #customize plot size
# plot(table(rtk_$influence, rtk_$visibility, dnn = c('Influence', 'Visibility')))

dev.off()
dev.new(width=6, height=5) #customize plot size
layout(cbind(1,2), widths = c(5,1)); layout.show(2)
size <- sqrt(rtk_$investment/pi)
N = nrow(rtk_)

op =c(heat.colors(N))
with(rtk_, {
  par(mai=c(1, 1, 0.3, 2.4), xpd=TRUE)
  palette(op)
  symbols(rtk_$visibility, rtk_$influence, circles = size/24, 
          bg = 1:N, #main = "Influence and Visibility of USAID Investment by Country",
          ylab = "Influence", xlab = "Visibility")
})
plot.new()
legend("center", inset=c(-0.5),legend=rtk_$country, y= NULL, ,pch = 15, 
       col = palette(op), bty="n")
#legend(bty="n") removes box around legend 

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
