#influence and visibility visualization
#as-hoc report for OGAC slides 2017/07/07
library(graphics) #piechar  t()
library(ggplot2)
library(scales) #format labels in percent
library(plyr) #mutate
library(grid) #for multiplot function
library(gridExtra)
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

dev.new()
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
  geom_text(aes(x = 1,y = dfp$pos,label = percent(value/totalB)),size = 5)+
  labs(title = paste("RTK data on commodites and TA as of 20170706 (Total " , totalb, ")"))

#  facet_grid(facets =.~group,labeller = label_value)


#BUBBLE plot

#Influence levels (numeric)
#(rtk_$rtk_gf_2016+rtk_$rtk_usaid_2016)  
rtk_$ghusaid = ifelse((rtk_[,2]+rtk_[,3]) != 0, rtk_[,2]/(rtk_[,2]+rtk_[,3]),0)
influence = rtk_$rtk_usaid_2016*rtk_$ghusaid +rtk_$TA_2016  + rtk_$non_rtk_com_2016
rtk_$influence = influence/100000

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
rtk_$investment = rtk_$rtk_budget_2017+rtk_$non_rtk_com_budget_2017 + rtk_$TA_budget_2017

#TA should increase visibility 
#(TA influences the capability at site level/ TA is at central)
#TA is more about personel
#if investments are occuring in a country, should we have greater influence to what is happening
#i.e. we should see stock levels

#SIZE
size <- sqrt(rtk_$investment)/pi
N = nrow(rtk_)
#COLOR
rtk_ = rtk_[order(rtk_$visibility),]
rtk_$op = unlist(sapply(table(rtk_$visibility), heat.colors))
# leg_rtk = rtk_$country; leg_op = rtk_$op #legend colors

#ORDER infleunce and visibility by increasing; investment by decreasing
rtk_= rtk_[with(rtk_, order(desc(visibility),desc(influence), desc(investment))),]

# dev.new(width=6, height=5) #customize plot size
# plot(table(rtk_$influence, rtk_$visibility, dnn = c('Influence', 'Visibility')))

dev.off()
dev.new(width=10, height=8) #customize plot size
layout(cbind(1,2), widths = c(3,1)); layout.show(2)

#GRAPH
par(mar = c(4, 4, 2, 2), oma = c(0, 0, 0, 0))
with(rtk_, {
  palette(rtk_$op)
  symbols(rtk_$visibility, rtk_$influence, circles = size, 
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

#divide by country
#divide by Global fund VS USAID

#Investment by country
dev.new(width = 12, height = 8)

###########################################################################
########################################################################## 
# Multiple plot function from Cookbook for R, by Winston Chang 
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#########################################################################


plots2016 = list()
 
for(i in 1:nrow(rtk_)){
  rdfp = data.frame(
    Group = c("RTK provided by USAID", "RTK provided by GF", "Commodities (non-RTK)", "TA"),
    value = c(rtk_[i, "rtk_usaid_2016"], rtk_[i, "rtk_gf_2016"], 
              rtk_[i, "non_rtk_com_2016"], rtk_[i, "TA_2016"])
  )
  bp = ggplot(rdfp, aes(x = "", y = value, fill = Group)) + geom_bar(width = 1, stat = "identity")
  pie = bp + coord_polar(theta = "y", start = 0)
  plots2016[[i]] = pie + blank_theme + 
    theme(axis.text.x =element_blank(), legend.position = "none") +
    labs(title = paste('2016 Commodity & TA Expense for', rtk_[i,"country"]))
  
}

###########################################################################
###########################################################################
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  legend
}
###########################################################################
dev.new()
plots2016[[1]]

legend <- g_legend(plots2016[[1]])#saves legend

for(i in 0:((nrow(rtk_)/6)-1)){
  dev.new()
  layout(matrix(c(1:6), 2, 3, byrow = T))  #layout.show(6)
   for( j in 1:6){
     plots2016[[(j+6*i)]]
     
   }  
}


dev.new(width = 12, height = 8)
plots2016_1 = list()
plots2016_2 = list()
plots2016_3 = list()
plots2016_4 = list()
for(i in 1:6){
  plots2016_4[[i]]= plots2016[[i+18]]
}
multiplot(plotlist= plots2016_4, cols =3)
