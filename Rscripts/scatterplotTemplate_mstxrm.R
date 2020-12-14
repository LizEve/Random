#Import ggplot
library(ggplot2)


#Trendline:

lm_eqn = function(xvals,yvals){
	m = lm(yvals ~ xvals) #y ~ x
	eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
	         list(a = format(coef(m)[1], digits = 2), 
	              b = format(coef(m)[2], digits = 2), 
	             r2 = format(summary(m)$r.squared, digits = 3)))
	as.character(as.expression(eq));
}

#Scatter Plot Template:
my.plot <- function(data, xvals, yvals, title, xaxis, yaxis, titlesize, xl, yl){
	
	#add plot and trend line
	p = ggplot(data, aes(xvals, yvals)) + geom_point(color="darkorchid4") + 	geom_smooth(method=lm, formula = y ~ x, color="cyan4", se=TRUE) 
	#add title and labels # make background white
	p = p + labs(x=xaxis,y=yaxis,title=title) + theme_bw()
	#edit titles vjust=how far away from axis
	p = p + theme(plot.title = element_text(size=titlesize, lineheight=0.8, face="bold", vjust=1, family='Courier', color="firebrick"))
	#edit labels
	p = p + theme(axis.title.x = element_text(color="firebrick", size=14, vjust=-0.35,family='Courier'), axis.title.y = element_text(color="firebrick" , size=14, vjust=1, family='Courier'))
	#add trendline to graph. Edit where the eqn goes on graph
	p=p+geom_text(aes(x = xl, y = yl, label = eqn), color="black", size=4.5, parse = TRUE)
	#set tick marks #which.min(xvals)
	#p=p+scale_y_continuous(breaks=0:1.25)+scale_x_continuous(breaks=0:20)
}

#Box Plot Template:
box.plot <- function(data, xvals, yvals, title, xaxis, yaxis, titlesize){
	p = ggplot(data, aes(xvals, yvals)) + geom_boxplot(fill = "cyan4")
	#add title and labels # make background white
	p = p + labs(x=xaxis,y=yaxis,title=title) + theme_bw()
	#edit titles vjust=how far away from axis
	p = p + theme(plot.title = element_text(size=titlesize, lineheight=0.8, face="bold", vjust=1, family='Courier', color="firebrick"))
	#edit labels
	p = p + theme(axis.title.x = element_text(color="firebrick", size=14, vjust=-0.35,family='Courier'), axis.title.y = element_text(color="firebrick" , size=14, vjust=0.35, family='Courier'))
}


#User input:
wd="/Users/ChatNoir/bin/Squam/data_files/mstxrm"
setwd(wd)
masterD=read.table("~/bin/Squam/data_files/mstxrm/masterD.05.20.15.txt", header=T)
data=data.frame(masterD)
colnames(data)

#For ppt
#TL vs wRF
xl=14
yl=0.2
xaxis="Tree Length"
yaxis="Robinson-Foulds value"
title=""
out="ppt_RFwTL_graph.png"
xvals=data$Tree.Length
yvals=data$RFw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
TL_wRF=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=TL_wRF, scale=1.2)

#TL vs AD
xl=14
yl=0.2
xaxis="Tree Length"
yaxis="Avg ambiguious data per taxa"
title=""
out="ppt_RFwavgAmbi_graph.png"
xvals=data$Tree.Length
yvals=data$avg_ambi
png(filename=out)
eqn=lm_eqn(xvals,yvals)
RFwavgAmbi=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=RFwavgAmbi, scale=1.2)

#boxplot

#clocklike
xvals=data$mono_tox
yvals=data$Δ.lnL
title="clocklike"
xaxis=""
yaxis="clocklike"
CL=box.plot(data, xvals, yvals, title, xaxis, yaxis, 20)
out="toxbox_CL.png"
png(filename=out)
ggsave(filename=out, plot=CL, scale=1.2)

#mstx
xvals=data$mono_tox
yvals=data$nummisstx
title="mstx"
xaxis=""
yaxis="mstx"
MTx=box.plot(data, xvals, yvals, title, xaxis, yaxis, 20)
out="toxbox_MTx.png"
png(filename=out)
ggsave(filename=out, plot=MTx, scale=1.2)


#treelen
xvals=data$mono_tox
yvals=data$Tree.Length
title="tree len"
xaxis=""
yaxis="tree len"
TL=box.plot(data, xvals, yvals, title, xaxis, yaxis, 20)
out="toxbox_TL.png"
png(filename=out)
ggsave(filename=out, plot=TL, scale=1.2)


#ambi data
xvals=data$mono_tox
yvals=data$avg_ambi
title="ambiguious data"
xaxis=""
yaxis="ambiguious data"
AD=box.plot(data, xvals, yvals, title, xaxis, yaxis, 20)
out="toxbox_AD.png"
png(filename=out)
ggsave(filename=out, plot=AD, scale=1.2)


#TL vs wRF
xl=14
yl=0.2
xaxis="Tree Length"
yaxis="Weighted Robinson-Foulds value"
title="Tree length vs RF distance to reference tree"
out="RFwTL_graph.png"
xvals=data$Tree.Length
yvals=data$RFw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
TL_wRF=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=TL_wRF, scale=1.2)

#TL vs MSw
xl=14
yl=0.2
xaxis="Tree Length"
yaxis="Weighted Matching Split Distance"
title="Tree length vs Matching Split \n Distance to reference tree"
out="MSwvTL_graph.png"
xvals=data$Tree.Length
yvals=data$MSw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
TL_MSw=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=TL_MSw, scale=1.2)

#TL vs missing data
xl=14
yl=0.2
xaxis="Tree Length"
yaxis="Number of missing taxa"
title="Tree length vs Number of missing taxa"
out="MTxvTL_graph.png"
xvals=data$Tree.Length
yvals=data$nummisstx
png(filename=out)
eqn=lm_eqn(xvals,yvals)
TL_MTx=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=TL_MTx, scale=1.2)

#TL vs % missing data
xl=14
yl=5
xaxis="Tree Length"
yaxis="Percent missing taxa"
title=""
out="MTxPvTL_graph.png"
xvals=data$Tree.Length
yvals=data$pmisstx
png(filename=out)
eqn=lm_eqn(xvals,yvals)
MTxPvTL=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=MTxPvTL, scale=1.2)

#wRF vs missing data
xl=0.4
yl=1
xaxis="Weighted Robinson-Foulds value"
yaxis="Number of Missing Taxa"
title="Number of Missing Taxa \n vs Weighted Robinson-Foulds value"
out="MTxvRFw_graph.png"
xvals=data$RFw
yvals=data$nummisstx
png(filename=out)
eqn=lm_eqn(xvals,yvals)
RFw_MTx=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=RFw_MTx, scale=1.2)

#wRF vs % missing data
xl=0.85
yl=0.01
xaxis="Weighted Robinson-Foulds value"
yaxis="Percent of Missing Taxa"
title="Percent of Missing Taxa \n vs Weighted Robinson-Foulds value"
out="MTxPvRFw_graph.png"
xvals=data$RFw
yvals=data$pmisstx
png(filename=out)
eqn=lm_eqn(xvals,yvals)
MTxPvRFw=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=MTxPvRFw, scale=1.2)

#wMS vs missing data
xl=3.5
yl=1
xaxis="Weighted Matching Split Distance"
yaxis="Number of Missing Taxa"
title="Weighted Matching Split Distance \n vs Number of missing taxa"
out="MTxvMSw_graph.png"
xvals=data$MSw
yvals=data$nummisstx
png(filename=out)
eqn=lm_eqn(xvals,yvals)
MTxvMSw=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=MTxvMSw, scale=1.2)

#wMS vs % missing data
xl=3.5
yl=0.01
xaxis="Weighted Matching Split Distance"
yaxis="Percent of Missing Taxa"
title="Weighted Matching Split Distance \n vs Percent of missing taxa"
out="MTxPvMSw_graph.png"
xvals=data$MSw
yvals=data$pmisstx
png(filename=out)
eqn=lm_eqn(xvals,yvals)
MTxPvMSw=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=MTxPvMSw, scale=1.2)

#wRF vs clocklike LRT
xl=0.85
yl=1
xaxis="Weighted Robinson-Foulds value"
yaxis="Number of Missing Taxa"
title="Number of Missing Taxa \n vs Weighted Robinson-Foulds value"
out="MTxvRFw_graph.png"
xvals=data$RFw
yvals=data$nummisstx
png(filename=out)
eqn=lm_eqn(xvals,yvals)
RFw_MTx=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=RFw_MTx, scale=1.2)

#wMS vs clocklike LRT
xl=3.5
yl=150
xaxis="Weighted Matching Split Distance"
yaxis="LRT Clocklike Score"
title="Violation of Clocklike model \n vs Weighted Matching Split Distance"
out="CLvMSw_graph.png"
xvals=data$MSw
yvals=data$Δ.lnL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
CLvMSw=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=CLvMSw, scale=1.2)

#wMS vs clocklike LRTogp
xl=3.5
yl=150
xaxis="Weighted Matching Split Distance"
yaxis="LRT Clocklike Score"
title="Violation of Clocklike model \n vs Weighted Matching Split Distance"
out="CLogpvMSw_graph.png"
xvals=data$MSw
yvals=data$Δ.lnL.ogp
png(filename=out)
eqn=lm_eqn(xvals,yvals)
CLogpvMSw=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=CLogpvMSw, scale=1.2)

#TL vs clocklike LRT
xl=14
yl=200
xaxis="Tree Length"
yaxis="LRT Clocklike Score"
title="Violation of Clocklike model \n vs Tree Length"
out="CLvTL_graph.png"
xvals=data$Tree.Length
yvals=data$Δ.lnL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
CLvTL=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=CLvTL, scale=1.2)

#missing taxa vs clocklike LRT
#x = 60, y = 200,
xl=60
yl=200
xaxis="Missing Taxa"
yaxis="LRT Clocklike Score"
title="Violation of Clocklike model \n vs Missing Taxa"
out="CLvMTx_graph.png"
xvals=data$nummisstx
yvals=data$Δ.lnL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
CLvMTx=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=CLvMTx, scale=1.2)

# % missing taxa vs clocklike LRT
xl=0.35
yl=200
xaxis="Percent Missing Taxa"
yaxis="LRT Clocklike Score"
title="Violation of Clocklike model \n vs Percent Missing Taxa"
out="CLvMTxP_graph.png"
xvals=data$pmisstx
yvals=data$Δ.lnL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
CLvMTxP=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=CLvMTxP, scale=1.2)

#AIC vs BIC
xl=0.35
yl=200
xaxis="AIC"
yaxis="BIC"
title="AIC vs BIC"
out="AICvBIC_graph.png"
xvals=data$ΔAICc
yvals=data$ΔBIC
png(filename=out)
eqn=lm_eqn(xvals,yvals)
AICvBIC=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=AICvBIC, scale=1.2)

#AIC vs LRT
xl=0.35
yl=200
xaxis="AIC"
yaxis="LRT"
title="AIC vs LRT"
out="AICvLRT_graph.png"
xvals=data$ΔAICc
yvals=data$Δ.lnL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
AICvLRT=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=AICvLRT, scale=1.2)

#BIC vs LRT
xl=0.35
yl=200
xaxis="BIC"
yaxis="LRT"
title="BIC vs LRT"
out="BICvLRT_graph.png"
xvals=data$ΔBIC
yvals=data$Δ.lnL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
BICvLRT=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=BICvLRT, scale=1.2)

# % missing taxa vs BIC
xl=0.35
yl=-500
xaxis="Percent Missing Taxa"
yaxis="BIC"
title="Violation of Clocklike model BIC \n vs Percent Missing Taxa"
out="CLbicvMTxP_graph.png"
xvals=data$pmisstx
yvals=data$ΔBIC
png(filename=out)
eqn=lm_eqn(xvals,yvals)
CLbicvMTxP=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=CLbicvMTxP, scale=1.2)

#Avg BL vs % missing pdata
xl=11
yl=0.05
xaxis="Average BL Length"
yaxis="Percent of missing taxa"
title="Avg BL length vs percent of missing taxa"
out="avgBLvTL.05.20.15.png"
xvals=data$Tree.Length
yvals=data$avgBL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
avgBLvTL=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=avgBLvTL, scale=1.2)


#Avg BL vs Rfw
xl=0.2
yl=0.001
xaxis="Average BL Length"
yaxis="RF dist"
title="Avg BL length vs RF"
out="avgBLvRFc.05.20.15.png"
xvals=data$RFw
yvals=data$avgBL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
avgBLvRFc=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=avgBLvRFc, scale=1.2)

#Avg BL vs CL
xl=0.2
yl=0.001
xaxis="Average BL Length"
yaxis="clocklike"
title="Avg BL length vs clocklike"
out="avgBLvCL.05.20.15.png"
xvals=data$RFw
yvals=data$Δ.lnL
png(filename=out)
eqn=lm_eqn(xvals,yvals)
avgBLvCL=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=avgBLvCL, scale=1.2)

masterD.noE=read.table("~/bin/Squam/data_files/mstxrm/masterD.05.20.15_noECEL.txt", header=T)
data.noE=data.frame(masterD.noE)
colnames(data.noE)
data.noE

#TL vs % missing data
xl=9
yl=0.01
xaxis="Tree Length"
yaxis="Percent of missing taxa"
title="Tree length vs percent of missing taxa"
out="MTxPvTL_noE.png"
xvals=data.noE$Tree.Length
yvals=data.noE$pmisstx
png(filename=out)
eqn=lm_eqn(xvals,yvals)
MTxPvTL_noE=my.plot(data.noE, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=MTxPvTL_noE, scale=1.2)

#TL vs AD
xl=8
yl=0.2
xaxis="Tree Length"
yaxis="Avg ambiguious data per taxa"
title=""
out="TLavgAmbi_noE.png"
xvals=data.noE$Tree.Length
yvals=data.noE$avg_ambi
png(filename=out)
eqn=lm_eqn(xvals,yvals)
TLavgAmbi_noE=my.plot(data.noE, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=TLavgAmbi_noE, scale=1.2)

#RFw vs AD
xl=0.2
yl=0.2
xaxis="corrected RF"
yaxis="Avg ambiguious data per taxa"
title=""
out="RFwavgAmbi_noE.png"
xvals=data.noE$RFw
yvals=data.noE$avg_ambi
png(filename=out)
eqn=lm_eqn(xvals,yvals)
RFwavgAmbi_noE=my.plot(data.noE, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=RFwavgAmbi_noE, scale=1.2)