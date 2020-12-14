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
	p = p + theme(axis.title.x = element_text(color="firebrick", size=18, vjust=-0.35,family='Courier'), axis.title.y = element_text(color="firebrick" , size=18, vjust=1, family='Courier'))
	#add trendline to graph. Edit where the eqn goes on graph
	p=p+geom_text(aes(x = xl, y = yl, label = eqn), color="black", size=5, parse = TRUE)
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
	p = p + theme(axis.title.x = element_text(color="firebrick", size=18, vjust=-0.35,family='Courier'), axis.title.y = element_text(color="firebrick" , size=18, vjust=0.35, family='Courier'), axis.text.x = element_text(color="firebrick", size=14, vjust=-0.35,family='Courier'))
}


#User input:
wd="/Users/ChatNoir/bin/Squam/data_files/mstxrm"
setwd(wd)
masterD=read.table("~/bin/Squam/data_files/mstxrm/masterD.05.22.15.txt", header=T)
data=data.frame(masterD)
colnames(data)

jmasterD=read.table("~/bin/Squam/data_files/mstxrm/jspheno_masterD.05.20.15.txt", header=T)
jdata=data.frame(jmasterD)
colnames(jdata)

#TREE LENGTH________________________________________________________________
xl=0.4
yl=1
yaxis="Total Tree Length"
xaxis="Robinson-Foulds value"
title="Molecular Evolution Rate"
out="ppt_RFwTL_graph.pdf"
yvals=data$Tree.Length
xvals=data$RFw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
TL_wRF=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=TL_wRF, width=4, height=4, dpi=200, scale=1.4)

#J data
xl=0.4
yl=1
yaxis="Total Tree Length"
xaxis="Robinson-Foulds value"
title="Molecular Evolution Rate"
out="ppt_RFwTLJ_graph.pdf"
yvals=jdata$Tree.Length
xvals=jdata$RFw.pdf
png(filename=out)
eqn=lm_eqn(xvals,yvals)
TL_wRFJ=my.plot(jdata, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=TL_wRFJ, scale=1)

#Box data
xvals=data$mono_tox
yvals=data$Tree.Length
title="Molecular Evolution Rate"
xaxis="Toxicofera"
yaxis="Total Tree Length"
TL=box.plot(data, xvals, yvals, title, xaxis, yaxis, 20)
out="ppt_toxbox_TL.pdf"
png(filename=out)
ggsave(filename=out, plot=TL, width=4, height=4, dpi=200, scale=1.4)


#CLOCKLIKE________________________________________________________________
xl=0.4
yl=200
yaxis="Violation of Clocklike Model"
xaxis="Robinson-Foulds value"
title="Clocklikeness"
out="ppt_RFwCL_graph.pdf"
yvals=data$Δ.lnL
xvals=data$RFw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
CL_wRF=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=CL_wRF, width=4, height=4, dpi=200, scale=1.4)

#J data_ USE
xl=0.4
yl=175
yaxis="Violation of Clocklike Model"
xaxis="Robinson-Foulds value"
title="Clocklikeness"
out="ppt_RFwCLJ_graph.pdf"
yvals=jdata$Δ.lnL
xvals=jdata$RFw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
CL_wRFJ=my.plot(jdata, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=CL_wRFJ, width=4, height=4, dpi=200, scale=1.4)

#Box data
xvals=data$mono_tox
yvals=data$Δ.lnL
title="Clocklikeness"
xaxis=""
yaxis="Violation of Clocklike Model"
CL=box.plot(data, xvals, yvals, title, xaxis, yaxis, 20)
out="ppt_toxbox_CL.pdf"
png(filename=out)
ggsave(filename=out, plot=CL, width=4, height=4, dpi=200, scale=1.4)

#MISSING TAXA________________________________________________________________
xl=0.45
yl=1
yaxis="Percent Missing Taxa"
xaxis="Robinson-Foulds value"
title="Missing Taxa"
out="ppt_RFwMT_graph.pdf"
yvals=data$pmisstx
xvals=data$RFw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
MT_wRF=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=MT_wRF, width=4, height=4, dpi=200, scale=1.4)

#J data_ USE
xl=0.35
yl=0.175
yaxis="Percent Missing Taxa"
xaxis="Robinson-Foulds value"
title="Percent Missing Taxa"
out="ppt_RFwMTJ_graph.pdf"
xvals=jdata$pmisstx
yvals=jdata$RFw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
MT_wRFJ=my.plot(jdata, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=MT_wRFJ, width=4, height=4, dpi=200, scale=1.4)

#Box data
xvals=data$mono_tox
yvals=data$pmisstx
title="Percent Missing Taxa"
xaxis=""
yaxis="Percent Missing Taxa"
MTx=box.plot(data, xvals, yvals, title, xaxis, yaxis, 20)
out="ppt_toxbox_MTx.pdf"
png(filename=out)
ggsave(filename=out, plot=MTx, width=4, height=4, dpi=200, scale=1.4)

#AMBI DATA________________________________________________________________
xl=0.45
yl=0
yaxis="Proportion Ambiguous Data"
xaxis="Robinson-Foulds value"
title="Proportion Ambiguous Data"
out="ppt_RFwAD_graph.pdf"
yvals=data$avg_ambi
xvals=data$RFw
png(filename=out)
eqn=lm_eqn(xvals,yvals)
AD_wRF=my.plot(data, xvals, yvals, title, xaxis, yaxis, 20, xl, yl)
ggsave(filename=out, plot=AD_wRF, scale=1.5)


xvals=data$mono_tox
yvals=data$avg_ambi
title="Ambiguous Data per Taxa"
xaxis=""
yaxis="Ambiguous Data per Taxa"
AD=box.plot(data, xvals, yvals, title, xaxis, yaxis, 20)
out="ppt_toxbox_AD.pdf"
png(filename=out)
ggsave(filename=out, plot=AD, scale=1.5)