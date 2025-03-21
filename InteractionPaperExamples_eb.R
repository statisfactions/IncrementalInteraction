# Supplemental Code to
# Tutorial: Power analyses for interaction effects in cross-sectional regressions
# David A. A. Baranger1, Megan C. Finsaas2, Brandon L. Goldstein3, Colin E. Vize4, Donald R. Lynam5, and Thomas M. Olino6.
#
# 1 Department of Psychiatry, Washington University in St. Louis
# 2 Department of Epidemiology, Columbia University
# 3 Department of Psychiatry, University of Connecticut
# 4 Department of Psychiatry, University of Pittsburgh
# 5 Department of Psychological Sciences, Purdue University
# 6 Department of Psychology and Neuroscience, Temple University

#
# Please address correspondence to: Dr. David Baranger, Dept. of Psychological & Brain Sciences,
# CB 1125 Washington University in St. Louis, One Brookings Drive, Saint Louis, MO 63110; Ph: 617-407-5645;
# Email: dbaranger@wustl.edu

####
# NOTE that this code uses {webshot} to create screenshots of code. These need to be manually saved and named correctly in
#  to fully reproduce all figures
#####


# install.packages("devtools")
# devtools::install_github("dbaranger/InteractionPoweR")

library(InteractionPoweR)
library(reprex)
library(webshot)
library(magick)
library(gridExtra)
library(ggplot2)
library(cowplot)
library(grid)
library(ggpubr)
library(beepr)

##############


###########


set.seed(6453936)
D1 = InteractionPoweR::generate_interaction(#n.iter = 1000,
                                               N = 1000000,
                                               r.x1.y = 0.2,
                                               r.x2.y = 0.15,
                                               r.x1.x2 = 0.1,
                                               r.x1x2.y = 0.1
)


P1 = InteractionPoweR::plot_interaction(data = D1,q = 3)
P1$layers[[3]]=NULL
P1
set.seed(74659407)
D2 = InteractionPoweR::generate_interaction(#n.iter = 1000,
  N = 1000,
  r.x1.y = 0.2,
  r.x2.y = 0.15,
  r.x1.x2 = 0.1,
  r.x1x2.y = 0.1
)


P2 = InteractionPoweR::plot_interaction(data = D2,q = 3)
P2

Example1 = InteractionPoweR::power_interaction_r2(N = 1000,
                                                  r.x1.y = 0.2,
                                                  r.x2.y = 0.15,
                                                  r.x1.x2 = 0.1,
                                                  r.x1x2.y = 0.1,
                                                  alpha = 0.05)
round(Example1,2)
reprex(advertise = F) # manually save html file
webshot::webshot("Example1.html", "Example1.png",zoom = 4,vwidth = 600)
Ex1 = magick::image_read("Example1.png")
Ex1 = magick::image_trim(Ex1,25)
Ex1 = magick::image_fill(Ex1,color = "white",point = "+10+10",fuzz = 25)
Ex1 = magick::image_crop(image = Ex1,geometry = "2040x900+0+0")


Ex1 = magick::image_fill(Ex1,color = "white",point = "+1+1",fuzz = 5)
Ex1 = magick::image_trim(Ex1,30)


blank <- grid.rect(gp=gpar(col="white"))

A1<-grid.arrange(
  ggdraw() + draw_label(label = "A",fontfamily = "sans",x=0.025,size = 15),
  P1,
  ncol=1,
  heights = c( .5, 5))
A2<-grid.arrange(
  ggdraw() + draw_label(label = "B",fontfamily = "sans",x=0.025,size = 15),
  P2,
  ncol=1,
  heights = c( .5, 5))
A3 = grid.arrange( A1,A2,ncol=2,widths = c(5,5))
A4<-grid.arrange(
  ggdraw() + draw_label(label = "C",fontfamily = "sans",x=0.025,size = 15),
  ggdraw() +  draw_image(Ex1),
  ncol=1,
  heights = c( .5, 5))

A5 = grid.arrange( A3,A4,ncol=1,heights = c(1,1))

ggsave(plot = A5,filename = "Fig1.pdf",dpi = 500,width = 8,height = 6)

ggsave(plot = A5,filename = "Fig1.png",dpi = 500,width = 8,height = 6)


######################################################################



D2 = InteractionPoweR::generate_interaction(
  N = 1000000,
  r.x1.y = 0.2,
  r.x2.y = 0.15,
  r.x1.x2 = 0.1,
  r.x1x2.y = 0.05
)


P2 = InteractionPoweR::plot_interaction(data = D2,q = 3)
P2$layers[[3]]=NULL
P2


Example2 = InteractionPoweR::power_interaction_r2(N = seq(500,6000,500),
                                                  r.x1.y = 0.2,
                                                  r.x2.y = 0.15,
                                                  r.x1.x2 = 0.1,
                                                  r.x1x2.y = 0.05,
                                                  alpha = 0.05)

reprex(advertise = F) # manually save html file
webshot::webshot("Example2.html", "Example2.png",zoom = 4,vwidth = 700)
Ex2 = magick::image_read("Example2.png")
Ex2 = magick::image_trim(Ex2,25)
Ex2 = magick::image_fill(Ex2,color = "white",point = "+10+10",fuzz = 25)
Ex2 = magick::image_crop(image = Ex2,geometry = "2200x674+0+0")


P3 = InteractionPoweR::plot_power_curve(Example2,power_target = .9)

P3


A1<-grid.arrange(
  ggdraw() + draw_label(label = "A",fontfamily = "sans",x=0.025,size = 15),
  P2,
  ncol=1,
  heights = c( .5, 5))

A2<-grid.arrange(
  ggdraw() + draw_label(label = "B",fontfamily = "sans",x=0.025,size = 15),
  P3,
  ncol=1,
  heights = c( .5, 5))

A4<-grid.arrange(
  ggdraw() + draw_label(label = "C",fontfamily = "sans",x=0.025,size = 15),
  ggdraw() +  draw_image(Ex2),
  ncol=1,
  heights = c( .5, 5))

A3 = grid.arrange( A1,A2,ncol=2,widths = c(5,5))

A5 = grid.arrange( A3,A4,ncol=1,heights = c(1,.9))


ggsave(plot = A5,filename = "Fig2.pdf",dpi = 500,width = 8,height = 6)

ggsave(plot = A5,filename = "Fig2.png",dpi = 500,width = 8,height = 6)


########################################################




P3 =InteractionPoweR::plot_power_curve(Example3,power_target = .8)

Example3 = InteractionPoweR::power_interaction_r2(N = 1000,
                                                  r.x1.y = .2,
                                                  r.x2.y = 0.15,
                                                  r.x1.x2 = 0.1,
                                                  r.x1x2.y = seq(0.05,0.15,0.005),
                                                  alpha = 0.05)
InteractionPoweR::power_estimate(power_data = Example3,x = "r.x1x2.y",power_target = .8)

reprex(advertise = F) # manually save html file
webshot::webshot("Example3.html", "Example3.png",zoom = 4,vwidth = 800)
Ex3 = magick::image_read("Example3.png")
Ex3 = magick::image_trim(Ex3,25)
Ex3 = magick::image_fill(Ex3,color = "white",point = "+10+10",fuzz = 25)


A1<-grid.arrange(
  ggdraw() + draw_label(label = "A",fontfamily = "sans",x=0.025,size = 15),
  ggdraw() +  draw_image(Ex3),
  ncol=1,
  heights = c( .5, 5))

A2<-grid.arrange(
  ggdraw() + draw_label(label = "B",fontfamily = "sans",x=0.025,size = 15),
  P3,
  ncol=1,
  heights = c( .5, 5))

blank <- grid.rect(gp=gpar(col="white"))

A3<-grid.arrange(
  blank,A2,blank,
  ncol=3,
  widths = c( .15,1,.15))

A5 = grid.arrange( A1,A3,ncol=1,heights = c(1,1.3))

ggsave(plot = A5,filename = "Fig3.pdf",dpi = 500,width = 8,height = 6)

ggsave(plot = A5,filename = "Fig3.png",dpi = 500,width = 8,height = 6)


########################################################

Example4a = InteractionPoweR::power_interaction_r2(N = 500,
                                                  r.x1.y = seq(-.5,.5,.05),
                                                  r.x2.y = seq(0,.5,.1),
                                                  r.x1.x2 = 0,
                                                  r.x1x2.y = .1,
                                                  alpha = 0.05)

P3 = InteractionPoweR::plot_power_curve(Example4a,power_target = .8)
P3 = P3 + ggtitle(label = "" ,subtitle = "r x1 - x2 = 0" )  + theme(plot.subtitle = element_text(hjust=0.5), plot.title = element_blank())
P3

Example4b = InteractionPoweR::power_interaction_r2(N = 500,
                                                   r.x1.y = seq(-.5,.5,.1),
                                                   r.x2.y = seq(-.5,.5,.1),
                                                   r.x1.x2= seq(-.5,.5,.2),
                                                   r.x1x2.y = .1,
                                                   alpha = 0.05)

P4 = InteractionPoweR::plot_power_curve(Example4b,power_target = .8,x = "r.x1.y",group = "r.x2.y",facets = "r.x1.x2")
P4


A1<-grid.arrange(
  ggdraw() + draw_label(label = "A",fontfamily = "sans",x=0.025,size = 15),
  P3,
  ncol=1,
  heights = c( .5, 5))

A2<-grid.arrange(
  ggdraw() + draw_label(label = "B",fontfamily = "sans",x=0.025,size = 15),
  P4,
  ncol=1,
  heights = c( .5, 5))

A5<-grid.arrange(
  A1,A2,
  ncol=2,
  widths = c(1,2))



ggsave(plot = A5,filename = "Fig4.pdf",dpi = 500,width = 10,height = 4)

ggsave(plot = A5,filename = "Fig4.png",dpi = 500,width = 10,height = 4)

##################################################################

Example5 = InteractionPoweR::power_interaction_r2(N = 1000,
                                                  r.x1.y = 0.2,
                                                  r.x2.y = 0.15,
                                                  r.x1.x2 = 0.1,
                                                  r.x1x2.y = 0.1,
                                                  alpha = 0.05,
                                                  rel.x1 = seq(.4,1,.1),
                                                  rel.x2 = seq(.4,1,.1),
                                                  rel.y = seq(.4,1,.2))
P1 = InteractionPoweR::plot_power_curve(Example5,power_target = .9,x = "rel.x1",group = "rel.x2",facets = "rel.y")
P1

ggsave(plot = P1,filename = "Fig5.pdf",dpi = 500,width = 6,height = 4)

ggsave(plot = P1,filename = "Fig5.png",dpi = 500,width = 6,height = 4)


Example5b = InteractionPoweR::power_interaction_r2(N = seq(1000,6000,100),
                                                  r.x1.y = 0.2,
                                                  r.x2.y = 0.15,
                                                  r.x1.x2 = 0.1,
                                                  r.x1x2.y = 0.1,
                                                  alpha = 0.05,
                                                  rel.x1 = 0.8,
                                                  rel.x2 = 0.8,
                                                  rel.y = 0.8)


########################################################################################

Example6 = InteractionPoweR::power_interaction(n.iter=10000,cl=6,
                                               N = 1000,
                                               r.x1.y = 0.2,
                                               r.x2.y = 0.15,
                                               r.x1.x2 = 0.1,
                                               r.x1x2.y = 0.1,
                                               alpha = 0.05,
                                               skew.x1 = seq(0,2,.25),
                                               skew.x2 = seq(0,2,.5),seed = 788480)

P2 = InteractionPoweR::plot_power_curve(Example6,power_target = .9,x = "skew.x1",group ="skew.x2")
P2
ggsave(plot = P2,filename = "Fig6.pdf",dpi = 500,width = 6,height = 4)
ggsave(plot = P2,filename = "Fig6.png",dpi = 500,width = 6,height = 4)



##########################################################################################

set.seed(6453936)
D6 = InteractionPoweR::generate_interaction(#n.iter = 1000,
  N = 1000000,
  r.x1.y = 0.2,
  r.x2.y = 0.15,
  r.x1.x2 = 0.1,
  r.x1x2.y = 0.1,k.y = 2
)

#D6$y = as.factor(D6$y)
P1 = InteractionPoweR::plot_interaction(data = D6,q = 3)
P1$layers[[2]]=NULL
P1




Example7 = InteractionPoweR::power_interaction(n.iter=10000,cl=6,
                                               N = 1000,
                                               r.x1.y = 0.2,
                                               r.x2.y = 0.15,
                                               r.x1.x2 = 0.1,
                                               r.x1x2.y = 0.1,
                                               alpha = 0.05,
                                               k.x1 = seq(2,8,1),
                                               k.x2 = c(2,3,4,8),seed=542615,
                                               adjust.correlations = F)

P3=InteractionPoweR::plot_power_curve(Example7,power_target = .9,x = "k.x1",group ="k.x2")
P3 = P3+theme(legend.position = "top")
P3



A1<-grid.arrange(
  ggdraw() + draw_label(label = "A",fontfamily = "sans",x=0.025,size = 15),
  P1,
  ncol=1,
  heights = c( .5, 5))

A2<-grid.arrange(
  ggdraw() + draw_label(label = "B",fontfamily = "sans",x=0.025,size = 15),
  P3,
  ncol=1,
  heights = c( .5, 5))

A5<-grid.arrange(
  A1,A2,
  ncol=2,
  widths = c(1,1.5))



ggsave(plot = A5,filename = "Fig7.pdf",dpi = 500,width =10,height = 4)

ggsave(plot = A5,filename = "Fig7.png",dpi = 500,width = 10,height = 4)




#############################################################


#set.seed(6453936)
D7a = InteractionPoweR::generate_interaction(#n.iter = 1000,
  N = 1000000,
  r.x1.y = 0.2,
  r.x2.y = 0.15,
  r.x1.x2 = 0,
  r.x1x2.y = 0.1
)

#D6$y = as.factor(D6$y)
P1 = InteractionPoweR::plot_interaction(data = D7a,q = 3)
P1$layers[[3]]=NULL


#set.seed(6453936)
D7b = InteractionPoweR::generate_interaction(#n.iter = 1000,
  N = 1000000,
  r.x1.y = 0.2,
  r.x2.y = 0.15,
  r.x1.x2 = 0,
  r.x1x2.y = 0.2
)

#D6$y = as.factor(D6$y)
P2 = InteractionPoweR::plot_interaction(data = D7b,q = 3)
P2$layers[[3]]=NULL



#set.seed(6453936)
D7c = InteractionPoweR::generate_interaction(#n.iter = 1000,
  N = 1000000,
  r.x1.y = 0.2,
  r.x2.y = 0.15,
  r.x1.x2 = 0,
  r.x1x2.y = 0.4
)

#D6$y = as.factor(D6$y)
P3 = InteractionPoweR::plot_interaction(data = D7c,q = 3)
P3$layers[[3]]=NULL




P1 = P1+labs(title =  expression(paste("Attenutated; ", frac(B[3],B[1]),' = ',0.5)),
             subtitle =expression(paste(cor(X[1]~X[2],Y),'=',0.1,'; ', cor(X[1],Y),'=',0.2  ) ))+
             theme(plot.subtitle = element_text(hjust = 0))+
             scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,1.5))+
             scale_x_continuous(limits = c(-5,5),breaks = seq(-5,5,2.5))

leg <- get_legend(P1 )

# Convert to a ggplot and print
leg2<-as_ggplot(leg)
leg2<-leg2+theme(plot.margin = margin(0,0,0,0))
leg2

P1 = P1+theme(legend.position = "none")

P2 = P2+labs(title = expression(paste("Knock-out; ", frac(B[3],B[1]),' = ',1.0)),
             subtitle =expression(paste(cor(X[1]~X[2],Y),'=',0.2,'; ', cor(X[1],Y),'=',0.2  ) ))+
  theme(plot.subtitle = element_text(hjust = 0),legend.position = "none")+
  scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,1.5))+
  scale_x_continuous(limits = c(-5,5),breaks = seq(-5,5,2.5))


P3 = P3+labs(title = expression(paste("Cross-over; ", frac(B[3],B[1]),' = ',2.0)),
             subtitle =expression(paste(cor(X[1]~X[2],Y),'=',0.4,'; ', cor(X[1],Y),'=',0.2  ) ))+
  theme(plot.subtitle = element_text(hjust = 0),legend.position = "none")+
  scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,1.5))+
  scale_x_continuous(limits = c(-5,5),breaks = seq(-5,5,2.5))



A1<-grid.arrange(
  ggdraw() + draw_label(label = "A",fontfamily = "sans",x=0.025,size = 15),
  P1,
  ncol=1,
  heights = c( .5, 5))

A2<-grid.arrange(
  ggdraw() + draw_label(label = "B",fontfamily = "sans",x=0.025,size = 15),
  P2,
  ncol=1,
  heights = c( .5, 5))

A3<-grid.arrange(
  ggdraw() + draw_label(label = "C",fontfamily = "sans",x=0.025,size = 15),
  P3,
  ncol=1,
  heights = c( .5, 5))

A5<-grid.arrange(
  A1,A2,A3,
  ncol=3,
  widths = c(1,1,1))

ggsave(plot = A5,filename = "Fig8.pdf",dpi = 500,width = 10,height = 4)

ggsave(plot = A5,filename = "Fig8.png",dpi = 500,width = 10,height = 4)



#######################################################################


#set.seed(6453936)
D8a = InteractionPoweR::generate_interaction(#n.iter = 1000,
  N = 1000000,
  r.x1.y = 0.2,
  r.x2.y = .4,
  r.x1.x2 = 0,
  r.x1x2.y = 0.2
)

#D6$y = as.factor(D6$y)
P1 = InteractionPoweR::plot_interaction(data = D8a,q = 3)
P1$layers[[3]]=NULL


#set.seed(6453936)
D8b = InteractionPoweR::generate_interaction(#n.iter = 1000,
  N = 1000000,
  r.x1.y = 0.2,
  r.x2.y = 0,
  r.x1.x2 = 0,
  r.x1x2.y = 0.2
)

#D6$y = as.factor(D6$y)
P2 = InteractionPoweR::plot_interaction(data = D8b,q = 3)
P2$layers[[3]]=NULL



#set.seed(6453936)
D8c = InteractionPoweR::generate_interaction(#n.iter = 1000,
  N = 1000000,
  r.x1.y = 0.2,
  r.x2.y = -.4,
  r.x1.x2 = 0,
  r.x1x2.y = 0.2
)

#D6$y = as.factor(D6$y)
P3 = InteractionPoweR::plot_interaction(data = D8c,q = 3)
P3$layers[[3]]=NULL



P1 = P1+labs(title = expression(paste("Left; ",-frac(B[2],B[3]),' = ',-2.0)),
             subtitle =expression(paste(cor(X[1]~X[2],Y),'=',0.2,'; ', cor(X[2],Y),'=',0.4  ) ))+
  theme(plot.subtitle = element_text(hjust = 0),legend.position = "none")+
  scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,1.5))+
  scale_x_continuous(limits = c(-5,5),breaks = seq(-5,5,2.5))


P2 = P2+labs(title = expression(paste("Center; ", -frac(B[2],B[3]),' = ',0.0)),
             subtitle =expression(paste(cor(X[1]~X[2],Y),'=',0.2,'; ', cor(X[2],Y),'=',0.0  ) ))+
  theme(plot.subtitle = element_text(hjust = 0),legend.position = "none")+
  scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,1.5))+
  scale_x_continuous(limits = c(-5,5),breaks = seq(-5,5,2.5))


P3 = P3+labs(title = expression(paste("Right; ", -frac(B[2],B[3]),' = ',2.0)),
             subtitle =expression(paste(cor(X[1]~X[2],Y),'=',0.2,'; ', cor(X[2],Y),'=',-0.4  ) ))+
  theme(plot.subtitle = element_text(hjust = 0),legend.position = "none")+
  scale_y_continuous(limits = c(-3,3),breaks = seq(-3,3,1.5))+
  scale_x_continuous(limits = c(-5,5),breaks = seq(-5,5,2.5))


A1<-grid.arrange(
  ggdraw() + draw_label(label = "D",fontfamily = "sans",x=0.025,size = 15),
  P1,
  ncol=1,
  heights = c( .5, 5))

A2<-grid.arrange(
  ggdraw() + draw_label(label = "E",fontfamily = "sans",x=0.025,size = 15),
  P2,
  ncol=1,
  heights = c( .5, 5))

A3<-grid.arrange(
  ggdraw() + draw_label(label = "F",fontfamily = "sans",x=0.025,size = 15),
  P3,
  ncol=1,
  heights = c( .5, 5))

A6<-grid.arrange(
  A1,A2,A3,
  ncol=3,
  widths = c(1,1,1))




ggsave(plot = A6,filename = "Fig9.pdf",dpi = 500,width = 10,height = 4)

ggsave(plot = A6,filename = "Fig9.png",dpi = 500,width = 10,height = 4)



A7<-grid.arrange(
  A5,leg2,A6,
  nrow=3,
  heights = c(1,.1,1))




ggsave(plot = A7,filename = "Fig10.pdf",dpi = 500,width = 10,height = 8)

ggsave(plot = A7,filename = "Fig10.png",dpi = 500,width = 10,height = 8)

beepr::beep(sound = 2)
