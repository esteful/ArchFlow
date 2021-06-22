#' Ternary Diagrams. 
#'
#' Plots diagrams showing different ternary systems based on Fe2O3, MgO, CaO, Al2O3 and SiO2 compositions. 
#'
#' @param df_raw Dataframe with the chemical composition and the catagorical values 
#' @param language 1 = Catala, 2 = Spanish, 3 = English
#' @param ordenar 1 = to avoid ordering, 2 = order the dataframe according to the given factor (plot.category)
#' @param c.palette 0 = black and white, 1 = default, 2 = rainbow, 3= heat.colors, 4 = terrain.colors, 5 = topo.colors, 6 = cm.colors
#' @param plot.category The column with categories to be colored (1,2,3...)
#' @param legend The column from which factors are colored (default = plot.category)
#' @param rounded_circle 1 = circle around the points, 2 = only dots, without circle

#' @return  Diagrams showing different ternary systems based on Fe2O3, MgO, CaO, Al2O3 and SiO2 compositions.
#' @export


"arch_triangles" <-

 #Attention! In windows uncomment the following codes "windows(record=T)"  



  function(df_raw, plot.category=1, language=3, ordenar=2, c.palette = 2, legend= plot.category, rounded_circle=1)
  {
     
    #the column with groups
    df_raw[,plot.category]<-as.factor(df_raw[,plot.category])
    
    #order
    if (ordenar==2) {df_raw<-df_raw[order(df_raw[,plot.category]), ]}
    
    #define color palettes  
    arqub<-vector(length=15)
    arqub<-c("gray75", "grey9", "cyan", "red", "goldenrod1", "dodgerblue", "darkgoldenrod4", "chartreuse1",  "darkgreen", "indianred1", "blue", "darkmagenta", "maroon1", "aquamarine", "lightpink")
    arqubBN<-vector(length=15)
    arqubBN<-c("white","grey90", "grey70", "grey50", "grey30", "grey10", "dodgerblue", "darkgoldenrod4", "chartreuse1",  "darkgreen", "indianred1", "blue", "darkmagenta", "maroon1", "aquamarine")
    
    idllegenda<-vector(length=nlevels(df_raw[,plot.category]))
    
    for (ll in 1:nlevels(df_raw[,plot.category])) {idllegenda[ll]<-ll}
    if (legend!=plot.category) {
      for (ll in 1:nlevels(df_raw[,plot.category])) {
        z<-which(df_raw[,plot.category]==levels(df_raw[,plot.category])[ll])
        idllegenda[ll]<-df_raw[z[1],legend]
      }
    }
    
    
    #color palettes
    if (c.palette==2) {
      colx<-c(rainbow(nlevels(df_raw[,plot.category])))[as.factor(df_raw[,plot.category])]
    }
    if (c.palette==3) {
      colx<-c(heat.colors(nlevels(df_raw[,plot.category])))[as.factor(df_raw[,plot.category])]
    }
    if (c.palette==4) {
      colx<-c(terrain.colors(nlevels(df_raw[,plot.category])))[as.factor(df_raw[,plot.category])]
    }
    if (c.palette==5) {
      colx<-c(topo.colors(nlevels(df_raw[,plot.category])))[as.factor(df_raw[,plot.category])]
    }
    if (c.palette==6) {
      colx<-c(cm.colors(nlevels(df_raw[,plot.category])))[as.factor(df_raw[,plot.category])]
    }
    #windows(record=T)
    par(mar=c(0,0,0,0)+0.1,mgp=c(3,1,0))
    n<-dim(df_raw)[1]
    punts<-matrix(0,n,4)
    
    #el primer triangle es el sialcap
    
    #text language
    plot(c(0,100,50,0),c(0,0,100,0),xlab="",ylab="",axes=F,type="n",xlim=c(-20,120),ylim=c(-20,120))
    if (language==1) {
      text(50,115,labels=expression(bold("Sistema CaO-Al"["2"]*"O"["3"]*"-SiO"["2"])),cex=1.4)
      text(50,108,labels="(% de massa)",font=2)
    }
    if (language==2) {
      text(50,115,labels=expression(bold("Sistema CaO-Al"["2"]*"O"["3"]*"-SiO"["2"])),cex=1.4)
      text(50,108,labels="(% de masa)",font=2)
    }
    
    if (language==3) {
      text(50,115,labels=expression(bold("CaO-Al"["2"]*"O"["3"]*"-SiO"["2"]~"System")),cex=1.4)
      text(50,108,labels="(% by mass)",font=2)}
    
    
    
    ###First triangle
    
    lines(c(0,100,50,0),c(0,0,86.60254,0))
    #lines(c(0,40,20,0),c(60,60,100,60))
    lines(c(25,62.5),c(0,64.951905),lty=4,col="grey")
    lines(c(50,75),c(0,43.30127),lty=4,col="grey")
    lines(c(75,87.5),c(0,21.650635),lty=4,col="grey")
    lines(c(12.5,87.5),c(21.650635, 21.650635),lty=3,col="grey")
    lines(c(25,75),c(43.30127, 43.30127),lty=3,col="grey")
    lines(c(37.5,62.5),c(64.951905, 64.951905),lty=3,col="grey")
    lines(c(25,12.5),c(0, 21.650635),lty=2,col="grey")
    lines(c(50,25),c(0, 43.30127),lty=2,col="grey")
    lines(c(75,37.5),c(0, 64.951905),lty=2,col="grey")
    lines(c(0,100,50,0),c(0,0, 86.60254,0))
    lines(c(50,58.245),c(86.60254,37.403637))
    lines(c(58.245,25.86),c(37.403637,44.7908337))
    lines(c(58.245,48.135),c(37.403637,18.9746165))
    lines(c(58.245,85.9),c(37.403637,24.4219163))
    lines(c(25.86,48.135),c(44.7908337, 18.9746165))
    
    text(100,-10, labels=expression(bold("Al"["2"]*"O"["3"])),pos=2,cex=1.4)
    text(60,82.272413, labels=expression(bold("SiO"["2"])),pos=4,cex=1.4)
    
    
    
    text(-5,5, labels=expression(bold("CaO")),pos=2,cex=1.4)
    text(c(1,25,50,75,98),c(-2,-2,-2,-2,-2),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(102,90.5,78,65.5,54),c(1.7320508,21.650635, 43.30127, 64.951905,85.7365146),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(48,34.5,22,9.5,-3),c(85.7365146, 64.951905, 43.30127, 21.650635, 1.7320508),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(50,30.3,59.5,82.5,48.135),c(89.2006162,45.8993462,35.0740287,23.25278199,17.320508), labels=c("Qz","Wo","An","Mul","Gh"),cex=1,font=2)
    
    
    
    punts[,1]<-df_raw$"Al2O3"
    punts[,2]<-df_raw$"SiO2" 
    punts[,3]<-df_raw$"CaO"
    punts[,c(1:3)]<-sweep(punts[,c(1:3)]/0.01,1,apply(punts[,c(1:3)],1,sum),FUN="/")
    
    #La coordenada df_raw es el valor de leix inferior mes el catet del triangle rectangle. La seva hipotenusa
    #es el valor del eix dret i es multiplica pel sinus de 30 graus, que es 0.5
    punts[,4]<-punts[,1]+(punts[,2]/2)
    
    
    
    
    
    #color palettes first triangle
    if (c.palette==0) {
      if (rounded_circle==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[df_raw[w,plot.category]])
        }
        legend(-15,91, bty="n",pch=19, legend = unique(df_raw[,plot.category]), col = arqubBN[1:nlevels(df_raw[,plot.category])])
      }
      if (rounded_circle==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[df_raw[w,plot.category]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqubBN[1:nlevels(df_raw[,plot.category])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    
    if (c.palette==1) {
      if (rounded_circle==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[df_raw[w,plot.category]])
        }
        legend(-15,91, bty="n",pch=19, legend = unique(df_raw[,plot.category]), col = arqub[1:nlevels(df_raw[,plot.category])])
      }
      if (rounded_circle==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[df_raw[w,plot.category]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend = unique(df_raw[,plot.category]), col = arqub[1:nlevels(df_raw[,plot.category])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==2) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = rainbow(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = rainbow(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==3) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = heat.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = heat.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==4) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = terrain.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = terrain.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==5) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = topo.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = topo.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==6) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = cm.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = cm.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    
    #plot first triangle
    xxx<-recordPlot()
    emf("triangle1.emf")
    replayPlot(xxx)
    dev.off()
    pdf("triangle1.pdf")
    replayPlot(xxx)
    dev.off()
    
    
    
    ##Second triangle 
    par(mar=c(0,0,0,0)+0.1,mgp=c(3,1,0))
    n<-dim(df_raw)[1]
    punts<-matrix(0,n,4)
    plot(c(0,100,50,0),c(0,0,100,0),xlab="",ylab="",axes=F,type="n",xlim=c(-20,120),ylim=c(-20,120))
    if (language==1) {
      text(50,115,labels=expression(bold("Triangle ceramic")),cex=1.4)
      text(50,108,labels="(% de massa)",font=2)
    }
    if (language==2) {
      text(50,115,labels=expression(bold("Triangulo ceramico")),cex=1.4)
      text(50,108,labels="(% de masa)",font=2)
    }
    if (language==3) {
      text(50,115,labels=expression(bold("Ceramic triangle")),cex=1.4)
      text(50,108,labels="(% by mass)",font=2)
    }
    lines(c(0,100,50,0),c(0,0,86.60254,0))
    #lines(c(0,40,20,0),c(60,60,100,60))
    lines(c(25,62.5),c(0,64.951905),lty=4,col="grey")
    lines(c(50,75),c(0,43.30127),lty=4,col="grey")
    lines(c(75,87.5),c(0,21.650635),lty=4,col="grey")
    lines(c(12.5,87.5),c(21.650635, 21.650635),lty=3,col="grey")
    lines(c(25,75),c(43.30127, 43.30127),lty=3,col="grey")
    lines(c(37.5,62.5),c(64.951905, 64.951905),lty=3,col="grey")
    lines(c(25,12.5),c(0, 21.650635),lty=2,col="grey")
    lines(c(50,25),c(0, 43.30127),lty=2,col="grey")
    lines(c(75,37.5),c(0, 64.951905),lty=2,col="grey")
    lines(c(0,100,50,0),c(0,0, 86.60254,0))
    lines(c(50,58.245),c(86.60254,37.403637))
    lines(c(58.245,25.86),c(37.403637,44.7908337))
    lines(c(58.245,48.135),c(37.403637,18.9746165))
    lines(c(58.245,85.9),c(37.403637,24.4219163))
    lines(c(25.86,48.135),c(44.7908337, 18.9746165))
    text(100,-10, labels=expression(bold("Al"["2"]*"O"["3"])),pos=2,cex=1.4)
    text(60,82.272413, labels=expression(bold("SiO"["2"])),pos=4,cex=1.4)
    
    
    
    text(2.8,14.9, labels=expression(bold("Fe"["2"]*"O"["3"]~"+")),pos=2,cex=1.4)
    text(0.2,9.2, labels=expression(bold("MgO +")),pos=2,cex=1.4)
    text(-5,5, labels=expression(bold("CaO")),pos=2,cex=1.4)
    text(c(1,25,50,75,98),c(-2,-2,-2,-2,-2),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(102,90.5,78,65.5,54),c(1.7320508,21.650635, 43.30127, 64.951905,85.7365146),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(48,34.5,22,9.5,-3),c(85.7365146, 64.951905, 43.30127, 21.650635, 1.7320508),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(50,30.3,59.5,82.5,48.135),c(89.2006162,45.8993462,35.0740287,23.25278199,17.320508), labels=c("Qz","Wo","An","Mul","Gh"),cex=1,font=2)
    
    
    
    punts[,1]<-df_raw[,"Al2O3"]
    punts[,2]<-df_raw[,"SiO2"] 
    punts[,3]<-df_raw[,"CaO"]+ df_raw[,"MgO"]+ df_raw[,"Fe2O3"]
    punts[,c(1:3)]<-sweep(punts[,c(1:3)]/0.01,1,apply(punts[,c(1:3)],1,sum),FUN="/")
    
    
    
    #La coordenada df_raw es el valor de l’eix inferior mes el catet del triangle rectangle. La seva hipotenusa
    #es el valor del eix dret i es multiplica pel sinus de 30 graus, que es 0.5
    punts[,4]<-punts[,1]+(punts[,2]/2)
    if (c.palette==0) {
      if (rounded_circle==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[df_raw[w,plot.category]])
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqubBN[1:nlevels(df_raw[,plot.category])])
      }
      if (rounded_circle==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[df_raw[w,plot.category]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqubBN[1:nlevels(df_raw[,plot.category])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==1) {
      if (rounded_circle==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[df_raw[w,plot.category]])
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqub[1:nlevels(df_raw[,plot.category])])
      }
      if (rounded_circle==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[df_raw[w,plot.category]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqub[1:nlevels(df_raw[,plot.category])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==2) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = rainbow(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = rainbow(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==3) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = heat.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = heat.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==4) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = terrain.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = terrain.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==5) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = topo.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = topo.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==6) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = cm.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = cm.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    xxx<-recordPlot()
    emf("triangle2.emf")
    replayPlot(xxx)
    dev.off()
    pdf("triangle2.pdf")
    replayPlot(xxx)
    dev.off()
    
    
    #third trianle 
    
    par(mar=c(0,0,0,0)+0.1,mgp=c(3,1,0))
    n<-dim(df_raw)[1]
    punts<-matrix(0,n,4)
    plot(c(0,100,50,0),c(0,0,100,0),xlab="",ylab="",axes=F,type="n",xlim=c(-20,120),ylim=c(-20,120))
    
    
    if (language==1) {
      text(50,115,labels=expression(bold("Sistema CaO-MgO-SiO"["2"])),cex=1.4)
      text(50,108,labels="(% de massa)",font=2)
    }
    if (language==2) {
      text(50,115,labels=expression(bold("Sistema CaO-MgO-SiO"["2"])),cex=1.4)
      text(50,108,labels="(% de masa)",font=2)
    }
    
    if (language==3) {
      text(50,115,labels=expression(bold("CaO-MgO-SiO"["2"]~"System")),cex=1.4)
      text(50,108,labels="(% by mass)",font=2)
    }
    
    
    
    
    lines(c(0,100,50,0),c(0,0,86.60254,0))
    #lines(c(0,40,20,0),c(60,60,100,60))
    #es dibuixa el grid
    lines(c(25,62.5),c(0,64.951905),lty=4,col="grey")
    lines(c(50,75),c(0,43.30127),lty=4,col="grey")
    lines(c(75,87.5),c(0,21.650635),lty=4,col="grey")
    lines(c(12.5,87.5),c(21.650635, 21.650635),lty=3,col="grey")
    lines(c(25,75),c(43.30127, 43.30127),lty=3,col="grey")
    lines(c(37.5,62.5),c(64.951905, 64.951905),lty=3,col="grey")
    lines(c(25,12.5),c(0, 21.650635),lty=2,col="grey")
    lines(c(50,25),c(0, 43.30127),lty=2,col="grey")
    lines(c(75,37.5),c(0, 64.951905),lty=2,col="grey")
    #es dibuixa el triangle
    lines(c(0,100,50,0),c(0,0, 86.60254,0))
    #es dibuixen els triangles d’equilibri termodinamic
    lines(c(50,46.355),c(86.60254,48.0557494))
    lines(c(25.86, 46.355),c(44.7908337,48.0557494))
    lines(c(70.075, 46.355),c(51.8316202, 48.0557494))
    lines(c(36.825, 46.355),c(38.1657394, 48.0557494))
    lines(c(78.65, 46.355),c(36.9792846, 48.0557494))
    lines(c(36.825, 25.86),c(38.1657394, 44.7908337))
    lines(c(36.825, 78.65),c(38.1657394, 36.9792846))
    lines(c(36.825, 44.96),c(38.1657394, 33.2553754))
    lines(c(78.65, 44.96),c(36.9792846, 33.2553754))
    
    
    #etiquetes del triangle
    text(100,-10, labels=expression(bold("MgO")),pos=2,cex=1.4)
    text(60,82.272413, labels=expression(bold("SiO"["2"])),pos=4,cex=1.4)
    text(-5,5, labels=expression(bold("CaO")),pos=2,cex=1.4)
    
    
    
    #etiquetes dels eixos
    text(c(1,25,50,75,98),c(-2,-2,-2,-2,-2),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(102,90.5,78,65.5,54),c(1.7320508,21.650635, 43.30127, 64.951905,85.7365146),labels=c("0","25","50","75","100"),cex=0.8)
    text(c(48,34.5,22,9.5,-3),c(85.7365146, 64.951905, 43.30127, 21.650635, 1.7320508),labels=c("0","25","50","75","100"),cex=0.8)
    
    #etiquetes de les fases
    text(c(50,27.2,46.9,69.4,35.4,77.6,44.96),c(89.2006162, 41.6,45.2, 49.8, 37,35,31), labels=c("Qz","Wo","Di","En","Ak","Fo","Mtc"),cex=1,font=2)
    
    punts[,1]<-df_raw[,"MgO"]
    punts[,2]<-df_raw[,"SiO2"] 
    punts[,3]<-df_raw[,"CaO"]
    punts[,c(1:3)]<-sweep(punts[,c(1:3)]/0.01,1,apply(punts[,c(1:3)],1,sum),FUN="/")
    
    #La coordenada df_raw es el valor de l’eix inferior mes el catet del triangle rectangle. La seva hipotenusa
    #es el valor del eix dret i es multiplica pel sinus de 30 graus, que es 0.5
    punts[,4]<-punts[,1]+(punts[,2]/2)
    
    
    ###different color palettes
    if (c.palette==0) {
      if (rounded_circle==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[df_raw[w,plot.category]])
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqubBN[1:nlevels(df_raw[,plot.category])])
      }
      if (rounded_circle==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqubBN[df_raw[w,plot.category]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqubBN[1:nlevels(df_raw[,plot.category])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==1) {
      if (rounded_circle==2) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[df_raw[w,plot.category]])
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqub[1:nlevels(df_raw[,plot.category])])
      }
      if (rounded_circle==1) {
        for (w in 1:n) {
          points(punts[w,4],(punts[w,2]*0.8660254),pch=19,cex=0.8,col=arqub[df_raw[w,plot.category]])
          points(punts[w,4],(punts[w,2]*0.8660254),pch=21,cex=0.8)
        }
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = arqub[1:nlevels(df_raw[,plot.category])])
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==2) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = rainbow(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = rainbow(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==3) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = heat.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = heat.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==4) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = terrain.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = terrain.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==5) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = topo.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = topo.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }
    if (c.palette==6) {
      if (rounded_circle==2) {
        points(punts[,4],(punts[,2]*0.8660254),pch=19,cex=0.8,col=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = cm.colors(nlevels(df_raw[,plot.category])))
      }
      if (rounded_circle==1) {
        points(punts[,4],(punts[,2]*0.8660254),pch=21,cex=0.8,bg=colx)
        legend(-15,91, bty="n",pch=19, legend =  unique(df_raw[,plot.category]), col = cm.colors(nlevels(df_raw[,plot.category])))
        legend(-15,91, bty="n",pch=21, legend = rep("",nlevels(df_raw[,plot.category])))
      }
    }


    
    ###output plots 
      xxx<-recordPlot()
      emf("triangle3.emf")
      replayPlot(xxx)
      dev.off()
      pdf("triangle3.pdf")
      replayPlot(xxx)
      dev.off()
      #fi i posem els marges per defecte
      par(mar=c(5,4,4,2)+0.1,mgp=c(3,1,0))
      palette("default")
    
    
  }
