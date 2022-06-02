defaultStrengthMultiplier = 3.7
strengthMultiplier = 30
labelSize = 8
heatMapColors = 4
outputWidth = 1920
outputHeight = 1080
minVertexSize = 8
minEdgeWidth = 2

pal1 <- viridis::viridis(3)

#' @import igraph
graphData <- function(dataset,xScale,yScale,maxWeight,maxStrength,groupSize,groupLabel,timepoint,positions)
{

  aGraph <- graph.data.frame(dataset[!(dataset$ae2 == ""),c("ae1","ae2","weight")],directed=FALSE)
  #aGraph <- subgraph(aGraph,c(2,1))

  E(aGraph)$weight <- E(aGraph)$weight / groupSize
  aGraph = simplify(aGraph,remove.multiple = TRUE, edge.attr.comb=list(weight="sum"),remove.loops = TRUE)

  if(maxWeight >0 )
  {
    E(aGraph)$weight = E(aGraph)$weight /maxWeight#normalize
  }else
  {
    E(aGraph)$weight = E(aGraph)$weight / max(E(aGraph)$weight)#normalize
  }

  names=names(V(aGraph))
  st=unname(graph.strength(aGraph))
  be=unname(betweenness(aGraph))
  degree = unname(degree(aGraph))

  nodeStrengthMultiplier = defaultStrengthMultiplier

  if(maxStrength >0)
  {
    st = st / maxStrength
    nodeStrengthMultiplier = strengthMultiplier
  }

  cl=unname(closeness(aGraph,normalized=FALSE))

  data = cbind.data.frame(names,strength=round(st,2),betweenness=round(be,2),closeness=round(cl,2),degree=round(degree,2))
  data$names = stringr::str_replace_all(data$names,"\n"," ")

  #V(aGraph)$label.cex = st*strengthMultiplier + minLabelSize
  V(aGraph)$label.cex = labelSize

  rainbowPal = rainbow(4)
  #heatMap = rev(brewer.pal(n = heatMapColors, name = "RdBu"))
  heatMap = RColorBrewer::brewer.pal(n = heatMapColors, name = "YlOrRd")
  #V(aGraph)$color = rainbowPal[clusters[match(names(V(aGraph)),clusters$soc),]$group]

  V(aGraph)$color = heatMap[1+round(st*heatMapColors)]

  E(aGraph)$color = pal1[1+round(E(aGraph)$weight*2)]

  #png(filename = paste0("C:\\EVOLV\\initial\\results\\e1912\\ae_allData_kk.png"),width=3840,height=2160)

  tempCo = positions

  tempCo[,1] = tempCo[,1] / outputWidth
  tempCo[,2] = tempCo[,2] / outputHeight

  tempCo[,1] = tempCo[,1] *2 - 1
  tempCo[,2] = (tempCo[,2] *2 - 1) * -1
  #should now be centered ardound 0.5

  #png(filename = paste0("C:\\ANALYSES\\EVOLV\\initial\\results\\e1912\\ae_allData_kk.png"),width=3840,height=2160)
  #plot.igraph(aGraph,layout=tempCo[match(names(V(aGraph)),names(V(allG))),])

  tempCo[,1] = tempCo[,1]*(outputWidth/outputHeight)#stretch x direction

  tempCo[,1] = tempCo[,1]*xScale#stretch y direction
  tempCo[,2] = tempCo[,2]*yScale#stretch y direction

  plot.igraph(aGraph,
              layout=tempCo[match(names(V(aGraph)),names(V(allG))),],
              vertex.label.color = "black",
              #vertex.size=be/max(be)*betweennessMultiplier+minVertexSize,
              vertex.size=st*nodeStrengthMultiplier+minVertexSize,
              edge.width=E(aGraph)$weight*30 + minEdgeWidth,
              edge.arrow.size=0.5,
              rescale = F,
              xlim=c(-1,1),
              ylim=c(-1,1))
  #dev.off()

  text(c(1.3), c(0.9), c(groupLabel),cex=8)
  legend('topleft',legend=seq(1,3),col=pal1,cex=6,lty=1,lwd=5)

  if(timepoint >= 0)
  {
    text(c(1.6), c(-0.9), c(paste0("timepoint = ",timepoint)),cex=8)
  }

  return(data)
}

getMaxWeight <- function(dataset,groupSize)
{
  aGraph <- graph.data.frame(dataset[!(dataset$ae2 == ""),c("ae1","ae2","weight")],directed=FALSE)
  #aGraph <- subgraph(aGraph,c(2,1))

  E(aGraph)$weight <- E(aGraph)$weight / groupSize
  aGraph = simplify(aGraph,remove.multiple = TRUE, edge.attr.comb=list(weight="sum"),remove.loops = TRUE)
  return(max(E(aGraph)$weight))
}

getMaxStrength <- function(dataset,maxWeight,groupSize)
{
  aGraph <- graph.data.frame(dataset[!(dataset$ae2 == ""),c("ae1","ae2","weight")],directed=FALSE)
  #aGraph <- subgraph(aGraph,c(2,1))

  E(aGraph)$weight <- E(aGraph)$weight / groupSize
  aGraph = simplify(aGraph,remove.multiple = TRUE, edge.attr.comb=list(weight="sum"),remove.loops = TRUE)

  E(aGraph)$weight = E(aGraph)$weight / maxWeight#normalize

  names=names(V(aGraph))
  st=unname(graph.strength(aGraph))

  return(max(unname(graph.strength(aGraph))))
}

getMax <- function(dataA,dataB,currentMax)
{
  newMax = currentMax

  if(dataA > newMax)
    newMax = dataA

  if(dataB > newMax)
    newMax = dataB

  return(newMax)
}

normData <- function(data)
{
  return( (data - min(data)) / (max(data) - min(data)) )
}

#' @import ggplot2
plotNetworkData <- function(gData)
{
  print(ggplot(gData, aes(x = value, y = vNames, group = group,color=group)) + geom_path(size=2) +
          #labs(title = "All Data",color = "Group") +
          labs(color = "Group") +
          #theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(20)) +
          theme(plot.title = element_text(hjust = 0.5,size=20),
                strip.text.x = element_text(size=20),
                legend.title = element_text(size=30),
                legend.text = element_text(size=30),
                axis.text.x = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
                axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = 1, vjust = 0, face = "plain"),
                axis.title.x = element_text(color = "grey20", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
                axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
          xlab("") + ylab("") + geom_point() + facet_grid( ~ type, scales = "free") )
}

graphLongData <- function(dataset,titleName,yVal,fileNameIn)
{
  png(filename = fileNameIn ,width=960,height=1080)

  g <- ggplot(dataset, aes(x = cycle, y = yVal, group = group,color=group)) + geom_path(size=2) +
    labs(title = titleName) +
    labs(color = "Group") +
    #ylim(min(yVal),max(yVal)) +
    ylim(0,max(yVal)) +
    xlim(1,6) +

    theme(plot.title = element_text(hjust = 0.5),axis.text.x = element_text(20)) +
    theme(plot.title = element_text(hjust = 0.5,size=20),
          strip.text.x = element_text(size=20),
          legend.title = element_text(size=30),
          legend.text = element_text(size=30),
          axis.text.x = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain"),
          axis.text.y = element_text(color = "grey20", size = 30, angle = 0, hjust = 1, vjust = 0, face = "plain"),
          axis.title.x = element_text(color = "grey20", size = 30, angle = 0, hjust = .5, vjust = 0, face = "plain"),
          axis.title.y = element_text(color = "grey20", size = 16, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
    xlab("") + ylab("") + geom_point() + facet_wrap( ~ names, scales = "free",ncol=1)
  print(g)
  dev.off()
  return(g)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
generate.images <- function(positions, aeList, allG, group1Name, group2Name, group1Size, group2Size){
  group1 = aeList[aeList$groupName == group1Name,]
  group2 = aeList[aeList$groupName == group2Name,]

  png(filename = file.path(results_dir, "ae_allData_weights.png"),width=3840,height=3840)
  #par(mar=c(0,0,0,0))
  par(mfrow=c(2,1))    # set the plotting area into a 1*2 array
  #graphData(rxA,1,1,"Group = A")
  allgroup1Data = graphData(group1,1,1,-1,-1,group1Size,group1Name,-1,positions)
  #graphData(rxB,1,1,"Group = B")
  alllgroup2Data = graphData(group2,1,1,-1,-1,group2Size,group2Name,-1,positions)
  #graphData(aeList)
  dev.off()

  maxWeight = 0
  maxStrength = 0

  group1 = aeList[aeList$groupName == group1Name,]
  group2 = aeList[aeList$groupName == group2Name,]

  for( i in 1:8)#groupB did not have a instance 7
  {
    group1 = aeList[cycle == i & aeList$groupName == group1Name,]
    group2 = aeList[cycle == i & aeList$groupName == group2Name,]

    maxWeight = getMax(getMaxWeight(group1,group1Size),getMaxWeight(group2,group2Size),maxWeight)
  }

  #maxStrength needs the maxWeight
  for( i in 1:8)
  {
    group1 = aeList[cycle == i & aeList$groupName == group1Name,]
    group2 = aeList[cycle == i & aeList$groupName == group2Name,]
    maxStrength = getMax(getMaxStrength(group1,maxWeight,group1Size),getMaxStrength(group2,maxWeight,group2Size),maxStrength)
  }

  #for( i in 0:max(aeList$InstanceRepeatNumber,na.rm=TRUE))
  allAData = list()
  allBData = list()
  for( i in 1:8)#groupB did not have a instance 7
  {

    group1 = aeList[cycle == i & aeList$groupName == group1Name,]
    group2 = aeList[cycle == i & aeList$groupName == group2Name,]

    png(filename = file.path(results_dir,paste0("ae_dataSet_",i,"_weights.png")),width=3840,height=3840)

    par(mfrow=c(2,1))    # set the plotting area into a 1*2 array
    #graphData(rxA,1,1,"Group = A")
    rxAData <- graphData(group1,1,1,maxWeight,maxStrength,group1Size,group1Name,-1,positions)
    #graphData(rxB,1,1,"Group = B")
    bData <- graphData(group2,1,1,maxWeight,maxStrength,group2Size,group2Name,i,positions)
    dev.off()

    gData <- as.data.frame(rbind(
      cbind(vNames=rxAData$names,group=replicate(length(rxAData$names),group1Name),type=replicate(length(rxAData$names),"Strength"),value=rxAData$strength),
      cbind(vNames=rxAData$names,group=replicate(length(rxAData$names),group1Name),type=replicate(length(rxAData$names),"Betweeness"),value=rxAData$betweenness),
      cbind(vNames=rxAData$names,group=replicate(length(rxAData$names),group1Name),type=replicate(length(rxAData$names),"Closeness"),value=rxAData$closeness),
      cbind(vNames=bData$names,group=replicate(length(bData$names),group2Name),type=replicate(length(bData$names),"Strength"),value=bData$strength),
      cbind(vNames=bData$names,group=replicate(length(bData$names),group2Name),type=replicate(length(bData$names),"Betweeness"),value=bData$betweenness),
      cbind(vNames=bData$names,group=replicate(length(bData$names),group2Name),type=replicate(length(bData$names),"Closeness"),value=bData$closeness)
    ))

    gData$value = as.numeric(as.character(gData$value))
    gData <- gData[gtools::mixedorder(gData$vNames),]

    png(filename = file.path(results_dir,paste0("ae_dataSet_",i,"_weights_plot.png")),width=1920*0.8,height=1080)
    plotNetworkData(gData)
    dev.off()

    #
    # options(width=10000)
    # printData(rxAData,bData,paste0(cwd,"\\..\\..\\results\\es5103\\weighted\\graphStats_weights.xlsx"),as.character(i))
    rxAData$cycle = i
    bData$cycle = i
    rxAData$group = group1Name
    bData$group = group2Name
    #
    allAData[[i]] = rxAData
    allBData[[i]] = bData
  }

  allGData = data.table::rbindlist(append(allAData,allBData))

  averages = allGData[,.(         strength=mean(strength),
                    betweenness = mean(betweenness),
                    closeness = mean(closeness),
                    degree = mean(degree)),by=.(names)][order(-degree),]

  baseline = allGData[allGData$cycle ==1,]

  topDegreeNames = averages[order(-averages$degree),]$names[1:5]

  topStrengthNames = unique(baseline[order(-baseline$strength),]$names)[1:5]#sort regardless of group and then take top 5 unique at baseline for strength
  topBetweenNames = unique(baseline[order(-baseline$betweenness),]$names)[1:5]
  topClosenessNames = unique(baseline[order(-baseline$closeness),]$names)[1:5]#sort regardless of group and then take top 5 unique at baseline for strength

  topDegreeFrame = as.data.frame(t(rbind(names=topDegreeNames,graphOrder=1:5)))

  temp =allGData[allGData$names %in% topDegreeNames,]

  groups=unique(temp$group)

  totalCycles = 6-1+1

  allCycles = as.data.frame(t(rbind(names=rep(topDegreeNames,totalCycles),cycle=as.numeric(rep(1:6,length(topDegreeNames))))),stringsAsFactors = FALSE)
  allCycles$cycle = as.numeric(allCycles$cycle)
  allCycles$group = groups[1]

  for(i in 2:length(groups))
  {
    temp1 = allCycles
    temp1$group = groups[i]
    allCycles = rbind(allCycles,temp1)
  }

  temp = merge(temp,allCycles,by=c("names","group","cycle"),all=TRUE)

  temp = merge(temp,topDegreeFrame,by=c("names"))
  temp = temp[order(-graphOrder,group,cycle),]

  temp[is.na(temp),] = 1

  #temp$strength = normData(temp$strength)
  #stG <- graphLongData(temp,"Strength",temp$strength,paste0(cwd,"\\..\\..\\results\\es5103\\weighted\\strengthDataLong.png"))
  degree <- graphLongData(temp,"Degree",temp$degree,file.path(results_dir,"degreeDataLong.png"))

  #temp = allGData[allGData$names %in% topBetweenNames,]
  #temp$betweenness = normData(temp$betweenness)
  btG <- graphLongData(temp,"Betweeness",temp$betweenness,file.path(results_dir,"betweenessDataLong.png"))

  # temp = allGData[allGData$names %in% topClosenessNames,]
  # temp$closeness = normData(temp$closeness)
  clG <- graphLongData(temp,"Closeness",temp$closeness,file.path(results_dir,"closenessDataLong.png"))

  png(filename = file.path(results_dir,"top5_avg_degree_over_time.png") ,width=1920,height=1080)
  figure <- ggpubr::ggarrange(degree,btG,clG,nrow=1,common.legend = TRUE,legend = "right")
  ggpubr::annotate_figure(figure,bottom = grid::textGrob("Cycle",x = 0.42,y=0.97, gp = grid::gpar(cex = 2.3)))
  dev.off()

}
