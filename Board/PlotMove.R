PlotMove<-function(from_node, to_node, GamePoints, Turn = 0){
  
  from_info = GamePoints[from_node,]
  to_info = GamePoints[which(GamePoints[,3] == to_node),]

  x.0 = from_info[[2]]
  x.1 = to_info[[2]]
  y.0 = from_info[[1]]
  y.1 = to_info[[1]]
  
  if(Turn == 0){
  segments(x.0,y.0,x.1,y.1, lwd = 2, col = 'grey50')
  }else if(Turn == 1){
    segments(x.0,y.0,x.1,y.1, lwd = 2, col = 'blue')
  }else if(Turn == 2){
    segments(x.0,y.0,x.1,y.1, lwd = 2, col = 'red')
  }
}

