GAMSurvdataset<-function(GL,data,fu,d)
  ## GL : Gauss Lobatto rule
  ## data: survival data
  ## fu: column number containing fu info
  ## d: column number with event indicator
{
  ## append artificial ID in the set
  data$id<-1:nrow(data)
  Gllx<-data.frame(stop=rep(GL$x,length(data$id)),
                   gam.dur=rep(GL$w,length(data$id)),
                   t=rep(data[,fu],each=length(GL$x)),
                   ev=rep(data[,d],each=length(GL$x)),
                   id=rep(data$id,each=length(GL$x)),
                   gam.ev=0,start=0)
  ## Change the final indicator to what
  ## was observed, map node positions,
  ## weights from [-1,1] back to the
  ## study time
  Gllx<-transform(Gllx,
                  gam.ev=as.numeric((gam.ev | ev)*I(stop==1)),
                  gam.dur=0.5*gam.dur*(t-start),
                  stop=0.5*(stop*(t-start)+(t+start)))
  ## now merge the remaining covariate info
  Gllx<-merge(Gllx,data[,-c(fu,d)])
  Gllx
}